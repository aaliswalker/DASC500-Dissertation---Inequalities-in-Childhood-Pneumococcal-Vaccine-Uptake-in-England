#===============================================================================
# SCRIPT: 04_susceptibility_modeling.R
# PURPOSE: Calculate and visualize pneumococcal disease susceptibility patterns
# 
# DESCRIPTION: 
# - Implements susceptibility modeling using vaccine effectiveness estimates
# - Uses 1-year lag methodology (same birth cohorts over time)
# - Uses non-imputed, boundary-corrected data
# - Calculates susceptibility by deprivation quintile and geographic area
# - Compares baseline vs alternate vaccine effectiveness assumptions
# - Generates susceptibility trend plots and geographic maps
# - Performs validation analysis for individual local authorities
#
# MAIN SECTIONS:
# - Vaccine effectiveness scenario definitions
# - 1-year lag susceptibility calculations by deprivation quintile
# - 1-year lag national level susceptibility calculations  
# - Geographic susceptibility mapping
# - London inclusion/exclusion analysis
# - Model validation and summary statistics
# - Uptake geographic figures
#
# INPUTS: 
# - output/COVER_All_Years_MERGED_WITH_IMD_NO_IMPUTATION.csv
# - data/Shapefile/Local_Authority_(Upper_Tier)_IMD_2019_(WGS84).shp
# OUTPUTS: Susceptibility trend plots, geographic maps, validation statistics, Uptake geographic figures
#===============================================================================

# =============================================================================
# LOAD LIBRARIES
# =============================================================================
library("tidyverse")
library("readr")
library("ggplot2")
library("viridis")
library("sf")
library("dplyr")
library("ggrepel")
library("patchwork")
library("ggnewscale")
library("grid")
library("stringr")
library("knitr")
library("kableExtra")
library("httr")
library("jsonlite")
library("here")

# =============================================================================
# LOAD DATA
# =============================================================================

# Load boundary-corrected, non-imputed data from Script 2 output
data_file <- here("output", "COVER_All_Years_MERGED_WITH_IMD_NO_IMPUTATION.csv")
shapefile_path <- here("data", "Shapefile", "Local_Authority_(Upper_Tier)_IMD_2019_(WGS84).shp")

# Load data and map
data_clean <- read.csv(data_file)
england_map <- st_read(shapefile_path)


if (!file.exists(data_file)) {
  stop("Boundary-corrected merged data not found. Please run Script 02 first.")
}

data_clean <- read.csv(data_file)

cat("=== DATA VERIFICATION ===\n")
cat("Using dataset:", basename(data_file), "\n")
cat("Data range:", min(data_clean$Year, na.rm = TRUE), "to", max(data_clean$Year, na.rm = TRUE), "\n")
cat("Total observations:", nrow(data_clean), "\n")
cat("Missing 12m data:", sum(is.na(data_clean$PCV_12m)), "\n")
cat("Missing 24m data:", sum(is.na(data_clean$PCV_24m)), "\n")

# Define color palette
pastel_palette <- c("#88CCEE", "#CC6677", "#117733", "#DDCC77", "#332288", "#EE7733", "#44AA99")

# =============================================================================
# VACCINE EFFECTIVENESS SCENARIOS
# =============================================================================

# Define VE scenarios based on manuscript Table 1 (central estimates only)
ve_scenarios <- list(
  # Baseline assumption: 1+1 single dose has lower effectiveness
  list(
    name = "Baseline Assumption",
    set = 1,
    assumption = "baseline",
    ve_2plus1_primary = 0.761,      # 2+1 primary: 76.1%
    ve_1plus1_primary = 0.606,      # 1+1 baseline: 60.6%
    ve_booster = 0.782              # Booster: 78.2%
  ),
  
  # Alternate assumption: 1+1 single dose matches 2+1 primary effectiveness
  list(
    name = "Alternate Assumption",
    set = 2,
    assumption = "alternate",
    ve_2plus1_primary = 0.761,      # 2+1 primary: 76.1%
    ve_1plus1_primary = 0.761,      # 1+1 alternate: 76.1% (same as 2+1)
    ve_booster = 0.782              # Booster: 78.2%
  )
)

# =============================================================================
# SUSCEPTIBILITY CALCULATIONS WITH 1-YEAR LAG BY DEPRIVATION QUINTILE
# =============================================================================

cat("\n=== IMPLEMENTING SUSCEPTIBILITY CALCULATION ===\n")
cat("Methodology: 1-year lag for primary coverage\n")
cat("- p0 = unvaccinated (from lagged 12m data)\n")
cat("- p1 = primary only (lagged 12m - current 24m)\n")
cat("- p2 = fully protected (current 24m)\n\n")

# Create lagged dataset for susceptibility calculations
create_lagged_susceptibility_data <- function(data) {
  data %>%
    arrange(ONS_Code, Year, Quarter) %>%
    group_by(ONS_Code) %>%
    mutate(
      # Create 1-year lag for 12-month coverage
      PCV_12m_lag = lag(PCV_12m, n = 4),  # 4 quarters = 1 year
      Population_12m_lag = lag(Population_12m, n = 4)
    ) %>%
    ungroup() %>%
    # Only include data where we have both lagged and current data
    filter(!is.na(PCV_12m_lag) & !is.na(PCV_24m)) %>%
    # Exclude 2013/2014 due to insufficient lag data
    filter(Year != "2013/2014")
}

data_clean_lagged <- create_lagged_susceptibility_data(data_clean)

cat("Observations after 1-year lag implementation:", nrow(data_clean_lagged), "\n")
cat("Available years for susceptibility analysis:", 
    paste(sort(unique(data_clean_lagged$Year)), collapse = ", "), "\n\n")

quarterly_deprivation_all_scenarios <- map_dfr(ve_scenarios, function(scenario) {
  data_clean_lagged %>%
    group_by(Year, Quarter, imd_quintile) %>%
    summarise(
      # Calculate weighted average coverage
      PCV_12m_lag_pct = weighted.mean(PCV_12m_lag, Population_12m_lag, na.rm = TRUE),
      PCV_24m_pct = weighted.mean(PCV_24m, Population_24m, na.rm = TRUE),
      Population = sum(Population_12m_lag, na.rm = TRUE),  # Use lagged population for weighting
      .groups = "drop"
    ) %>%
    filter(!is.na(imd_quintile)) %>%
    arrange(Year, Quarter, imd_quintile) %>%
    mutate(
      # Convert to proportions 
      PCV_12m_lag_prop = pmax(0, pmin(1, PCV_12m_lag_pct / 100)),
      PCV_24m_prop = pmax(0, pmin(1, PCV_24m_pct / 100)),
      
      # Calculate population proportions using 1-year lag methodology
      p0 = 1 - pmax(PCV_12m_lag_prop, PCV_24m_prop),               # Unvaccinated (from larger of lagged 12m or current 24m)
      p1 = pmax(0, PCV_12m_lag_prop - PCV_24m_prop),               # Primary only (lagged 12m - current 24m)
      p2 = PCV_24m_prop,                                           # Fully protected (current 24m)
      
      # Determine schedule based on year
      schedule_group = if_else(Year < "2020/2021", "2+1", "1+1"),
      
      # Assign VE based on schedule and scenario
      VE_primary = case_when(
        schedule_group == "2+1" ~ scenario$ve_2plus1_primary,
        schedule_group == "1+1" ~ scenario$ve_1plus1_primary
      ),
      VE_booster = scenario$ve_booster,
      
      # Calculate susceptibility using 1-year lag methodology
      Susceptibility_prop = p0 + p1 * (1 - VE_primary) + p2 * (1 - VE_booster),
      Susceptibility_n = Susceptibility_prop * Population,
      
      # Metadata
      YearQuarter = paste0(Year, " ", Quarter),
      scenario_name = scenario$name,
      scenario_set = scenario$set,
      ve_assumption = scenario$assumption
    ) %>%
    filter(!is.na(Susceptibility_prop) & !is.infinite(Susceptibility_prop))
})

# =============================================================================
# NATIONAL LEVEL SUSCEPTIBILITY CALCULATIONS WITH 1-YEAR LAG
# =============================================================================

quarterly_schedule_all_scenarios <- map_dfr(ve_scenarios, function(scenario) {
  data_clean_lagged %>%
    group_by(Year, Quarter) %>%
    summarise(
      PCV_12m_lag_pct = weighted.mean(PCV_12m_lag, Population_12m_lag, na.rm = TRUE),
      PCV_24m_pct = weighted.mean(PCV_24m, Population_24m, na.rm = TRUE),
      Population = sum(Population_12m_lag, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(Year, Quarter) %>%
    mutate(
      # Convert to proportions
      PCV_12m_lag_prop = pmax(0, pmin(1, PCV_12m_lag_pct / 100)),
      PCV_24m_prop = pmax(0, pmin(1, PCV_24m_pct / 100)),
      
      # Population proportions using 1-year lag
      p0 = 1 - pmax(PCV_12m_lag_prop, PCV_24m_prop),     # Unvaccinated (from larger of lagged 12m or current 24m)
      p1 = pmax(0, PCV_12m_lag_prop - PCV_24m_prop),     # Primary only 
      p2 = PCV_24m_prop,                                 # Fully protected 
      
      # Schedule determination
      schedule_group = if_else(Year < "2020/2021", "2+1", "1+1"),
      
      # VE assignment
      VE_primary = case_when(
        schedule_group == "2+1" ~ scenario$ve_2plus1_primary,
        schedule_group == "1+1" ~ scenario$ve_1plus1_primary
      ),
      VE_booster = scenario$ve_booster,
      
      # Susceptibility calculation using 1-year lag
      Susceptibility_prop = p0 + p1 * (1 - VE_primary) + p2 * (1 - VE_booster),
      Susceptibility_n = Susceptibility_prop * Population,
      
      # Metadata
      YearQuarter = paste0(Year, " ", Quarter),
      scenario_name = scenario$name,
      scenario_set = scenario$set,
      ve_assumption = scenario$assumption
    ) %>%
    filter(!is.na(Susceptibility_prop) & !is.infinite(Susceptibility_prop))
})

# =============================================================================
# GEOGRAPHIC DATA PREPARATION
# =============================================================================


# Calculate geographic susceptibility
la_susceptibility_all_scenarios <- map_dfr(ve_scenarios, function(scenario) {
  data_clean_lagged %>%
    group_by(Year, ONS_Code, utla_name) %>%
    summarise(
      PCV_12m_lag_pct = weighted.mean(PCV_12m_lag, Population_12m_lag, na.rm = TRUE),
      PCV_24m_pct = weighted.mean(PCV_24m, Population_24m, na.rm = TRUE),
      Population = sum(Population_12m_lag, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      # Convert to proportions
      PCV_12m_lag_prop = pmax(0, pmin(1, PCV_12m_lag_pct / 100)),
      PCV_24m_prop = pmax(0, pmin(1, PCV_24m_pct / 100)),
      
      # Population proportions using 1-year lag
      p0 = 1 - pmax(PCV_12m_lag_prop, PCV_24m_prop),     # Unvaccinated (from larger of lagged 12m or current 24m)
      p1 = pmax(0, PCV_12m_lag_prop - PCV_24m_prop),     # Primary only (lagged - current)
      p2 = PCV_24m_prop,                                 # Fully protected (current)
      
      # Schedule determination
      schedule_group = if_else(Year < "2020/2021", "2+1", "1+1"),
      
      # VE assignment
      VE_primary = case_when(
        schedule_group == "2+1" ~ scenario$ve_2plus1_primary,
        schedule_group == "1+1" ~ scenario$ve_1plus1_primary
      ),
      VE_booster = scenario$ve_booster,
      
      # Susceptibility calculation using 1-year lag
      Susceptibility = p0 + p1 * (1 - VE_primary) + p2 * (1 - VE_booster),
      
      # Metadata
      scenario_name = scenario$name,
      scenario_set = scenario$set,
      ve_assumption = scenario$assumption,
      Year = as.character(Year),
      ONS_Code = as.character(ONS_Code)
    ) %>%
    filter(!is.na(Susceptibility) & !is.infinite(Susceptibility))
})

# Handle Northamptonshire boundary changes
# Find Northamptonshire code in shapefile
northants_shapefile_code <- england_map %>%
  filter(ctyua19nm == "Northamptonshire") %>%
  pull(ctyua19cd) %>%
  unique()

if(length(northants_shapefile_code) == 0) {
  northants_shapefile_code <- "E10000021"  # Default old code
}

# Aggregate split Northamptonshire data
northants_aggregated <- la_susceptibility_all_scenarios %>%
  filter(utla_name %in% c("West Northamptonshire", "North Northamptonshire")) %>%
  group_by(Year, scenario_name, scenario_set, ve_assumption, schedule_group) %>%
  summarise(
    Susceptibility = mean(Susceptibility, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    ONS_Code = northants_shapefile_code,
    utla_name = "Northamptonshire"
  )

# Add aggregated Northamptonshire back to dataset
la_susceptibility_all_scenarios <- bind_rows(
  la_susceptibility_all_scenarios,
  northants_aggregated
)

# =============================================================================
# SUSCEPTIBILITY TRENDS BY DEPRIVATION
# =============================================================================

# Prepare data for visualization
main_scenarios <- quarterly_deprivation_all_scenarios %>%
  mutate(
    time_order = as.numeric(factor(YearQuarter, levels = unique(YearQuarter))),
    assumption_label = if_else(
      ve_assumption == "baseline",
      "Baseline: 1+1 single dose = 60.6% VE",
      "Alternate: 1+1 single dose = 76.1% VE"
    )
  )

# Create susceptibility trend plot
suscep_trend_plot <- ggplot(main_scenarios, aes(x = time_order, y = Susceptibility_prop)) +
  geom_line(
    aes(color = as.factor(imd_quintile)),
    linewidth = 1.8
  ) +
  geom_vline(
    xintercept = which(unique(main_scenarios$YearQuarter) == "2019/2020 Q4"),
    linetype = "dashed",
    color = "gray40",
    linewidth = 0.8
  ) +
  facet_wrap(~assumption_label, ncol = 1) +
  scale_color_manual(
    name = "IMD Quintile",
    values = pastel_palette[1:5],
    labels = c("1 (Least deprived)", "2", "3", "4", "5 (Most deprived)")
  ) +
  scale_x_continuous(
    breaks = seq(1, max(main_scenarios$time_order), by = 4),
    labels = unique(main_scenarios$YearQuarter)[seq(1, length(unique(main_scenarios$YearQuarter)), by = 4)]
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    #title = "Susceptibility Trends with 1-Year Lag Methodology",
    x = "Year & Quarter",
    y = "Proportion Susceptible",
    caption = "Vertical line indicates schedule change (Jan 2020)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    strip.text = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    legend.key.width = unit(1.5, "cm"),
    legend.key.height = unit(0.8, "cm"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 12, color = "grey60"),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold")
  )

print(suscep_trend_plot)

# =============================================================================
# GEOGRAPHIC SUSCEPTIBILITY MAPS
# =============================================================================

# Prepare map data (baseline assumption only for main figure)
map_data_main <- england_map %>%
  left_join(
    la_susceptibility_all_scenarios %>% filter(scenario_set == 1),
    by = c("ctyua19cd" = "ONS_Code")
  ) %>%
  mutate(
    schedule_group = case_when(
      !is.na(schedule_group) ~ schedule_group,
      Year < "2020/2021" ~ "2+1",
      Year >= "2020/2021" ~ "1+1",
      TRUE ~ NA_character_
    ),
    Schedule = factor(
      schedule_group,
      levels = c("2+1", "1+1"),
      labels = c("2+1 Schedule", "1+1 Schedule")
    ),
    Year_Schedule = paste(Year, Schedule, sep = " - ")
  ) %>%
  filter(!is.na(Year) & !is.na(Susceptibility))

# Select representative years for visualization (adjusted for lag)
selected_years <- c("2019/2020", "2020/2021", "2021/2022")
map_data_subset <- map_data_main %>%
  filter(Year %in% selected_years)

# Create geographic susceptibility map
geographic_suscep_map <- ggplot(map_data_subset) +
  geom_sf(aes(fill = Susceptibility), color = "white", linewidth = 0.05) +
  scale_fill_viridis_c(
    name = "Proportion Susceptible",  
    option = "plasma",
    direction = 1,
    limits = c(0.20, 0.40),
    breaks = seq(0.20, 0.40, by = 0.05),
    labels = scales::percent_format(accuracy = 1),
    guide = guide_colorbar(
      title.position = "top",        
      title.hjust = 0.5,             
      barwidth = unit(15, "cm"),     
      barheight = unit(0.6, "cm"),   
      ticks = TRUE,                  
      ticks.colour = "black",        
      ticks.linewidth = 0.5,         
      frame.colour = "black",        
      frame.linewidth = 0.5          
    )
  ) +
  facet_wrap(~ Year_Schedule, ncol = 3) +
  labs(
    #title = " Geographic Susceptibility",
    #subtitle = "Baseline assumption: 1+1 single dose VE = 60.6%",
    #caption = "Central VE estimates"
  ) +
  theme_void(base_size = 10) +
  theme(
    strip.text = element_text(size = 9, margin = margin(b = 2), face = "bold"),
    legend.position = "bottom",
    legend.box = "horizontal",              
    legend.title = element_text(
      size = 10,                            
      face = "bold",                        
      margin = margin(b = 5)                
    ),
    legend.text = element_text(
      size = 9,                             
      margin = margin(t = 5)                
    ),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
    plot.caption = element_text(hjust = 0.5, size = 8, color = "grey60"),
    plot.margin = margin(10, 10, 15, 10),   
    panel.spacing = unit(0.3, "lines"),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold")
  )

print(geographic_suscep_map)

# Create alternate assumption map for supplementary materials
map_data_alternate <- england_map %>%
  left_join(
    la_susceptibility_all_scenarios %>% filter(scenario_set == 2),  
    by = c("ctyua19cd" = "ONS_Code")
  ) %>%
  filter(!is.na(Year) & !is.na(Susceptibility)) %>%
  mutate(
    Schedule = factor(
      schedule_group,
      levels = c("2+1", "1+1"),
      labels = c("2+1 Schedule", "1+1 Schedule")
    ),
    Year_Schedule = paste(Year, Schedule, sep = " - ")
  ) %>%
  filter(Year %in% selected_years)

alternate_geographic_suscep_map <- ggplot(map_data_alternate) +
  geom_sf(aes(fill = Susceptibility), color = "white", linewidth = 0.05) +
  scale_fill_viridis_c(
    name = "Proportion Susceptible",  
    option = "plasma",
    direction = 1,
    limits = c(0.20, 0.40),
    breaks = seq(0.20, 0.40, by = 0.05),
    labels = scales::percent_format(accuracy = 1),
    guide = guide_colorbar(
      title.position = "top",        
      title.hjust = 0.5,             
      barwidth = unit(15, "cm"),     
      barheight = unit(0.6, "cm"),   
      ticks = TRUE,                  
      ticks.colour = "black",        
      ticks.linewidth = 0.5,         
      frame.colour = "black",        
      frame.linewidth = 0.5          
    )
  ) +
  facet_wrap(~ Year_Schedule, ncol = 3) +
  labs(
    # title = "Geographic Susceptibility - Alternate Assumption",
    # subtitle = "Alternate assumption: 1+1 single dose VE = 76.1%",
    # caption = "Central VE estimates | Uses 1-year lag for primary coverage"
  ) +
  theme_void(base_size = 10) +
  theme(
    strip.text = element_text(size = 9, margin = margin(b = 2), face = "bold"),
    legend.position = "bottom",
    legend.box = "horizontal",              
    legend.title = element_text(
      size = 10,                            
      face = "bold",                        
      margin = margin(b = 5)                
    ),
    legend.text = element_text(
      size = 9,                             
      margin = margin(t = 5)                
    ),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
    plot.caption = element_text(hjust = 0.5, size = 8, color = "grey60"),
    plot.margin = margin(10, 10, 15, 10),  
    panel.spacing = unit(0.3, "lines"),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold")
  )

print(alternate_geographic_suscep_map)

# =============================================================================
# LONDON ANALYSIS - WITH AND WITHOUT LONDON
# =============================================================================

# Identify London UTLAs using ONS codes (E09 prefix)
london_utlas_e09 <- data_clean %>%
  filter(str_starts(ONS_Code, "E09")) %>%
  pull(utla_name) %>%
  unique() %>%
  sort()

# Create dataset excluding London 
data_no_london <- data_clean %>%
  filter(!str_starts(ONS_Code, "E09"))

# Calculate coverage trends with and without London (using original data for comparison)
pcv_with_london_12m <- data_clean %>%
  group_by(Year, Quarter, imd_quintile) %>%
  summarise(mean_PCV_12m = mean(PCV_12m, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Quarter_Label = paste(Year, Quarter, sep = " "),
    Dataset = "With London"
  )

pcv_without_london_12m <- data_no_london %>%
  group_by(Year, Quarter, imd_quintile) %>%
  summarise(mean_PCV_12m = mean(PCV_12m, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Quarter_Label = paste(Year, Quarter, sep = " "),
    Dataset = "Without London"
  )

pcv_with_london_24m <- data_clean %>%
  group_by(Year, Quarter, imd_quintile) %>%
  summarise(mean_PCV_24m = mean(PCV_24m, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Quarter_Label = paste(Year, Quarter, sep = " "),
    Dataset = "With London"
  )

pcv_without_london_24m <- data_no_london %>%
  group_by(Year, Quarter, imd_quintile) %>%
  summarise(mean_PCV_24m = mean(PCV_24m, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Quarter_Label = paste(Year, Quarter, sep = " "),
    Dataset = "Without London"
  )

# Combine data for comparison plots
pcv_comparison_12m <- bind_rows(pcv_with_london_12m, pcv_without_london_12m) %>%
  mutate(Quarter_Label = factor(Quarter_Label, levels = unique(paste(Year, Quarter, sep = " "))))

pcv_comparison_24m <- bind_rows(pcv_with_london_24m, pcv_without_london_24m) %>%
  mutate(Quarter_Label = factor(Quarter_Label, levels = unique(paste(Year, Quarter, sep = " "))))

# Set time markers for plots
all_quarters <- unique(pcv_comparison_12m$Quarter_Label)
change_point <- which(levels(pcv_comparison_12m$Quarter_Label) == "2020/2021 Q1")
covid_start <- change_point
covid_end <- which(levels(pcv_comparison_12m$Quarter_Label) == "2021/2022 Q4")

# Create comparison plot for 12-month coverage
plot_12m_comparison <- ggplot(pcv_comparison_12m, 
                              aes(x = Quarter_Label, y = mean_PCV_12m, 
                                  color = as.factor(imd_quintile), 
                                  group = imd_quintile)) +
  annotate("rect", xmin = covid_start, xmax = covid_end, ymin = -Inf, ymax = Inf,
           fill = "grey80", alpha = 0.4) +
  geom_hline(yintercept = 95, linetype = "dotted", color = "#44AA99", linewidth = 0.8) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = change_point, linetype = "dashed", color = "grey40", linewidth = 1) +
  facet_wrap(~ Dataset, ncol = 1) +
  scale_color_manual(
    values = pastel_palette,
    name = "IMD Quintile",
    labels = c("1 (Least deprived)", "2", "3", "4", "5 (Most deprived)")
  ) +
  scale_y_continuous(limits = c(80, 100), breaks = seq(80, 100, 5)) +
  labs(
    # title = "London Comparison Analysis",
    x = "Time (Year - Quarter)",
    y = "Mean PCV 12m Uptake (%)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    legend.position = "top",
    strip.background = element_rect(fill = "grey95"),
    strip.text = element_text(face = "bold", size = 11),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

print(plot_12m_comparison)

# Create comparison plot for 24-month coverage
plot_24m_comparison <- ggplot(pcv_comparison_24m, 
                              aes(x = Quarter_Label, y = mean_PCV_24m, 
                                  color = as.factor(imd_quintile), 
                                  group = imd_quintile)) +
  annotate("rect", xmin = covid_start, xmax = covid_end, ymin = -Inf, ymax = Inf,
           fill = "grey80", alpha = 0.4) +
  geom_hline(yintercept = 95, linetype = "dotted", color = "#44AA99", linewidth = 0.8) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = change_point, linetype = "dashed", color = "grey40", linewidth = 1) +
  facet_wrap(~ Dataset, ncol = 1) +
  scale_color_manual(
    values = pastel_palette,
    name = "IMD Quintile",
    labels = c("1 (Least deprived)", "2", "3", "4", "5 (Most deprived)")
  ) +
  scale_y_continuous(limits = c(80, 100), breaks = seq(80, 100, 5)) +
  labs(
    # title = "London Comparison Analysis - 24m Coverage",
    x = "Time (Year - Quarter)",
    y = "Mean PCV 24m Uptake (%)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    legend.position = "top",
    strip.background = element_rect(fill = "grey95"),
    strip.text = element_text(face = "bold", size = 11),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

print(plot_24m_comparison)

# =============================================================================
# VALIDATION AND SUMMARY STATISTICS
# =============================================================================

# Calculate booster gaps using 1-year lag
cat("\n=== BOOSTER GAP ANALYSIS WITH 1-YEAR LAG ===\n")

corrected_gaps_lagged <- data_clean_lagged %>%
  mutate(
    schedule_period = if_else(Year < "2020/2021", "2+1 (pre-2020)", "1+1 (post-2020)"),
    booster_gap_corrected = PCV_12m_lag - PCV_24m  # lagged 12m - current 24m
  ) %>%
  group_by(schedule_period) %>%
  summarise(
    Mean_Gap = weighted.mean(booster_gap_corrected, Population_12m_lag, na.rm = TRUE),
    Median_Gap = median(booster_gap_corrected, na.rm = TRUE),
    SD_Gap = sd(booster_gap_corrected, na.rm = TRUE),
    n_observations = n(),
    .groups = "drop"
  ) %>%
  arrange(schedule_period)

cat("Booster Gap Analysis:\n")
print(corrected_gaps_lagged)

# Calculate the change in booster gaps
gap_2plus1_corrected <- corrected_gaps_lagged$Mean_Gap[corrected_gaps_lagged$schedule_period == "2+1 (pre-2020)"]
gap_1plus1_corrected <- corrected_gaps_lagged$Mean_Gap[corrected_gaps_lagged$schedule_period == "1+1 (post-2020)"]

if(length(gap_2plus1_corrected) > 0 && length(gap_1plus1_corrected) > 0) {
  gap_increase_corrected <- gap_1plus1_corrected - gap_2plus1_corrected
  relative_increase_corrected <- round((gap_increase_corrected / gap_2plus1_corrected) * 100, 0)
  
  cat("\nFINDINGS:\n")
  cat("Booster gaps changed from", round(gap_2plus1_corrected, 2), 
      "to", round(gap_1plus1_corrected, 2), "=", round(gap_increase_corrected, 2), 
      "percentage points (", relative_increase_corrected, "% relative change)\n")
}

# Overall summary by scenario and schedule
scenario_summary_corrected <- quarterly_schedule_all_scenarios %>%
  group_by(scenario_name, schedule_group) %>%
  summarise(
    Mean_Susceptibility = mean(Susceptibility_prop, na.rm = TRUE),
    SD_Susceptibility = sd(Susceptibility_prop, na.rm = TRUE),
    Min_Susceptibility = min(Susceptibility_prop, na.rm = TRUE),
    Max_Susceptibility = max(Susceptibility_prop, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Mean_Susceptibility_pct = round(Mean_Susceptibility * 100, 1),
    SD_Susceptibility_pct = round(SD_Susceptibility * 100, 2)
  )

cat("\n Summary of Susceptibility by Scenario and Schedule:\n")
print(scenario_summary_corrected)

# =============================================================================
# SINGLE LOCAL AUTHORITY VALIDATION (BIRMINGHAM EXAMPLE)
# =============================================================================

# Manual calculation to verify susceptibility model
test_utla <- "Birmingham"

cat("\n=== LOCAL AUTHORITY VALIDATION ===\n")
cat("Testing methodology for:", test_utla, "\n")

# Get Birmingham's coverage data
birmingham_data_corrected <- data_clean_lagged %>%
  filter(utla_name == test_utla) %>%
  filter(Year %in% c("2018/2019", "2022/2023")) %>%
  mutate(
    schedule_period = case_when(
      Year == "2018/2019" ~ "2+1 (2018/19)",
      Year == "2022/2023" ~ "1+1 (2022/23)"
    )
  ) %>%
  group_by(schedule_period, Year) %>%
  summarise(
    PCV_12m_lag_avg = weighted.mean(PCV_12m_lag, Population_12m_lag, na.rm = TRUE),
    PCV_24m_avg = weighted.mean(PCV_24m, Population_24m, na.rm = TRUE),
    n_quarters = n(),
    .groups = "drop"
  )

cat(" coverage data for", test_utla, ":\n")
print(birmingham_data_corrected)

# Manual calculation for 2+1 period using methodology
bham_2plus1_12m_lag <- birmingham_data_corrected$PCV_12m_lag_avg[birmingham_data_corrected$schedule_period == "2+1 (2018/19)"]
bham_2plus1_24m <- birmingham_data_corrected$PCV_24m_avg[birmingham_data_corrected$schedule_period == "2+1 (2018/19)"]

if(length(bham_2plus1_12m_lag) > 0) {
  # Convert to proportions
  p0_2plus1_corrected <- (100 - bham_2plus1_12m_lag) / 100                     # Unvaccinated 
  p1_2plus1_corrected <- pmax(0, (bham_2plus1_12m_lag - bham_2plus1_24m) / 100) # Primary only
  p2_2plus1_corrected <- bham_2plus1_24m / 100                                 # Fully protected 
  
  # Calculate susceptibility using 2+1 VE values
  VE_2plus1_primary <- 0.761
  VE_booster <- 0.782
  susceptibility_2plus1_corrected <- p0_2plus1_corrected + p1_2plus1_corrected * (1 - VE_2plus1_primary) + p2_2plus1_corrected * (1 - VE_booster)
  
  cat("\n2+1 schedule susceptibility for", test_utla, ":", round(susceptibility_2plus1_corrected*100, 1), "%\n")
}

# Manual calculation for 1+1 period 
bham_1plus1_12m_lag <- birmingham_data_corrected$PCV_12m_lag_avg[birmingham_data_corrected$schedule_period == "1+1 (2022/23)"]
bham_1plus1_24m <- birmingham_data_corrected$PCV_24m_avg[birmingham_data_corrected$schedule_period == "1+1 (2022/23)"]

if(length(bham_1plus1_12m_lag) > 0) {
  # Convert to proportions
  p0_1plus1_corrected <- (100 - bham_1plus1_12m_lag) / 100                     # Unvaccinated 
  p1_1plus1_corrected <- pmax(0, (bham_1plus1_12m_lag - bham_1plus1_24m) / 100) # Primary only 
  p2_1plus1_corrected <- bham_1plus1_24m / 100                                 # Fully protected 
  
  # Calculate susceptibility using both 1+1 VE assumptions
  VE_1plus1_baseline <- 0.606
  VE_1plus1_alternate <- 0.761
  
  # Baseline assumption
  susceptibility_1plus1_baseline_corrected <- p0_1plus1_corrected + p1_1plus1_corrected * (1 - VE_1plus1_baseline) + p2_1plus1_corrected * (1 - VE_booster)
  
  # Alternate assumption
  susceptibility_1plus1_alternate_corrected <- p0_1plus1_corrected + p1_1plus1_corrected * (1 - VE_1plus1_alternate) + p2_1plus1_corrected * (1 - VE_booster)
  
  cat(" 1+1 baseline assumption for", test_utla, ":", round(susceptibility_1plus1_baseline_corrected*100, 1), "%\n")
  cat(" 1+1 alternate assumption for", test_utla, ":", round(susceptibility_1plus1_alternate_corrected*100, 1), "%\n")
  
  if(exists("susceptibility_2plus1_corrected")) {
    cat(" difference from 2+1 (baseline):", sprintf("%+.1f", (susceptibility_1plus1_baseline_corrected - susceptibility_2plus1_corrected)*100), "pp\n")
    cat(" difference from 2+1 (alternate):", sprintf("%+.1f", (susceptibility_1plus1_alternate_corrected - susceptibility_2plus1_corrected)*100), "pp\n")
  }
}

# =============================================================================
# 12-MONTH AND 24-MONTH COVERAGE MAPS
# =============================================================================

# Prepare 12-month coverage map data
map_data_12m <- england_map %>%
  left_join(
    data_clean %>%
      group_by(Year, ONS_Code, utla_name) %>%
      summarise(
        PCV_12m_avg = weighted.mean(PCV_12m, Population_12m, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(ONS_Code = as.character(ONS_Code)),
    by = c("ctyua19cd" = "ONS_Code")
  ) %>%
  mutate(
    schedule_group = case_when(
      Year < "2020/2021" ~ "2+1",
      Year >= "2020/2021" ~ "1+1",
      TRUE ~ NA_character_
    ),
    Schedule = factor(
      schedule_group,
      levels = c("2+1", "1+1"),
      labels = c("2+1 Schedule", "1+1 Schedule")
    ),
    Year_Schedule = paste(Year, "-", Schedule)
  ) %>%
  filter(!is.na(Year) & !is.na(PCV_12m_avg))

# Create 12-month coverage map
coverage_12m_map <- ggplot(map_data_12m) +
  geom_sf(aes(fill = PCV_12m_avg), color = "white", linewidth = 0.05) +
  scale_fill_viridis_c(
    name = "12-month\nCoverage (%)",
    option = "viridis",
    direction = 1,
    limits = c(70, 100),
    breaks = seq(70, 100, by = 10),
    labels = function(x) paste0(x, "%"),
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(15, "cm"),
      barheight = unit(0.6, "cm")
    )
  ) +
  facet_wrap(~ Year_Schedule, ncol = 4) +
  labs(
    # caption = "Data: COVER / ONS | Higher values indicate better primary coverage"
  ) +
  theme_void(base_size = 10) +
  theme(
    strip.text = element_text(size = 9, margin = margin(b = 2), face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 9),
    plot.caption = element_text(hjust = 0.5, size = 8, color = "grey60"),
    plot.margin = margin(10, 10, 15, 10),
    panel.spacing = unit(0.3, "lines")
  )

print(coverage_12m_map)

# Prepare 24-month coverage map data
map_data_24m <- england_map %>%
  left_join(
    data_clean %>%
      group_by(Year, ONS_Code, utla_name) %>%
      summarise(
        PCV_24m_avg = weighted.mean(PCV_24m, Population_24m, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(ONS_Code = as.character(ONS_Code)),
    by = c("ctyua19cd" = "ONS_Code")
  ) %>%
  mutate(
    schedule_group = case_when(
      Year < "2020/2021" ~ "2+1",
      Year >= "2020/2021" ~ "1+1",
      TRUE ~ NA_character_
    ),
    Schedule = factor(
      schedule_group,
      levels = c("2+1", "1+1"),
      labels = c("2+1 Schedule", "1+1 Schedule")
    ),
    Year_Schedule = paste(Year, "-", Schedule)
  ) %>%
  filter(!is.na(Year) & !is.na(PCV_24m_avg))

# Create 24-month coverage map
coverage_24m_map <- ggplot(map_data_24m) +
  geom_sf(aes(fill = PCV_24m_avg), color = "white", linewidth = 0.05) +
  scale_fill_viridis_c(
    name = "24-month\nCoverage (%)",
    option = "viridis",
    direction = 1,
    limits = c(60, 100),
    breaks = seq(60, 100, by = 10),
    labels = function(x) paste0(x, "%"),
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(15, "cm"),
      barheight = unit(0.6, "cm")
    )
  ) +
  facet_wrap(~ Year_Schedule, ncol = 4) +
  labs(
    #caption = "Data: COVER / ONS | Higher values indicate better booster coverage"
  ) +
  theme_void(base_size = 10) +
  theme(
    strip.text = element_text(size = 9, margin = margin(b = 15), face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 9),
    plot.caption = element_text(hjust = 0.5, size = 8, color = "grey60"),
    plot.margin = margin(10, 10, 15, 10),
    panel.spacing = unit(0.3, "lines")
  )

print(coverage_24m_map)

# Optional: Save the plots
ggsave("output/coverage_12m_geographic_map.png", coverage_12m_map, 
       width = 14, height = 12, dpi = 300)
ggsave("output/coverage_24m_geographic_map.png", coverage_24m_map, 
       width = 14, height = 12, dpi = 300)
