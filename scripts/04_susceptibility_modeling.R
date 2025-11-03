#===============================================================================
# SCRIPT: 04_susceptibility_modeling.R
# PURPOSE: Calculate and visualize pneumococcal disease susceptibility patterns
# 
# DESCRIPTION: 
# - Implements susceptibility modeling using vaccine effectiveness estimates
# - Calculates susceptibility by deprivation quintile and geographic area
# - Compares baseline vs alternate vaccine effectiveness assumptions
# - Generates susceptibility trend plots and geographic maps
# - Performs validation analysis for individual local authorities
#
# MAIN SECTIONS:
# - Vaccine effectiveness scenario definitions
# - Susceptibility calculations by deprivation quintile
# - National level susceptibility calculations  
# - Geographic susceptibility mapping
# - London inclusion/exclusion analysis
# - Model validation and summary statistics
#
# INPUTS: 
# - cleaned_Data/COVER_All_Years_MERGED_WITH_IMD.csv
# - data/Local_Authority_(Upper_Tier)_IMD_2019_(WGS84)/ (shapefile folder)
# OUTPUTS: Susceptibility trend plots, geographic maps, validation statistics
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
# LOAD AND PREPARE DATA
# =============================================================================

# Load main dataset
data_clean <- read.csv(here("cleaned_Data", "COVER_All_Years_MERGED_WITH_IMD.csv"))

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
# SUSCEPTIBILITY CALCULATIONS BY DEPRIVATION QUINTILE
# =============================================================================

quarterly_deprivation_all_scenarios <- map_dfr(ve_scenarios, function(scenario) {
  data_clean %>%
    group_by(Year, Quarter, imd_quintile) %>%
    summarise(
      # Calculate weighted average coverage
      PCV_12m_pct = weighted.mean(PCV_12m, Population_12m, na.rm = TRUE),
      PCV_24m_pct = weighted.mean(PCV_24m, Population_24m, na.rm = TRUE),
      Population = sum(Population_12m, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!is.na(imd_quintile)) %>%
    arrange(Year, Quarter, imd_quintile) %>%
    mutate(
      # Convert to proportions 
      PCV_12m_prop = pmax(0, pmin(1, PCV_12m_pct / 100)),
      PCV_24m_prop = pmax(0, pmin(1, PCV_24m_pct / 100)),
      # Ensure booster coverage doesn't exceed primary coverage
      PCV_24m_prop = pmin(PCV_24m_prop, PCV_12m_prop),
      
      # Calculate population proportions for cross-sectional snapshot
      p0 = 1 - PCV_12m_prop,                    # Unvaccinated
      p1 = pmax(0, PCV_12m_prop - PCV_24m_prop), # Primary doses only (ensure non-negative)
      p2 = PCV_24m_prop,                        # Primary + booster
      
      # Determine schedule based on year
      schedule_group = if_else(Year < "2020/2021", "2+1", "1+1"),
      
      # Assign VE based on schedule and scenario
      VE_primary = case_when(
        schedule_group == "2+1" ~ scenario$ve_2plus1_primary,
        schedule_group == "1+1" ~ scenario$ve_1plus1_primary
      ),
      VE_booster = scenario$ve_booster,
      
      # Calculate susceptibility: proportion at risk of VT IPD
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
# NATIONAL LEVEL SUSCEPTIBILITY CALCULATIONS
# =============================================================================

quarterly_schedule_all_scenarios <- map_dfr(ve_scenarios, function(scenario) {
  data_clean %>%
    group_by(Year, Quarter) %>%
    summarise(
      PCV_12m_pct = weighted.mean(PCV_12m, Population_12m, na.rm = TRUE),
      PCV_24m_pct = weighted.mean(PCV_24m, Population_24m, na.rm = TRUE),
      Population = sum(Population_12m, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(Year, Quarter) %>%
    mutate(
      # Convert to proportions
      PCV_12m_prop = pmax(0, pmin(1, PCV_12m_pct / 100)),
      PCV_24m_prop = pmax(0, pmin(1, PCV_24m_pct / 100)),
      PCV_24m_prop = pmin(PCV_24m_prop, PCV_12m_prop),
      
      # Population proportions
      p0 = 1 - PCV_12m_prop,
      p1 = pmax(0, PCV_12m_prop - PCV_24m_prop), # Ensure non-negative
      p2 = PCV_24m_prop,
      
      # Schedule determination
      schedule_group = if_else(Year < "2020/2021", "2+1", "1+1"),
      
      # VE assignment
      VE_primary = case_when(
        schedule_group == "2+1" ~ scenario$ve_2plus1_primary,
        schedule_group == "1+1" ~ scenario$ve_1plus1_primary
      ),
      VE_booster = scenario$ve_booster,
      
      # Susceptibility calculation
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

# Load England map 
england_map <- st_read(here("data", "Shapefile", "Local_Authority_(Upper_Tier)_IMD_2019_(WGS84).shp"))

# Calculate geographic susceptibility for all scenarios
la_susceptibility_all_scenarios <- map_dfr(ve_scenarios, function(scenario) {
  data_clean %>%
    group_by(Year, ONS_Code, utla_name) %>%
    summarise(
      PCV_12m_pct = weighted.mean(PCV_12m, Population_12m, na.rm = TRUE),
      PCV_24m_pct = weighted.mean(PCV_24m, Population_24m, na.rm = TRUE),
      Population = sum(Population_12m, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      # Convert to proportions
      PCV_12m_prop = pmax(0, pmin(1, PCV_12m_pct / 100)),
      PCV_24m_prop = pmax(0, pmin(1, PCV_24m_pct / 100)),
      PCV_24m_prop = pmin(PCV_24m_prop, PCV_12m_prop),
      
      # Population proportions
      p0 = 1 - PCV_12m_prop,
      p1 = pmax(0, PCV_12m_prop - PCV_24m_prop), # Ensure non-negative
      p2 = PCV_24m_prop,
      
      # Schedule determination
      schedule_group = if_else(Year < "2020/2021", "2+1", "1+1"),
      
      # VE assignment
      VE_primary = case_when(
        schedule_group == "2+1" ~ scenario$ve_2plus1_primary,
        schedule_group == "1+1" ~ scenario$ve_1plus1_primary
      ),
      VE_booster = scenario$ve_booster,
      
      # Susceptibility calculation
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
# FIGURE 4: SUSCEPTIBILITY TRENDS BY DEPRIVATION
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
figure4_simplified <- ggplot(main_scenarios, aes(x = time_order, y = Susceptibility_prop)) +
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
    x = "Year & Quarter",
    y = "Proportion Susceptible",
    caption = "Vertical line indicates schedule change (Jan 2020) | Central VE estimates only"
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
    plot.caption = element_text(size = 12, color = "grey60")
  )

print(figure4_simplified)

# =============================================================================
# FIGURE 5: GEOGRAPHIC SUSCEPTIBILITY MAPS
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

# Select representative years for visualization
selected_years <- c("2019/2020", "2020/2021", "2021/2022")
map_data_subset <- map_data_main %>%
  filter(Year %in% selected_years)

# Create geographic susceptibility map
figure5_main <- ggplot(map_data_subset) +
  geom_sf(aes(fill = Susceptibility), color = "white", linewidth = 0.05) +
  scale_fill_viridis_c(
    name = "Proportion Susceptible",  
    option = "plasma",
    direction = 1,
    limits = c(0.20, 0.35),
    breaks = seq(0.20, 0.35, by = 0.05),
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
    #subtitle = "Baseline assumption: 1+1 single dose VE = 60.6%",
    #caption = "Central VE estimates | Cross-sectional analysis"
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
    panel.spacing = unit(0.3, "lines")
  )

print(figure5_main)

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

figure5_alternate <- ggplot(map_data_alternate) +
  geom_sf(aes(fill = Susceptibility), color = "white", linewidth = 0.05) +
  scale_fill_viridis_c(
    name = "Proportion Susceptible",  
    option = "plasma",
    direction = 1,
    limits = c(0.20, 0.35),
    breaks = seq(0.20, 0.35, by = 0.05),
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
    #subtitle = "Alternate assumption: 1+1 single dose VE = 76.1%",
    #caption = "Central VE estimates | Cross-sectional analysis"
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
    panel.spacing = unit(0.3, "lines")
  )

print(figure5_alternate)

# =============================================================================
# GENERATE UTLA LISTS BY QUINTILE
# =============================================================================

# Create clean list of LAs by IMD quintile
la_by_quintile <- data_clean %>%
  filter(Year == "2023/2024") %>%
  select(utla_name, imd_quintile) %>%
  distinct() %>%
  arrange(imd_quintile, utla_name)

# Print formatted lists for Supporting Information
for(q in 1:5) {
  cat("\n", "=================", "\n")
  cat("IMD Quintile", q, if(q == 1) "(Least Deprived)" else if(q == 5) "(Most Deprived)", "\n")
  cat("=================", "\n")
  las <- la_by_quintile %>% 
    filter(imd_quintile == q) %>% 
    pull(utla_name) %>%
    sort()
  cat(paste(las, collapse = "\n"), "\n")
}

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

# Calculate coverage trends with and without London
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
    x = "Time (Year - Quarter)",
    y = "Mean PCV 12m Uptake (%)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    legend.position = "top",
    strip.background = element_rect(fill = "grey95"),
    strip.text = element_text(face = "bold", size = 11)
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
    x = "Time (Year - Quarter)",
    y = "Mean PCV 24m Uptake (%)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    legend.position = "top",
    strip.background = element_rect(fill = "grey95"),
    strip.text = element_text(face = "bold", size = 11)
  )

print(plot_24m_comparison)

# =============================================================================
# VALIDATION AND SUMMARY STATISTICS
# =============================================================================

# Calculate booster gaps by schedule period
corrected_gaps <- data_clean %>%
  mutate(
    schedule_period = if_else(Year < "2020/2021", "2+1 (pre-2020)", "1+1 (post-2020)"),
    booster_gap = PCV_12m - PCV_24m
  ) %>%
  group_by(schedule_period) %>%
  summarise(
    Mean_Gap = weighted.mean(booster_gap, Population_12m, na.rm = TRUE),
    Median_Gap = median(booster_gap, na.rm = TRUE),
    SD_Gap = sd(booster_gap, na.rm = TRUE),
    n_observations = n(),
    .groups = "drop"
  ) %>%
  arrange(schedule_period)

print("Booster Gap Analysis:")
print(corrected_gaps)

# Calculate the increase in booster gaps
gap_2plus1 <- corrected_gaps$Mean_Gap[corrected_gaps$schedule_period == "2+1 (pre-2020)"]
gap_1plus1 <- corrected_gaps$Mean_Gap[corrected_gaps$schedule_period == "1+1 (post-2020)"]
gap_increase <- gap_1plus1 - gap_2plus1
relative_increase <- round((gap_increase / gap_2plus1) * 100, 0)

print(paste("Booster gaps increased from", round(gap_2plus1, 2), 
            "to", round(gap_1plus1, 2), "=", round(gap_increase, 2), 
            "percentage points (", relative_increase, "% relative increase)"))

# Overall summary by scenario and schedule
scenario_summary <- quarterly_schedule_all_scenarios %>%
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

print("Summary of Susceptibility by Scenario and Schedule:")
print(scenario_summary)

# =============================================================================
# SINGLE LOCAL AUTHORITY VALIDATION (BIRMINGHAM EXAMPLE)
# =============================================================================

# Manual calculation to verify susceptibility model
test_utla <- "Birmingham"

# Get Birmingham's coverage data for representative years
birmingham_data <- data_clean %>%
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
    PCV_12m_avg = weighted.mean(PCV_12m, Population_12m, na.rm = TRUE),
    PCV_24m_avg = weighted.mean(PCV_24m, Population_24m, na.rm = TRUE),
    n_quarters = n(),
    .groups = "drop"
  )

print(paste("Coverage data for", test_utla, ":"))
print(birmingham_data)

# Manual calculation for 2+1 period
bham_2plus1_12m <- birmingham_data$PCV_12m_avg[birmingham_data$schedule_period == "2+1 (2018/19)"]
bham_2plus1_24m <- birmingham_data$PCV_24m_avg[birmingham_data$schedule_period == "2+1 (2018/19)"]

if(length(bham_2plus1_12m) > 0) {
  # Convert to proportions
  p0_2plus1 <- (100 - bham_2plus1_12m) / 100
  p1_2plus1 <- pmax(0, (bham_2plus1_12m - bham_2plus1_24m) / 100)  # Ensure non-negative
  p2_2plus1 <- bham_2plus1_24m / 100
  
  # Calculate susceptibility using 2+1 VE values
  VE_2plus1_primary <- 0.761
  VE_booster <- 0.782
  susceptibility_2plus1 <- p0_2plus1 + p1_2plus1 * (1 - VE_2plus1_primary) + p2_2plus1 * (1 - VE_booster)
  
  cat("\n2+1 schedule susceptibility for", test_utla, ":", round(susceptibility_2plus1*100, 1), "%\n")
}

# Manual calculation for 1+1 period
bham_1plus1_12m <- birmingham_data$PCV_12m_avg[birmingham_data$schedule_period == "1+1 (2022/23)"]
bham_1plus1_24m <- birmingham_data$PCV_24m_avg[birmingham_data$schedule_period == "1+1 (2022/23)"]

if(length(bham_1plus1_12m) > 0) {
  # Convert to proportions
  p0_1plus1 <- (100 - bham_1plus1_12m) / 100
  p1_1plus1 <- pmax(0, (bham_1plus1_12m - bham_1plus1_24m) / 100)  # Ensure non-negative
  p2_1plus1 <- bham_1plus1_24m / 100
  
  # Calculate susceptibility using both 1+1 VE assumptions
  VE_1plus1_baseline <- 0.606
  VE_1plus1_alternate <- 0.761
  
  # Baseline assumption
  susceptibility_1plus1_baseline <- p0_1plus1 + p1_1plus1 * (1 - VE_1plus1_baseline) + p2_1plus1 * (1 - VE_booster)
  
  # Alternate assumption
  susceptibility_1plus1_alternate <- p0_1plus1 + p1_1plus1 * (1 - VE_1plus1_alternate) + p2_1plus1 * (1 - VE_booster)
  
  cat("1+1 baseline assumption for", test_utla, ":", round(susceptibility_1plus1_baseline*100, 1), "%\n")
  cat("1+1 alternate assumption for", test_utla, ":", round(susceptibility_1plus1_alternate*100, 1), "%\n")
  
  if(exists("susceptibility_2plus1")) {
    cat("Difference from 2+1 (baseline):", sprintf("%+.1f", (susceptibility_1plus1_baseline - susceptibility_2plus1)*100), "pp\n")
    cat("Difference from 2+1 (alternate):", sprintf("%+.1f", (susceptibility_1plus1_alternate - susceptibility_2plus1)*100), "pp\n")
  }
}