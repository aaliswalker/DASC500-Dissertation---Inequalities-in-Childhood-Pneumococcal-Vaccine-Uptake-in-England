#===============================================================================
# SCRIPT: 03_descriptive_statistics_and_deprivation_analysis.R
# PURPOSE: Generate descriptive statistics and visualizations for PCV uptake patterns by deprivation
# 
# DESCRIPTION: 
# - Uses boundary-corrected, non-imputed data from Scripts 1 & 2
# - Calculates national coverage statistics by schedule period (2+1 vs 1+1)
# - Analyzes vaccine uptake patterns across deprivation quintiles
# - Produces WHO target achievement summaries
# - Generates booster retention analysis with 1-year lag (primary from previous year)
# - Creates time trend visualizations showing inequalities
#
# INPUTS: 
# - output/COVER_All_Years_MERGED_WITH_IMD_NO_IMPUTATION.csv
# - output/COVER_Booster_Gap_1YearLag.csv (if available)
# OUTPUTS: Summary statistics, trend plots, retention analysis plots
#===============================================================================

library("readxl")
library("readr") 
library("RColorBrewer")
library("ggrepel")
library("tidyverse") 
library("here")

# Set working directory and paths
data_file <- here("output", "COVER_All_Years_MERGED_WITH_IMD_NO_IMPUTATION.csv")

if (!file.exists(data_file)) {
  stop("Boundary-corrected merged data not found. Please run Script 02 first.")
}

data <- read.csv(data_file)

# Convert columns to appropriate types
data <- data %>%
  mutate(
    PCV_12m = as.numeric(PCV_12m),
    PCV_24m = as.numeric(PCV_24m),
    Population_12m = as.numeric(Population_12m),
    Population_24m = as.numeric(Population_24m),
    Timepoint = as.factor(Timepoint),
    Quarter = as.factor(Quarter),
    Vaccine_Schedule = as.factor(Vaccine_Schedule),
    imd_quintile = as.factor(imd_quintile)
  )

#### Dataset Characteristics and National Coverage ####

# Basic dataset characteristics
cat("=== DATASET CHARACTERISTICS ===\n")
cat("Analysis period:", min(data$Year, na.rm = TRUE), "to", max(data$Year, na.rm = TRUE), "\n")
cat("Number of UTLAs included:", length(unique(data$utla_name)), "\n")
cat("Number of quarters analyzed:", length(unique(paste(data$Year, data$Quarter))), "\n")
cat("Total observations:", nrow(data), "\n\n")

# Verify boundary changes are properly included
boundary_check <- data %>%
  filter(utla_name %in% c("Bournemouth, Christchurch and Poole", "Northamptonshire")) %>%
  group_by(utla_name, Year) %>%
  summarise(
    Quarters = n_distinct(Quarter),
    Has_12m_Data = any(!is.na(PCV_12m)),
    Has_24m_Data = any(!is.na(PCV_24m)),
    .groups = "drop"
  )

cat("=== BOUNDARY CHANGE VERIFICATION ===\n")
print(boundary_check)

# Calculate total population covered (sum of all eligible populations)
total_12m_population <- sum(data$Population_12m, na.rm = TRUE)
total_24m_population <- sum(data$Population_24m, na.rm = TRUE)

cat("=== POPULATION COVERAGE ===\n")
cat("Total eligible population (12-month cohorts):", format(total_12m_population, big.mark = ","), "children\n")
cat("Total eligible population (24-month cohorts):", format(total_24m_population, big.mark = ","), "children\n")

# Calculate weighted national averages across entire study period
weighted_12m_coverage <- weighted.mean(data$PCV_12m, data$Population_12m, na.rm = TRUE)
weighted_24m_coverage <- weighted.mean(data$PCV_24m, data$Population_24m, na.rm = TRUE)
national_booster_gap <- weighted_12m_coverage - weighted_24m_coverage

cat("\n=== NATIONAL COVERAGE (ENTIRE STUDY PERIOD) ===\n")
cat("Overall 12-month PCV coverage:", round(weighted_12m_coverage, 1), "%\n")
cat("Overall 24-month PCV coverage:", round(weighted_24m_coverage, 1), "%\n")
cat("Overall booster gap (concurrent):", round(national_booster_gap, 1), "percentage points\n\n")

# Coverage by schedule period (using original definitions for overall coverage)
pre_schedule <- data %>% filter(Year < "2020/2021")
post_schedule <- data %>% filter(Year >= "2020/2021")

pre_12m_coverage <- weighted.mean(pre_schedule$PCV_12m, pre_schedule$Population_12m, na.rm = TRUE)
pre_24m_coverage <- weighted.mean(pre_schedule$PCV_24m, pre_schedule$Population_24m, na.rm = TRUE)
pre_gap <- pre_12m_coverage - pre_24m_coverage

post_12m_coverage <- weighted.mean(post_schedule$PCV_12m, post_schedule$Population_12m, na.rm = TRUE)
post_24m_coverage <- weighted.mean(post_schedule$PCV_24m, post_schedule$Population_24m, na.rm = TRUE)
post_gap <- post_12m_coverage - post_24m_coverage

cat("=== COVERAGE BY SCHEDULE PERIOD ===\n")
cat("PRE-Schedule Change (2+1, 2013-2020):\n")
cat("• 12-month coverage:", round(pre_12m_coverage, 1), "%\n")
cat("• 24-month coverage:", round(pre_24m_coverage, 1), "%\n")
cat("• Booster gap:", round(pre_gap, 1), "percentage points\n\n")

cat("POST-Schedule Change (1+1, 2020-2024):\n")
cat("• 12-month coverage:", round(post_12m_coverage, 1), "%\n")
cat("• 24-month coverage:", round(post_24m_coverage, 1), "%\n")
cat("• Booster gap:", round(post_gap, 1), "percentage points\n\n")

# Coverage by deprivation quintile
cat("=== COVERAGE BY DEPRIVATION QUINTILE (ENTIRE PERIOD) ===\n")
quintile_summary <- data %>%
  group_by(imd_quintile) %>%
  summarise(
    coverage_12m = weighted.mean(PCV_12m, Population_12m, na.rm = TRUE),
    coverage_24m = weighted.mean(PCV_24m, Population_24m, na.rm = TRUE),
    gap = coverage_12m - coverage_24m,
    total_pop_12m = sum(Population_12m, na.rm = TRUE),
    .groups = "drop"
  )

for(i in 1:nrow(quintile_summary)) {
  cat("Quintile", quintile_summary$imd_quintile[i], 
      ifelse(quintile_summary$imd_quintile[i] == 1, "(Least deprived)", 
             ifelse(quintile_summary$imd_quintile[i] == 5, "(Most deprived)", "")), ":\n")
  cat("• 12-month coverage:", round(quintile_summary$coverage_12m[i], 1), "%\n")
  cat("• 24-month coverage:", round(quintile_summary$coverage_24m[i], 1), "%\n")
  cat("• Booster gap:", round(quintile_summary$gap[i], 1), "percentage points\n")
  cat("• Population:", format(quintile_summary$total_pop_12m[i], big.mark = ","), "children\n\n")
}

# WHO target achievement
cat("=== WHO TARGET ACHIEVEMENT (95%) ===\n")
quarters_above_95_12m <- sum(data$PCV_12m >= 95, na.rm = TRUE)
quarters_above_95_24m <- sum(data$PCV_24m >= 95, na.rm = TRUE)
total_observations <- sum(!is.na(data$PCV_12m))

cat("Quarterly observations meeting 95% target:\n")
cat("• 12-month coverage:", quarters_above_95_12m, "out of", total_observations, 
    "(", round(100 * quarters_above_95_12m / total_observations, 1), "%)\n")
cat("• 24-month coverage:", quarters_above_95_24m, "out of", total_observations, 
    "(", round(100 * quarters_above_95_24m / total_observations, 1), "%)\n\n")

# WHO target achievement by schedule period
cat("\n=== WHO TARGET ACHIEVEMENT BY SCHEDULE PERIOD ===\n")

# Pre-schedule period (2+1)
pre_total_obs <- sum(!is.na(pre_schedule$PCV_12m))
pre_12m_above_95 <- sum(pre_schedule$PCV_12m >= 95, na.rm = TRUE)
pre_24m_above_95 <- sum(pre_schedule$PCV_24m >= 95, na.rm = TRUE)

cat("PRE-Schedule Change (2+1, 2013-2020):\n")
cat("• 12-month coverage ≥95%:", pre_12m_above_95, "out of", pre_total_obs, 
    "(", round(100 * pre_12m_above_95 / pre_total_obs, 1), "%)\n")
cat("• 24-month coverage ≥95%:", pre_24m_above_95, "out of", pre_total_obs, 
    "(", round(100 * pre_24m_above_95 / pre_total_obs, 1), "%)\n\n")

# Post-schedule period (1+1)
post_total_obs <- sum(!is.na(post_schedule$PCV_12m))
post_12m_above_95 <- sum(post_schedule$PCV_12m >= 95, na.rm = TRUE)
post_24m_above_95 <- sum(post_schedule$PCV_24m >= 95, na.rm = TRUE)

cat("POST-Schedule Change (1+1, 2020-2024):\n")
cat("• 12-month coverage ≥95%:", post_12m_above_95, "out of", post_total_obs, 
    "(", round(100 * post_12m_above_95 / post_total_obs, 1), "%)\n")
cat("• 24-month coverage ≥95%:", post_24m_above_95, "out of", post_total_obs, 
    "(", round(100 * post_24m_above_95 / post_total_obs, 1), "%)\n\n")

# Calculate changes
change_12m <- (post_12m_above_95 / post_total_obs - pre_12m_above_95 / pre_total_obs) * 100
change_24m <- (post_24m_above_95 / post_total_obs - pre_24m_above_95 / pre_total_obs) * 100

cat("CHANGES:\n")
cat("• 12-month target achievement changed by:", round(change_12m, 1), "pp\n")
cat("• 24-month target achievement changed by:", round(change_24m, 1), "pp\n")

# Missing data summary (transparent reporting - no imputation)
missing_12m <- sum(is.na(data$PCV_12m))
missing_24m <- sum(is.na(data$PCV_24m))

cat("=== DATA COMPLETENESS (NO IMPUTATION) ===\n")
cat("Missing 12-month coverage data:", missing_12m, "observations (", 
    round(100 * missing_12m / nrow(data), 1), "%)\n")
cat("Missing 24-month coverage data:", missing_24m, "observations (", 
    round(100 * missing_24m / nrow(data), 1), "%)\n")

#######################################
#### BOOSTER GAP ANALYSIS
#######################################

cat("\n=== BOOSTER GAP ANALYSIS WITH 1-YEAR LAG ===\n")

# Try to load pre-calculated booster gap data from Script 01
cleaned_data_dir <- here("data/cleaned") 
booster_gap_file <- file.path(cleaned_data_dir, "COVER_Booster_Gap_1YearLag.csv")

if (file.exists(booster_gap_file)) {
  cat("Loading pre-calculated booster gap data with 1-year lag\n")
  booster_gap_data <- read.csv(booster_gap_file)
  
  # Convert columns
  booster_gap_data <- booster_gap_data %>%
    mutate(
      PCV_12m_lag = as.numeric(PCV_12m_lag),
      PCV_24m = as.numeric(PCV_24m),
      Booster_Gap = as.numeric(Booster_Gap),
      Susceptible = as.numeric(Susceptible),
      Only_Primary = as.numeric(Only_Primary),
      Fully_Protected = as.numeric(Fully_Protected)
    )
  
} else {
  cat("Calculating booster gap with 1-year lag...\n")
  
  # Create primary uptake with 1-year lag
  primary_lag <- data %>%
    mutate(Year_lag = case_when(
      Year == "2013/2014" ~ "2014/2015",
      Year == "2014/2015" ~ "2015/2016", 
      Year == "2015/2016" ~ "2016/2017",
      Year == "2016/2017" ~ "2017/2018",
      Year == "2017/2018" ~ "2018/2019",
      Year == "2018/2019" ~ "2019/2020",
      Year == "2020/2021" ~ "2021/2022",
      Year == "2021/2022" ~ "2022/2023",
      Year == "2022/2023" ~ "2023/2024",
      Year == "2023/2024" ~ "2024/2025",
      TRUE ~ NA_character_
    )) %>%
    select(ONS_Code, UTLA_Name, Year_lag, Quarter, PCV_12m_lag = PCV_12m, Population_12m_lag = Population_12m) %>%
    filter(!is.na(Year_lag))
  
  # Join with current booster data and calculate gap
  booster_gap_data <- data %>%
    inner_join(primary_lag, by = c("ONS_Code", "UTLA_Name", "Year" = "Year_lag", "Quarter")) %>%
    mutate(
      Booster_Gap = PCV_12m_lag - PCV_24m,
      Susceptible = 100 - PCV_12m_lag,
      Only_Primary = Booster_Gap,
      Fully_Protected = PCV_24m
    ) %>%
    filter(!is.na(Booster_Gap)) %>%
    select(ONS_Code, UTLA_Name, Year, Quarter, Timepoint, Vaccine_Schedule, imd_quintile, utla_name,
           Population_12m_lag, Population_24m, 
           PCV_12m_lag, PCV_24m, Booster_Gap, Susceptible, Only_Primary, Fully_Protected)
}

# Report booster gap analysis periods
cat("Booster gap analysis periods (with 1-year lag):\n")
cat("Available years:", paste(unique(booster_gap_data$Year), collapse = ", "), "\n")

# Adjusted schedule periods for booster gap analysis
pre_schedule_gap <- booster_gap_data %>% filter(Year %in% c("2014/2015", "2015/2016", "2016/2017", 
                                                            "2017/2018", "2018/2019", "2019/2020"))
post_schedule_gap <- booster_gap_data %>% filter(Year %in% c("2021/2022", "2022/2023", "2023/2024", "2024/2025"))

cat("PRE-schedule booster gap analysis: 2014/2015 - 2019/2020 (2+1 schedule)\n")
cat("POST-schedule booster gap analysis: 2021/2022 - 2024/2025 (1+1 schedule)\n")
cat("EXCLUDED: 2013/2014 (insufficient lag data), 2020/2021 (mixed schedule period)\n\n")

# Booster gap summary statistics
pre_gap_mean <- weighted.mean(pre_schedule_gap$Booster_Gap, pre_schedule_gap$Population_12m_lag, na.rm = TRUE)
pre_gap_median <- median(pre_schedule_gap$Booster_Gap, na.rm = TRUE)

post_gap_mean <- weighted.mean(post_schedule_gap$Booster_Gap, post_schedule_gap$Population_12m_lag, na.rm = TRUE)
post_gap_median <- median(post_schedule_gap$Booster_Gap, na.rm = TRUE)

gap_change <- post_gap_mean - pre_gap_mean

cat("=== BOOSTER GAP WITH 1-YEAR LAG ===\n")
cat("PRE-Schedule Change (2+1):\n")
cat("• Mean booster gap:", round(pre_gap_mean, 2), "percentage points\n")
cat("• Median booster gap:", round(pre_gap_median, 2), "percentage points\n\n")

cat("POST-Schedule Change (1+1):\n")
cat("• Mean booster gap:", round(post_gap_mean, 2), "percentage points\n")
cat("• Median booster gap:", round(post_gap_median, 2), "percentage points\n\n")

cat("CHANGE: Booster gap changed by", round(gap_change, 2), "percentage points after schedule change\n")

#######################################
#### VISUALIZATIONS ####
#######################################

#### Colour palette ####
pastel_palette <- c(
  "#88CCEE", 
  "#CC6677",  
  "#117733", 
  "#DDCC77",
  "#332288",
  "#EE7733",
  "#44AA99" 
)

############################################
#### Time Trend Visualizations ####
############################################

# ---- Overall time trends ----
overall_trend_both <- data %>%
  group_by(Year, Quarter) %>%
  summarise(
    PCV_12m = mean(PCV_12m, na.rm = TRUE),
    PCV_24m = mean(PCV_24m, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Year, Quarter) %>%
  mutate(
    Quarter_Label = paste(Year, Quarter, sep = " "),
    Quarter_ID = row_number()
  )

overall_trend_long <- overall_trend_both %>%
  pivot_longer(
    cols = c("PCV_12m", "PCV_24m"),
    names_to = "Timepoint",
    values_to = "Uptake"
  )

# Set factor levels for Quarter_Label
all_quarters <- unique(overall_trend_both$Quarter_Label)
overall_trend_long$Quarter_Label <- factor(overall_trend_long$Quarter_Label, levels = all_quarters)

# Recode for pretty legend
overall_trend_long$Timepoint <- recode(overall_trend_long$Timepoint,
                                       "PCV_12m" = "12 months",
                                       "PCV_24m" = "24 months")

# Target line (95% expected uptake)
target_line <- data.frame(
  Quarter_Label = all_quarters,
  Quarter_ID = seq_along(all_quarters),
  Uptake = 95,
  Timepoint = "Expected uptake (95%)"
)
target_line$Quarter_Label <- factor(target_line$Quarter_Label, levels = all_quarters)

# Ensure columns match for rbind
overall_trend_long <- overall_trend_long[, c("Quarter_Label", "Quarter_ID", "Uptake", "Timepoint")]
target_line <- target_line[, c("Quarter_Label", "Quarter_ID", "Uptake", "Timepoint")]

# Combine for plotting
plot_data <- rbind(overall_trend_long, target_line)

# ---- Plot settings ----
schedule_quarter <- "2020/2021 Q1"
change_point <- which(levels(plot_data$Quarter_Label) == schedule_quarter)
covid_start <- which(levels(plot_data$Quarter_Label) == "2020/2021 Q1")
covid_end <- which(levels(plot_data$Quarter_Label) == "2021/2022 Q4")

# Define a soft pastel palette for this plot
line_colours <- c(
  "12 months" = "#AA4499",          
  "24 months" = "#332288",           
  "Expected uptake (95%)" = "#88CCEE" 
)

line_types <- c(
  "12 months" = "solid",
  "24 months" = "solid",
  "Expected uptake (95%)" = "dotdash"
)

# Final Plot - Overall Time Trends
ggplot(plot_data, aes(x = Quarter_Label, y = Uptake,
                      color = Timepoint, linetype = Timepoint, group = Timepoint)) +
  # Shaded COVID-19 area
  annotate("rect", xmin = covid_start, xmax = covid_end,
           ymin = -Inf, ymax = Inf, fill = "grey90", alpha = 0.4) +
  
  # Line plots for PCV uptake and expected target
  geom_line(linewidth = 1.2, na.rm = TRUE) +
  
  # Schedule change vertical line
  geom_vline(xintercept = change_point, linetype = "dashed", color = "grey40", linewidth = 1.2) +
  
  # Labels: Schedule change
  annotate("text", x = change_point + 1, y = 81,
           label = "Schedule Change", color = "grey40", size = 3, angle = 90, hjust = 0, vjust = -4.2) +
  
  # Label: COVID period
  annotate("text", x = mean(c(covid_start, covid_end)), y = 80,
           label = "COVID-19 Pandemic", size = 3, color = "black") +
  
  # Schedule labels: 2+1 and 1+1
  annotate("text", x = which(levels(plot_data$Quarter_Label) == "2017/2018 Q4"),
           y = 99.5, label = "2+1", fontface = "bold", color = "black", size = 3) +
  annotate("text", x = which(levels(plot_data$Quarter_Label) == "2022/2023 Q1"),
           y = 99.5, label = "1+1", fontface = "bold", color = "black", size = 3) +
  
  # Manual color and line style
  scale_color_manual(values = line_colours) +
  scale_linetype_manual(values = line_types) +
  
  # Coordinate/Axis settings
  coord_cartesian(ylim = c(80, 100)) +
  scale_y_continuous(breaks = seq(80, 100, 5)) +
  
  # Labels and theme
  labs(
    x = "Time (Year - Quarter)",
    y = "Mean PCV Uptake (%)",
    color = "Timepoint",
    linetype = "Timepoint"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    legend.position = "top"
  )

#### Uptake by quintile at 12m ####

# Calculate deprivation trend
pcv_trend <- data %>%
  group_by(Year, Quarter, imd_quintile) %>%
  summarise(mean_PCV_12m = mean(PCV_12m, na.rm = TRUE), .groups = "drop") %>%
  arrange(Year, Quarter, imd_quintile) %>%
  mutate(
    Quarter_Label = paste(Year, Quarter, sep = " "),
    Quarter_Label = factor(Quarter_Label, levels = unique(paste(Year, Quarter, sep = " ")))
  )

# Set key time points
change_point <- which(levels(pcv_trend$Quarter_Label) == "2020/2021 Q1")
covid_start <- change_point
covid_end <- which(levels(pcv_trend$Quarter_Label) == "2021/2022 Q4")

# Plot
ggplot(pcv_trend, aes(x = Quarter_Label, y = mean_PCV_12m, 
                      color = as.factor(imd_quintile), group = imd_quintile)) +
  # COVID-19 shaded area
  annotate("rect", xmin = covid_start, xmax = covid_end, ymin = -Inf, ymax = Inf,
           fill = "grey80", alpha = 0.4) +
  # 95% target line
  geom_hline(aes(yintercept = 95, linetype = "95% Target"), color = "#44AA99", linewidth = 1) +
  # Uptake lines
  geom_line(linewidth = 1) +
  # Schedule change marker
  geom_vline(xintercept = change_point, linetype = "dashed", color = "grey40", linewidth = 1.2) +
  # Labels
  annotate("text", x = change_point - 2, y = 82, 
           label = "Schedule Change", color = "grey40", size = 3, angle = 90, vjust = -0.5) +
  annotate("text", x = mean(c(covid_start, covid_end)) + 1, y = 80.5,
           label = "COVID-19 pandemic", size = 3.2, color = "black") +
  annotate("text", x = 7, y = 97, label = "2+1", color = "black", fontface = "bold", size = 4) +
  annotate("text", x = 38, y = 97, label = "1+1", color = "black", fontface = "bold", size = 4) +
  # Colours and legend
  scale_color_manual(
    values = pastel_palette,
    name = "IMD Quintile",
    labels = c("1 (Least deprived)", "2", "3", "4", "5 (Most deprived)")
  ) +
  scale_linetype_manual(
    name = NULL,
    values = c("95% Target" = "dotted"),
    guide = guide_legend(order = 2, override.aes = list(color = "#44AA99"))
  ) +
  scale_y_continuous(limits = c(80, 100), breaks = seq(80, 100, 5)) +
  labs(
    x = "Time (Year - Quarter)",
    y = "Mean PCV 12m Uptake (%)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top"
  )

#### Uptake by quintile at 24m ####

# Calculate deprivation trend
pcv_trend_24m <- data %>%
  group_by(Year, Quarter, imd_quintile) %>%
  summarise(mean_PCV_24m = mean(PCV_24m, na.rm = TRUE), .groups = "drop") %>%
  arrange(Year, Quarter, imd_quintile) %>%
  mutate(
    Quarter_Label = paste(Year, Quarter, sep = " "),
    Quarter_Label = factor(Quarter_Label, levels = unique(paste(Year, Quarter, sep = " ")))
  )

# Schedule + COVID windows
change_point_24m <- which(levels(pcv_trend_24m$Quarter_Label) == "2020/2021 Q1")
covid_start <- change_point_24m
covid_end <- which(levels(pcv_trend_24m$Quarter_Label) == "2021/2022 Q4")

# Plot
ggplot(pcv_trend_24m, aes(x = Quarter_Label, y = mean_PCV_24m,
                          color = as.factor(imd_quintile),
                          group = imd_quintile)) +
  # COVID-19 shaded area
  annotate("rect", xmin = covid_start, xmax = covid_end, ymin = -Inf, ymax = Inf,
           fill = "grey80", alpha = 0.4) +
  # 95% target line
  geom_hline(aes(yintercept = 95, linetype = "95% Target"), color = "#44AA99", linewidth = 1) +
  # PCV uptake lines
  geom_line(linewidth = 1) +
  # Schedule change line
  geom_vline(xintercept = change_point_24m, linetype = "dashed", color = "grey40", linewidth = 1.2) +
  # Schedule change label
  annotate("text", x = change_point_24m - 2, 
           y = min(pcv_trend_24m$mean_PCV_24m, na.rm = TRUE) + 1.5,
           label = "Schedule Change", color = "grey40", size = 3) +
  # COVID-19 pandemic label
  annotate("text", 
           x = mean(c(covid_start, covid_end)) + 1,
           y = min(pcv_trend_24m$mean_PCV_24m, na.rm = TRUE) + 0.3,
           label = "COVID-19 pandemic", size = 3.2, color = "black") +
  # 2+1 and 1+1 text annotations
  annotate("text", x = 7, y = 97, label = "2+1", color = "black", fontface = "bold", size = 4) +
  annotate("text", x = 38, y = 97, label = "1+1", color = "black", fontface = "bold", size = 4) +
  # Color scale + linetype
  scale_color_manual(
    values = pastel_palette,
    name = "IMD Quintile",
    labels = c("1 (Least deprived)", "2", "3", "4", "5 (Most deprived)")
  ) +
  scale_linetype_manual(
    name = NULL,
    values = c("95% Target" = "dotted"),
    guide = guide_legend(order = 2, override.aes = list(color = "#44AA99"))
  ) +
  scale_y_continuous(limits = c(80, 100), breaks = seq(80, 100, 5)) +
  labs(
    x = "Time (Year - Quarter)",
    y = "Mean PCV 24m Uptake (%)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top"
  )

#### FACET - Combined 12m and 24m trends ####
# Prepare 12m data
pcv_trend_12m <- data %>%
  group_by(Year, Quarter, imd_quintile) %>%
  summarise(mean_PCV = mean(PCV_12m, na.rm = TRUE), .groups = "drop") %>%
  mutate(Timepoint = "12 months")

# Prepare 24m data
pcv_trend_24m <- data %>%
  group_by(Year, Quarter, imd_quintile) %>%
  summarise(mean_PCV = mean(PCV_24m, na.rm = TRUE), .groups = "drop") %>%
  mutate(Timepoint = "24 months")

# Combine for faceting
pcv_combined <- bind_rows(pcv_trend_12m, pcv_trend_24m) %>%
  arrange(Year, Quarter, imd_quintile) %>%
  mutate(
    Quarter_Label = paste(Year, Quarter, sep = " "),
    Quarter_Label = factor(Quarter_Label, levels = unique(paste(Year, Quarter, sep = " ")))
  )

# Key time points
change_point <- which(levels(pcv_combined$Quarter_Label) == "2020/2021 Q1")
covid_start <- change_point
covid_end <- which(levels(pcv_combined$Quarter_Label) == "2021/2022 Q4")

# Plot with facet_wrap
ggplot(pcv_combined, aes(x = Quarter_Label, y = mean_PCV,
                         color = as.factor(imd_quintile), group = imd_quintile)) +
  annotate("rect", xmin = covid_start, xmax = covid_end, ymin = -Inf, ymax = Inf,
           fill = "grey80", alpha = 0.4) +
  geom_hline(aes(yintercept = 95, linetype = "95% Target"), color = "#44AA99", linewidth = 1) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = change_point, linetype = "dashed", color = "grey40", linewidth = 1.2) +
  annotate("text", x = change_point - 2, y = 83, label = "Schedule Change", color = "grey40", size = 3, angle = 90) +
  annotate("text", x = mean(c(covid_start, covid_end)) + 1, y = 80.5,
           label = "COVID-19 pandemic", size = 3.2, color = "black") +
  annotate("text", x = 7, y = 97, label = "2+1", color = "black", fontface = "bold", size = 4) +
  annotate("text", x = 38, y = 97, label = "1+1", color = "black", fontface = "bold", size = 4) +
  scale_color_manual(values = pastel_palette,
                     name = "IMD Quintile",
                     labels = c("1 (Least deprived)", "2", "3", "4", "5 (Most deprived)")) +
  scale_linetype_manual(name = NULL, values = c("95% Target" = "dotted"),
                        guide = guide_legend(order = 2, override.aes = list(color = "#44AA99"))) +
  scale_y_continuous(limits = c(80, 100), breaks = seq(80, 100, 5)) +
  labs(x = "Time (Year - Quarter)",
       y = "Mean PCV Uptake (%)") +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "top") +
  facet_wrap(~Timepoint, ncol = 1)

############################################
#### BOOSTER RETENTION ANALYSIS ####
############################################

#### Gap Scatterplot: Pre vs Post Schedule Change (with 1-year lag) ####

# Pre-schedule change (2+1 schedule: 2014/2015-2019/2020)
pre_schedule_gap_avg <- pre_schedule_gap %>%
  group_by(utla_name, imd_quintile) %>%
  summarise(
    PCV_12m_lag = mean(PCV_12m_lag, na.rm = TRUE),
    PCV_24m = mean(PCV_24m, na.rm = TRUE),
    gap = mean(Booster_Gap, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(period = "1. Pre-Schedule Change (2+1)")

# Post-schedule change (1+1 schedule: 2021/2022-2024/2025)
post_schedule_gap_avg <- post_schedule_gap %>%
  group_by(utla_name, imd_quintile) %>%
  summarise(
    PCV_12m_lag = mean(PCV_12m_lag, na.rm = TRUE),
    PCV_24m = mean(PCV_24m, na.rm = TRUE),
    gap = mean(Booster_Gap, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(period = "2. Post-Schedule Change (1+1)")

# Combine both periods
combined_schedule_gap_data <- bind_rows(pre_schedule_gap_avg, post_schedule_gap_avg)

# Outlier detection function
get_outliers <- function(data, n_outliers = 3) {
  iqr_val <- IQR(data$gap, na.rm = TRUE)
  q3 <- quantile(data$gap, 0.75, na.rm = TRUE)
  outlier_threshold <- q3 + 1.5 * iqr_val
  
  # Only label the worst N outliers to reduce crowding
  outliers <- data %>% 
    filter(gap >= outlier_threshold) %>%
    arrange(desc(gap)) %>%
    slice_head(n = n_outliers)
  
  above_line <- data %>% filter(PCV_24m > PCV_12m_lag)
  
  lowest_uptake <- data %>%
    mutate(min_uptake = pmin(PCV_12m_lag, PCV_24m, na.rm = TRUE)) %>%
    filter(min_uptake == min(min_uptake, na.rm = TRUE)) %>%
    slice(1)
  
  return(list(outliers = outliers, above_line = above_line, lowest = lowest_uptake))
}

# Get outliers for each period (limit to 1)
pre_outliers <- get_outliers(pre_schedule_gap_avg, n_outliers = 1)
post_outliers <- get_outliers(post_schedule_gap_avg, n_outliers = 1)

# Function to identify UTLAs above the retention line
get_above_line_utlas <- function(data) {
  data %>% filter(PCV_24m > PCV_12m_lag)  # Points above the diagonal line
}

# Get above-line UTLAs for each period
pre_above_line <- get_above_line_utlas(pre_schedule_gap_avg)
post_above_line <- get_above_line_utlas(post_schedule_gap_avg)

ggplot(combined_schedule_gap_data, aes(x = PCV_12m_lag, y = PCV_24m, color = as.factor(imd_quintile))) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey70", linewidth = 1) +
  geom_point(size = 2.5, shape = 16, alpha = 0.8) +
  
  # Color scale  
  scale_color_manual(
    values = pastel_palette[1:5],
    name = "IMD Quintile",
    labels = c("1 (Least deprived)", "2", "3", "4", "5 (Most deprived)")
  ) +
  
  # Labels 
  ggrepel::geom_text_repel(
    data = pre_outliers$outliers,
    aes(label = paste0(utla_name, " (", round(gap, 1), "%)")),
    color = "red", size = 3, fontface = "bold", max.overlaps = Inf
  ) +
  
  ggrepel::geom_text_repel(
    data = post_outliers$outliers,
    aes(label = paste0(utla_name, " (", round(gap, 1), "%)")),
    color = "darkred", size = 3, fontface = "bold", max.overlaps = Inf
  ) +
  
  ggrepel::geom_text_repel(
    data = bind_rows(pre_above_line, post_above_line) %>%
      mutate(positive_gap = PCV_24m - PCV_12m_lag) %>%
      filter(positive_gap == max(positive_gap, na.rm = TRUE)) %>%
      slice(1),  # Just in case of ties, take the first one
    aes(label = paste0(utla_name, "\n(+", round(PCV_24m - PCV_12m_lag, 1), "%)")),
    color = "blue",
    size = 3,
    fontface = "bold",
    max.overlaps = Inf
  ) +
  
  
  # Add manual linetype legend without inheriting aesthetics
  geom_line(data = data.frame(x = c(-1, -1), y = c(-1, -1), 
                              linetype = "Perfect retention (12m = 24m)"),
            aes(x = x, y = y, linetype = linetype), 
            color = "grey70", alpha = 0, inherit.aes = FALSE) +
  
  scale_linetype_manual(
    name = "",
    values = c("Perfect retention (12m = 24m)" = "dashed"),
    guide = guide_legend(override.aes = list(color = "grey70", alpha = 1))
  ) +
  
  # Facet by time period
  facet_wrap(~ period, ncol = 2) +
  
  # Labels and styling
  labs(
    x = "PCV Uptake at 12 Months (Previous Year) (%)",
    y = "PCV Uptake at 24 Months (%)",
    caption = "Red: Largest booster gaps | Green: Lowest overall coverage | Blue: Higher 24m than 12m coverage"
  ) +
  
  # Coordinate limits
  coord_fixed(ratio = 1, xlim = c(69, 101), ylim = c(65, 100)) +
  
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey95", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.caption = element_text(hjust = 0.5, size = 10, margin = margin(t = 10)),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12, margin = margin(b = 15)),
    strip.text = element_text(face = "bold", size = 12, margin = margin(b = 10)),
    panel.spacing = unit(1.5, "lines"),
    plot.margin = margin(t = 20, r = 100, b = 20, l = 100, unit = "pt")
  )

#### Booster Drop-off Histogram: Pre vs Post Schedule Change ####

# Use the pre and post schedule gap data
pre_dropoff_data <- pre_schedule_gap_avg %>%
  mutate(
    dropoff_pct = gap,
    period = "1. Pre-Schedule Change (2+1)"
  ) %>%
  filter(!is.na(dropoff_pct))

post_dropoff_data <- post_schedule_gap_avg %>%
  mutate(
    dropoff_pct = gap,
    period = "2. Post-Schedule Change (1+1)"
  ) %>%
  filter(!is.na(dropoff_pct))

# Combine the data
combined_dropoff_data <- bind_rows(pre_dropoff_data, post_dropoff_data)

# Get outlier thresholds for each period
pre_iqr <- IQR(pre_dropoff_data$dropoff_pct, na.rm = TRUE)
pre_q3 <- quantile(pre_dropoff_data$dropoff_pct, 0.75, na.rm = TRUE)
pre_outlier_threshold <- pre_q3 + 1.5 * pre_iqr

post_iqr <- IQR(post_dropoff_data$dropoff_pct, na.rm = TRUE)
post_q3 <- quantile(post_dropoff_data$dropoff_pct, 0.75, na.rm = TRUE)
post_outlier_threshold <- post_q3 + 1.5 * post_iqr

# Create the comparison histogram
ggplot(combined_dropoff_data, aes(x = dropoff_pct)) +
  geom_histogram(
    aes(fill = after_stat(x)),
    binwidth = 1,
    color = "white",
    alpha = 0.8,
    center = 0,
    na.rm = TRUE
  ) +
  scale_fill_gradient2(
    low = "#332288", 
    mid = "#DDCC77", 
    high = "#CC6677",
    midpoint = 0, 
    guide = "none"
  ) +
  
  # Reference line at zero for both panels
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 1) +
  
  # Outlier region shading (different for each panel)
  geom_rect(data = data.frame(period = "1. Pre-Schedule Change (2+1)", 
                              xmin = pre_outlier_threshold, 
                              xmax = max(pre_dropoff_data$dropoff_pct) + 0.5),
            aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = Inf), 
            fill = "#CC6677", alpha = 0.2, inherit.aes = FALSE) +
  
  geom_rect(data = data.frame(period = "2. Post-Schedule Change (1+1)", 
                              xmin = post_outlier_threshold, 
                              xmax = max(post_dropoff_data$dropoff_pct) + 0.5),
            aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = Inf), 
            fill = "#CC6677", alpha = 0.2, inherit.aes = FALSE) +
  
  # Labels for perfect retention
  annotate("text", x = 0.5, y = Inf, 
           label = "Perfect retention", 
           color = "grey40", hjust = 0, vjust = 1.1, size = 3) +
  
  # Facet by time period
  facet_wrap(~ period, ncol = 2) +
  
  labs(
    x = "Booster Drop-off (12m[previous year] - 24m coverage, percentage points)",
    y = "Number of Local Authorities",
    caption = sprintf("Pre-schedule: Mean %.1f%%, Median %.1f%% | Post-schedule: Mean %.1f%%, Median %.1f%%",
                      mean(pre_dropoff_data$dropoff_pct, na.rm = TRUE),
                      median(pre_dropoff_data$dropoff_pct, na.rm = TRUE),
                      mean(post_dropoff_data$dropoff_pct, na.rm = TRUE),
                      median(post_dropoff_data$dropoff_pct, na.rm = TRUE))
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    plot.caption = element_text(hjust = 0.5, size = 10, margin = margin(t = 10))
  ) +
  
  scale_x_continuous(
    breaks = seq(-5, 25, by = 5),
    minor_breaks = seq(-5, 25, by = 1),
    limits = c(-2, max(combined_dropoff_data$dropoff_pct) + 1)
  )

# Print summary statistics for booster gap with 1-year lag
cat("\n=== BOOSTER DROP-OFF DISTRIBUTION WITH 1-YEAR LAG ===\n")
cat("PRE-Schedule Change (2+1):\n")
cat("• Mean drop-off:", round(mean(pre_dropoff_data$dropoff_pct, na.rm = TRUE), 2), "percentage points\n")
cat("• Median drop-off:", round(median(pre_dropoff_data$dropoff_pct, na.rm = TRUE), 2), "percentage points\n")
cat("• Outlier threshold:", round(pre_outlier_threshold, 1), "percentage points\n\n")

cat("POST-Schedule Change (1+1):\n")
cat("• Mean drop-off:", round(mean(post_dropoff_data$dropoff_pct, na.rm = TRUE), 2), "percentage points\n")
cat("• Median drop-off:", round(median(post_dropoff_data$dropoff_pct, na.rm = TRUE), 2), "percentage points\n")
cat("• Outlier threshold:", round(post_outlier_threshold, 1), "percentage points\n")
