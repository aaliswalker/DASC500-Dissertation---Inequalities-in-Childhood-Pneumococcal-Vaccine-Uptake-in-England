setwd("C:/Users/User/OneDrive/HSD MSC/Diss Data/Stage 2 data") ## CHANGE WD

#### 🫧 Load stuff ####
library("tidyverse")
library("readr")
library("ggplot2")
library("viridis")
library("sf")
library("dplyr")
library("ggrepel")
library("patchwork")
library("ggnewscale")
library("sf")
library("grid")
library("dplyr")
library("stringr")

data = read.csv("COVER_All_Years_MERGED_WITH_IMD.csv")

#### Colour palette ####
pastel_palette = c(
  "#88CCEE", 
  "#CC6677",  
  "#117733", 
  "#DDCC77",
  "#332288",
  "#EE7733",
  "#44AA99" 
)

#### 🫧 Prepare and clean data ####
data_clean = data %>%
  filter(!is.na(PCV_12m) & !is.na(PCV_24m) & !is.na(Population_12m) & !is.na(imd_quintile)) %>%
  filter(PCV_12m >= 0 & PCV_24m >= 0) %>%  # Remove negative values
  filter(PCV_12m <= 100 & PCV_24m <= 100)  # Remove values >100%

#### 🫧 Calculate susceptibility by deprivation quintile ####
quarterly_deprivation = data_clean %>%
  group_by(Year, Quarter, imd_quintile) %>%
  summarise(
    PCV_12m_pct = weighted.mean(PCV_12m, Population_12m, na.rm = TRUE),
    PCV_24m_pct = weighted.mean(PCV_24m, Population_24m, na.rm = TRUE),
    Population = sum(Population_12m, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(imd_quintile)) %>%  # Remove rows with missing quintile
  arrange(Year, Quarter, imd_quintile) %>%
  mutate(
    # Convert to proportions
    PCV_12m = pmax(0, pmin(1, PCV_12m_pct / 100)),  # Bound between 0 and 1
    PCV_24m = pmax(0, pmin(1, PCV_24m_pct / 100)),
    
    # Ensure 24m doesn't exceed 12m
    PCV_24m = pmin(PCV_24m, PCV_12m),
    
    # Calculate dose groups
    p0 = 1 - PCV_12m,           # No doses
    p1 = PCV_12m - PCV_24m,     # Primary only
    p2 = PCV_24m,               # Complete schedule
    
    # Assign schedule
    schedule_group = if_else(Year < "2020/2021", "2+1", "1+1"),
    
    # Assign VE values
    VE_1 = case_when(
      schedule_group == "2+1" ~ 0.606,  # Primary dose in 2+1
      schedule_group == "1+1" ~ 0.761   # Primary dose in 1+1 (using secondary from 2+1)
    ),
    VE_2 = 0.782,  # Booster dose (same for both schedules)
    
    # Calculate susceptibility
    Susceptibility_prop = p0 + p1 * (1 - VE_1) + p2 * (1 - VE_2),
    Susceptibility_n = Susceptibility_prop * Population,
    
    # Create time variable
    YearQuarter = paste0(Year, " ", Quarter)
  ) %>%
  filter(!is.na(Susceptibility_prop) & !is.infinite(Susceptibility_prop))  # Remove invalid values

#### 🫧 Calculate susceptibility by schedule only ####
quarterly_schedule = data_clean %>%
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
    PCV_12m = pmax(0, pmin(1, PCV_12m_pct / 100)),
    PCV_24m = pmax(0, pmin(1, PCV_24m_pct / 100)),
    
    # Ensure 24m doesn't exceed 12m
    PCV_24m = pmin(PCV_24m, PCV_12m),
    
    # Calculate dose groups
    p0 = 1 - PCV_12m,
    p1 = PCV_12m - PCV_24m,
    p2 = PCV_24m,
    
    # Assign schedule
    schedule_group = if_else(Year < "2020/2021", "2+1", "1+1"),
    
    # Assign VE values
    VE_1 = case_when(
      schedule_group == "2+1" ~ 0.606,
      schedule_group == "1+1" ~ 0.761
    ),
    VE_2 = 0.782,
    
    # Calculate susceptibility
    Susceptibility_prop = p0 + p1 * (1 - VE_1) + p2 * (1 - VE_2),
    Susceptibility_n = Susceptibility_prop * Population,
    
    # Create time variable
    YearQuarter = paste0(Year, " ", Quarter)
  ) %>%
  filter(!is.na(Susceptibility_prop) & !is.infinite(Susceptibility_prop))

#### 🫧 Create faceted plot ####

# Prepare data for faceting
quarterly_facet_dep <- quarterly_deprivation %>%
  mutate(
    facet_group = "By Deprivation",
    Group = as.factor(imd_quintile)
  ) %>%
  select(YearQuarter, Susceptibility_prop, Group, facet_group, Year, Quarter) %>%
  filter(!is.na(Group))  # Remove missing quintiles

quarterly_facet_sched <- quarterly_schedule %>%
  mutate(
    facet_group = "By Schedule",
    Group = schedule_group
  ) %>%
  select(YearQuarter, Susceptibility_prop, Group, facet_group, Year, Quarter)

# Combine data
facet_data <- bind_rows(quarterly_facet_dep, quarterly_facet_sched)

# Create proper time ordering
unique_quarters <- facet_data %>%
  distinct(YearQuarter, Year, Quarter) %>%
  arrange(Year, Quarter) %>%
  pull(YearQuarter)

facet_data <- facet_data %>%
  mutate(
    time_order = match(YearQuarter, unique_quarters)
  ) %>%
  filter(!is.na(time_order))  # Remove any unmatched time points

# Select Q2 labels for cleaner x-axis
q2_positions <- which(str_detect(unique_quarters, "Q2"))
q2_labels <- unique_quarters[q2_positions]

# Split data for separate color scales
facet_data_dep <- facet_data %>% filter(facet_group == "By Deprivation")
facet_data_sched <- facet_data %>% filter(facet_group == "By Schedule")

# Find schedule change position
schedule_change_pos <- which(unique_quarters == "2020/2021 Q1")
if(length(schedule_change_pos) == 0) {
  schedule_change_pos <- which(str_detect(unique_quarters, "2020.*Q1"))[1]
}

# Create the plot
ggplot() +
  # Deprivation layer
  geom_line(
    data = facet_data_dep,
    aes(x = time_order, y = Susceptibility_prop, color = Group, group = Group),
    linewidth = 1.2  # Changed from size to linewidth
  ) +
  geom_point(
    data = facet_data_dep,
    aes(x = time_order, y = Susceptibility_prop, color = Group),
    size = 2
  ) +
  scale_color_manual(
    name = "IMD Quintile",
    values = c(
      "1" = "#88CCEE",  # Least deprived
      "2" = "#CC6677",
      "3" = "#117733",
      "4" = "#DDCC77",
      "5" = "#332288"   # Most deprived
    ),
    guide = guide_legend(order = 1)
  ) +
  
  # Reset color scale
  ggnewscale::new_scale_color() +
  
  # Schedule layer
  geom_line(
    data = facet_data_sched,
    aes(x = time_order, y = Susceptibility_prop, color = Group, group = Group),
    linewidth = 1.0  # Changed from size to linewidth
  ) +
  geom_point(
    data = facet_data_sched,
    aes(x = time_order, y = Susceptibility_prop, color = Group),
    size = 2
  ) +
  scale_color_manual(
    name = "Schedule",
    values = c(
      "2+1" = "#88CCEE",
      "1+1" = "#CC6677"
    ),
    guide = guide_legend(order = 2)
  ) +
  
  # Add vertical line at schedule change
  {if(!is.na(schedule_change_pos)) 
    geom_vline(xintercept = schedule_change_pos, 
               linetype = "dashed", color = "grey50", linewidth = 0.8)
  } +
  
  # Faceting and styling
  facet_wrap(~facet_group) +
  scale_x_continuous(
    breaks = q2_positions,
    labels = q2_labels
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 0.01),
    limits = c(0, 0.40) 
  ) +
  labs(
    # title = "Estimated Proportion of Susceptible Children by Quarter",
    #subtitle = "Stratified by Deprivation and Vaccination Schedule",
    x = "Cohort (Year & Quarter)",
    y = "Proportion Susceptible",
    caption = "Dashed line indicates schedule change from 2+1 to 1+1 (January 2020)"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    strip.text = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    legend.box = "vertical",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
  )


############################################################
############################################################


#### 🫧 Prepare the map data ####
england_map <- st_read("C:/Users/User/OneDrive/HSD MSC/Diss Data/Stage 2 data/Local_Authority_(Upper_Tier)_IMD_2019_(WGS84)/Local_Authority_(Upper_Tier)_IMD_2019_(WGS84).shp")

#### 🫧 Prepare susceptibility data by LA and Year (using current methodology) ####
la_susceptibility <- data_clean %>%
  group_by(Year, ONS_Code, utla_name) %>%
  summarise(
    PCV_12m_pct = weighted.mean(PCV_12m, Population_12m, na.rm = TRUE),
    PCV_24m_pct = weighted.mean(PCV_24m, Population_24m, na.rm = TRUE),
    Population = sum(Population_12m, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Convert to proportions (same as current code)
    PCV_12m = pmax(0, pmin(1, PCV_12m_pct / 100)),
    PCV_24m = pmax(0, pmin(1, PCV_24m_pct / 100)),
    
    # Ensure 24m doesn't exceed 12m
    PCV_24m = pmin(PCV_24m, PCV_12m),
    
    # Calculate dose groups
    p0 = 1 - PCV_12m,           # No doses
    p1 = PCV_12m - PCV_24m,     # Primary only
    p2 = PCV_24m,               # Complete schedule
    
    # Assign schedule and VE values
    schedule_group = if_else(Year < "2020/2021", "2+1", "1+1"),
    Vaccine_Schedule = if_else(Year < "2020/2021", 0, 1),  # For compatibility
    
    VE_1 = case_when(
      schedule_group == "2+1" ~ 0.606,
      schedule_group == "1+1" ~ 0.761
    ),
    VE_2 = 0.782,
    
    # Calculate susceptibility
    Susceptibility = p0 + p1 * (1 - VE_1) + p2 * (1 - VE_2)
  ) %>%
  filter(!is.na(Susceptibility) & !is.infinite(Susceptibility)) %>%
  mutate(
    Year = as.character(Year),
    ONS_Code = as.character(ONS_Code)
  )

#### 🫧 Join with shapefile ####
map_data <- england_map %>%
  left_join(la_susceptibility, by = c("ctyua19cd" = "ONS_Code")) %>%
  filter(!is.na(Year))  # Remove areas with no data

#### 🫧 Check for data mismatches ####
# Identify shapefile codes not in your data
missing_in_data <- setdiff(england_map$ctyua19cd, la_susceptibility$ONS_Code)
cat("Shapefile codes not in data:", length(missing_in_data), "\n")

# Identify data codes not in shapefile
missing_in_shp <- setdiff(la_susceptibility$ONS_Code, england_map$ctyua19cd)
cat("Data codes not in shapefile:\n")
print(missing_in_shp)

#### 🫧 Create the susceptibility map ####
# Prepare plot data with proper labeling
plot_data <- map_data %>%
  mutate(
    Schedule = factor(schedule_group, 
                      levels = c("2+1", "1+1"),
                      labels = c("2+1 Schedule", "1+1 Schedule")),
    Year_Schedule = paste(Year, Schedule, sep = " - ")
  ) %>%
  filter(!is.na(Susceptibility))

# Create the map
susceptibility_map <- ggplot(plot_data) +
  geom_sf(aes(fill = Susceptibility), color = "white", linewidth = 0.05) +
  scale_fill_viridis_c(
    name = "Proportion\nSusceptible",
    option = "plasma",
    direction = 1,
    limits = c(0.20, 0.50),  # Slightly wider than your data range
    breaks = seq(0.20, 0.50, by = 0.05),
    labels = scales::percent_format(accuracy = 1)
  ) +
  facet_wrap(~ Year_Schedule, 
             ncol = 4,  # 4 columns for better layout
             labeller = labeller(.cols = label_value)) +
  labs(
    #title = "Geographic Variation in PCV Susceptibility Across UK Local Authorities",
    # subtitle = "Estimated susceptibility to pneumococcal disease under 2+1 and 1+1 schedules (2013-2024)",
    caption = "Data: COVER / ONS \nVE assumptions: 2+1 primary=60.6%, secondary=76.1%, booster=78.2%; 1+1 primary=76.1%, booster=78.2%"
  ) +
  theme_void(base_size = 10) +
  theme(
    # Facet labels
    strip.text = element_text(size = 8, margin = margin(b = 2), face = "bold"),
    
    # Legend
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.4, "cm"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    
    # Titles
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 10), size = 10),
    plot.caption = element_text(hjust = 0.5, size = 8, color = "grey60"),
    
    # Panel spacing
    panel.spacing = unit(0.3, "lines"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  ) +
  guides(fill = guide_colorbar(
    title.position = "top",
    title.hjust = 0.5
  ))

# Display the map
print(susceptibility_map)

#### 🫧 Check for missing susceptibility data ####
cat("\nMissing susceptibility data:\n")
missing_susceptibility <- plot_data %>%
  filter(is.na(Susceptibility)) %>%
  distinct(Year, ctyua19cd, ctyua19nm) %>%
  arrange(Year, ctyua19nm)

if(nrow(missing_susceptibility) > 0) {
  print(missing_susceptibility)
} else {
  cat("No missing susceptibility data found.\n")
}

#### 🫧 Summary statistics ####
cat("\nSusceptibility mapping summary:\n")
cat("Years included:", paste(sort(unique(plot_data$Year)), collapse = ", "), "\n")
cat("Number of LAs with data:", length(unique(plot_data$ctyua19cd)), "\n")
cat("Susceptibility range:", round(min(plot_data$Susceptibility, na.rm = TRUE), 3), 
    "to", round(max(plot_data$Susceptibility, na.rm = TRUE), 3), "\n")


############################################################
############################################################

#### 🫧 Geographical Vaccine Uptake and Gap Maps ####

#### 🫧 Build uptake_map_data for coverage & booster-gap maps ####

# Aggregate by LA & Year (weighted)
la_uptake <- data_clean %>%
  group_by(Year, ONS_Code, utla_name) %>%
  summarise(
    PCV_12m_pct = weighted.mean(PCV_12m, Population_12m, na.rm = TRUE),
    PCV_24m_pct = weighted.mean(PCV_24m, Population_24m, na.rm = TRUE),
    Population  = sum(Population_12m, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Bound to [0, 100] and enforce 24m <= 12m
  mutate(
    PCV_12m_pct = pmax(0, pmin(100, PCV_12m_pct)),
    PCV_24m_pct = pmax(0, pmin(100, PCV_24m_pct)),
    PCV_24m_pct = pmin(PCV_24m_pct, PCV_12m_pct)
  ) %>%
  # Booster gap (pp) and schedule flag
  mutate(
    booster_gap = PCV_12m_pct - PCV_24m_pct,
    schedule_group = if_else(Year < "2020/2021", "2+1", "1+1"),
    Year_Schedule  = paste(Year, if_else(schedule_group=="2+1","2+1 Schedule","1+1 Schedule"), sep = " - ")
  ) %>%
  mutate(
    ONS_Code = as.character(ONS_Code),
    Year = as.character(Year)
  )

# Join to shapefile (england_map already loaded)
uptake_map_data <- england_map %>%
  left_join(la_uptake, by = c("ctyua19cd" = "ONS_Code")) %>%
  filter(!is.na(Year))   # keep only rows with mapped data


#### 🫧 12-month coverage map (standalone) ####
map_12m_large <- ggplot(uptake_map_data %>% filter(!is.na(PCV_12m_pct))) +
  geom_sf(aes(fill = PCV_12m_pct), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(
    name = "12-month\nCoverage (%)",
    option = "viridis",
    direction = 1,
    limits = c(70, 100),
    breaks = seq(70, 100, by = 10),
    labels = function(x) paste0(x, "%")
  ) +
  facet_wrap(~ Year_Schedule, ncol = 4) +
  labs(caption = "Data: COVER / ONS | Higher values indicate better primary coverage") +
  theme_void(base_size = 12) +
  theme(
    strip.text = element_text(size = 10, face = "bold", margin = margin(b = 5)),
    legend.position = "bottom",
    legend.key.width = unit(3, "cm"),
    legend.key.height = unit(0.6, "cm"),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    plot.caption = element_text(hjust = 0.5, size = 10, color = "grey60", margin = margin(t = 10)),
    panel.spacing = unit(0.5, "lines")
  ) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))

print(map_12m_large)

#### 🫧 24-month coverage map (standalone) ####
map_24m_large <- ggplot(uptake_map_data %>% filter(!is.na(PCV_24m_pct))) +
  geom_sf(aes(fill = PCV_24m_pct), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(
    name = "24-month\nCoverage (%)",
    option = "viridis",
    direction = 1,
    limits = c(60, 100),
    breaks = seq(60, 100, by = 10),
    labels = function(x) paste0(x, "%")
  ) +
  facet_wrap(~ Year_Schedule, ncol = 4) +
  labs(
    #title = "Geographic Variation in PCV Booster Coverage (24 months) Across Local Authorities",
    #subtitle = "Booster dose coverage under 2+1 and 1+1 schedules (2013-2024)",
    caption = "Data: COVER / ONS | Higher values indicate better booster coverage"
  ) +
  theme_void(base_size = 12) +
  theme(
    strip.text = element_text(size = 10, face = "bold", margin = margin(b = 5)),
    legend.position = "bottom",
    legend.key.width = unit(3, "cm"),
    legend.key.height = unit(0.6, "cm"),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12, margin = margin(b = 15)),
    plot.caption = element_text(hjust = 0.5, size = 10, color = "grey60", margin = margin(t = 10)),
    panel.spacing = unit(0.5, "lines")
  ) +
  guides(fill = guide_colorbar(
    title.position = "top",
    title.hjust = 0.5
  ))

print(map_24m_large)

