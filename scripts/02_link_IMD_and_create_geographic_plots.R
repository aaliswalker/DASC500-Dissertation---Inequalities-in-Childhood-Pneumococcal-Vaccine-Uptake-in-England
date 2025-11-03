#===============================================================================
# SCRIPT: 02_link_IMD_and_create_geographic_plots.R
# PURPOSE: Link PCV uptake data to Index of Multiple Deprivation and create geographic visualizations
# 
# DESCRIPTION: 
# - Processes and cleans IMD data from UTLA_summaries.xlsx
# - Combines all cleaned quarterly COVER files into master dataset
# - Handles missing data through imputation strategies
# - Merges COVER vaccine data with IMD quintile assignments
# - Creates choropleth maps showing IMD quintile distributions across England
#
# MAIN SECTIONS:
# - Package loading and setup
# - IMD data processing and quintile assignment
# - COVER data consolidation and cleaning
# - Data merging and validation
# - Geographic visualization of IMD quintiles
#
# INPUTS: Cleaned COVER quarterly files, UTLA_summaries.xlsx, shapefile data
# OUTPUTS: COVER_All_Years_MERGED_WITH_IMD.csv, IMD quintile maps
#===============================================================================

#### 🫧 Package Loading and Setup ####
library(here)
library(readxl)
library(readr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(colorspace)
library(tidyr)
library(ggrepel)
library(sf)         
library(viridis) 

# Define directory paths
imd_data_dir <- here("data")  
cleaned_data_dir <- here("cleaned_Data") 
output_dir <- here("output")

# Create output directory if it doesn't exist
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

#### 🫧 IMD Data Processing and Quintile Assignment ####

# Load IMD data from data folder
imd_file_path <- file.path(imd_data_dir, "UTLA_summaries.xlsx")

# Check if file exists
if (!file.exists(imd_file_path)) {
  stop("IMD file not found at: ", imd_file_path)
}

# Check available sheets
cat("Available sheets in IMD file:\n")
print(excel_sheets(imd_file_path))

# Load IMD data
imd_data <- read_excel(imd_file_path, sheet = "IMD") 

# View column names
cat("IMD data columns:\n")
print(names(imd_data))

# Extract columns needed
imd_subset <- imd_data[, c("Upper Tier Local Authority District code (2019)", 
                           "Upper Tier Local Authority District name (2019)", 
                           "IMD - Average score")]

# Rename for clarity
colnames(imd_subset) <- c("utla_code", "utla_name", "imd_score")

# Remove City of London & Isles of Scilly
imd_retained_utla_subset = imd_subset %>%
  filter(
    !utla_name %in% c("City of London", "Isles of Scilly")
  )

# Create IMD quintiles
imd_quintiles_assigned <- imd_retained_utla_subset %>%
  mutate(imd_quintile = ntile(imd_score, 5))

# Save cleaned IMD data to output directory
write.csv(imd_quintiles_assigned, file.path(output_dir, "cleaned_imd_data.csv"), row.names = FALSE)
cat("IMD data processed and saved\n")

#### 🫧 COVER Data Consolidation and Cleaning ####

# Find COVER files (without changing working directory)
files <- list.files(cleaned_data_dir, pattern = "COVER_\\d{4}.*_Cleaned\\.csv", full.names = TRUE)

if (length(files) == 0) {
  stop("No COVER files found in: ", cleaned_data_dir)
}

cat("Found", length(files), "COVER files to process\n")

# Read and combine all COVER files
cover_all <- bind_rows(lapply(files, function(file) {
  cat("Processing:", basename(file), "\n")
  # Read everything as character to avoid type mismatch
  df <- read_csv(file, col_types = cols(.default = "c"), show_col_types = FALSE)
  return(df)
}))

# Check data
cat("Combined COVER data dimensions:", nrow(cover_all), "x", ncol(cover_all), "\n")

# Convert all entries starting with 'N' to NA
cover_all <- cover_all %>%
  mutate(across(
    everything(),
    ~ ifelse(grepl("^N", .x), NA, .x)
  ))

# Replace anything starting with '[' to NA
cover_all <- cover_all %>%
  mutate(across(
    everything(),
    ~ ifelse(grepl("^\\[", .x), NA, .x)
  ))

# Fix column characteristics 
cover_all <- cover_all %>%
  mutate(
    PCV_12m = as.numeric(PCV_12m),
    PCV_24m = as.numeric(PCV_24m),
    Population_12m = as.numeric(Population_12m),
    Population_24m = as.numeric(Population_24m),
    Timepoint = as.factor(Timepoint),
    Quarter = as.factor(Quarter),
    Vaccine_Schedule = as.factor(Vaccine_Schedule)
  )

# Check data structure
cat("Data structure summary:\n")
cat("Rows:", nrow(cover_all), "\n")
cat("Columns:", ncol(cover_all), "\n")

# Check missing values 
total_missing <- sum(is.na(cover_all))
col_missing <- colSums(is.na(cover_all))
pct_missing <- round(colMeans(is.na(cover_all)) * 100, 2)

cat("Total missing values:", total_missing, "\n")
cat("Missing values by column:\n")
print(pct_missing[pct_missing > 0])

#### 🫧 Data Quality and Imputation ####

# Save dataset before imputing
write.csv(cover_all, file.path(output_dir, "COVER_All_Years_UNIMPUTED.csv"), row.names = FALSE)
cat("Unimputed data saved\n")

# Impute missing values by vaccine schedule group
cover_all_imputed <- cover_all %>%
  group_by(Vaccine_Schedule) %>%
  mutate(
    PCV_12m = ifelse(is.na(PCV_12m), mean(PCV_12m, na.rm = TRUE), PCV_12m),
    PCV_24m = ifelse(is.na(PCV_24m), mean(PCV_24m, na.rm = TRUE), PCV_24m),
    Population_12m = ifelse(is.na(Population_12m), median(Population_12m, na.rm = TRUE), Population_12m),
    Population_24m = ifelse(is.na(Population_24m), median(Population_24m, na.rm = TRUE), Population_24m)
  ) %>%
  ungroup()
# Note: mean for PCV since it's % uptake; median for population since it's more likely to be skewed

# Save imputed dataset
write.csv(cover_all_imputed, file.path(output_dir, "COVER_All_Years_IMPUTED.csv"), row.names = FALSE)
cat("Imputed data saved\n")

#### 🫧 Data Merging and Validation ####

# Load saved data
cover <- read.csv(file.path(output_dir, "COVER_All_Years_IMPUTED.csv"))
imd <- read.csv(file.path(output_dir, "cleaned_imd_data.csv"))

# Check data types and content
cat("Sample ONS codes from COVER:", head(cover$ONS_Code), "\n")
cat("Sample UTLA codes from IMD:", head(imd$utla_code), "\n")

# Trim white space
cover$ONS_Code <- trimws(cover$ONS_Code)
imd$utla_code <- trimws(imd$utla_code)

# Check for mismatches
cover_ons_not_in_imd <- setdiff(cover$ONS_Code, imd$utla_code)
imd_not_in_cover <- setdiff(imd$utla_code, cover$ONS_Code) # City of London and Isle of Scilly

cat("=== UNMATCHED RECORDS ===\n")
cat("COVER codes not in IMD:", length(cover_ons_not_in_imd), "\n")
cat("IMD codes not in COVER:", length(imd_not_in_cover), "\n")
if (length(imd_not_in_cover) > 0) {
  cat("Missing from COVER:\n")
  print(imd_not_in_cover)
}

# Count records before filtering
records_before <- nrow(cover)
cat("\nRecords BEFORE filtering:", format(records_before, big.mark = ","), "\n")

# Drop rows that don't match IMD data
cover <- cover %>%
  filter(ONS_Code %in% imd$utla_code)

# Count records after filtering
records_after <- nrow(cover)
records_removed <- records_before - records_after

cat("Records AFTER filtering:", format(records_after, big.mark = ","), "\n")
cat("Records REMOVED:", format(records_removed, big.mark = ","), 
    "(", round(100 * records_removed / records_before, 1), "%)\n")

# Merge datasets and create IMD quintiles
cover_merged <- cover %>%
  left_join(imd, by = c("ONS_Code" = "utla_code")) 

# Check quintile distribution
quintile_summary <- cover_merged %>%
  distinct(ONS_Code, imd_quintile) %>%
  count(imd_quintile)

cat("IMD Quintile Distribution:\n")
print(quintile_summary)

# Save final merged dataset
write.csv(cover_merged, file.path(output_dir, "COVER_All_Years_MERGED_WITH_IMD.csv"), row.names = FALSE)
cat("Merged data saved\n")

#### 🫧 Geographic Visualization of IMD Quintiles ####

# Try to load England shapefile
shapefile_path <- file.path(imd_data_dir, "Shapefile", 
                            "Local_Authority_(Upper_Tier)_IMD_2019_(WGS84).shp")

if (!file.exists(shapefile_path)) {
  cat("⚠️ Shapefile not found at:", shapefile_path, "\n")
  cat("Skipping geographic visualization\n")
} else {
  cat("Loading shapefile from:", shapefile_path, "\n")
  
  # Load England shapefile
  england_map <- st_read(shapefile_path, quiet = TRUE)
  
  # Prepare data for quintile mapping
  imd_quintile_map_data <- england_map %>%
    left_join(
      cover_merged %>%
        filter(Year == "2019/2020") %>%  # Use most recent year
        select(ONS_Code, utla_name, imd_quintile) %>%
        distinct() %>%
        mutate(ONS_Code = as.character(ONS_Code)),
      by = c("ctyua19cd" = "ONS_Code")
    )
  
  # Set up colour scheme for quintile mapping
  purples <- brewer.pal(5, "Purples")
  purples_intense <- darken(purples, amount = 0.15)
  
  # Create IMD quintile assignment map
  quintile_map <- ggplot(imd_quintile_map_data) +
    geom_sf(aes(fill = as.factor(imd_quintile)), color = "white", linewidth = 0.2) +
    scale_fill_manual(
      name = "IMD Quintile",
      values = purples_intense, #viridis::viridis(5),
      labels = c("1 (Least Deprived)", "2", "3", "4", "5 (Most Deprived)"),
      na.value = "grey0"
    ) +
    labs(
      title = "Index of Multiple Deprivation Quintiles by Upper Tier Local Authority",
      subtitle = "England, 2019",
      caption = "Each local authority assigned to quintile based on average IMD score"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      plot.caption = element_text(size = 9, hjust = 0.5, color = "gray40"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.key.width = unit(2, "cm"),
      legend.key.height = unit(0.5, "cm")
    )
  
  # Display the map
  print(quintile_map)
  
  # Save the map
  ggsave(file.path(output_dir, "IMD_quintiles_map.png"), quintile_map, 
         width = 10, height = 12, dpi = 300)
  cat("IMD quintile map saved\n")
}

#### 🫧 Generate UTLA Lists by IMD Quintile ####

# Create clean list of Local Authorities by IMD quintile
la_by_quintile <- cover_merged %>%
  select(utla_name, imd_quintile) %>%
  distinct() %>%
  filter(!is.na(imd_quintile)) %>%
  arrange(imd_quintile, utla_name)

# Print formatted lists for reference
cat("\n=== UTLA ASSIGNMENTS BY IMD QUINTILE ===\n")
for(q in 1:5) {
  cat("\n=================\n")
  cat("IMD Quintile", q, if(q == 1) "(Least Deprived)" else if(q == 5) "(Most Deprived)", "\n")
  cat("=================\n")
  las <- la_by_quintile %>% 
    filter(imd_quintile == q) %>% 
    pull(utla_name) %>%
    sort()
  cat(paste(las, collapse = "\n"), "\n")
  cat("Total UTLAs in quintile:", length(las), "\n")
}

# Save quintile assignments to CSV for reference
write.csv(la_by_quintile, file.path(output_dir, "UTLA_IMD_quintile_assignments.csv"), row.names = FALSE)
cat("Quintile assignments saved\n")

#### 🫧 Final Validation Summary ####
cat("\n=== FINAL DATA VALIDATION ===\n")
cat("Total UTLAs with quintile assignments:", nrow(la_by_quintile), "\n")
cat("Expected UTLAs (149):", 149, "\n")
cat("Missing assignments:", 149 - nrow(la_by_quintile), "\n")

# Check quintile distribution
quintile_counts <- la_by_quintile %>%
  count(imd_quintile, name = "n_utlas")
cat("UTLAs per quintile:\n")
print(quintile_counts)
