#===============================================================================
# SCRIPT: 02_link_IMD_and_create_geographic_plots.R
# PURPOSE: Link PCV uptake data to Index of Multiple Deprivation and create geographic visualizations
# 
# DESCRIPTION: 
# - Processes and cleans IMD data from UTLA_summaries.xlsx
# - Uses boundary-change corrected COVER data (NO IMPUTATION)
# - Ensures Northamptonshire and BCP are properly included
# - Merges COVER vaccine data with IMD quintile assignments
# - Creates choropleth maps showing IMD quintile distributions across England
#
# INPUTS: Boundary-corrected COVER files, UTLA_summaries.xlsx, shapefile data
# OUTPUTS: COVER_All_Years_MERGED_WITH_IMD_NO_IMPUTATION.csv, IMD quintile maps
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

imd_data_dir <- here("data")
cleaned_data_dir <- here("data/cleaned") 
output_dir <- here("output")

# Create output directory if it doesn't exist
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Load IMD data from data folder
imd_file_path <- file.path(imd_data_dir, "UTLA_summaries.xlsx")

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

# Report quintile assignments
cat("\n=== IMD QUINTILE ASSIGNMENTS ===\n")
quintile_summary <- imd_quintiles_assigned %>%
  count(imd_quintile, name = "n_utlas")
print(quintile_summary)

# Save cleaned IMD data to output directory
write.csv(imd_quintiles_assigned, file.path(output_dir, "cleaned_imd_data.csv"), row.names = FALSE)
cat("IMD data processed and saved\n")

#### 🫧 COVER Data Consolidation - BOUNDARY CORRECTED, NO IMPUTATION ####

# Use the final boundary-corrected comprehensive file if available
final_boundary_file <- file.path(cleaned_data_dir, "COVER_All_Years_NoImputation.csv")

if (file.exists(final_boundary_file)) {
  cat("Using boundary-corrected comprehensive file from script 01\n")
  cover_all <- read_csv(final_boundary_file, col_types = cols(.default = "c"), show_col_types = FALSE)
  cat("Loaded comprehensive boundary-corrected data\n")
  
} else {
  cat("Using individual cleaned files with boundary corrections\n")
  
  # Find boundary-corrected COVER files 
  files <- list.files(cleaned_data_dir, pattern = "COVER_\\d{4}_Cleaned\\.csv", full.names = TRUE)
  
  if (length(files) == 0) {
    stop("No boundary-corrected COVER files found in: ", cleaned_data_dir)
  }
  
  cat("Found", length(files), "boundary-corrected COVER files to process\n")
  print(basename(files))
  
  # Read and combine all boundary-corrected COVER files
  cover_all <- bind_rows(lapply(files, function(file) {
    cat("Processing:", basename(file), "\n")
    # Read everything as character to avoid type mismatch
    df <- read_csv(file, col_types = cols(.default = "c"), show_col_types = FALSE)
    return(df)
  }))
}

# Check data dimensions
cat("Combined boundary-corrected COVER data dimensions:", nrow(cover_all), "x", ncol(cover_all), "\n")

# VERIFY BOUNDARY CHANGES ARE PRESERVED
boundary_check <- cover_all %>%
  filter(ONS_Code %in% c("E06000058", "E10000021")) %>%  # BCP and Northamptonshire
  group_by(ONS_Code, UTLA_Name) %>%
  summarise(
    Years = n_distinct(Year),
    Quarters = n_distinct(Quarter),
    Has_Data = any(!is.na(PCV_12m) | !is.na(PCV_24m)),
    .groups = "drop"
  )

cat("\n=== BOUNDARY CHANGE VERIFICATION ===\n")
print(boundary_check)

if(nrow(boundary_check) < 2) {
  cat("WARNING: Expected BCP and/or Northamptonshire missing from data\n")
}

over_all <- cover_all %>%
  mutate(across(
    everything(),
    ~ case_when(
      .x %in% c("N.A.", "N/A", "NA", "N.A", "n.a.", "n/a") ~ NA_character_,
      grepl("^\\[", .x) ~ NA_character_,  # Convert [codes] to NA
      TRUE ~ .x
    )
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
cat("\n=== DATA STRUCTURE SUMMARY ===\n")
cat("Rows:", nrow(cover_all), "\n")
cat("Columns:", ncol(cover_all), "\n")
cat("Unique UTLAs:", n_distinct(cover_all$ONS_Code), "\n")
cat("Year range:", min(cover_all$Year), "to", max(cover_all$Year), "\n")

#### 🫧 MISSING DATA TRANSPARENCY - NO IMPUTATION ####

# REPORT MISSING DATA PATTERNS BUT DO NOT IMPUTE
total_missing <- sum(is.na(cover_all))
col_missing <- colSums(is.na(cover_all))
pct_missing <- round(colMeans(is.na(cover_all)) * 100, 2)

cat("\n=== MISSING DATA ANALYSIS (NO IMPUTATION PERFORMED) ===\n")
cat("Total missing values:", format(total_missing, big.mark = ","), "\n")
cat("Missing values by column:\n")
print(pct_missing[pct_missing > 0])

# Report missing patterns by year
missing_by_year <- cover_all %>%
  group_by(Year) %>%
  summarise(
    Total_Records = n(),
    Missing_PCV12 = sum(is.na(PCV_12m)),
    Missing_PCV24 = sum(is.na(PCV_24m)),
    Missing_Pop12 = sum(is.na(Population_12m)),
    Missing_Pop24 = sum(is.na(Population_24m)),
    .groups = "drop"
  ) %>%
  mutate(
    Pct_Missing_PCV12 = round(Missing_PCV12 / Total_Records * 100, 1),
    Pct_Missing_PCV24 = round(Missing_PCV24 / Total_Records * 100, 1)
  )

cat("\nMissing data patterns by year:\n")
print(missing_by_year)

# SAVE NON-IMPUTED DATASET 
write.csv(cover_all, file.path(output_dir, "COVER_All_Years_NO_IMPUTATION.csv"), row.names = FALSE)
cat("Non-imputed boundary-corrected data saved\n")

#### 🫧 Data Merging and Validation ####

# Use non-imputed data directly - NO IMPUTATION STEP
cover <- cover_all

# Check data types and content
cat("\n=== PRE-MERGE VALIDATION ===\n")
cat("Sample ONS codes from COVER:", head(cover$ONS_Code), "\n")
cat("Sample UTLA codes from IMD:", head(imd_quintiles_assigned$utla_code), "\n")

# Trim white space
cover$ONS_Code <- trimws(cover$ONS_Code)
imd_quintiles_assigned$utla_code <- trimws(imd_quintiles_assigned$utla_code)

# Check for mismatches
cover_ons_not_in_imd <- setdiff(cover$ONS_Code, imd_quintiles_assigned$utla_code)
imd_not_in_cover <- setdiff(imd_quintiles_assigned$utla_code, cover$ONS_Code)

cat("\n=== UNMATCHED RECORDS ANALYSIS ===\n")
cat("COVER codes not in IMD:", length(cover_ons_not_in_imd), "\n")
if(length(cover_ons_not_in_imd) > 0) {
  cat("Missing from IMD:\n")
  print(cover_ons_not_in_imd)
}

cat("IMD codes not in COVER:", length(imd_not_in_cover), "\n")
if (length(imd_not_in_cover) > 0) {
  cat("Missing from COVER (expected: City of London, Isles of Scilly):\n")
  missing_utla_details <- imd_quintiles_assigned %>%
    filter(utla_code %in% imd_not_in_cover)
  print(missing_utla_details)
}

# Count records before filtering
records_before <- nrow(cover)
cat("\nRecords BEFORE filtering:", format(records_before, big.mark = ","), "\n")

# ONLY INCLUDE UTLAs WITH IMD ASSIGNMENTS
cover <- cover %>%
  filter(ONS_Code %in% imd_quintiles_assigned$utla_code)

# Count records after filtering
records_after <- nrow(cover)
records_removed <- records_before - records_after

cat("Records AFTER filtering:", format(records_after, big.mark = ","), "\n")
cat("Records REMOVED:", format(records_removed, big.mark = ","), 
    "(", round(100 * records_removed / records_before, 1), "%)\n")

# Merge datasets and create IMD quintiles
cover_merged <- cover %>%
  left_join(imd_quintiles_assigned, by = c("ONS_Code" = "utla_code")) 

# ✅ VALIDATE MERGE SUCCESS
merge_success <- cover_merged %>%
  summarise(
    Total_Records = n(),
    Records_With_Quintile = sum(!is.na(imd_quintile)),
    Records_Missing_Quintile = sum(is.na(imd_quintile)),
    Pct_Successfully_Merged = round(Records_With_Quintile / Total_Records * 100, 1)
  )

cat("\n=== MERGE VALIDATION ===\n")
print(merge_success)

# Check quintile distribution
quintile_summary <- cover_merged %>%
  distinct(ONS_Code, imd_quintile) %>%
  count(imd_quintile, name = "n_utlas")

cat("\nFinal IMD Quintile Distribution:\n")
print(quintile_summary)

# VERIFY BOUNDARY CHANGE AUTHORITIES HAVE QUINTILES
boundary_quintile_check <- cover_merged %>%
  filter(ONS_Code %in% c("E06000058", "E10000021")) %>%
  select(ONS_Code, UTLA_Name, imd_quintile) %>%
  distinct()

cat("\n=== BOUNDARY CHANGE AUTHORITY QUINTILE CHECK ===\n")
print(boundary_quintile_check)

# Save final merged dataset - NO IMPUTATION
write.csv(cover_merged, file.path(output_dir, "COVER_All_Years_MERGED_WITH_IMD_NO_IMPUTATION.csv"), row.names = FALSE)
cat("✅ Merged data (no imputation, boundary-corrected) saved\n")

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
        filter(Year == "2019/2020") %>%  # Use representative year
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
      values = purples_intense,
      labels = c("1 (Least Deprived)", "2", "3", "4", "5 (Most Deprived)"),
      na.value = "grey0"
    ) +
    labs(
      title = "Index of Multiple Deprivation Quintiles by Upper Tier Local Authority",
      subtitle = "England, 2019 - Boundary-Corrected Data",
      caption = "Each local authority assigned to quintile based on average IMD score\nBCP and Northamptonshire boundary changes handled appropriately"
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
  ggsave(file.path(output_dir, "PNG_figures/IMD_quintiles_map_boundary_corrected.png"), quintile_map, 
         width = 10, height = 12, dpi = 300)
  ggsave(file.path(output_dir, "PDF_figures/IMD_quintiles_map_boundary_corrected.pdf"), quintile_map, 
         width = 10, height = 12)
  cat("IMD quintile map (boundary-corrected) saved\n")
}

#### 🫧 Generate UTLA Lists by IMD Quintile ####

# Create clean list of Local Authorities by IMD quintile
la_by_quintile <- cover_merged %>%
  select(utla_name, imd_quintile) %>%
  distinct() %>%
  filter(!is.na(imd_quintile)) %>%
  arrange(imd_quintile, utla_name)

# Print formatted lists for reference
cat("\n=== UTLA ASSIGNMENTS BY IMD QUINTILE (BOUNDARY-CORRECTED) ===\n")
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
write.csv(la_by_quintile, file.path(output_dir, "UTLA_IMD_quintile_assignments_boundary_corrected.csv"), row.names = FALSE)
cat("Quintile assignments (boundary-corrected) saved\n")

#### 🫧 Final Validation Summary ####
cat("\n=== FINAL VALIDATION SUMMARY ===\n")
cat("Total UTLAs with quintile assignments:", nrow(la_by_quintile), "\n")
cat("Expected UTLAs (149):", 149, "\n")
cat("Missing assignments:", 149 - nrow(la_by_quintile), "\n")

# Check quintile distribution
quintile_counts <- la_by_quintile %>%
  count(imd_quintile, name = "n_utlas")
cat("UTLAs per quintile:\n")
print(quintile_counts)

# Final check for boundary change authorities
boundary_final_check <- la_by_quintile %>%
  filter(grepl("Bournemouth|Northamptonshire", utla_name, ignore.case = TRUE))

cat("\nBoundary change authorities in final dataset:\n")
print(boundary_final_check)

if(nrow(boundary_final_check) == 2) {
  cat("Both BCP and Northamptonshire successfully included with IMD quintiles\n")
} else {
  cat(" Missing boundary change authorities - check data processing\n")
}



