#===============================================================================
# SCRIPT: 01_clean_and_process_COVER_data_COMPLETE_UPDATED.R
# PURPOSE: Clean and process quarterly PCV vaccine uptake data from COVER programme
# 
# DESCRIPTION: 
# - Processes COVER vaccine uptake files from 2013-2024 (Q2 2013 to Q3 2024)
# - Handles different file formats (.xls, .xlsx, .ods) across years
# - Merges 12-month primary and 24-month booster coverage data
# - Includes separate Bournemouth/Poole data for 2013-2019
# - Includes West/North Northamptonshire data for 2021/2022 onwards
# - Enhanced UTLA validation with boundary changes
# - Outputs cleaned quarterly and annual datasets
#

# MAIN SECTIONS:
# - Package loading and setup
# - UTLA validation list creation
# - Year-by-year data processing (2013-2024)
# - Data validation and completeness checks
#
# INPUTS: Raw COVER quarterly files, UTLA_summaries.xlsx
# OUTPUTS: Cleaned CSV files
#===============================================================================

####🫧 Loading and installing packages 🫧####
library(readxl)
library(dplyr)
library(data.table)
library(readODS)
library(here)
library(purrr)

####🫧 Set up folders 🫧####
main_dir = here("data")
imd_file = here("data", "UTLA_summaries.xlsx")
clean_dir = here("data", "cleaned")
dir.create(clean_dir, showWarnings = FALSE, recursive = TRUE)

#### 🫧 Load and enhance UTLA codes 🫧 ####
imd_sheet = "IMD" 
utla_imd = read_excel(imd_file, sheet = imd_sheet)

# UTLA list with City of London and Isles of Scilly removed
utla_list = utla_imd %>%
  select(UTLA_code = 1, UTLA_name = 2) %>%
  filter(!UTLA_name %in% c("City of London", "Isles of Scilly")) # Remove City of London and Isles of Scilly

# Set up valid UTLA list for up to and including 2018/2019
valid_utla_list_to_2019 = utla_list %>%
                          filter(!UTLA_name %in% "Bournemouth, Christchurch and Poole") %>%  # Remove 'Bournemouth, Christchurch and Poole'
                          # Add Bournemouth & Poole
                          bind_rows(data.frame(UTLA_code = "E06000028", UTLA_name = "Bournemouth")
                                    ) %>% 
                          bind_rows(data.frame(UTLA_code = "E06000029", UTLA_name = "Poole")
                                    )
valid_utlas_to_2019 = valid_utla_list_to_2019$UTLA_code

# Set up valid UTLA list for 2019/2020 & 2020/2021
valid_utlas_2019_to_2021 = utla_list$UTLA_code

# Set up valid UTLA list for 2021/2022 onwards
valid_utla_list_from_2021_onwards = utla_list %>%
                                  filter(!UTLA_name %in% "Northamptonshire") %>% # Remove Northamptonshire
                                  # Add North Northamptonshire & West Northamptonshire
                                  bind_rows(data.frame(UTLA_code = "E06000061", UTLA_name = "North Northamptonshire")
                                  ) %>% 
                                  bind_rows(data.frame(UTLA_code = "E06000062", UTLA_name = "West Northamptonshire")
                                  )
valid_utlas_from_2021_onwards = valid_utla_list_from_2021_onwards$UTLA_code

####🫧 Variable setup for UTLAs that had boundary changes during the study period 🫧####
utla_boundary_changes = list(
  # Bournemouth, Christchurch and Poole (created 2019)
  "E06000058" = list(
    name = "Bournemouth, Christchurch and Poole",
    pre_2019_codes = c("E06000028", "E06000029"), # Bournemouth, Poole
    pre_2019_names = c("Bournemouth", "Poole"),
    start_year = "2019/2020"
  ),
  # Northamptonshire split (2021)
  "E10000021" = list(
    name = "Northamptonshire", 
    post_2021_codes = c("E06000061", "E06000062"), # West Northamptonshire, North Northamptonshire
    post_2021_names = c("West Northamptonshire", "North Northamptonshire"),
    end_year = "2020/2021"
  )
)

####🫧 Set up functions 🫧####

# Function to transform character vectors into numeric vectors
transform_popn_PCV_data_into_numeric_type <- function(df) {
  
  df_processed = df %>%
  mutate(
        Population_12m = as.numeric(Population_12m),
        Population_24m = as.numeric(Population_24m),
        PCV_12m = as.numeric(PCV_12m),
        PCV_24m = as.numeric(PCV_24m)
  )
  
  return(df_processed)
}

# Mapping function with boundary change handling
map_utla_codes_enhanced <- function(df, year) {
  
  # Handle the case where full_join creates UTLA_Name.x and UTLA_Name.y
  if ("UTLA_Name.x" %in% names(df) && "UTLA_Name.y" %in% names(df)) {
    df <- df %>%
      mutate(UTLA_Name = coalesce(UTLA_Name.x, UTLA_Name.y)) %>%
      select(-UTLA_Name.x, -UTLA_Name.y)
  }
  
  # Handle Bournemouth, Christchurch and Poole for pre-2019 years
  if (year %in% c("2013/2014", "2014/2015", "2015/2016", "2016/2017", "2017/2018", "2018/2019")) {

    df_processed = df %>%
      left_join(valid_utla_list_to_2019, by = c("UTLA_Name" = "UTLA_name")) %>%
      mutate(
        ONS_Code = ifelse(ONS_Code %in% valid_utlas_to_2019, ONS_Code, UTLA_code)
      ) %>%
      select(-UTLA_code)
    
    # Ensure the names are standardized if needed
    df_processed = df_processed %>%
      mutate(
        UTLA_Name = case_when(
          ONS_Code == "E06000028" ~ "Bournemouth",
          ONS_Code == "E06000029" ~ "Poole", 
          TRUE ~ UTLA_Name
        )
      )
  }
  
  # Handle Northamptonshire split for post-2020 years
  if (year %in% c("2021/2022", "2022/2023", "2023/2024", "2024/2025")) {
    
    df_processed = df %>%
      left_join(valid_utla_list_from_2021_onwards, by = c("UTLA_Name" = "UTLA_name")) %>%
      mutate(
        ONS_Code = ifelse(ONS_Code %in% valid_utlas_from_2021_onwards, ONS_Code, UTLA_code)
      ) %>%
      select(-UTLA_code)
    
    # # Map new codes back to old Northamptonshire code
    # northants_info = utla_boundary_changes[["E10000021"]]
    # 
    # df_processed = df_processed %>%
    #   mutate(
    #     ONS_Code = case_when(
    #       ONS_Code %in% northants_info$post_2021_codes ~ "E10000021",
    #       UTLA_Name %in% northants_info$post_2021_names ~ "E10000021",
    #       TRUE ~ ONS_Code
    #     ),
    #     UTLA_Name = case_when(
    #       ONS_Code == "E10000021" ~ "Northamptonshire",
    #       TRUE ~ UTLA_Name
    #     )
    #   )
  }
  
  # Handle 2019/2020 and 2020/2021 years
  if (year %in% c("2019/2020", "2020/2021")) {
    df_processed = df %>%
      left_join(utla_list, by = c("UTLA_Name" = "UTLA_name")) %>%
      mutate(
        ONS_Code = ifelse(ONS_Code %in% valid_utlas_2019_to_2021, ONS_Code, UTLA_code)
      ) %>%
      select(-UTLA_code)
  }
  
  return(df_processed)
}

# Function to aggregate data for split/merged authorities
aggregate_boundary_changes <- function(df, year) {
  
  # Call function to transform character vectors into numeric vectors
  df = transform_popn_PCV_data_into_numeric_type(df)
  
  # Aggregate Bournemouth, Christchurch and Poole for pre-2019 years
  if (year %in% c("2013/2014", "2014/2015", "2015/2016", "2016/2017", "2017/2018", "2018/2019")) {

    bcp_codes = utla_boundary_changes[["E06000058"]]$pre_2019_codes
    
    if(any(bcp_codes %in% df$ONS_Code)) {
      
      # Compute aggregated populaton and PCV uptake statistics for 'Bournemouth, Christchurch and Poole' from 'Bournemouth' and 'Poole' UTLAs 
      bcp_aggregated = df %>%
        filter(ONS_Code %in% bcp_codes) %>%
        group_by(Year, Quarter, Timepoint, Vaccine_Schedule) %>%
        summarise(
          # Compute weighted average for the PCV uptake from 'Bournemouth' and 'Poole' UTLAs
          PCV_12m = ifelse(all(is.na(Population_12m)) | sum(Population_12m, na.rm = TRUE) == 0, 
                           NA, weighted.mean(PCV_12m, Population_12m, na.rm = TRUE)),
          PCV_24m = ifelse(all(is.na(Population_24m)) | sum(Population_24m, na.rm = TRUE) == 0,
                           NA, weighted.mean(PCV_24m, Population_24m, na.rm = TRUE)),
          
          # Get total population in 'Bournemouth' and 'Poole' UTLAs
          Population_12m = sum(Population_12m, na.rm = TRUE),
          Population_24m = sum(Population_24m, na.rm = TRUE),
          .groups = "drop"
        )  %>%
        mutate(ONS_Code = "E06000058", UTLA_Name = "Bournemouth, Christchurch and Poole")
      
      df = df %>%
        filter(!ONS_Code %in% bcp_codes) %>%
        bind_rows(bcp_aggregated)
    }
  }
  
  # Aggregate Northamptonshire for post-2020 years
  if (year %in% c("2021/2022", "2022/2023", "2023/2024", "2024/2025")) {
    northants_codes = utla_boundary_changes[["E10000021"]]$post_2021_codes
    
    if(any(northants_codes %in% df$ONS_Code)) {

      # Compute aggregated populaton and PCV uptake statistics for 'Northamptonshire' from 'West Northamptonshire' and 'North Northamptonshire' UTLAs
      northants_aggregated = df %>%
        filter(ONS_Code %in% northants_codes) %>%
        group_by(Year, Quarter, Timepoint, Vaccine_Schedule) %>%
        summarise(
          # Compute weighted average for the PCV uptake from 'West Northamptonshire' and 'North Northamptonshire' UTLAs
          PCV_12m = ifelse(all(is.na(Population_12m)) | sum(Population_12m, na.rm = TRUE) == 0, 
                           NA, weighted.mean(PCV_12m, Population_12m, na.rm = TRUE)),
          PCV_24m = ifelse(all(is.na(Population_24m)) | sum(Population_24m, na.rm = TRUE) == 0,
                           NA, weighted.mean(PCV_24m, Population_24m, na.rm = TRUE)),
          
          # Get total population in 'West Northamptonshire' and 'North Northamptonshire' UTLAs
          Population_12m = sum(Population_12m, na.rm = TRUE),
          Population_24m = sum(Population_24m, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(ONS_Code = "E10000021", UTLA_Name = "Northamptonshire")
      
      df = df %>%
        filter(!ONS_Code %in% northants_codes) %>%
        bind_rows(northants_aggregated)
    }
  }
  
  return(df)
}

# Data Validation Function
validate_cover_data <- function(cover_data, year, valid_utlas_vec) {
  cat("\n=== VALIDATION FOR:", year, "===\n")
  
  coverage = cover_data %>%
    group_by(Quarter) %>%
    summarise(
      Unique_UTLAs = n_distinct(ONS_Code),
      Expected_UTLAs = length(valid_utlas_vec),
      Coverage_Pct = round(Unique_UTLAs / Expected_UTLAs * 100, 1)
    )
  
  print(coverage)
  
  return(coverage)
}

#####################################
####🫧 COVER data processing 🫧######
#####################################

#### ⋆˚࿔ 2 0 1 3 — Q2 🫧 ####

#### 🫧 2013 Q2 🫧 ####
file_path = file.path(main_dir, "2013_Q2.xls")

# Load 12-month sheet
la12 = read_excel(file_path, sheet = "LA-12m", skip = 8) %>%
  select(`ONS Upper Tier LA Code`, `ONS Upper Tier LA Name`, `No. Children`, `PCV %`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `ONS Upper Tier LA Name`,
    Population_12m = `No. Children`,
    PCV_12m = `PCV %`
  )

# Load 24-month sheet
la24 = read_excel(file_path, sheet = "LA-24m", skip = 8) %>%
  select(`ONS Upper Tier LA Code`, `ONS Upper Tier LA Name`, `No. Children`, `PCV Booster %`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `ONS Upper Tier LA Name`,
    Population_24m = `No. Children`,
    PCV_24m = `PCV Booster %`
  )

# Join by both ONS_Code and UTLA_Name to avoid .x/.y columns
merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  map_utla_codes_enhanced("2013/2014") %>%
  filter(ONS_Code %in% valid_utlas_to_2019) %>%
  mutate(
    Year = "2013/2014",
    Quarter = "Q2",
    Timepoint = 1,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

# Replace string representing 'NA' with NA
merged_LA[merged_LA == "N.A."] <- NA

# Apply boundary change aggregation
merged_LA = aggregate_boundary_changes(merged_LA, "2013/2014")

# Save Q2
fwrite(merged_LA, file.path(clean_dir, "COVER_2013_Q2_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(merged_LA, "2013/2014 Q2", valid_utlas_2019_to_2021) 

#### 🫧 2013 Q3 🫧 ####
file_path = file.path(main_dir, "2013_Q3.xls")

la12 = read_excel(file_path, sheet = "LA-12m", skip = 8) %>%
  select(`ONS Upper Tier LA Code`, `ONS Upper Tier LA Name`, `No. Children`, `12m_PCV%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `ONS Upper Tier LA Name`,
    Population_12m = `No. Children`,
    PCV_12m = `12m_PCV%`
  )

la24 = read_excel(file_path, sheet = "LA-24m", skip = 8) %>%
  select(`ONS Upper Tier LA Code`, `ONS Upper Tier LA Name`, `No. Children`, `24m_PCVB%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `ONS Upper Tier LA Name`,
    Population_24m = `No. Children`,
    PCV_24m = `24m_PCVB%`
  )

# Join by both ONS_Code and UTLA_Name
merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  map_utla_codes_enhanced("2013/2014") %>%
  filter(ONS_Code %in% valid_utlas_to_2019) %>%
  mutate(
    Year = "2013/2014",
    Quarter = "Q3",
    Timepoint = 2,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

merged_LA = aggregate_boundary_changes(merged_LA, "2013/2014")

q2_data = fread(file.path(clean_dir, "COVER_2013_Q2_Cleaned.csv"))
combined_2013 = bind_rows(q2_data, merged_LA)
fwrite(combined_2013, file.path(clean_dir, "COVER_2013_Q2_Q3_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(combined_2013, "2013/2014 Q2-Q3", valid_utlas_2019_to_2021)

#### 🫧 2013 Q4 🫧 ####
file_path = file.path(main_dir, "2013_Q4.xls")

la12 = read_excel(file_path, sheet = "LA-12m", skip = 8) %>%
  select(`ONS Upper Tier LA Code`, `ONS Upper Tier LA Name`, `No. Children`, `12m_PCV%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `ONS Upper Tier LA Name`,
    Population_12m = `No. Children`,
    PCV_12m = `12m_PCV%`
  )

la24 = read_excel(file_path, sheet = "LA-24m", skip = 8) %>%
  select(`ONS Upper Tier LA Code`, `ONS Upper Tier LA Name`, `No. Children`, `24m_PCVB%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `ONS Upper Tier LA Name`,
    Population_24m = `No. Children`,
    PCV_24m = `24m_PCVB%`
  )

# Join by both ONS_Code and UTLA_Name
merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  map_utla_codes_enhanced("2013/2014") %>%
  filter(ONS_Code %in% valid_utlas_to_2019) %>%
  mutate(
    Year = "2013/2014",
    Quarter = "Q4",
    Timepoint = 3,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

merged_LA = aggregate_boundary_changes(merged_LA, "2013/2014")

q3_data = fread(file.path(clean_dir, "COVER_2013_Q2_Q3_Cleaned.csv"))
combined_2013 = bind_rows(q3_data, merged_LA)
fwrite(combined_2013, file.path(clean_dir, "COVER_2013_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(combined_2013, "2013/2014 Full", valid_utlas_2019_to_2021)

#### ⋆˚࿔ 2 0 1 4 𝜗𝜚˚⋆ ####

#### 🫧 2014 Q1 🫧 ####
file_path = file.path(main_dir, "2014_Q1.xlsx")

# Load 12-month sheet
la12 = read_excel(file_path, sheet = "12m_UT LA", range = "F1:M153") %>%
  select(`ONS Upper Tier LA Code`, `ONS Upper Tier LA Name`, `12 month Denominator`, `PCV2 (%)`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `ONS Upper Tier LA Name`,
    Population_12m = `12 month Denominator`,
    PCV_12m = `PCV2 (%)`
  )

# Load 24-month sheet
la24 = read_excel(file_path, sheet = "24m_UT LA", range = "F1:M153") %>%
  select(`ONS Upper Tier LA Code`, `ONS Upper Tier LA Name`, `24m Denominator`, `24m_PCV Booster (%)`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `ONS Upper Tier LA Name`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m_PCV Booster (%)`
  )

# Join by both ONS_Code and UTLA_Name
merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  map_utla_codes_enhanced("2014/2015") %>%
  filter(ONS_Code %in% valid_utlas_to_2019) %>%
  mutate(
    Year = "2014/2015",
    Quarter = "Q1",
    Timepoint = 0,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

# Apply boundary change aggregation
merged_LA = aggregate_boundary_changes(merged_LA, "2014/2015")

# Save Q1
fwrite(merged_LA, file.path(clean_dir, "COVER_2014_Q1_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(merged_LA, "2014/2015 Q1", valid_utlas_2019_to_2021)

#### 🫧 2014 Q2 🫧 ####
file_path = file.path(main_dir, "2014_Q2.xlsx")

# Load 12-month sheet
la12 = read_excel(file_path, sheet = "12m_UT LA", range = "F1:M153") %>%
  select(`ONS LA Code`, `UTLA name`, `Sum of Denom_12m`, `Sum of 12m_PCV%`) %>%
  rename(
    ONS_Code = `ONS LA Code`,
    UTLA_Name = `UTLA name`,
    Population_12m = `Sum of Denom_12m`,
    PCV_12m = `Sum of 12m_PCV%`
  )

# Load 24-month sheet
la24 = read_excel(file_path, sheet = "24m_UT LA", range = "F1:M153") %>%
  select(`ONS Upper Tier LA Code`, `UTLA name`, `Sum of Denom_24m`, `Sum of 24m_PCVB%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `UTLA name`,
    Population_24m = `Sum of Denom_24m`,
    PCV_24m = `Sum of 24m_PCVB%`
  )

# Join by both ONS_Code and UTLA_Name
merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  map_utla_codes_enhanced("2014/2015") %>%
  filter(ONS_Code %in% valid_utlas_to_2019) %>%
  mutate(
    Year = "2014/2015",
    Quarter = "Q2",
    Timepoint = 1,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

# Apply boundary change aggregation
merged_LA = aggregate_boundary_changes(merged_LA, "2014/2015")

# Save Q2
q1_data = fread(file.path(clean_dir, "COVER_2014_Q1_Cleaned.csv"))
combined_2014 = bind_rows(q1_data, merged_LA)
fwrite(combined_2014, file.path(clean_dir, "COVER_2014_Q1_Q2_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(combined_2014, "2014/2015 Q1-Q2", valid_utlas_2019_to_2021)

#### 🫧 2014 Q3 🫧 ####
file_path = file.path(main_dir, "2014_Q3.xlsx")

# Load 12-month sheet
la12 = read_excel(file_path, sheet = "12m_UT LA", range = "E1:M153") %>%
  select(`ONS LA Code`, `UTLA name`, `Sum of Denom_12m`, `Sum of 12m_PCV%`) %>%
  rename(
    ONS_Code = `ONS LA Code`,
    UTLA_Name = `UTLA name`,
    Population_12m = `Sum of Denom_12m`,
    PCV_12m = `Sum of 12m_PCV%`
  )

# Load 24-month sheet
la24 = read_excel(file_path, sheet = "24m_UT LA", range = "E1:M153") %>%
  select(`ONS LA Code`, `Local Authority Name`, `24m Denominator`, `24m PCV Booster %`) %>%
  rename(
    ONS_Code = `ONS LA Code`,
    UTLA_Name = `Local Authority Name`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster %`
  )

# Join by both ONS_Code and UTLA_Name
merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  map_utla_codes_enhanced("2014/2015") %>%
  filter(ONS_Code %in% valid_utlas_to_2019) %>%
  mutate(
    Year = "2014/2015",
    Quarter = "Q3",
    Timepoint = 2,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

# Apply boundary change aggregation
merged_LA = aggregate_boundary_changes(merged_LA, "2014/2015")

# Save Q3
q2_data = fread(file.path(clean_dir, "COVER_2014_Q1_Q2_Cleaned.csv"))
combined_2014 = bind_rows(q2_data, merged_LA)
fwrite(combined_2014, file.path(clean_dir, "COVER_2014_Q1_Q2_Q3_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(combined_2014, "2014/2015 Q1-Q3", valid_utlas_2019_to_2021)

#### 🫧 2014 Q4 🫧 ####
file_path = file.path(main_dir, "2014_Q4.xlsx")

# Load 12-month sheet
la12 = read_excel(file_path, sheet = "12m_UT LA", range = "E1:M154") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV %`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV %`
  )

# Load 24-month sheet
la24 = read_excel(file_path, sheet = "24m_UT LA", range = "E1:M154") %>%
  select(`ONS LA Code`, `Local Authority Name`, `24m Denominator`, `24m PCV Booster %`) %>%
  rename(
    ONS_Code = `ONS LA Code`,
    UTLA_Name = `Local Authority Name`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster %`
  )

# Join by both ONS_Code and UTLA_Name
merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  map_utla_codes_enhanced("2014/2015") %>%
  filter(ONS_Code %in% valid_utlas_to_2019) %>%
  mutate(
    Year = "2014/2015",
    Quarter = "Q4",
    Timepoint = 3,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

# Apply boundary change aggregation
merged_LA = aggregate_boundary_changes(merged_LA, "2014/2015")

# Save final 2014 file
q3_data = fread(file.path(clean_dir, "COVER_2014_Q1_Q2_Q3_Cleaned.csv"))
combined_2014 = bind_rows(q3_data, merged_LA)
fwrite(combined_2014, file.path(clean_dir, "COVER_2014_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(combined_2014, "2014/2015 Full", valid_utlas_2019_to_2021)

#### ⋆˚࿔ 2 0 1 5 𝜗𝜚˚ ⋆ ####

#### 🫧 2015 Q1 🫧 ####
file_path = file.path(main_dir, "2015_Q1.xlsx")

# Load 12-month sheet
la12 = read_excel(file_path, sheet = "12m_UT LA", range = "E1:M154") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV %`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV %`
  )

# Load 24-month sheet
la24 = read_excel(file_path, sheet = "24m_UT LA", range = "E1:M154") %>%
  select(`ONS LA Code`, `Local Authority Name`, `24m Denominator`, `24m PCV Booster %`) %>%
  rename(
    ONS_Code = `ONS LA Code`,
    UTLA_Name = `Local Authority Name`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster %`
  )

# Join by both ONS_Code and UTLA_Name
merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  map_utla_codes_enhanced("2015/2016") %>%
  filter(ONS_Code %in% valid_utlas_to_2019) %>%
  mutate(
    Year = "2015/2016",
    Quarter = "Q1",
    Timepoint = 0,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

# Apply boundary change aggregation
merged_LA = aggregate_boundary_changes(merged_LA, "2015/2016")

# Save Q1
fwrite(merged_LA, file.path(clean_dir, "COVER_2015_Q1_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(merged_LA, "2015/2016 Q1", valid_utlas_2019_to_2021)

#### 🫧 2015 Q2 🫧 ####
file_path = file.path(main_dir, "2015_Q2.xlsx")

# Load 12-month sheet
la12 = read_excel(file_path, sheet = "12m_UT LA", range = "E1:M154") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV %`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV %`
  )

# Load 24-month sheet
la24 = read_excel(file_path, sheet = "24m_UT LA", range = "E1:M154") %>%
  select(`ONS LA Code`, `Local Authority Name`, `24m Denominator`, `24m PCV Booster %`) %>%
  rename(
    ONS_Code = `ONS LA Code`,
    UTLA_Name = `Local Authority Name`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster %`
  )

# Join by both ONS_Code and UTLA_Name
merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  map_utla_codes_enhanced("2015/2016") %>%
  filter(ONS_Code %in% valid_utlas_to_2019) %>%
  mutate(
    Year = "2015/2016",
    Quarter = "Q2",
    Timepoint = 1,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

# Apply boundary change aggregation
merged_LA = aggregate_boundary_changes(merged_LA, "2015/2016")

# Save Q2
q1_data = fread(file.path(clean_dir, "COVER_2015_Q1_Cleaned.csv"))
combined_2015 = bind_rows(q1_data, merged_LA)
fwrite(combined_2015, file.path(clean_dir, "COVER_2015_Q1_Q2_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(combined_2015, "2015/2016 Q1-Q2", valid_utlas_2019_to_2021)

#### 🫧 2015 Q3 🫧 ####
file_path = file.path(main_dir, "2015_Q3.xlsx")

# Load 12-month sheet
la12 = read_excel(file_path, sheet = "12m_UT LA", range = "E1:M154") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV %`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV %`
  )

# Load 24-month sheet
la24 = read_excel(file_path, sheet = "24m_UT LA", range = "E1:M154") %>%
  select(`ONS LA Code`, `Local Authority Name`, `24m Denominator`, `24m PCV Booster %`) %>%
  rename(
    ONS_Code = `ONS LA Code`,
    UTLA_Name = `Local Authority Name`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster %`
  )

# Join by both ONS_Code and UTLA_Name
merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  map_utla_codes_enhanced("2015/2016") %>%
  filter(ONS_Code %in% valid_utlas_to_2019) %>%
  mutate(
    Year = "2015/2016",
    Quarter = "Q3",
    Timepoint = 2,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

# Apply boundary change aggregation
merged_LA = aggregate_boundary_changes(merged_LA, "2015/2016")

# Save Q3
q2_data = fread(file.path(clean_dir, "COVER_2015_Q1_Q2_Cleaned.csv"))
combined_2015 = bind_rows(q2_data, merged_LA)
fwrite(combined_2015, file.path(clean_dir, "COVER_2015_Q1_Q2_Q3_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(combined_2015, "2015/2016 Q1-Q3", valid_utlas_2019_to_2021)

#### 🫧 2015 Q4 🫧 ####
file_path = file.path(main_dir, "2015_Q4.xlsx")

# Load 12-month sheet
la12 = read_excel(file_path, sheet = "12m_UT LA", range = "E1:M154") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV %`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV %`
  )

# Load 24-month sheet
la24 = read_excel(file_path, sheet = "24m_UT LA", range = "E1:M154") %>%
  select(`ONS LA Code`, `Local Authority Name`, `24m Denominator`, `24m PCV Booster %`) %>%
  rename(
    ONS_Code = `ONS LA Code`,
    UTLA_Name = `Local Authority Name`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster %`
  )

#  Join by both ONS_Code and UTLA_Name
merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  map_utla_codes_enhanced("2015/2016") %>%
  filter(ONS_Code %in% valid_utlas_to_2019) %>%
  mutate(
    Year = "2015/2016",
    Quarter = "Q4",
    Timepoint = 3,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

# Apply boundary change aggregation
merged_LA = aggregate_boundary_changes(merged_LA, "2015/2016")

# Save final 2015 file
q3_data = fread(file.path(clean_dir, "COVER_2015_Q1_Q2_Q3_Cleaned.csv"))
combined_2015 = bind_rows(q3_data, merged_LA)
fwrite(combined_2015, file.path(clean_dir, "COVER_2015_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(combined_2015, "2015/2016 Full", valid_utlas_2019_to_2021)

#### 🫧 Final check for 2015 🫧 ####
combined_2015 %>%
  group_by(Quarter) %>%
  summarise(Unique_UTLAs = n_distinct(ONS_Code))

# List of ONS codes in final processed dataset should match the utla list from 2019 
missing_utlas = setdiff(valid_utlas_2019_to_2021, combined_2015$ONS_Code)
print(missing_utlas)

utla_list %>% filter(UTLA_code %in% missing_utlas)


#### ⋆˚࿔ 2 0 1 6 𝜗𝜚˚ ⋆ ####

#### 🫧 2016 Q1 🫧 ####
file_path = file.path(main_dir, "2016_Q1.xlsx")

# Load 12-month sheet
la12 = read_excel(file_path, sheet = "12m_UTLA_AT") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  )  %>%
  filter(!is.na(ONS_Code))

# Load 24-month sheet
la24 = read_excel(file_path, sheet = "24m_UTLA_AT") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  ) %>%
  filter(!is.na(ONS_Code))

# Join by both ONS_Code and UTLA_Name
merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  map_utla_codes_enhanced("2016/2017") %>%
  filter(ONS_Code %in% valid_utlas_to_2019) %>%
  mutate(
    Year = "2016/2017",
    Quarter = "Q1",
    Timepoint = 0,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

# Apply boundary change aggregation
merged_LA = aggregate_boundary_changes(merged_LA, "2016/2017")

# Save Q1
fwrite(merged_LA, file.path(clean_dir, "COVER_2016_Q1_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(merged_LA, "2016/2017 Q1", valid_utlas_2019_to_2021)

#### 🫧 2016 Q2 🫧 ####
file_path = file.path(main_dir, "2016_Q2.xlsx")

# Load 12-month sheet
la12 = read_excel(file_path, sheet = "12m_UTLA_AT") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  ) %>%
  filter(!is.na(ONS_Code))


# Check what values are causing the coercion problem
la12_raw = read_excel(file_path, sheet = "12m_UTLA_AT")
unique(la12_raw$`12m Denominator`[is.na(as.numeric(la12_raw$`12m Denominator`))])

# Load 24-month sheet
la24 = read_excel(file_path, sheet = "24m_UTLA_AT") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  ) %>%
  filter(!is.na(ONS_Code))

#  Join by both ONS_Code and UTLA_Name
merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  map_utla_codes_enhanced("2016/2017") %>%
  filter(ONS_Code %in% valid_utlas_to_2019) %>%
  mutate(
    Year = "2016/2017",
    Quarter = "Q2",
    Timepoint = 1,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

# Apply boundary change aggregation
merged_LA = aggregate_boundary_changes(merged_LA, "2016/2017")

# Save Q2
q1_data = fread(file.path(clean_dir, "COVER_2016_Q1_Cleaned.csv"))
combined_2016 = bind_rows(q1_data, merged_LA)
fwrite(combined_2016, file.path(clean_dir, "COVER_2016_Q1_Q2_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(combined_2016, "2016/2017 Q1-Q2", valid_utlas_2019_to_2021)

#### 🫧 2016 Q3 🫧 ####
file_path = file.path(main_dir, "2016_Q3.xlsx")

# Load 12-month sheet
la12 = read_excel(file_path, sheet = "12m_UTLA_AT") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  ) %>%
  filter(!is.na(ONS_Code))

# Load 24-month sheet
la24 = read_excel(file_path, sheet = "24m_UTLA_AT") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  ) %>%
  filter(!is.na(ONS_Code))

# Join by both ONS_Code and UTLA_Name
merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  map_utla_codes_enhanced("2016/2017") %>%
  filter(ONS_Code %in% valid_utlas_to_2019) %>%
  mutate(
    Year = "2016/2017",
    Quarter = "Q3",
    Timepoint = 2,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

# Apply boundary change aggregation
merged_LA = aggregate_boundary_changes(merged_LA, "2016/2017")

# Save Q3
q2_data = fread(file.path(clean_dir, "COVER_2016_Q1_Q2_Cleaned.csv"))
combined_2016 = bind_rows(q2_data, merged_LA)
fwrite(combined_2016, file.path(clean_dir, "COVER_2016_Q1_Q2_Q3_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(combined_2016, "2016/2017 Q1-Q3", valid_utlas_2019_to_2021)

#### 🫧 2016 Q4 🫧 ####
file_path = file.path(main_dir, "2016_Q4.xlsx")

# Load 12-month sheet
la12 = read_excel(file_path, sheet = "12m_UTLA_AT") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  ) %>%
  filter(!is.na(ONS_Code))

# Load 24-month sheet
la24 = read_excel(file_path, sheet = "24m_UTLA_AT") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  ) %>%
  filter(!is.na(ONS_Code))

# Join by both ONS_Code and UTLA_Name
merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  map_utla_codes_enhanced("2016/2017") %>%
  filter(ONS_Code %in% valid_utlas_to_2019) %>%
  mutate(
    Year = "2016/2017",
    Quarter = "Q4",
    Timepoint = 3,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

# Apply boundary change aggregation
merged_LA = aggregate_boundary_changes(merged_LA, "2016/2017")

# Save final 2016 file
q3_data = fread(file.path(clean_dir, "COVER_2016_Q1_Q2_Q3_Cleaned.csv"))
combined_2016 = bind_rows(q3_data, merged_LA)
fwrite(combined_2016, file.path(clean_dir, "COVER_2016_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019
validate_cover_data(combined_2016, "2016/2017 Full", valid_utlas_2019_to_2021)

#### 🫧 Final check for 2016 🫧 ####
combined_2016 %>%
  group_by(Quarter) %>%
  summarise(Unique_UTLAs = n_distinct(ONS_Code))

# List of ONS codes in final processed dataset should match the utla list from 2019 
missing_utlas = setdiff(valid_utlas_2019_to_2021, combined_2016$ONS_Code)
print(missing_utlas)

utla_list %>% filter(UTLA_code %in% missing_utlas)


#### ⋆˚࿔ 2 0 1 7 𝜗𝜚˚ ⋆ ####

#### 🫧 2017 Q1 🫧 ####
file_path = file.path(main_dir, "2017_Q1.xlsx")

# Load 12-month sheet
la12 = read_excel(file_path, sheet = "12m_UTLA_AT") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  ) %>%
  filter(!is.na(ONS_Code))

# Check what values are causing the coercion problem
la12_raw = read_excel(file_path, sheet = "12m_UTLA_AT")
unique(la12_raw$`12m Denominator`[is.na(as.numeric(la12_raw$`12m Denominator`))]) ### * (too small)

# Load 24-month sheet
la24 = read_excel(file_path, sheet = "24m_UTLA_AT") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  ) %>%
  filter(!is.na(ONS_Code))

# Join by both ONS_Code and UTLA_Name
merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  map_utla_codes_enhanced("2017/2018") %>%
  filter(ONS_Code %in% valid_utlas_to_2019) %>%
  mutate(
    Year = "2017/2018",
    Quarter = "Q1",
    Timepoint = 0,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

# Apply boundary change aggregation
merged_LA = aggregate_boundary_changes(merged_LA, "2017/2018")

# Save Q1
fwrite(merged_LA, file.path(clean_dir, "COVER_2017_Q1_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(merged_LA, "2017/2018 Q1", valid_utlas_2019_to_2021)

#### 🫧 2017 Q2 🫧 ####
file_path = file.path(main_dir, "2017_Q2.xlsx")

# Load 12-month sheet
la12 = read_excel(file_path, sheet = "12m_UTLA_AT") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  ) %>%
  filter(!is.na(ONS_Code))

# Load 24-month sheet
la24 = read_excel(file_path, sheet = "24m_UTLA_AT") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  ) %>%
  filter(!is.na(ONS_Code))

# Join by both ONS_Code and UTLA_Name
merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  map_utla_codes_enhanced("2017/2018") %>%
  filter(ONS_Code %in% valid_utlas_to_2019) %>%
  mutate(
    Year = "2017/2018",
    Quarter = "Q2",
    Timepoint = 1,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

# Apply boundary change aggregation
merged_LA = aggregate_boundary_changes(merged_LA, "2017/2018")

# Save Q2
q1_data = fread(file.path(clean_dir, "COVER_2017_Q1_Cleaned.csv"))
combined_2017 = bind_rows(q1_data, merged_LA)
fwrite(combined_2017, file.path(clean_dir, "COVER_2017_Q1_Q2_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(combined_2017, "2017/2018 Q1-Q2", valid_utlas_2019_to_2021)

#### 🫧 2017 Q3 🫧 ####
file_path = file.path(main_dir, "2017_Q3.xlsx")

# Load 12-month sheet
la12 = read_excel(file_path, sheet = "12m_UTLA_AT") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  ) %>%
  filter(!is.na(ONS_Code))

# Load 24-month sheet
la24 = read_excel(file_path, sheet = "24m_UTLA_AT") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  ) %>%
  filter(!is.na(ONS_Code))

# Join by both ONS_Code and UTLA_Name
merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  map_utla_codes_enhanced("2017/2018") %>%
  filter(ONS_Code %in% valid_utlas_to_2019) %>%
  mutate(
    Year = "2017/2018",
    Quarter = "Q3",
    Timepoint = 2,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

# Apply boundary change aggregation
merged_LA = aggregate_boundary_changes(merged_LA, "2017/2018")

# Save Q3
q2_data = fread(file.path(clean_dir, "COVER_2017_Q1_Q2_Cleaned.csv"))
combined_2017 = bind_rows(q2_data, merged_LA)
fwrite(combined_2017, file.path(clean_dir, "COVER_2017_Q1_Q2_Q3_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(combined_2017, "2017/2018 Q1-Q3", valid_utlas_2019_to_2021)

#### 🫧 2017 Q4 🫧 ####
file_path = file.path(main_dir, "2017_Q4.xlsx")

# Load 12-month sheet
la12 = read_excel(file_path, sheet = "12m_UTLA_AT") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  ) %>%
  filter(!is.na(ONS_Code))

# Load 24-month sheet
la24 = read_excel(file_path, sheet = "24m_UTLA_AT") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  ) %>%
  filter(!is.na(ONS_Code))

# Join by both ONS_Code and UTLA_Name
merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  map_utla_codes_enhanced("2017/2018") %>%
  filter(ONS_Code %in% valid_utlas_to_2019) %>%
  mutate(
    Year = "2017/2018",
    Quarter = "Q4",
    Timepoint = 3,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

# Apply boundary change aggregation
merged_LA = aggregate_boundary_changes(merged_LA, "2017/2018")

# Save final 2017 file
q3_data = fread(file.path(clean_dir, "COVER_2017_Q1_Q2_Q3_Cleaned.csv"))
combined_2017 = bind_rows(q3_data, merged_LA)
fwrite(combined_2017, file.path(clean_dir, "COVER_2017_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(combined_2017, "2017/2018 Full", valid_utlas_2019_to_2021)

#### 🫧 Final check for 2017 🫧 ####
combined_2017 %>%
  group_by(Quarter) %>%
  summarise(Unique_UTLAs = n_distinct(ONS_Code))

# List of ONS codes in final processed dataset should match the utla list from 2019 
missing_utlas = setdiff(valid_utlas_2019_to_2021, combined_2017$ONS_Code)
print(missing_utlas)

utla_list %>% filter(UTLA_code %in% missing_utlas)


#### ⋆˚࿔ 2 0 1 8 𝜗𝜚˚ ⋆ ####

#### 🫧 2018 Q1 🫧 ####
file_path = file.path(main_dir, "2018_Q1.xlsx")

# Load 12-month sheet
la12 = read_excel(file_path, sheet = "12m_UTLA_GOR") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  ) %>%
  filter(!is.na(ONS_Code))

# Load 24-month sheet
la24 = read_excel(file_path, sheet = "24m_UTLA_GOR") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  ) %>%
  filter(!is.na(ONS_Code))

# Join by both ONS_Code and UTLA_Name
merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  map_utla_codes_enhanced("2018/2019") %>%
  filter(ONS_Code %in% valid_utlas_to_2019) %>%
  mutate(
    Year = "2018/2019",
    Quarter = "Q1",
    Timepoint = 0,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

# Apply boundary change aggregation
merged_LA = aggregate_boundary_changes(merged_LA, "2018/2019")

# Save Q1
fwrite(merged_LA, file.path(clean_dir, "COVER_2018_Q1_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(merged_LA, "2018/2019 Q1", valid_utlas_2019_to_2021)

#### 🫧 2018 Q2 🫧 ####
file_path = file.path(main_dir, "2018_Q2.xlsx")

# Load 12-month sheet
la12 = read_excel(file_path, sheet = "12m_UTLA_GOR") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  ) %>%
  filter(!is.na(ONS_Code))

# Load 24-month sheet
la24 = read_excel(file_path, sheet = "24m_UTLA_GOR") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  ) %>%
  filter(!is.na(ONS_Code))

# Join by both ONS_Code and UTLA_Name
merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  map_utla_codes_enhanced("2018/2019") %>%
  filter(ONS_Code %in% valid_utlas_to_2019) %>%
  mutate(
    Year = "2018/2019",
    Quarter = "Q2",
    Timepoint = 1,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

# Apply boundary change aggregation
merged_LA = aggregate_boundary_changes(merged_LA, "2018/2019")

# Save Q2
q1_data = fread(file.path(clean_dir, "COVER_2018_Q1_Cleaned.csv"))
combined_2018 = bind_rows(q1_data, merged_LA)
fwrite(combined_2018, file.path(clean_dir, "COVER_2018_Q1_Q2_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(combined_2018, "2018/2019 Q1-Q2", valid_utlas_2019_to_2021)

#### 🫧 2018 Q3 🫧 ####
file_path = file.path(main_dir, "2018_Q3.xlsx")

# Load 12-month sheet
la12 = read_excel(file_path, sheet = "12m_UTLA_GOR") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  ) %>%
  filter(!is.na(ONS_Code))

# Load 24-month sheet
la24 = read_excel(file_path, sheet = "24m_UTLA_GOR") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  ) %>%
  filter(!is.na(ONS_Code))

# Join by both ONS_Code and UTLA_Name
merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  map_utla_codes_enhanced("2018/2019") %>%
  filter(ONS_Code %in% valid_utlas_to_2019) %>%
  mutate(
    Year = "2018/2019",
    Quarter = "Q3",
    Timepoint = 2,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

# Apply boundary change aggregation
merged_LA = aggregate_boundary_changes(merged_LA, "2018/2019")

# Save Q3
q2_data = fread(file.path(clean_dir, "COVER_2018_Q1_Q2_Cleaned.csv"))
combined_2018 = bind_rows(q2_data, merged_LA)
fwrite(combined_2018, file.path(clean_dir, "COVER_2018_Q1_Q2_Q3_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(combined_2018, "2018/2019 Q1-Q3", valid_utlas_2019_to_2021)

#### 🫧 2018 Q4 🫧 ####
file_path = file.path(main_dir, "2018_Q4.xlsx")

# Load 12-month sheet
la12 = read_excel(file_path, sheet = "12m_UTLA_GOR") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  ) %>%
  filter(!is.na(ONS_Code))

# Load 24-month sheet
la24 = read_excel(file_path, sheet = "24m_UTLA_GOR") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  ) %>%
  filter(!is.na(ONS_Code))

# Join by both ONS_Code and UTLA_Name
merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  map_utla_codes_enhanced("2018/2019") %>%
  filter(ONS_Code %in% valid_utlas_to_2019) %>%
  mutate(
    Year = "2018/2019",
    Quarter = "Q4",
    Timepoint = 3,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

# Apply boundary change aggregation
merged_LA = aggregate_boundary_changes(merged_LA, "2018/2019")

# Save final 2018 file
q3_data = fread(file.path(clean_dir, "COVER_2018_Q1_Q2_Q3_Cleaned.csv"))
combined_2018 = bind_rows(q3_data, merged_LA)
fwrite(combined_2018, file.path(clean_dir, "COVER_2018_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(combined_2018, "2018/2019 Full", valid_utlas_2019_to_2021)

#### 🫧 Final check for 2018 🫧 ####
combined_2018 %>%
  group_by(Quarter) %>%
  summarise(Unique_UTLAs = n_distinct(ONS_Code))

# List of ONS codes in final processed dataset should match the utla list from 2019 
missing_utlas = setdiff(valid_utlas_2019_to_2021, combined_2018$ONS_Code)
print(missing_utlas)

utla_list %>% filter(UTLA_code %in% missing_utlas)


#### ⋆˚࿔ 2 0 1 9 𝜗𝜚˚ ⋆ ####

#### 🫧 2019 Q1 🫧 ####
file_path = file.path(main_dir, "2019_Q1.ods")

# Load 12-month sheet
la12 = read_ods(file_path, sheet = "12m_UTLA_GOR") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  ) %>%
  filter(!is.na(ONS_Code))

# Load 24-month sheet
la24 = read_ods(file_path, sheet = "24m_UTLA_GOR") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  ) %>%
  filter(!is.na(ONS_Code))

# Join by both ONS_Code and UTLA_Name
merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  map_utla_codes_enhanced("2019/2020") %>%
  filter(ONS_Code %in% valid_utlas_2019_to_2021) %>%
  mutate(
    Year = "2019/2020",
    Quarter = "Q1",
    Timepoint = 0,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

# Apply boundary change aggregation
merged_LA = aggregate_boundary_changes(merged_LA, "2019/2020")

# Save Q1
fwrite(merged_LA, file.path(clean_dir, "COVER_2019_Q1_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(merged_LA, "2019/2020 Q1", valid_utlas_2019_to_2021)

#### 🫧 2019 Q2 🫧 ####
file_path = file.path(main_dir, "2019_Q2.ods")

# Load 12-month sheet
la12 = read_ods(file_path, sheet = "12m_UTLA_GOR") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  ) %>%
  filter(!is.na(ONS_Code))

# Load 24-month sheet
la24 = read_ods(file_path, sheet = "24m_UTLA_GOR") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  ) %>%
  filter(!is.na(ONS_Code))

# Join by both ONS_Code and UTLA_Name
merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  map_utla_codes_enhanced("2019/2020") %>%
  filter(ONS_Code %in% valid_utlas_2019_to_2021) %>%
  mutate(
    Year = "2019/2020",
    Quarter = "Q2",
    Timepoint = 1,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

# Apply boundary change aggregation
merged_LA = aggregate_boundary_changes(merged_LA, "2019/2020")

# Save Q2
q1_data = fread(file.path(clean_dir, "COVER_2019_Q1_Cleaned.csv"))
combined_2019 = bind_rows(q1_data, merged_LA)
fwrite(combined_2019, file.path(clean_dir, "COVER_2019_Q1_Q2_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(combined_2019, "2019/2020 Q1-Q2", valid_utlas_2019_to_2021)

#### 🫧 2019 Q3 🫧 ####
file_path = file.path(main_dir, "2019_Q3.ods")

# Load 12-month sheet
la12 = read_ods(file_path, sheet = "12m_UTLA_GOR") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  ) %>%
  filter(!is.na(ONS_Code))

# Load 24-month sheet
la24 = read_ods(file_path, sheet = "24m_UTLA_GOR") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  ) %>%
  filter(!is.na(ONS_Code))

# Join by both ONS_Code and UTLA_Name
merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  map_utla_codes_enhanced("2019/2020") %>%
  filter(ONS_Code %in% valid_utlas_2019_to_2021) %>%
  mutate(
    Year = "2019/2020",
    Quarter = "Q3",
    Timepoint = 2,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

# Apply boundary change aggregation
merged_LA = aggregate_boundary_changes(merged_LA, "2019/2020")

# Save Q3
q2_data = fread(file.path(clean_dir, "COVER_2019_Q1_Q2_Cleaned.csv"))
combined_2019 = bind_rows(q2_data, merged_LA)
fwrite(combined_2019, file.path(clean_dir, "COVER_2019_Q1_Q2_Q3_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(combined_2019, "2019/2020 Q1-Q3", valid_utlas_2019_to_2021)

#### 🫧 2019 Q4 🫧 ####
file_path = file.path(main_dir, "2019_Q4.ods")

# Load 12-month sheet
la12 = read_ods(file_path, sheet = "12m_UTLA_GOR") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  ) %>%
  filter(!is.na(ONS_Code))

# Load 24-month sheet
la24 = read_ods(file_path, sheet = "24m_UTLA_GOR") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  ) %>%
  filter(!is.na(ONS_Code))

#  Join by both ONS_Code and UTLA_Name
merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  map_utla_codes_enhanced("2019/2020") %>%
  filter(ONS_Code %in% valid_utlas_2019_to_2021) %>%
  mutate(
    Year = "2019/2020",
    Quarter = "Q4",
    Timepoint = 3,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

# Apply boundary change aggregation
merged_LA = aggregate_boundary_changes(merged_LA, "2019/2020")

# Save final 2019 file
q3_data = fread(file.path(clean_dir, "COVER_2019_Q1_Q2_Q3_Cleaned.csv"))
combined_2019 = bind_rows(q3_data, merged_LA)
fwrite(combined_2019, file.path(clean_dir, "COVER_2019_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(combined_2019, "2019/2020 Full", valid_utlas_2019_to_2021)

#### 🫧 Final check for 2019 🫧 ####
combined_2019 %>%
  group_by(Quarter) %>%
  summarise(Unique_UTLAs = n_distinct(ONS_Code))

# List of ONS codes in final processed dataset should match the utla list from 2019 
missing_utlas = setdiff(valid_utlas_2019_to_2021, combined_2019$ONS_Code)
print(missing_utlas)

utla_list %>% filter(UTLA_code %in% missing_utlas)

#####################################
#### *ੈ✩‧₊˚༺☆༻*ੈ✩‧₊˚ V  A  C  C  I  N  E    C  H  A  N  G  E *ੈ✩‧₊˚༺☆༻*ੈ✩‧₊˚  ####
#####################################

#### ⋆˚࿔ 2 0 2 0 𝜗𝜚˚ ⋆ ####

#### 🫧 2020 Q1 🫧 ####
file_path = file.path(main_dir, "2020 Q1.ods")

# Load 12-month sheet
la12 = read_ods(file_path, sheet = "12m_UTLA_GOR") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  ) %>%
  filter(!is.na(ONS_Code))

# Load 24-month sheet
la24 = read_ods(file_path, sheet = "24m_UTLA_GOR") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  ) %>%
  filter(!is.na(ONS_Code))

# Merge and process
merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  map_utla_codes_enhanced("2020/2021") %>%
  filter(ONS_Code %in% valid_utlas_2019_to_2021) %>%
  mutate(
    Year = "2020/2021",
    Quarter = "Q1",
    Timepoint = 0,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

# Apply boundary change aggregation
merged_LA = aggregate_boundary_changes(merged_LA, "2020/2021")
fwrite(merged_LA, file.path(clean_dir, "COVER_2020_Q1_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(merged_LA, "2020/2021 Q1", valid_utlas_2019_to_2021)

#### 🫧 2020 Q2 🫧 ####
file_path = file.path(main_dir, "2020 Q2.ods")

la12 = read_ods(file_path, sheet = "12m_UTLA_GOR") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  ) %>%
  filter(!is.na(ONS_Code))

la24 = read_ods(file_path, sheet = "24m_UTLA_GOR") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  ) %>%
  filter(!is.na(ONS_Code))

merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  map_utla_codes_enhanced("2020/2021") %>%
  filter(ONS_Code %in% valid_utlas_2019_to_2021) %>%
  mutate(
    Year = "2020/2021",
    Quarter = "Q2",
    Timepoint = 1,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

merged_LA = aggregate_boundary_changes(merged_LA, "2020/2021")
q1_data = fread(file.path(clean_dir, "COVER_2020_Q1_Cleaned.csv"))
combined_2020 = bind_rows(q1_data, merged_LA)
fwrite(combined_2020, file.path(clean_dir, "COVER_2020_Q1_Q2_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(combined_2020, "2020/2021 Q1-Q2", valid_utlas_2019_to_2021)

#### 🫧 2020 Q3 🫧 ####
file_path = file.path(main_dir, "2020 Q3.ods")

la12 = read_ods(file_path, sheet = "12m_UTLA_GOR") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  ) %>%
  filter(!is.na(ONS_Code))

la24 = read_ods(file_path, sheet = "24m_UTLA_GOR") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  ) %>%
  filter(!is.na(ONS_Code))

merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  map_utla_codes_enhanced("2020/2021") %>%
  filter(ONS_Code %in% valid_utlas_2019_to_2021) %>%
  mutate(
    Year = "2020/2021",
    Quarter = "Q3",
    Timepoint = 2,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

merged_LA = aggregate_boundary_changes(merged_LA, "2020/2021")
q2_data = fread(file.path(clean_dir, "COVER_2020_Q1_Q2_Cleaned.csv"))
combined_2020 = bind_rows(q2_data, merged_LA)
fwrite(combined_2020, file.path(clean_dir, "COVER_2020_Q1_Q2_Q3_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(combined_2020, "2020/2021 Q1-Q3", valid_utlas_2019_to_2021)

#### 🫧 2020 Q4 🫧 ####
file_path = file.path(main_dir, "2020 Q4.ods")

la12 = read_ods(file_path, sheet = "12m_UTLA_GOR") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV1%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV1%`
  )  %>%
  filter(!is.na(ONS_Code))

la24 = read_ods(file_path, sheet = "24m_UTLA_GOR") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  ) %>%
  filter(!is.na(ONS_Code))

merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  map_utla_codes_enhanced("2020/2021") %>%
  filter(ONS_Code %in% valid_utlas_2019_to_2021) %>%
  mutate(
    Year = "2020/2021",
    Quarter = "Q4",
    Timepoint = 3,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

merged_LA = aggregate_boundary_changes(merged_LA, "2020/2021")
q3_data = fread(file.path(clean_dir, "COVER_2020_Q1_Q2_Q3_Cleaned.csv"))
combined_2020 = bind_rows(q3_data, merged_LA)
fwrite(combined_2020, file.path(clean_dir, "COVER_2020_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(combined_2020, "2020/2021 Full", valid_utlas_2019_to_2021)

#### 🫧 Final check for 2020 🫧 ####
combined_2020 %>%
  group_by(Quarter) %>%
  summarise(Unique_UTLAs = n_distinct(ONS_Code))

# List of ONS codes in final processed dataset should match the utla list from 2019 
missing_utlas = setdiff(valid_utlas_2019_to_2021, combined_2020$ONS_Code)
print(missing_utlas)
utla_list %>% filter(UTLA_code %in% missing_utlas)

#####################################
#####################################

#### ⋆˚࿔ 2 0 2 1 𝜗𝜚˚ ⋆ ####

#### 🫧 2021 Q1 🫧 ####
file_path = file.path(main_dir, "2021 Q1.ods")

la12 = read_ods(file_path, sheet = "12m_UTLA_GOR") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV1%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV1%`
  ) %>%
  map_utla_codes_enhanced("2021/2022") %>%
  filter(ONS_Code %in% valid_utlas_from_2021_onwards)

la24 = read_ods(file_path, sheet = "24m_UTLA_GOR") %>%
  select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    UTLA_Name = `Upper Tier LA Name`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  ) %>%
  map_utla_codes_enhanced("2021/2022") %>%
  filter(ONS_Code %in% valid_utlas_from_2021_onwards)

merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  mutate(
    Year = "2021/2022",
    Quarter = "Q1",
    Timepoint = 0,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

merged_LA = aggregate_boundary_changes(merged_LA, "2021/2022")
fwrite(merged_LA, file.path(clean_dir, "COVER_2021_Q1_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(merged_LA, "2021/2022 Q1", valid_utlas_2019_to_2021)

#### 🫧 2021 Q2 🫧 ####
file_path = file.path(main_dir, "2021 Q2.ods")

la12 = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 4) %>%
  select(`ONS UTLA code`, `UTLA name`, `12m denominator`, `12m PCV1%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    UTLA_Name = `UTLA name`,
    Population_12m = `12m denominator`,
    PCV_12m = `12m PCV1%`
  ) %>%
  map_utla_codes_enhanced("2021/2022") %>%
  filter(ONS_Code %in% valid_utlas_from_2021_onwards)

la24 = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 4) %>%
  select(`ONS UTLA code`, `UTLA name`, `24m denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    UTLA_Name = `UTLA name`,
    Population_24m = `24m denominator`,
    PCV_24m = `24m PCV Booster%`
  ) %>%
  map_utla_codes_enhanced("2021/2022") %>%
  filter(ONS_Code %in% valid_utlas_from_2021_onwards)

merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  mutate(
    Year = "2021/2022",
    Quarter = "Q2",
    Timepoint = 1,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

merged_LA = aggregate_boundary_changes(merged_LA, "2021/2022")
q1_data = fread(file.path(clean_dir, "COVER_2021_Q1_Cleaned.csv"))
combined_2021 = bind_rows(q1_data, merged_LA)
fwrite(combined_2021, file.path(clean_dir, "COVER_2021_Q1_Q2_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(combined_2021, "2021/2022 Q1-Q2", valid_utlas_2019_to_2021)

#### 🫧 2021 Q3 🫧 ####
file_path = file.path(main_dir, "2021 Q3.ods")

la12 = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 4) %>%
  select(`ONS UTLA code`, `UTLA name`, `12 month denominator`, `12 month PCV1%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    UTLA_Name = `UTLA name`,
    Population_12m = `12 month denominator`,
    PCV_12m = `12 month PCV1%`
  ) %>%
  map_utla_codes_enhanced("2021/2022") %>%
  filter(ONS_Code %in% valid_utlas_from_2021_onwards)

la24 = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 4) %>%
  select(`ONS UTLA code`, `UTLA name`, `24 month denominator`, `24 month PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    UTLA_Name = `UTLA name`,
    Population_24m = `24 month denominator`,
    PCV_24m = `24 month PCV Booster%`
  ) %>%
  map_utla_codes_enhanced("2021/2022") %>%
  filter(ONS_Code %in% valid_utlas_from_2021_onwards)

merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  mutate(
    Year = "2021/2022",
    Quarter = "Q3",
    Timepoint = 2,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

merged_LA = aggregate_boundary_changes(merged_LA, "2021/2022")
q2_data = fread(file.path(clean_dir, "COVER_2021_Q1_Q2_Cleaned.csv"))
combined_2021 = bind_rows(q2_data, merged_LA)
fwrite(combined_2021, file.path(clean_dir, "COVER_2021_Q1_Q2_Q3_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(combined_2021, "2021/2022 Q1-Q3", valid_utlas_2019_to_2021)

#### 🫧 2021 Q4 🫧 ####
file_path = file.path(main_dir, "2021 Q4.ods")

la12 = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 4) %>%
  select(`ONS UTLA code`, `UTLA name`, `12m denominator`, `12m PCV1%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    UTLA_Name = `UTLA name`,
    Population_12m = `12m denominator`,
    PCV_12m = `12m PCV1%`
  ) %>%
  map_utla_codes_enhanced("2021/2022") %>%
  filter(ONS_Code %in% valid_utlas_from_2021_onwards)

la24 = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 4) %>%
  select(`ONS UTLA code`, `UTLA name`, `24m denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    UTLA_Name = `UTLA name`,
    Population_24m = `24m denominator`,
    PCV_24m = `24m PCV Booster%`
  ) %>%
  map_utla_codes_enhanced("2021/2022") %>%
  filter(ONS_Code %in% valid_utlas_from_2021_onwards)

merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  mutate(
    Year = "2021/2022",
    Quarter = "Q4",
    Timepoint = 3,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

merged_LA = aggregate_boundary_changes(merged_LA, "2021/2022")
q3_data = fread(file.path(clean_dir, "COVER_2021_Q1_Q2_Q3_Cleaned.csv"))
combined_2021 = bind_rows(q3_data, merged_LA)
fwrite(combined_2021, file.path(clean_dir, "COVER_2021_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(combined_2021, "2021/2022 Full", valid_utlas_2019_to_2021)

#### 🫧 Final check for 2021 🫧 ####
combined_2021 %>%
  group_by(Quarter) %>%
  summarise(Unique_UTLAs = n_distinct(ONS_Code))

# List of ONS codes in final processed dataset should match the utla list from 2019 
missing_utlas = setdiff(valid_utlas_2019_to_2021, combined_2021$ONS_Code)
print(missing_utlas)
utla_list %>% filter(UTLA_code %in% missing_utlas)

#####################################
#####################################

#### ⋆˚࿔ 2 0 2 2 𝜗𝜚˚ ⋆ ####

#### 🫧 2022 Q1 🫧 ####
file_path = file.path(main_dir, "2022 Q1.ods")

la12 = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 4) %>%
  select(`ONS UTLA code`, `UTLA name`, `12m denominator`, `12m PCV1%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    UTLA_Name = `UTLA name`,
    Population_12m = `12m denominator`,
    PCV_12m = `12m PCV1%`
  ) %>%
  map_utla_codes_enhanced("2022/2023") %>%
  filter(ONS_Code %in% valid_utlas_from_2021_onwards)

la24 = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 4) %>%
  select(`ONS UTLA code`, `UTLA name`, `24m denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    UTLA_Name = `UTLA name`,
    Population_24m = `24m denominator`,
    PCV_24m = `24m PCV Booster%`
  ) %>%
  map_utla_codes_enhanced("2022/2023") %>%
  filter(ONS_Code %in% valid_utlas_from_2021_onwards)

merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  mutate(
    Year = "2022/2023",
    Quarter = "Q1",
    Timepoint = 0,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

merged_LA = aggregate_boundary_changes(merged_LA, "2022/2023")
fwrite(merged_LA, file.path(clean_dir, "COVER_2022_Q1_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(merged_LA, "2022/2023 Q1", valid_utlas_2019_to_2021)

#### 🫧 2022 Q2 🫧 ####
file_path = file.path(main_dir, "2022 Q2.ods")

la12 = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 4) %>%
  select(`ONS UTLA code`, `UTLA name`, `12m denominator`, `12m PCV1%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    UTLA_Name = `UTLA name`,
    Population_12m = `12m denominator`,
    PCV_12m = `12m PCV1%`
  ) %>%
  map_utla_codes_enhanced("2022/2023") %>%
  filter(ONS_Code %in% valid_utlas_from_2021_onwards)

la24 = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 5) %>%
  select(`ONS UTLA code`, `UTLA name`, `24m denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    UTLA_Name = `UTLA name`,
    Population_24m = `24m denominator`,
    PCV_24m = `24m PCV Booster%`
  ) %>%
  map_utla_codes_enhanced("2022/2023") %>%
  filter(ONS_Code %in% valid_utlas_from_2021_onwards)

merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  mutate(
    Year = "2022/2023",
    Quarter = "Q2",
    Timepoint = 1,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

merged_LA = aggregate_boundary_changes(merged_LA, "2022/2023")
q1_data = fread(file.path(clean_dir, "COVER_2022_Q1_Cleaned.csv"))
combined_2022 = bind_rows(q1_data, merged_LA)
fwrite(combined_2022, file.path(clean_dir, "COVER_2022_Q1_Q2_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(combined_2022, "2022/2023 Q1-Q2", valid_utlas_2019_to_2021)

#### 🫧 2022 Q3 🫧 ####
file_path = file.path(main_dir, "2022 Q3.ods")

la12 = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 4) %>%
  select(`ONS UTLA code`, `UTLA name`, `12m denominator`, `12m PCV1%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    UTLA_Name = `UTLA name`,
    Population_12m = `12m denominator`,
    PCV_12m = `12m PCV1%`
  ) %>%
  map_utla_codes_enhanced("2022/2023") %>%
  filter(ONS_Code %in% valid_utlas_from_2021_onwards)

la24 = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 5) %>%
  select(`ONS UTLA code`, `UTLA name`, `24m denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    UTLA_Name = `UTLA name`,
    Population_24m = `24m denominator`,
    PCV_24m = `24m PCV Booster%`
  ) %>%
  map_utla_codes_enhanced("2022/2023") %>%
  filter(ONS_Code %in% valid_utlas_from_2021_onwards)

merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  mutate(
    Year = "2022/2023",
    Quarter = "Q3",
    Timepoint = 2,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

merged_LA = aggregate_boundary_changes(merged_LA, "2022/2023")
q2_data = fread(file.path(clean_dir, "COVER_2022_Q1_Q2_Cleaned.csv"))
combined_2022 = bind_rows(q2_data, merged_LA)
fwrite(combined_2022, file.path(clean_dir, "COVER_2022_Q1_Q2_Q3_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(combined_2022, "2022/2023 Q1-Q3", valid_utlas_2019_to_2021)

#### 🫧 2022 Q4 🫧 ####
file_path = file.path(main_dir, "2022 Q4.ods")

la12 = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 5) %>%
  select(`ONS upper tier local authority code`, `Upper tier local authority name`, `12 month denominator`, `12 month PCV1%`) %>%
  rename(
    ONS_Code = `ONS upper tier local authority code`,
    UTLA_Name = `Upper tier local authority name`,
    Population_12m = `12 month denominator`,
    PCV_12m = `12 month PCV1%`
  ) %>%
  map_utla_codes_enhanced("2022/2023") %>%
  filter(ONS_Code %in% valid_utlas_from_2021_onwards)

la24 = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 5) %>%
  select(`ONS upper tier local authority code`, `Upper tier local authority name`, `24 month denominator`, `24 month PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS upper tier local authority code`,
    UTLA_Name = `Upper tier local authority name`,
    Population_24m = `24 month denominator`,
    PCV_24m = `24 month PCV Booster%`
  ) %>%
  map_utla_codes_enhanced("2022/2023") %>%
  filter(ONS_Code %in% valid_utlas_from_2021_onwards)

merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  mutate(
    Year = "2022/2023",
    Quarter = "Q4",
    Timepoint = 3,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

merged_LA = aggregate_boundary_changes(merged_LA, "2022/2023")
q3_data = fread(file.path(clean_dir, "COVER_2022_Q1_Q2_Q3_Cleaned.csv"))
combined_2022 = bind_rows(q3_data, merged_LA)
fwrite(combined_2022, file.path(clean_dir, "COVER_2022_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(combined_2022, "2022/2023 Full", valid_utlas_2019_to_2021)

#### 🫧 Final check for 2022 🫧 ####
combined_2022 %>%
  group_by(Quarter) %>%
  summarise(Unique_UTLAs = n_distinct(ONS_Code))

# List of ONS codes in final processed dataset should match the utla list from 2019 
missing_utlas = setdiff(valid_utlas_2019_to_2021, combined_2022$ONS_Code)
print(missing_utlas)
utla_list %>% filter(UTLA_code %in% missing_utlas)

#####################################
#####################################

#### ⋆˚࿔ 2 0 2 3 𝜗𝜚˚ ⋆ ####

#### 🫧 2023 Q1 🫧 ####
file_path = file.path(main_dir, "2023 Q1.ods")

la12 = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 5) %>%
  select(`ONS UTLA code`, `UTLA name`, `12m denominator`, `12m PCV1%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    UTLA_Name = `UTLA name`,
    Population_12m = `12m denominator`,
    PCV_12m = `12m PCV1%`
  ) %>%
  map_utla_codes_enhanced("2023/2024") %>%
  filter(!is.na(ONS_Code))

la24 = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 5) %>%
  select(`ONS UTLA code`, `UTLA name`, `24m denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    UTLA_Name = `UTLA name`,
    Population_24m = `24m denominator`,
    PCV_24m = `24m PCV Booster%`
  ) %>%
  map_utla_codes_enhanced("2023/2024") %>%
  filter(!is.na(ONS_Code))

merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  mutate(
    Year = "2023/2024",
    Quarter = "Q1",
    Timepoint = 0,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

merged_LA = aggregate_boundary_changes(merged_LA, "2023/2024")
fwrite(merged_LA, file.path(clean_dir, "COVER_2023_Q1_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(merged_LA, "2023/2024 Q1", valid_utlas_2019_to_2021)

#### 🫧 2023 Q2 🫧 ####
file_path = file.path(main_dir, "2023 Q2.ods")

la12 = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 6) %>%
  select(`ONS UTLA code`, `UTLA name`, `12m denominator`, `12m PCV1%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    UTLA_Name = `UTLA name`,
    Population_12m = `12m denominator`,
    PCV_12m = `12m PCV1%`
  ) %>%
  map_utla_codes_enhanced("2023/2024") %>%
  filter(!is.na(ONS_Code))

la24 = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 5) %>%
  select(`ONS UTLA code`, `UTLA name`, `24m denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    UTLA_Name = `UTLA name`,
    Population_24m = `24m denominator`,
    PCV_24m = `24m PCV Booster%`
  ) %>%
  map_utla_codes_enhanced("2023/2024") %>%
  filter(!is.na(ONS_Code))

merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  mutate(
    Year = "2023/2024",
    Quarter = "Q2",
    Timepoint = 1,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

merged_LA = aggregate_boundary_changes(merged_LA, "2023/2024")
q1_data = fread(file.path(clean_dir, "COVER_2023_Q1_Cleaned.csv"))
combined_2023 = bind_rows(q1_data, merged_LA)
fwrite(combined_2023, file.path(clean_dir, "COVER_2023_Q1_Q2_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(combined_2023, "2023/2024 Q1-Q2", valid_utlas_2019_to_2021)

#### 🫧 2023 Q3 🫧 ####
file_path = file.path(main_dir, "2023 Q3.ods")

la12 = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 5) %>%
  select(`ONS UTLA code`, `UTLA name`, `12m denominator`, `12m PCV1%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    UTLA_Name = `UTLA name`,
    Population_12m = `12m denominator`,
    PCV_12m = `12m PCV1%`
  ) %>%
  map_utla_codes_enhanced("2023/2024") %>%
  filter(!is.na(ONS_Code))

la24 = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 5) %>%
  select(`ONS UTLA code`, `UTLA name`, `24m denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    UTLA_Name = `UTLA name`,
    Population_24m = `24m denominator`,
    PCV_24m = `24m PCV Booster%`
  ) %>%
  map_utla_codes_enhanced("2023/2024") %>%
  filter(!is.na(ONS_Code))

merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  mutate(
    Year = "2023/2024",
    Quarter = "Q3",
    Timepoint = 2,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

merged_LA = aggregate_boundary_changes(merged_LA, "2023/2024")
q2_data = fread(file.path(clean_dir, "COVER_2023_Q1_Q2_Cleaned.csv"))
combined_2023 = bind_rows(q2_data, merged_LA)
fwrite(combined_2023, file.path(clean_dir, "COVER_2023_Q1_Q2_Q3_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(combined_2023, "2023/2024 Q1-Q3", valid_utlas_2019_to_2021)

#### 🫧 2023 Q4 🫧 ####
file_path = file.path(main_dir, "2023 Q4.ods")

la12 = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 5) %>%
  select(`ONS UTLA code`, `UTLA name`, `12m denominator`, `12m PCV1%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    UTLA_Name = `UTLA name`,
    Population_12m = `12m denominator`,
    PCV_12m = `12m PCV1%`
  ) %>%
  map_utla_codes_enhanced("2023/2024") %>%
  filter(!is.na(ONS_Code), ONS_Code != "[z]")

la24 = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 5) %>%
  select(`ONS UTLA code`, `UTLA name`, `24m denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    UTLA_Name = `UTLA name`,
    Population_24m = `24m denominator`,
    PCV_24m = `24m PCV Booster%`
  ) %>%
  map_utla_codes_enhanced("2023/2024") %>%
  filter(!is.na(ONS_Code), ONS_Code != "[z]")

merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  mutate(
    Year = "2023/2024",
    Quarter = "Q4",
    Timepoint = 3,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

merged_LA = aggregate_boundary_changes(merged_LA, "2023/2024")
q3_data = fread(file.path(clean_dir, "COVER_2023_Q1_Q2_Q3_Cleaned.csv"))
combined_2023 = bind_rows(q3_data, merged_LA)
fwrite(combined_2023, file.path(clean_dir, "COVER_2023_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(combined_2023, "2023/2024 Full", valid_utlas_2019_to_2021)

#### 🫧 Final check for 2023 🫧 ####
combined_2023 %>%
  group_by(Quarter) %>%
  summarise(Unique_UTLAs = n_distinct(ONS_Code))

# List of ONS codes in final processed dataset should match the utla list from 2019 
missing_utlas = setdiff(valid_utlas_2019_to_2021, combined_2023$ONS_Code)
print(missing_utlas)
utla_list %>% filter(UTLA_code %in% missing_utlas)

#####################################
#####################################

#### ⋆˚࿔ 2 0 2 4 𝜗𝜚˚ ⋆ ####

#### 🫧 2024 Q1 🫧 ####
file_path = file.path(main_dir, "2024 Q1.ods")

la12 = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 5) %>%
  select(`ONS UTLA code`, `UTLA name`, `12m denominator`, `12m PCV1 (%)`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    UTLA_Name = `UTLA name`,
    Population_12m = `12m denominator`,
    PCV_12m = `12m PCV1 (%)`
  ) %>%
  map_utla_codes_enhanced("2024/2025") %>%
  filter(!is.na(ONS_Code), ONS_Code != "[z]")

la24 = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 5) %>%
  select(`ONS UTLA code`, `UTLA name`, `24m denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    UTLA_Name = `UTLA name`,
    Population_24m = `24m denominator`,
    PCV_24m = `24m PCV Booster%`
  ) %>%
  map_utla_codes_enhanced("2024/2025") %>%
  filter(!is.na(ONS_Code), ONS_Code != "[z]")

merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  mutate(
    Year = "2024/2025",
    Quarter = "Q1",
    Timepoint = 0,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

merged_LA = aggregate_boundary_changes(merged_LA, "2024/2025")
fwrite(merged_LA, file.path(clean_dir, "COVER_2024_Q1_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(merged_LA, "2024/2025 Q1", valid_utlas_2019_to_2021)

#### 🫧 2024 Q2 🫧 ####
file_path = file.path(main_dir, "2024 Q2.ods")

la12 = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 6) %>%
  select(`Code`, `Local authority`, `Number of children who reached 12 months in reporting quarter`, `Coverage at 12 months PCV1 (%)`) %>%
  rename(
    ONS_Code = `Code`,
    UTLA_Name = `Local authority`,
    Population_12m = `Number of children who reached 12 months in reporting quarter`,
    PCV_12m = `Coverage at 12 months PCV1 (%)`
  ) %>%
  map_utla_codes_enhanced("2024/2025") %>%
  filter(!is.na(ONS_Code), ONS_Code != "[z]")

la24 = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 6) %>%
  select(`Code`, `Local authority`, `Number of children who reached 24 months in reporting quarter`, `Coverage at 24 months PCV Booster (%)`) %>%
  rename(
    ONS_Code = `Code`,
    UTLA_Name = `Local authority`,
    Population_24m = `Number of children who reached 24 months in reporting quarter`,
    PCV_24m = `Coverage at 24 months PCV Booster (%)`
  ) %>%
  map_utla_codes_enhanced("2024/2025") %>%
  filter(!is.na(ONS_Code), ONS_Code != "[z]")

merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  mutate(
    Year = "2024/2025",
    Quarter = "Q2",
    Timepoint = 1,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

merged_LA = aggregate_boundary_changes(merged_LA, "2024/2025")
q1_data = fread(file.path(clean_dir, "COVER_2024_Q1_Cleaned.csv"))
combined_2024 = bind_rows(q1_data, merged_LA)
fwrite(combined_2024, file.path(clean_dir, "COVER_2024_Q1_Q2_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(combined_2024, "2024/2025 Q1-Q2", valid_utlas_2019_to_2021)

#### 🫧 2024 Q3 🫧 ####
file_path = file.path(main_dir, "2024 Q3.ods")

la12 = read_ods(file_path, sheet = "Table5", skip = 5) %>%
  select(`Code`, `Local authority`, `Number of children who reached 12 months in reporting quarter`, `Coverage at 12 months PCV1 (%)`) %>%
  rename(
    ONS_Code = `Code`,
    UTLA_Name = `Local authority`,
    Population_12m = `Number of children who reached 12 months in reporting quarter`,
    PCV_12m = `Coverage at 12 months PCV1 (%)`
  ) %>%
  map_utla_codes_enhanced("2024/2025") %>%
  filter(!is.na(ONS_Code), ONS_Code != "[z]")

la24 = read_ods(file_path, sheet = "Table6", skip = 4) %>%
  select(`Code`, `Local authority`, `Number of children who reached 24 months in reporting quarter`, `Coverage at 24 months PCV Booster (%)`) %>%
  rename(
    ONS_Code = `Code`,
    UTLA_Name = `Local authority`,
    Population_24m = `Number of children who reached 24 months in reporting quarter`,
    PCV_24m = `Coverage at 24 months PCV Booster (%)`
  ) %>%
  map_utla_codes_enhanced("2024/2025") %>%
  filter(!is.na(ONS_Code), ONS_Code != "[z]")

merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
  mutate(
    Year = "2024/2025",
    Quarter = "Q3",
    Timepoint = 2,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, UTLA_Name, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

merged_LA = aggregate_boundary_changes(merged_LA, "2024/2025")
q2_data = fread(file.path(clean_dir, "COVER_2024_Q1_Q2_Cleaned.csv"))
combined_2024 = bind_rows(q2_data, merged_LA)
fwrite(combined_2024, file.path(clean_dir, "COVER_2024_Cleaned.csv"))

# Validation check: List of ONS codes in final processed dataset should match the utla list from 2019 
validate_cover_data(combined_2024, "2024/2025 Full", valid_utlas_2019_to_2021)

#### 🫧 Final Check for 2024 🫧 ####
combined_2024 %>%
  group_by(Quarter) %>%
  summarise(Unique_UTLAs = n_distinct(ONS_Code))

# List of ONS codes in final processed dataset should match the utla list from 2019 
missing_utlas = setdiff(valid_utlas_2019_to_2021, combined_2024$ONS_Code)
print(missing_utlas)
utla_list %>% filter(UTLA_code %in% missing_utlas)

