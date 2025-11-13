
#===============================================================================
# SCRIPT: 01_clean_and_process_COVER_data.R
# PURPOSE: Clean and process quarterly PCV vaccine uptake data from COVER programme
# 
# DESCRIPTION: 
# - Processes COVER vaccine uptake files from 2013-2024 (Q2 2013 to Q3 2024)
# - Handles different file formats (.xls, .xlsx, .ods) across years
# - Merges 12-month primary and 24-month booster coverage data
# - Manages UTLA boundary changes and missing geographic identifiers
# - Outputs cleaned quarterly and annual datasets
#
# MAIN SECTIONS:
# - Package loading and setup
# - UTLA validation list creation
# - Year-by-year data processing (2013-2024)
# - Data validation and completeness checks
#
# INPUTS: Raw COVER quarterly files, UTLA_summaries.xlsx
# OUTPUTS: Cleaned CSV files in Cleaned_COVER_Files2/ directory
#===============================================================================


####🫧 Loading and installing packages 🫧####
# install.packages("readxl")
library(readxl)
# install.packages("dplyr")
library(dplyr)
# install.packages("data.table")
library(data.table)
# install.packages("readODS")
library(readODS)
# install.packages("here")
library(here)
# install.packages("purrr")
library(purrr)


####🫧 Set up folders 🫧####

# Raw data is now in repository data folder
main_dir = here("data")
imd_file = here("data", "UTLA_summaries.xlsx")

# Create cleaned data subfolder
clean_dir = here("data", "cleaned")
dir.create(clean_dir, showWarnings = FALSE, recursive = TRUE)

#### 🫧 Load UTLA codes from IMD file 🫧 ####
imd_sheet = "IMD" 

# Read the data
utla_imd = read_excel(imd_file, sheet = imd_sheet)
# Preview data structure
str(utla_imd)

# Extract just UTLA codes and names, remove City of London and Isles of Scilly
utla_list = utla_imd %>%
  select(UTLA_code = 1, UTLA_name = 2) %>%
  filter(!UTLA_name %in% c("City of London", "Isles of Scilly")) %>%
  # Add Northamptonshire which might be missing
  bind_rows(
    data.frame(UTLA_code = "E10000021", UTLA_name = "Northamptonshire")
  )

# Optional: View results
print(utla_list)

valid_utlas = utla_list$UTLA_code

#####################################
#####################################

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

# Enhanced mapping function with boundary change handling
map_utla_codes_enhanced <- function(df, year) {
  
  # FIX: Handle the case where full_join creates UTLA_Name.x and UTLA_Name.y
  if ("UTLA_Name.x" %in% names(df) && "UTLA_Name.y" %in% names(df)) {
    df <- df %>%
      mutate(UTLA_Name = coalesce(UTLA_Name.x, UTLA_Name.y)) %>%
      select(-UTLA_Name.x, -UTLA_Name.y)
  }
  
  df_processed = df %>%
    left_join(utla_list, by = c("UTLA_Name" = "UTLA_name")) %>%
    mutate(
      ONS_Code = ifelse(ONS_Code %in% valid_utlas, ONS_Code, UTLA_code)
    ) %>%
    select(-UTLA_code)
  
  # Handle Bournemouth, Christchurch and Poole for pre-2019 years
  if (year %in% c("2013/2014", "2014/2015", "2015/2016", "2016/2017", "2017/2018", "2018/2019")) {
        bcp_info = utla_boundary_changes[["E06000058"]]
    
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
    northants_info = utla_boundary_changes[["E10000021"]]
    
    # Map new codes back to old Northamptonshire code
    df_processed = df_processed %>%
      mutate(
        ONS_Code = case_when(
          ONS_Code %in% northants_info$post_2021_codes ~ "E10000021",
          UTLA_Name %in% northants_info$post_2021_names ~ "E10000021",
          TRUE ~ ONS_Code
        ),
        UTLA_Name = case_when(
          ONS_Code == "E10000021" ~ "Northamptonshire",
          TRUE ~ UTLA_Name
        )
      )
  }
  
  return(df_processed)
}

# Function to aggregate data for split/merged authorities
aggregate_boundary_changes <- function(df, year) {
  
  # Aggregate Bournemouth, Christchurch and Poole for pre-2019 years
  if (year %in% c("2013/2014", "2014/2015", "2015/2016", "2016/2017", "2017/2018", "2018/2019")) {
    bcp_codes = utla_boundary_changes[["E06000058"]]$pre_2019_codes
    
    if(any(bcp_codes %in% df$ONS_Code)) {
      bcp_aggregated = df %>%
        filter(ONS_Code %in% bcp_codes) %>%
        group_by(Year, Quarter, Timepoint, Vaccine_Schedule) %>%
        summarise(
          Population_12m = sum(Population_12m, na.rm = TRUE),
          Population_24m = sum(Population_24m, na.rm = TRUE),
          PCV_12m = ifelse(all(is.na(Population_12m)) | sum(Population_12m, na.rm = TRUE) == 0, 
                           NA, weighted.mean(PCV_12m, Population_12m, na.rm = TRUE)),
          PCV_24m = ifelse(all(is.na(Population_24m)) | sum(Population_24m, na.rm = TRUE) == 0,
                           NA, weighted.mean(PCV_24m, Population_24m, na.rm = TRUE)),
          .groups = "drop"
        ) %>%
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
      northants_aggregated = df %>%
        filter(ONS_Code %in% northants_codes) %>%
        group_by(Year, Quarter, Timepoint, Vaccine_Schedule) %>%
        summarise(
          Population_12m = sum(Population_12m, na.rm = TRUE),
          Population_24m = sum(Population_24m, na.rm = TRUE),
          PCV_12m = ifelse(all(is.na(Population_12m)) | sum(Population_12m, na.rm = TRUE) == 0,
                           NA, weighted.mean(PCV_12m, Population_12m, na.rm = TRUE)),
          PCV_24m = ifelse(all(is.na(Population_24m)) | sum(Population_24m, na.rm = TRUE) == 0,
                           NA, weighted.mean(PCV_24m, Population_24m, na.rm = TRUE)),
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

#### 🫧 Data Validation Function 🫧 ####
validate_cover_data <- function(cover_data, year) {
  cat("\n=== VALIDATION FOR:", year, "===\n")
  
  coverage = cover_data %>%
    group_by(Quarter) %>%
    summarise(
      Unique_UTLAs = n_distinct(ONS_Code),
      Expected_UTLAs = length(valid_utlas),
      Coverage_Pct = round(Unique_UTLAs / Expected_UTLAs * 100, 1)
    )
  
  print(coverage)
  
  return(coverage)
}


#####################################
#####################################

#### ⋆˚࿔ 2 0 1 3 — Q2 🫧 ####

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
  mutate(
    Population_12m = as.numeric(Population_12m),
    PCV_12m = as.numeric(PCV_12m)
  ) %>%
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
  mutate(
    Population_24m = as.numeric(Population_24m),
    PCV_24m = as.numeric(PCV_24m)
  ) %>%
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
validate_cover_data(merged_LA, "2023/2024 Q1")

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
  mutate(
    Population_12m = as.numeric(Population_12m),
    PCV_12m = as.numeric(PCV_12m)
  ) %>%
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
  mutate(
    Population_24m = as.numeric(Population_24m),
    PCV_24m = as.numeric(PCV_24m)
  ) %>%
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
validate_cover_data(combined_2023, "2023/2024 Q1-Q2")

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
  mutate(
    Population_12m = as.numeric(Population_12m),
    PCV_12m = as.numeric(PCV_12m)
  ) %>%
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
  mutate(
    Population_24m = as.numeric(Population_24m),
    PCV_24m = as.numeric(PCV_24m)
  ) %>%
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
validate_cover_data(combined_2023, "2023/2024 Q1-Q3")

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
  mutate(
    Population_12m = as.numeric(Population_12m),
    PCV_12m = as.numeric(PCV_12m)
  ) %>%
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
  mutate(
    Population_24m = as.numeric(Population_24m),
    PCV_24m = as.numeric(PCV_24m)
  ) %>%
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
validate_cover_data(combined_2023, "2023/2024 Full")

#### 🫧 Final check for 2023 🫧 ####
combined_2023 %>%
  group_by(Quarter) %>%
  summarise(Unique_UTLAs = n_distinct(ONS_Code))

missing_utlas = setdiff(valid_utlas, combined_2023$ONS_Code)
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
  mutate(
    Population_12m = as.numeric(Population_12m),
    PCV_12m = as.numeric(PCV_12m)
  ) %>%
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
  mutate(
    Population_24m = as.numeric(Population_24m),
    PCV_24m = as.numeric(PCV_24m)
  ) %>%
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
validate_cover_data(merged_LA, "2024/2025 Q1")

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
  mutate(
    Population_12m = as.numeric(Population_12m),
    PCV_12m = as.numeric(PCV_12m)
  ) %>%
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
  mutate(
    Population_24m = as.numeric(Population_24m),
    PCV_24m = as.numeric(PCV_24m)
  ) %>%
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
validate_cover_data(combined_2024, "2024/2025 Q1-Q2")

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
  mutate(
    Population_12m = as.numeric(Population_12m),
    PCV_12m = as.numeric(PCV_12m)
  ) %>%
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
  mutate(
    Population_24m = as.numeric(Population_24m),
    PCV_24m = as.numeric(PCV_24m)
  ) %>%
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
validate_cover_data(combined_2024, "2024/2025 Full")

#### 🫧 Final Check for 2024 🫧 ####
combined_2024 %>%
  group_by(Quarter) %>%
  summarise(Unique_UTLAs = n_distinct(ONS_Code))

missing_utlas = setdiff(valid_utlas, combined_2024$ONS_Code)
print(missing_utlas)
utla_list %>% filter(UTLA_code %in% missing_utlas)


#### 🫧 YEAR-BY-YEAR UTLA COVERAGE 🫧 ####
all_years_data =read.csv(here("cleaned_Data", "COVER_All_Years_UNIMPUTED.csv"))

coverage_by_year <- all_years_data %>%
  group_by(Year) %>%
  summarise(
    Unique_UTLAs = n_distinct(ONS_Code),
    Missing_UTLAs = length(valid_utlas) - n_distinct(ONS_Code),
    .groups = "drop"
  )

cat("\n=== UTLA COVERAGE BY YEAR ===\n")
print(coverage_by_year)

# Identify which UTLAs are missing in which years
missing_by_year <- map_dfr(unique(all_years_data$Year), function(year) {
  year_utlas <- all_years_data %>%
    filter(Year == year) %>%
    pull(ONS_Code) %>%
    unique()
  
  missing_in_year <- setdiff(valid_utlas, year_utlas)
  
  if(length(missing_in_year) > 0) {
    tibble(
      Year = year,
      Missing_Count = length(missing_in_year),
      Missing_Codes = paste(missing_in_year, collapse = ", ")
    )
  }
})

if(nrow(missing_by_year) > 0) {
  cat("\n=== MISSING UTLAS BY YEAR ===\n")
  print(missing_by_year)
  
  # Get names for missing codes
  for(i in 1:nrow(missing_by_year)) {
    year <- missing_by_year$Year[i]
    codes <- strsplit(missing_by_year$Missing_Codes[i], ", ")[[1]]
    names <- utla_list %>% 
      filter(UTLA_code %in% codes) %>%
      pull(UTLA_name)
    
    cat("\n", year, ": Missing", length(codes), "UTLAs:\n")
    cat(paste(names, collapse = "\n"), "\n")
  }
} else {
  cat("\n✅ No missing UTLAs in any year!\n")
}
