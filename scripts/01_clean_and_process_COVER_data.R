
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

####🫧 Set up folders 🫧####
library(here)

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
  filter(
    !UTLA_name %in% c("City of London", "Isles of Scilly")
  )

# Optional: View results
print(utla_list)

valid_utlas = utla_list$UTLA_code

#####################################
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
  
  # Merge
  merged_LA = full_join(la12, la24, by = "ONS_Code") %>%
    filter(ONS_Code %in% valid_utlas) %>%
    mutate(
      Year = "2013/2014",
      Quarter = "Q2",
      Timepoint = 1,
      Vaccine_Schedule = 0
    ) %>%
    select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m,
           Year, Quarter, Timepoint, Vaccine_Schedule)
  
  # Check for missing but present by name
  missing_utlas_q2 = setdiff(valid_utlas, merged_LA$ONS_Code)
  if (length(missing_utlas_q2) > 0) {
    missing_names_q2 = utla_list %>% filter(UTLA_code %in% missing_utlas_q2)
    la12_extra = la12 %>% filter(UTLA_Name %in% missing_names_q2$UTLA_name)
    la24_extra = la24 %>% filter(UTLA_Name %in% missing_names_q2$UTLA_name)
    
    merged_extra = full_join(la12_extra, la24_extra, by = "UTLA_Name") %>%
      left_join(missing_names_q2, by = c("UTLA_Name" = "UTLA_name")) %>%
      rename(ONS_Code = UTLA_code) %>%
      select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m) %>%
      mutate(
        Year = "2013/2014",
        Quarter = "Q2",
        Timepoint = 1,
        Vaccine_Schedule = 0
      )
    
    merged_LA = bind_rows(merged_LA, merged_extra) %>% distinct()
  }
  
  # Save Q2
  fwrite(merged_LA, file.path(clean_dir, "COVER_2013_Q2_Cleaned.csv"))
  message("✅ Q2 cleaned and saved")
  
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
  
  merged_LA = full_join(la12, la24, by = "ONS_Code") %>%
    filter(ONS_Code %in% valid_utlas) %>%
    mutate(
      Year = "2013/2014",
      Quarter = "Q3",
      Timepoint = 2,
      Vaccine_Schedule = 0
    ) %>%
    select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m,
           Year, Quarter, Timepoint, Vaccine_Schedule)
  
  missing_utlas_q3 = setdiff(valid_utlas, merged_LA$ONS_Code)
  if (length(missing_utlas_q3) > 0) {
    missing_names_q3 = utla_list %>% filter(UTLA_code %in% missing_utlas_q3)
    la12_extra = la12 %>% filter(UTLA_Name %in% missing_names_q3$UTLA_name)
    la24_extra = la24 %>% filter(UTLA_Name %in% missing_names_q3$UTLA_name)
    
    merged_extra = full_join(la12_extra, la24_extra, by = "UTLA_Name") %>%
      left_join(missing_names_q3, by = c("UTLA_Name" = "UTLA_name")) %>%
      rename(ONS_Code = UTLA_code) %>%
      select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m) %>%
      mutate(
        Year = "2013/2014",
        Quarter = "Q3",
        Timepoint = 2,
        Vaccine_Schedule = 0
      )
    
    merged_LA = bind_rows(merged_LA, merged_extra) %>% distinct()
  }
  
  q2_data = fread(file.path(clean_dir, "COVER_2013_Q2_Cleaned.csv"))
  combined_2013 = bind_rows(q2_data, merged_LA)
  fwrite(combined_2013, file.path(clean_dir, "COVER_2013_Q2_Q3_Cleaned.csv"))
  message("✅ Q3 cleaned and added to Q2 data")
  
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
  
  merged_LA = full_join(la12, la24, by = "ONS_Code") %>%
    filter(ONS_Code %in% valid_utlas) %>%
    mutate(
      Year = "2013/2014",
      Quarter = "Q4",
      Timepoint = 3,
      Vaccine_Schedule = 0
    ) %>%
    select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m,
           Year, Quarter, Timepoint, Vaccine_Schedule)
  
  missing_utlas_q4 = setdiff(valid_utlas, merged_LA$ONS_Code)
  if (length(missing_utlas_q4) > 0) {
    missing_names_q4 = utla_list %>% filter(UTLA_code %in% missing_utlas_q4)
    la12_extra = la12 %>% filter(UTLA_Name %in% missing_names_q4$UTLA_name)
    la24_extra = la24 %>% filter(UTLA_Name %in% missing_names_q4$UTLA_name)
    
    merged_extra = full_join(la12_extra, la24_extra, by = "UTLA_Name") %>%
      left_join(missing_names_q4, by = c("UTLA_Name" = "UTLA_name")) %>%
      rename(ONS_Code = UTLA_code) %>%
      select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m) %>%
      mutate(
        Year = "2013/2014",
        Quarter = "Q4",
        Timepoint = 3,
        Vaccine_Schedule = 0
      )
    
    merged_LA = bind_rows(merged_LA, merged_extra) %>% distinct()
  }
  
  q3_data = fread(file.path(clean_dir, "COVER_2013_Q2_Q3_Cleaned.csv"))
  combined_2013 = bind_rows(q3_data, merged_LA)
  fwrite(combined_2013, file.path(clean_dir, "COVER_2013_Cleaned.csv"))
  message("✅ Q4 cleaned and final 2013 file saved")
  
#### 🫧 Final check 🫧 ####
  combined_2013 %>%
    group_by(Quarter) %>%
    summarise(Unique_UTLAs = n_distinct(ONS_Code))
  
  missing_utlas = setdiff(valid_utlas, combined_2013$ONS_Code)
  print(missing_utlas)
  
  utla_list %>% filter(UTLA_code %in% missing_utlas)  
  
  
#####################################
#####################################



#### ⋆˚࿔ 2 0 1 4 𝜗𝜚˚⋆ ####

  #### 🫧 2014 Q1 🫧 ####
# File path
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

# Merge and filter
merged_LA = full_join(la12, la24, by = "ONS_Code") %>%
  filter(ONS_Code %in% valid_utlas) %>%
  mutate(
    Year = "2014/2015",
    Quarter = "Q1",
    Timepoint = 0,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

# Check for missing but present by name
missing_utlas_q1 = setdiff(valid_utlas, merged_LA$ONS_Code)
if (length(missing_utlas_q1) > 0) {
  missing_names_q1 = utla_list %>% filter(UTLA_code %in% missing_utlas_q1)
  la12_extra = la12 %>% filter(UTLA_Name %in% missing_names_q1$UTLA_name)
  la24_extra = la24 %>% filter(UTLA_Name %in% missing_names_q1$UTLA_name)
  
  merged_extra = full_join(la12_extra, la24_extra, by = "UTLA_Name") %>%
    left_join(missing_names_q1, by = c("UTLA_Name" = "UTLA_name")) %>%
    rename(ONS_Code = UTLA_code) %>%
    select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m) %>%
    mutate(
      Year = "2014/2015",
      Quarter = "Q1",
      Timepoint = 0,
      Vaccine_Schedule = 0
    )
  
  merged_LA = bind_rows(merged_LA, merged_extra) %>% distinct()
}

# Save Q1
fwrite(merged_LA, file.path(clean_dir, "COVER_2014_Q1_Cleaned.csv"))
message("✅ Q1 cleaned and saved")

  
  #### 🫧 2014 Q2 🫧 ####
# File path
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

# Merge and filter
merged_LA = full_join(la12, la24, by = "ONS_Code") %>%
  filter(ONS_Code %in% valid_utlas) %>%
  mutate(
    Year = "2014/2015",
    Quarter = "Q2",
    Timepoint = 1,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

# Check for missing but present by name
missing_utlas_q2 = setdiff(valid_utlas, merged_LA$ONS_Code)
if (length(missing_utlas_q2) > 0) {
  missing_names_q2 = utla_list %>% filter(UTLA_code %in% missing_utlas_q2)
  la12_extra = la12 %>% filter(UTLA_Name %in% missing_names_q2$UTLA_name)
  la24_extra = la24 %>% filter(UTLA_Name %in% missing_names_q2$UTLA_name)
  
  merged_extra = full_join(la12_extra, la24_extra, by = "UTLA_Name") %>%
    left_join(missing_names_q2, by = c("UTLA_Name" = "UTLA_name")) %>%
    rename(ONS_Code = UTLA_code) %>%
    select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m) %>%
    mutate(
      Year = "2014/2015",
      Quarter = "Q2",
      Timepoint = 1,
      Vaccine_Schedule = 0
    )
  
  merged_LA = bind_rows(merged_LA, merged_extra) %>% distinct()
}

# Save Q2
q1_data = fread(file.path(clean_dir, "COVER_2014_Q1_Cleaned.csv"))
combined_2014 = bind_rows(q1_data, merged_LA)
fwrite(combined_2014, file.path(clean_dir, "COVER_2014_Q1_Q2_Cleaned.csv"))
message("✅ Q2 cleaned and added to Q1 data")


  #### 🫧 2014 Q3 🫧 ####
# File path
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

# Merge and filter
merged_LA = full_join(la12, la24, by = "ONS_Code") %>%
  filter(ONS_Code %in% valid_utlas) %>%
  mutate(
    Year = "2014/2015",
    Quarter = "Q3",
    Timepoint = 2,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

# Check for missing but present by name
missing_utlas_q3 = setdiff(valid_utlas, merged_LA$ONS_Code)
if (length(missing_utlas_q3) > 0) {
  missing_names_q3 = utla_list %>% filter(UTLA_code %in% missing_utlas_q3)
  la12_extra = la12 %>% filter(UTLA_Name %in% missing_names_q3$UTLA_name)
  la24_extra = la24 %>% filter(UTLA_Name %in% missing_names_q3$UTLA_name)
  
  merged_extra = full_join(la12_extra, la24_extra, by = "UTLA_Name") %>%
    left_join(missing_names_q3, by = c("UTLA_Name" = "UTLA_name")) %>%
    rename(ONS_Code = UTLA_code) %>%
    select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m) %>%
    mutate(
      Year = "2014/2015",
      Quarter = "Q3",
      Timepoint = 2,
      Vaccine_Schedule = 0
    )
  
  merged_LA = bind_rows(merged_LA, merged_extra) %>% distinct()
}

# Save Q3
q2_data = fread(file.path(clean_dir, "COVER_2014_Q1_Q2_Cleaned.csv"))
combined_2014 = bind_rows(q2_data, merged_LA)
fwrite(combined_2014, file.path(clean_dir, "COVER_2014_Q1_Q2_Q3_Cleaned.csv"))
message("✅ Q3 cleaned and added to Q2 data")

  
  #### 🫧 2014 Q4 🫧 ####
# File path
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

# 4. Merge and filter
merged_LA = full_join(la12, la24, by = "ONS_Code") %>%
  filter(ONS_Code %in% valid_utlas) %>%
  mutate(
    Year = "2014/2015",
    Quarter = "Q4",
    Timepoint = 3,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m,
         Year, Quarter, Timepoint, Vaccine_Schedule)

# Check for missing but present by name
missing_utlas_q4 = setdiff(valid_utlas, merged_LA$ONS_Code)
if (length(missing_utlas_q4) > 0) {
  missing_names_q4 = utla_list %>% filter(UTLA_code %in% missing_utlas_q4)
  la12_extra = la12 %>% filter(UTLA_Name %in% missing_names_q4$UTLA_name)
  la24_extra = la24 %>% filter(UTLA_Name %in% missing_names_q4$UTLA_name)
  
  merged_extra = full_join(la12_extra, la24_extra, by = "UTLA_Name") %>%
    left_join(missing_names_q4, by = c("UTLA_Name" = "UTLA_name")) %>%
    rename(ONS_Code = UTLA_code) %>%
    select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m) %>%
    mutate(
      Year = "2014/2015",
      Quarter = "Q4",
      Timepoint = 3,
      Vaccine_Schedule = 0
    )
  
  merged_LA = bind_rows(merged_LA, merged_extra) %>% distinct()
}

# Save Q4
q3_data = fread(file.path(clean_dir, "COVER_2014_Q1_Q2_Q3_Cleaned.csv"))
combined_2014 = bind_rows(q3_data, merged_LA)
fwrite(combined_2014, file.path(clean_dir, "COVER_2014_Cleaned.csv"))
message("✅ Q4 cleaned and final 2014 file saved")

#### 🫧 Final check 🫧 ####
combined_2014 %>%
  group_by(Quarter) %>%
  summarise(Unique_UTLAs = n_distinct(ONS_Code))

missing_utlas = setdiff(valid_utlas, combined_2014$ONS_Code)
print(missing_utlas)

utla_list %>% filter(UTLA_code %in% missing_utlas)





#####################################
#####################################

#### ⋆˚࿔ 2 0 1 5 𝜗𝜚˚ ⋆ ####

  #### 🫧 2015 Q1 🫧 ####
  # File path
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
  
  # Merge & filter
  merged_LA = full_join(la12, la24, by = "ONS_Code") %>%
    filter(ONS_Code %in% valid_utlas) %>%
    mutate(Year = "2015/2016", Quarter = "Q1", Timepoint = 0, Vaccine_Schedule = 0) %>%
    select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m,
           Year, Quarter, Timepoint, Vaccine_Schedule)
  
  # Manual check for missing UTLA names
  missing_utlas_q1 = setdiff(valid_utlas, merged_LA$ONS_Code)
  if (length(missing_utlas_q1) > 0) {
    missing_names_q1 = utla_list %>% filter(UTLA_code %in% missing_utlas_q1)
    la12_extra = la12 %>% filter(UTLA_Name %in% missing_names_q1$UTLA_name)
    la24_extra = la24 %>% filter(UTLA_Name %in% missing_names_q1$UTLA_name)
  
    merged_extra = full_join(la12_extra, la24_extra, by = "UTLA_Name") %>%
      left_join(missing_names_q1, by = c("UTLA_Name" = "UTLA_name")) %>%
      rename(ONS_Code = UTLA_code) %>%
      select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m) %>%
      mutate(Year = "2015/2016", Quarter = "Q1", Timepoint = 0, Vaccine_Schedule = 0)
  
    merged_LA = bind_rows(merged_LA, merged_extra) %>% distinct()
  }
  
  # Save Q1
  fwrite(merged_LA, file.path(clean_dir, "COVER_2015_Q1_Cleaned.csv"))
  
  
  #### 🫧 2015 Q2 🫧 ####
  # File path
  file_path = file.path(main_dir, "2015_Q2.xlsx")
  
  # Load 12-month sheet
  la12 = read_excel(file_path, sheet = "12m_UT LA", range = "E1:M154") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV %`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           UTLA_Name = `Upper Tier LA Name`,
           Population_12m = `12m Denominator`,
           PCV_12m = `12m PCV %`)
  
  # Load 24-month sheet
  la24 = read_excel(file_path, sheet = "24m_UT LA", range = "E1:M154") %>%
    select(`ONS LA Code`, `Local Authority Name`, `24m Denominator`, `24m PCV Booster %`) %>%
    rename(ONS_Code = `ONS LA Code`,
           UTLA_Name = `Local Authority Name`,
           Population_24m = `24m Denominator`,
           PCV_24m = `24m PCV Booster %`)
  
  # Merge & filter
  merged_LA = full_join(la12, la24, by = "ONS_Code") %>%
    filter(ONS_Code %in% valid_utlas) %>%
    mutate(Year = "2015/2016", Quarter = "Q2", Timepoint = 1, Vaccine_Schedule = 0) %>%
    select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m,
           Year, Quarter, Timepoint, Vaccine_Schedule)
  
  # Manual check
  missing_utlas_q2 = setdiff(valid_utlas, merged_LA$ONS_Code)
  if (length(missing_utlas_q2) > 0) {
    missing_names_q2 = utla_list %>% filter(UTLA_code %in% missing_utlas_q2)
    la12_extra = la12 %>% filter(UTLA_Name %in% missing_names_q2$UTLA_name)
    la24_extra = la24 %>% filter(UTLA_Name %in% missing_names_q2$UTLA_name)
  
    merged_extra = full_join(la12_extra, la24_extra, by = "UTLA_Name") %>%
      left_join(missing_names_q2, by = c("UTLA_Name" = "UTLA_name")) %>%
      rename(ONS_Code = UTLA_code) %>%
      select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m) %>%
      mutate(Year = "2015/2016", Quarter = "Q2", Timepoint = 1, Vaccine_Schedule = 0)
  
    merged_LA = bind_rows(merged_LA, merged_extra) %>% distinct()
  }
  
  # Save Q2
  q1_data = fread(file.path(clean_dir, "COVER_2015_Q1_Cleaned.csv"))
  combined_2015 = bind_rows(q1_data, merged_LA)
  fwrite(combined_2015, file.path(clean_dir, "COVER_2015_Q1_Q2_Cleaned.csv"))
  
  
  #### 🫧 2015 Q3 🫧 ####
  # File path
  file_path = file.path(main_dir, "2015_Q3.xlsx")
  
  # Load 12-month sheet
  la12 = read_excel(file_path, sheet = "12m_UT LA", range = "E1:M154") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV %`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           UTLA_Name = `Upper Tier LA Name`,
           Population_12m = `12m Denominator`,
           PCV_12m = `12m PCV %`)
  
  # Load 24-month sheet
  la24 = read_excel(file_path, sheet = "24m_UT LA", range = "E1:M154") %>%
    select(`ONS LA Code`, `Local Authority Name`, `24m Denominator`, `24m PCV Booster %`) %>%
    rename(ONS_Code = `ONS LA Code`,
           UTLA_Name = `Local Authority Name`,
           Population_24m = `24m Denominator`,
           PCV_24m = `24m PCV Booster %`)
  
  # Merge and filter
  merged_LA = full_join(la12, la24, by = "ONS_Code") %>%
    filter(ONS_Code %in% valid_utlas) %>%
    mutate(Year = "2015/2016", Quarter = "Q3", Timepoint = 2, Vaccine_Schedule = 0) %>%
    select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m,
           Year, Quarter, Timepoint, Vaccine_Schedule)
  
  # Manual missing UTLA check
  missing_utlas_q3 = setdiff(valid_utlas, merged_LA$ONS_Code)
  if (length(missing_utlas_q3) > 0) {
    missing_names_q3 = utla_list %>% filter(UTLA_code %in% missing_utlas_q3)
    la12_extra = la12 %>% filter(UTLA_Name %in% missing_names_q3$UTLA_name)
    la24_extra = la24 %>% filter(UTLA_Name %in% missing_names_q3$UTLA_name)
    
    merged_extra = full_join(la12_extra, la24_extra, by = "UTLA_Name") %>%
      left_join(missing_names_q3, by = c("UTLA_Name" = "UTLA_name")) %>%
      rename(ONS_Code = UTLA_code) %>%
      select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m) %>%
      mutate(Year = "2015/2016", Quarter = "Q3", Timepoint = 2, Vaccine_Schedule = 0)
    
    merged_LA = bind_rows(merged_LA, merged_extra) %>% distinct()
  }
  
  # Append & save
  q2_data = fread(file.path(clean_dir, "COVER_2015_Q1_Q2_Cleaned.csv"))
  combined_2015 = bind_rows(q2_data, merged_LA)
  fwrite(combined_2015, file.path(clean_dir, "COVER_2015_Q1_Q2_Q3_Cleaned.csv"))
  
  
  #### 🫧 2015 Q4 🫧 ####
  # File path
  file_path = file.path(main_dir, "2015_Q4.xlsx")
  
  # Load 12-month sheet
  la12 = read_excel(file_path, sheet = "12m_UT LA", range = "E1:M154") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV %`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           UTLA_Name = `Upper Tier LA Name`,
           Population_12m = `12m Denominator`,
           PCV_12m = `12m PCV %`)
  
  # Load 24-month sheet
  la24 = read_excel(file_path, sheet = "24m_UT LA", range = "E1:M154") %>%
    select(`ONS LA Code`, `Local Authority Name`, `24m Denominator`, `24m PCV Booster %`) %>%
    rename(ONS_Code = `ONS LA Code`,
           UTLA_Name = `Local Authority Name`,
           Population_24m = `24m Denominator`,
           PCV_24m = `24m PCV Booster %`)
  
  # Merge and filter
  merged_LA = full_join(la12, la24, by = "ONS_Code") %>%
    filter(ONS_Code %in% valid_utlas) %>%
    mutate(Year = "2015/2016", Quarter = "Q4", Timepoint = 3, Vaccine_Schedule = 0) %>%
    select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m,
           Year, Quarter, Timepoint, Vaccine_Schedule)
  
  # Manual missing UTLA check
  missing_utlas_q4 = setdiff(valid_utlas, merged_LA$ONS_Code)
  if (length(missing_utlas_q4) > 0) {
    missing_names_q4 = utla_list %>% filter(UTLA_code %in% missing_utlas_q4)
    la12_extra = la12 %>% filter(UTLA_Name %in% missing_names_q4$UTLA_name)
    la24_extra = la24 %>% filter(UTLA_Name %in% missing_names_q4$UTLA_name)
    
    merged_extra = full_join(la12_extra, la24_extra, by = "UTLA_Name") %>%
      left_join(missing_names_q4, by = c("UTLA_Name" = "UTLA_name")) %>%
      rename(ONS_Code = UTLA_code) %>%
      select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m) %>%
      mutate(Year = "2015/2016", Quarter = "Q4", Timepoint = 3, Vaccine_Schedule = 0)
    
    merged_LA = bind_rows(merged_LA, merged_extra) %>% distinct()
  }
  
  # Append & save
  q3_data = fread(file.path(clean_dir, "COVER_2015_Q1_Q2_Q3_Cleaned.csv"))
  combined_2015 = bind_rows(q3_data, merged_LA)
  fwrite(combined_2015, file.path(clean_dir, "COVER_2015_Cleaned.csv"))
  
#### 🫧 Final check 🫧 ####
combined_2015 %>%
  group_by(Quarter) %>%
  summarise(Unique_UTLAs = n_distinct(ONS_Code))

missing_utlas = setdiff(valid_utlas, combined_2015$ONS_Code)
print(missing_utlas)

utla_list %>% filter(UTLA_code %in% missing_utlas)

#####################################
#####################################

#### ⋆˚࿔ 2 0 1 6 𝜗𝜚˚ ⋆ ####

  #### 🫧 2016 Q1 🫧 ####
  file_path = file.path(main_dir, "2016_Q1.xlsx")
  
  la12 = read_excel(file_path, sheet = "12m_UTLA_AT") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
    rename(
      ONS_Code = `ONS Upper Tier LA Code`,
      UTLA_Name = `Upper Tier LA Name`,
      Population_12m = `12m Denominator`,
      PCV_12m = `12m PCV2%`
    ) %>%
    mutate(
      Population_12m = as.numeric(Population_12m),
      PCV_12m = as.numeric(PCV_12m)
    ) %>%
    filter(!is.na(ONS_Code))
  
  la24 = read_excel(file_path, sheet = "24m_UTLA_AT") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
    rename(
      ONS_Code = `ONS Upper Tier LA Code`,
      UTLA_Name = `Upper Tier LA Name`,
      Population_24m = `24m Denominator`,
      PCV_24m = `24m PCV Booster%`
    ) %>%
    mutate(
      Population_24m = as.numeric(Population_24m),
      PCV_24m = as.numeric(PCV_24m)
    ) %>%
    filter(!is.na(ONS_Code))
  
  merged_LA = full_join(la12, la24, by = "ONS_Code") %>%
    filter(ONS_Code %in% valid_utlas) %>%
    mutate(Year = "2016/2017", Quarter = "Q1", Timepoint = 0, Vaccine_Schedule = 0) %>%
    select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m,
           Year, Quarter, Timepoint, Vaccine_Schedule)
  
  missing_utlas_q1 = setdiff(valid_utlas, merged_LA$ONS_Code)
  if (length(missing_utlas_q1) > 0) {
    missing_names_q1 = utla_list %>% filter(UTLA_code %in% missing_utlas_q1)
    la12_extra = la12 %>% filter(UTLA_Name %in% missing_names_q1$UTLA_name)
    la24_extra = la24 %>% filter(UTLA_Name %in% missing_names_q1$UTLA_name)
    
    merged_extra = full_join(la12_extra, la24_extra, by = "UTLA_Name") %>%
      left_join(missing_names_q1, by = c("UTLA_Name" = "UTLA_name")) %>%
      rename(ONS_Code = UTLA_code) %>%
      select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m) %>%
      mutate(Year = "2016/2017", Quarter = "Q1", Timepoint = 0, Vaccine_Schedule = 0)
    
    merged_LA = bind_rows(merged_LA, merged_extra) %>% distinct()
  }
  
  fwrite(merged_LA, file.path(clean_dir, "COVER_2016_Q1_Cleaned.csv"))
  
  
  #### 🫧 2016 Q2 🫧 ####
  file_path = file.path(main_dir, "2016_Q2.xlsx")
  
  la12 = read_excel(file_path, sheet = "12m_UTLA_AT") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           UTLA_Name = `Upper Tier LA Name`,
           Population_12m = `12m Denominator`,
           PCV_12m = `12m PCV2%`) %>%
    mutate(
      Population_12m = as.numeric(Population_12m),
      PCV_12m = as.numeric(PCV_12m)
    ) %>%
    filter(!is.na(ONS_Code))
  
  la24 = read_excel(file_path, sheet = "24m_UTLA_AT") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           UTLA_Name = `Upper Tier LA Name`,
           Population_24m = `24m Denominator`,
           PCV_24m = `24m PCV Booster%`) %>%
    mutate(
      Population_24m = as.numeric(Population_24m),
      PCV_24m = as.numeric(PCV_24m)
    ) %>%
    filter(!is.na(ONS_Code))
  
  merged_LA = full_join(la12, la24, by = "ONS_Code") %>%
    filter(ONS_Code %in% valid_utlas) %>%
    mutate(Year = "2016/2017", Quarter = "Q2", Timepoint = 1, Vaccine_Schedule = 0) %>%
    select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m,
           Year, Quarter, Timepoint, Vaccine_Schedule)
  
  missing_utlas_q2 = setdiff(valid_utlas, merged_LA$ONS_Code)
  if (length(missing_utlas_q2) > 0) {
    missing_names_q2 = utla_list %>% filter(UTLA_code %in% missing_utlas_q2)
    la12_extra = la12 %>% filter(UTLA_Name %in% missing_names_q2$UTLA_name)
    la24_extra = la24 %>% filter(UTLA_Name %in% missing_names_q2$UTLA_name)
    
    merged_extra = full_join(la12_extra, la24_extra, by = "UTLA_Name") %>%
      left_join(missing_names_q2, by = c("UTLA_Name" = "UTLA_name")) %>%
      rename(ONS_Code = UTLA_code) %>%
      select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m) %>%
      mutate(Year = "2016/2017", Quarter = "Q2", Timepoint = 1, Vaccine_Schedule = 0)
    
    merged_LA = bind_rows(merged_LA, merged_extra) %>% distinct()
  }
  
  q1_data = fread(file.path(clean_dir, "COVER_2016_Q1_Cleaned.csv"))
  combined_2016 = bind_rows(q1_data, merged_LA)
  fwrite(combined_2016, file.path(clean_dir, "COVER_2016_Q1_Q2_Cleaned.csv"))
  
  
  #### 🫧 2016 Q3 🫧 ####
  file_path = file.path(main_dir, "2016_Q3.xlsx")
  
  la12 = read_excel(file_path, sheet = "12m_UTLA_AT") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           UTLA_Name = `Upper Tier LA Name`,
           Population_12m = `12m Denominator`,
           PCV_12m = `12m PCV2%`) %>%
    mutate(
      Population_12m = as.numeric(Population_12m),
      PCV_12m = as.numeric(PCV_12m)
    ) %>%
    filter(!is.na(ONS_Code))
  
  la24 = read_excel(file_path, sheet = "24m_UTLA_AT") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           UTLA_Name = `Upper Tier LA Name`,
           Population_24m = `24m Denominator`,
           PCV_24m = `24m PCV Booster%`) %>%
    mutate(
      Population_24m = as.numeric(Population_24m),
      PCV_24m = as.numeric(PCV_24m)
    ) %>%
    filter(!is.na(ONS_Code))
  
  merged_LA = full_join(la12, la24, by = "ONS_Code") %>%
    filter(ONS_Code %in% valid_utlas) %>%
    mutate(Year = "2016/2017", Quarter = "Q3", Timepoint = 2, Vaccine_Schedule = 0) %>%
    select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m,
           Year, Quarter, Timepoint, Vaccine_Schedule)
  
  missing_utlas_q3 = setdiff(valid_utlas, merged_LA$ONS_Code)
  if (length(missing_utlas_q3) > 0) {
    missing_names_q3 = utla_list %>% filter(UTLA_code %in% missing_utlas_q3)
    la12_extra = la12 %>% filter(UTLA_Name %in% missing_names_q3$UTLA_name)
    la24_extra = la24 %>% filter(UTLA_Name %in% missing_names_q3$UTLA_name)
    
    merged_extra = full_join(la12_extra, la24_extra, by = "UTLA_Name") %>%
      left_join(missing_names_q3, by = c("UTLA_Name" = "UTLA_name")) %>%
      rename(ONS_Code = UTLA_code) %>%
      select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m) %>%
      mutate(Year = "2016/2017", Quarter = "Q3", Timepoint = 2, Vaccine_Schedule = 0)
    
    merged_LA = bind_rows(merged_LA, merged_extra) %>% distinct()
  }
  
  q2_data = fread(file.path(clean_dir, "COVER_2016_Q1_Q2_Cleaned.csv"))
  combined_2016 = bind_rows(q2_data, merged_LA)
  fwrite(combined_2016, file.path(clean_dir, "COVER_2016_Q1_Q2_Q3_Cleaned.csv"))
  
  
  #### 🫧 2016 Q4 🫧 ####
  file_path = file.path(main_dir, "2016_Q4.xlsx")
  
  la12 = read_excel(file_path, sheet = "12m_UTLA_AT") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           UTLA_Name = `Upper Tier LA Name`,
           Population_12m = `12m Denominator`,
           PCV_12m = `12m PCV2%`) %>%
    mutate(
      Population_12m = as.numeric(Population_12m),
      PCV_12m = as.numeric(PCV_12m)
    ) %>%
    filter(!is.na(ONS_Code))
  
  la24 = read_excel(file_path, sheet = "24m_UTLA_AT") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           UTLA_Name = `Upper Tier LA Name`,
           Population_24m = `24m Denominator`,
           PCV_24m = `24m PCV Booster%`) %>%
    mutate(
      Population_24m = as.numeric(Population_24m),
      PCV_24m = as.numeric(PCV_24m)
    ) %>%
    filter(!is.na(ONS_Code))
  
  merged_LA = full_join(la12, la24, by = "ONS_Code") %>%
    filter(ONS_Code %in% valid_utlas) %>%
    mutate(Year = "2016/2017", Quarter = "Q4", Timepoint = 3, Vaccine_Schedule = 0) %>%
    select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m,
           Year, Quarter, Timepoint, Vaccine_Schedule)
  
  missing_utlas_q4 = setdiff(valid_utlas, merged_LA$ONS_Code)
  if (length(missing_utlas_q4) > 0) {
    missing_names_q4 = utla_list %>% filter(UTLA_code %in% missing_utlas_q4)
    la12_extra = la12 %>% filter(UTLA_Name %in% missing_names_q4$UTLA_name)
    la24_extra = la24 %>% filter(UTLA_Name %in% missing_names_q4$UTLA_name)
    
    merged_extra = full_join(la12_extra, la24_extra, by = "UTLA_Name") %>%
      left_join(missing_names_q4, by = c("UTLA_Name" = "UTLA_name")) %>%
      rename(ONS_Code = UTLA_code) %>%
      select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m) %>%
      mutate(Year = "2016/2017", Quarter = "Q4", Timepoint = 3, Vaccine_Schedule = 0)
    
    merged_LA = bind_rows(merged_LA, merged_extra) %>% distinct()
  }
  
  q3_data = fread(file.path(clean_dir, "COVER_2016_Q1_Q2_Q3_Cleaned.csv"))
  combined_2016 = bind_rows(q3_data, merged_LA)
  fwrite(combined_2016, file.path(clean_dir, "COVER_2016_Cleaned.csv"))
  
  
#### 🫧 Final check 🫧 ####
combined_2016 %>%
  group_by(Quarter) %>%
  summarise(Unique_UTLAs = n_distinct(ONS_Code))

missing_utlas = setdiff(valid_utlas, combined_2016$ONS_Code)
print(missing_utlas)

utla_list %>% filter(UTLA_code %in% missing_utlas)

#####################################
#####################################

#### ⋆˚࿔ 2 0 1 7 𝜗𝜚˚ ⋆ ####

  #### 🫧 2017 Q1 🫧 ####
  file_path = file.path(main_dir, "2017_Q1.xlsx")
  
  # Load 12-month sheet
  la12 = read_excel(file_path, sheet = "12m_UTLA_AT") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           UTLA_Name = `Upper Tier LA Name`,
           Population_12m = `12m Denominator`,
           PCV_12m = `12m PCV2%`) %>%
    mutate(Population_12m = as.numeric(Population_12m),
           PCV_12m = as.numeric(PCV_12m)) %>%
    filter(!is.na(ONS_Code))
  
  # Load 24-month sheet
  la24 = read_excel(file_path, sheet = "24m_UTLA_AT") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           UTLA_Name = `Upper Tier LA Name`,
           Population_24m = `24m Denominator`,
           PCV_24m = `24m PCV Booster%`) %>%
    mutate(Population_24m = as.numeric(Population_24m),
           PCV_24m = as.numeric(PCV_24m)) %>%
    filter(!is.na(ONS_Code))
  
  # Merge & filter
  merged_LA = full_join(la12, la24, by = "ONS_Code") %>%
    filter(ONS_Code %in% valid_utlas) %>%
    mutate(Year = "2017/2018", Quarter = "Q1", Timepoint = 0, Vaccine_Schedule = 0) %>%
    select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m,
           Year, Quarter, Timepoint, Vaccine_Schedule)
  
  # Manual UTLA check
  missing_utlas_q1 = setdiff(valid_utlas, merged_LA$ONS_Code)
  if (length(missing_utlas_q1) > 0) {
    missing_names_q1 = utla_list %>% filter(UTLA_code %in% missing_utlas_q1)
    la12_extra = la12 %>% filter(UTLA_Name %in% missing_names_q1$UTLA_name)
    la24_extra = la24 %>% filter(UTLA_Name %in% missing_names_q1$UTLA_name)
    
    merged_extra = full_join(la12_extra, la24_extra, by = "UTLA_Name") %>%
      left_join(missing_names_q1, by = c("UTLA_Name" = "UTLA_name")) %>%
      rename(ONS_Code = UTLA_code) %>%
      select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m) %>%
      mutate(Year = "2017/2018", Quarter = "Q1", Timepoint = 0, Vaccine_Schedule = 0)
    
    merged_LA = bind_rows(merged_LA, merged_extra) %>% distinct()
  }
  
  fwrite(merged_LA, file.path(clean_dir, "COVER_2017_Q1_Cleaned.csv"))
  
  
  #### 🫧 2017 Q2 🫧 ####
  file_path = file.path(main_dir, "2017_Q2.xlsx")
  
  la12 = read_excel(file_path, sheet = "12m_UTLA_AT") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           UTLA_Name = `Upper Tier LA Name`,
           Population_12m = `12m Denominator`,
           PCV_12m = `12m PCV2%`) %>%
    mutate(Population_12m = as.numeric(Population_12m),
           PCV_12m = as.numeric(PCV_12m)) %>%
    filter(!is.na(ONS_Code))
  
  la24 = read_excel(file_path, sheet = "24m_UTLA_AT") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           UTLA_Name = `Upper Tier LA Name`,
           Population_24m = `24m Denominator`,
           PCV_24m = `24m PCV Booster%`) %>%
    mutate(Population_24m = as.numeric(Population_24m),
           PCV_24m = as.numeric(PCV_24m)) %>%
    filter(!is.na(ONS_Code))
  
  merged_LA = full_join(la12, la24, by = "ONS_Code") %>%
    filter(ONS_Code %in% valid_utlas) %>%
    mutate(Year = "2017/2018", Quarter = "Q2", Timepoint = 1, Vaccine_Schedule = 0) %>%
    select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m,
           Year, Quarter, Timepoint, Vaccine_Schedule)
  
  # Manual UTLA check
  missing_utlas_q2 = setdiff(valid_utlas, merged_LA$ONS_Code)
  if (length(missing_utlas_q2) > 0) {
    missing_names_q2 = utla_list %>% filter(UTLA_code %in% missing_utlas_q2)
    la12_extra = la12 %>% filter(UTLA_Name %in% missing_names_q2$UTLA_name)
    la24_extra = la24 %>% filter(UTLA_Name %in% missing_names_q2$UTLA_name)
    
    merged_extra = full_join(la12_extra, la24_extra, by = "UTLA_Name") %>%
      left_join(missing_names_q2, by = c("UTLA_Name" = "UTLA_name")) %>%
      rename(ONS_Code = UTLA_code) %>%
      select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m) %>%
      mutate(Year = "2017/2018", Quarter = "Q2", Timepoint = 1, Vaccine_Schedule = 0)
    
    merged_LA = bind_rows(merged_LA, merged_extra) %>% distinct()
  }
  
  q1_data = fread(file.path(clean_dir, "COVER_2017_Q1_Cleaned.csv"))
  combined_2017 = bind_rows(q1_data, merged_LA)
  fwrite(combined_2017, file.path(clean_dir, "COVER_2017_Q1_Q2_Cleaned.csv"))
  
  
  #### 🫧 2017 Q3 🫧 ####
  file_path = file.path(main_dir, "2017_Q3.xlsx")
  
  la12 = read_excel(file_path, sheet = "12m_UTLA_AT") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           UTLA_Name = `Upper Tier LA Name`,
           Population_12m = `12m Denominator`,
           PCV_12m = `12m PCV2%`) %>%
    mutate(Population_12m = as.numeric(Population_12m),
           PCV_12m = as.numeric(PCV_12m)) %>%
    filter(!is.na(ONS_Code))
  
  la24 = read_excel(file_path, sheet = "24m_UTLA_AT") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           UTLA_Name = `Upper Tier LA Name`,
           Population_24m = `24m Denominator`,
           PCV_24m = `24m PCV Booster%`) %>%
    mutate(Population_24m = as.numeric(Population_24m),
           PCV_24m = as.numeric(PCV_24m)) %>%
    filter(!is.na(ONS_Code))
  
  merged_LA = full_join(la12, la24, by = "ONS_Code") %>%
    filter(ONS_Code %in% valid_utlas) %>%
    mutate(Year = "2017/2018", Quarter = "Q3", Timepoint = 2, Vaccine_Schedule = 0) %>%
    select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m,
           Year, Quarter, Timepoint, Vaccine_Schedule)
  
  # Manual UTLA check
  missing_utlas_q3 = setdiff(valid_utlas, merged_LA$ONS_Code)
  if (length(missing_utlas_q3) > 0) {
    missing_names_q3 = utla_list %>% filter(UTLA_code %in% missing_utlas_q3)
    la12_extra = la12 %>% filter(UTLA_Name %in% missing_names_q3$UTLA_name)
    la24_extra = la24 %>% filter(UTLA_Name %in% missing_names_q3$UTLA_name)
    
    merged_extra = full_join(la12_extra, la24_extra, by = "UTLA_Name") %>%
      left_join(missing_names_q3, by = c("UTLA_Name" = "UTLA_name")) %>%
      rename(ONS_Code = UTLA_code) %>%
      select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m) %>%
      mutate(Year = "2017/2018", Quarter = "Q3", Timepoint = 2, Vaccine_Schedule = 0)
    
    merged_LA = bind_rows(merged_LA, merged_extra) %>% distinct()
  }
  
  q2_data = fread(file.path(clean_dir, "COVER_2017_Q1_Q2_Cleaned.csv"))
  combined_2017 = bind_rows(q2_data, merged_LA)
  fwrite(combined_2017, file.path(clean_dir, "COVER_2017_Q1_Q2_Q3_Cleaned.csv"))
  
  
  #### 🫧 2017 Q4 🫧 ####
  file_path = file.path(main_dir, "2017_Q4.xlsx")
  
  la12 = read_excel(file_path, sheet = "12m_UTLA_AT") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           UTLA_Name = `Upper Tier LA Name`,
           Population_12m = `12m Denominator`,
           PCV_12m = `12m PCV2%`) %>%
    mutate(Population_12m = as.numeric(Population_12m),
           PCV_12m = as.numeric(PCV_12m)) %>%
    filter(!is.na(ONS_Code))
  
  la24 = read_excel(file_path, sheet = "24m_UTLA_AT") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           UTLA_Name = `Upper Tier LA Name`,
           Population_24m = `24m Denominator`,
           PCV_24m = `24m PCV Booster%`) %>%
    mutate(Population_24m = as.numeric(Population_24m),
           PCV_24m = as.numeric(PCV_24m)) %>%
    filter(!is.na(ONS_Code))
  
  merged_LA = full_join(la12, la24, by = "ONS_Code") %>%
    filter(ONS_Code %in% valid_utlas) %>%
    mutate(Year = "2017/2018", Quarter = "Q4", Timepoint = 3, Vaccine_Schedule = 0) %>%
    select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m,
           Year, Quarter, Timepoint, Vaccine_Schedule)
  
  # Manual UTLA check
  missing_utlas_q4 = setdiff(valid_utlas, merged_LA$ONS_Code)
  if (length(missing_utlas_q4) > 0) {
    missing_names_q4 = utla_list %>% filter(UTLA_code %in% missing_utlas_q4)
    la12_extra = la12 %>% filter(UTLA_Name %in% missing_names_q4$UTLA_name)
    la24_extra = la24 %>% filter(UTLA_Name %in% missing_names_q4$UTLA_name)
    
    merged_extra = full_join(la12_extra, la24_extra, by = "UTLA_Name") %>%
      left_join(missing_names_q4, by = c("UTLA_Name" = "UTLA_name")) %>%
      rename(ONS_Code = UTLA_code) %>%
      select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m) %>%
      mutate(Year = "2017/2018", Quarter = "Q4", Timepoint = 3, Vaccine_Schedule = 0)
    
    merged_LA = bind_rows(merged_LA, merged_extra) %>% distinct()
  }
  
  q3_data = fread(file.path(clean_dir, "COVER_2017_Q1_Q2_Q3_Cleaned.csv"))
  combined_2017 = bind_rows(q3_data, merged_LA)
  fwrite(combined_2017, file.path(clean_dir, "COVER_2017_Cleaned.csv"))
  
  
#### 🫧 Final check 🫧 ####
combined_2017 %>%
  group_by(Quarter) %>%
  summarise(Unique_UTLAs = n_distinct(ONS_Code))

missing_utlas = setdiff(valid_utlas, combined_2017$ONS_Code)
print(missing_utlas)

utla_list %>% filter(UTLA_code %in% missing_utlas)

#####################################
#####################################

#### ⋆˚࿔ 2 0 1 8 𝜗𝜚˚ ⋆ ####

  #### 🫧 2018 Q1 🫧 ####
  file_path = file.path(main_dir, "2018_Q1.xlsx")
  
  # Load 12-month sheet
  la12 = read_excel(file_path, sheet = "12m_UTLA_GOR") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           UTLA_Name = `Upper Tier LA Name`,
           Population_12m = `12m Denominator`,
           PCV_12m = `12m PCV2%`) %>%
    mutate(Population_12m = as.numeric(Population_12m),
           PCV_12m = as.numeric(PCV_12m)) %>%
    filter(!is.na(ONS_Code))
  
  # Load 24-month sheet
  la24 = read_excel(file_path, sheet = "24m_UTLA_GOR") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           UTLA_Name = `Upper Tier LA Name`,
           Population_24m = `24m Denominator`,
           PCV_24m = `24m PCV Booster%`) %>%
    mutate(Population_24m = as.numeric(Population_24m),
           PCV_24m = as.numeric(PCV_24m)) %>%
    filter(!is.na(ONS_Code))
  
  # Merge & filter
  merged_LA = full_join(la12, la24, by = "ONS_Code") %>%
    filter(ONS_Code %in% valid_utlas) %>%
    mutate(Year = "2018/2019", Quarter = "Q1", Timepoint = 0, Vaccine_Schedule = 0) %>%
    select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m,
           Year, Quarter, Timepoint, Vaccine_Schedule)
  
  # Manual UTLA check
  missing_utlas_q1 = setdiff(valid_utlas, merged_LA$ONS_Code)
  if (length(missing_utlas_q1) > 0) {
    missing_names_q1 = utla_list %>% filter(UTLA_code %in% missing_utlas_q1)
    la12_extra = la12 %>% filter(UTLA_Name %in% missing_names_q1$UTLA_name)
    la24_extra = la24 %>% filter(UTLA_Name %in% missing_names_q1$UTLA_name)
    
    merged_extra = full_join(la12_extra, la24_extra, by = "UTLA_Name") %>%
      left_join(missing_names_q1, by = c("UTLA_Name" = "UTLA_name")) %>%
      rename(ONS_Code = UTLA_code) %>%
      select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m) %>%
      mutate(Year = "2018/2019", Quarter = "Q1", Timepoint = 0, Vaccine_Schedule = 0)
    
    merged_LA = bind_rows(merged_LA, merged_extra) %>% distinct()
  }
  
  fwrite(merged_LA, file.path(clean_dir, "COVER_2018_Q1_Cleaned.csv"))
  
  
  #### 🫧 2018 Q2 🫧 ####
  file_path = file.path(main_dir, "2018_Q2.xlsx")
  
  la12 = read_excel(file_path, sheet = "12m_UTLA_GOR") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           UTLA_Name = `Upper Tier LA Name`,
           Population_12m = `12m Denominator`,
           PCV_12m = `12m PCV2%`) %>%
    mutate(Population_12m = as.numeric(Population_12m),
           PCV_12m = as.numeric(PCV_12m)) %>%
    filter(!is.na(ONS_Code))
  
  la24 = read_excel(file_path, sheet = "24m_UTLA_GOR") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           UTLA_Name = `Upper Tier LA Name`,
           Population_24m = `24m Denominator`,
           PCV_24m = `24m PCV Booster%`) %>%
    mutate(Population_24m = as.numeric(Population_24m),
           PCV_24m = as.numeric(PCV_24m)) %>%
    filter(!is.na(ONS_Code))
  
  merged_LA = full_join(la12, la24, by = "ONS_Code") %>%
    filter(ONS_Code %in% valid_utlas) %>%
    mutate(Year = "2018/2019", Quarter = "Q2", Timepoint = 1, Vaccine_Schedule = 0) %>%
    select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m,
           Year, Quarter, Timepoint, Vaccine_Schedule)
  
  # Manual UTLA check
  missing_utlas_q2 = setdiff(valid_utlas, merged_LA$ONS_Code)
  if (length(missing_utlas_q2) > 0) {
    missing_names_q2 = utla_list %>% filter(UTLA_code %in% missing_utlas_q2)
    la12_extra = la12 %>% filter(UTLA_Name %in% missing_names_q2$UTLA_name)
    la24_extra = la24 %>% filter(UTLA_Name %in% missing_names_q2$UTLA_name)
    
    merged_extra = full_join(la12_extra, la24_extra, by = "UTLA_Name") %>%
      left_join(missing_names_q2, by = c("UTLA_Name" = "UTLA_name")) %>%
      rename(ONS_Code = UTLA_code) %>%
      select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m) %>%
      mutate(Year = "2018/2019", Quarter = "Q2", Timepoint = 1, Vaccine_Schedule = 0)
    
    merged_LA = bind_rows(merged_LA, merged_extra) %>% distinct()
  }
  
  q1_data = fread(file.path(clean_dir, "COVER_2018_Q1_Cleaned.csv"))
  combined_2018 = bind_rows(q1_data, merged_LA)
  fwrite(combined_2018, file.path(clean_dir, "COVER_2018_Q1_Q2_Cleaned.csv"))
  
  
  #### 🫧 2018 Q3 🫧 ####
  file_path = file.path(main_dir, "2018_Q3.xlsx")
  
  la12 = read_excel(file_path, sheet = "12m_UTLA_GOR") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           UTLA_Name = `Upper Tier LA Name`,
           Population_12m = `12m Denominator`,
           PCV_12m = `12m PCV2%`) %>%
    mutate(Population_12m = as.numeric(Population_12m),
           PCV_12m = as.numeric(PCV_12m)) %>%
    filter(!is.na(ONS_Code))
  
  la24 = read_excel(file_path, sheet = "24m_UTLA_GOR") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           UTLA_Name = `Upper Tier LA Name`,
           Population_24m = `24m Denominator`,
           PCV_24m = `24m PCV Booster%`) %>%
    mutate(Population_24m = as.numeric(Population_24m),
           PCV_24m = as.numeric(PCV_24m)) %>%
    filter(!is.na(ONS_Code))
  
  merged_LA = full_join(la12, la24, by = "ONS_Code") %>%
    filter(ONS_Code %in% valid_utlas) %>%
    mutate(Year = "2018/2019", Quarter = "Q3", Timepoint = 2, Vaccine_Schedule = 0) %>%
    select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m,
           Year, Quarter, Timepoint, Vaccine_Schedule)
  
  # Manual UTLA check
  missing_utlas_q3 = setdiff(valid_utlas, merged_LA$ONS_Code)
  if (length(missing_utlas_q3) > 0) {
    missing_names_q3 = utla_list %>% filter(UTLA_code %in% missing_utlas_q3)
    la12_extra = la12 %>% filter(UTLA_Name %in% missing_names_q3$UTLA_name)
    la24_extra = la24 %>% filter(UTLA_Name %in% missing_names_q3$UTLA_name)
    
    merged_extra = full_join(la12_extra, la24_extra, by = "UTLA_Name") %>%
      left_join(missing_names_q3, by = c("UTLA_Name" = "UTLA_name")) %>%
      rename(ONS_Code = UTLA_code) %>%
      select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m) %>%
      mutate(Year = "2018/2019", Quarter = "Q3", Timepoint = 2, Vaccine_Schedule = 0)
    
    merged_LA = bind_rows(merged_LA, merged_extra) %>% distinct()
  }
  
  q2_data = fread(file.path(clean_dir, "COVER_2018_Q1_Q2_Cleaned.csv"))
  combined_2018 = bind_rows(q2_data, merged_LA)
  fwrite(combined_2018, file.path(clean_dir, "COVER_2018_Q1_Q2_Q3_Cleaned.csv"))
  
  
  #### 🫧 2018 Q4 🫧 ####
  file_path = file.path(main_dir, "2018_Q4.xlsx")
  
  la12 = read_excel(file_path, sheet = "12m_UTLA_GOR") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           UTLA_Name = `Upper Tier LA Name`,
           Population_12m = `12m Denominator`,
           PCV_12m = `12m PCV2%`) %>%
    mutate(Population_12m = as.numeric(Population_12m),
           PCV_12m = as.numeric(PCV_12m)) %>%
    filter(!is.na(ONS_Code))
  
  la24 = read_excel(file_path, sheet = "24m_UTLA_GOR") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           UTLA_Name = `Upper Tier LA Name`,
           Population_24m = `24m Denominator`,
           PCV_24m = `24m PCV Booster%`) %>%
    mutate(Population_24m = as.numeric(Population_24m),
           PCV_24m = as.numeric(PCV_24m)) %>%
    filter(!is.na(ONS_Code))
  
  merged_LA = full_join(la12, la24, by = "ONS_Code") %>%
    filter(ONS_Code %in% valid_utlas) %>%
    mutate(Year = "2018/2019", Quarter = "Q4", Timepoint = 3, Vaccine_Schedule = 0) %>%
    select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m,
           Year, Quarter, Timepoint, Vaccine_Schedule)
  
  # Manual UTLA check
  missing_utlas_q4 = setdiff(valid_utlas, merged_LA$ONS_Code)
  if (length(missing_utlas_q4) > 0) {
    missing_names_q4 = utla_list %>% filter(UTLA_code %in% missing_utlas_q4)
    la12_extra = la12 %>% filter(UTLA_Name %in% missing_names_q4$UTLA_name)
    la24_extra = la24 %>% filter(UTLA_Name %in% missing_names_q4$UTLA_name)
    
    merged_extra = full_join(la12_extra, la24_extra, by = "UTLA_Name") %>%
      left_join(missing_names_q4, by = c("UTLA_Name" = "UTLA_name")) %>%
      rename(ONS_Code = UTLA_code) %>%
      select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m) %>%
      mutate(Year = "2018/2019", Quarter = "Q4", Timepoint = 3, Vaccine_Schedule = 0)
    
    merged_LA = bind_rows(merged_LA, merged_extra) %>% distinct()
  }
  
  q3_data = fread(file.path(clean_dir, "COVER_2018_Q1_Q2_Q3_Cleaned.csv"))
  combined_2018 = bind_rows(q3_data, merged_LA)
  fwrite(combined_2018, file.path(clean_dir, "COVER_2018_Cleaned.csv"))
  
  
#### 🫧 Final check 🫧 ####
combined_2018 %>%
  group_by(Quarter) %>%
  summarise(Unique_UTLAs = n_distinct(ONS_Code))

missing_utlas = setdiff(valid_utlas, combined_2018$ONS_Code)
print(missing_utlas)

utla_list %>% filter(UTLA_code %in% missing_utlas)

#####################################
#####################################

#### ⋆˚࿔ 2 0 1 9 𝜗𝜚˚ ⋆ ####

  #### 🫧 2019 Q1 🫧 ####
  file_path = file.path(main_dir, "2019_Q1.ods")
  
  la12 = read_ods(file_path, sheet = "12m_UTLA_GOR") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           UTLA_Name = `Upper Tier LA Name`,
           Population_12m = `12m Denominator`,
           PCV_12m = `12m PCV2%`) %>%
    mutate(Population_12m = as.numeric(Population_12m),
           PCV_12m = as.numeric(PCV_12m)) %>%
    filter(!is.na(ONS_Code))
  
  la24 = read_ods(file_path, sheet = "24m_UTLA_GOR") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           UTLA_Name = `Upper Tier LA Name`,
           Population_24m = `24m Denominator`,
           PCV_24m = `24m PCV Booster%`) %>%
    mutate(Population_24m = as.numeric(Population_24m),
           PCV_24m = as.numeric(PCV_24m)) %>%
    filter(!is.na(ONS_Code))
  
  merged_LA = full_join(la12, la24, by = "ONS_Code") %>%
    filter(ONS_Code %in% valid_utlas) %>%
    mutate(Year = "2019/2020", Quarter = "Q1", Timepoint = 0, Vaccine_Schedule = 0) %>%
    select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m,
           Year, Quarter, Timepoint, Vaccine_Schedule)
  
  # Manual UTLA check
  missing_utlas_q1 = setdiff(valid_utlas, merged_LA$ONS_Code)
  if (length(missing_utlas_q1) > 0) {
    missing_names_q1 = utla_list %>% filter(UTLA_code %in% missing_utlas_q1)
    la12_extra = la12 %>% filter(UTLA_Name %in% missing_names_q1$UTLA_name)
    la24_extra = la24 %>% filter(UTLA_Name %in% missing_names_q1$UTLA_name)
    
    merged_extra = full_join(la12_extra, la24_extra, by = "UTLA_Name") %>%
      left_join(missing_names_q1, by = c("UTLA_Name" = "UTLA_name")) %>%
      rename(ONS_Code = UTLA_code) %>%
      select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m) %>%
      mutate(Year = "2019/2020", Quarter = "Q1", Timepoint = 0, Vaccine_Schedule = 0)
    
    merged_LA = bind_rows(merged_LA, merged_extra) %>% distinct()
  }
  
  fwrite(merged_LA, file.path(clean_dir, "COVER_2019_Q1_Cleaned.csv"))
  
  
  #### 🫧 2019 Q2 🫧 ####
  file_path = file.path(main_dir, "2019_Q2.ods")
  
  la12 = read_ods(file_path, sheet = "12m_UTLA_GOR") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           UTLA_Name = `Upper Tier LA Name`,
           Population_12m = `12m Denominator`,
           PCV_12m = `12m PCV2%`) %>%
    mutate(Population_12m = as.numeric(Population_12m),
           PCV_12m = as.numeric(PCV_12m)) %>%
    filter(!is.na(ONS_Code))
  
  la24 = read_ods(file_path, sheet = "24m_UTLA_GOR") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           UTLA_Name = `Upper Tier LA Name`,
           Population_24m = `24m Denominator`,
           PCV_24m = `24m PCV Booster%`) %>%
    mutate(Population_24m = as.numeric(Population_24m),
           PCV_24m = as.numeric(PCV_24m)) %>%
    filter(!is.na(ONS_Code))
  
  merged_LA = full_join(la12, la24, by = "ONS_Code") %>%
    filter(ONS_Code %in% valid_utlas) %>%
    mutate(Year = "2019/2020", Quarter = "Q2", Timepoint = 1, Vaccine_Schedule = 0) %>%
    select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m,
           Year, Quarter, Timepoint, Vaccine_Schedule)
  
  # Manual UTLA check
  missing_utlas_q2 = setdiff(valid_utlas, merged_LA$ONS_Code)
  if (length(missing_utlas_q2) > 0) {
    missing_names_q2 = utla_list %>% filter(UTLA_code %in% missing_utlas_q2)
    la12_extra = la12 %>% filter(UTLA_Name %in% missing_names_q2$UTLA_name)
    la24_extra = la24 %>% filter(UTLA_Name %in% missing_names_q2$UTLA_name)
    
    merged_extra = full_join(la12_extra, la24_extra, by = "UTLA_Name") %>%
      left_join(missing_names_q2, by = c("UTLA_Name" = "UTLA_name")) %>%
      rename(ONS_Code = UTLA_code) %>%
      select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m) %>%
      mutate(Year = "2019/2020", Quarter = "Q2", Timepoint = 1, Vaccine_Schedule = 0)
    
    merged_LA = bind_rows(merged_LA, merged_extra) %>% distinct()
  }
  
  q1_data = fread(file.path(clean_dir, "COVER_2019_Q1_Cleaned.csv"))
  combined_2019 = bind_rows(q1_data, merged_LA)
  fwrite(combined_2019, file.path(clean_dir, "COVER_2019_Q1_Q2_Cleaned.csv"))
  
  
  #### 🫧 2019 Q3 🫧 ####
  file_path = file.path(main_dir, "2019_Q3.ods")
  
  la12 = read_ods(file_path, sheet = "12m_UTLA_GOR") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           UTLA_Name = `Upper Tier LA Name`,
           Population_12m = `12m Denominator`,
           PCV_12m = `12m PCV2%`) %>%
    mutate(Population_12m = as.numeric(Population_12m),
           PCV_12m = as.numeric(PCV_12m)) %>%
    filter(!is.na(ONS_Code))
  
  la24 = read_ods(file_path, sheet = "24m_UTLA_GOR") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           UTLA_Name = `Upper Tier LA Name`,
           Population_24m = `24m Denominator`,
           PCV_24m = `24m PCV Booster%`) %>%
    mutate(Population_24m = as.numeric(Population_24m),
           PCV_24m = as.numeric(PCV_24m)) %>%
    filter(!is.na(ONS_Code))
  
  merged_LA = full_join(la12, la24, by = "ONS_Code") %>%
    filter(ONS_Code %in% valid_utlas) %>%
    mutate(Year = "2019/2020", Quarter = "Q3", Timepoint = 2, Vaccine_Schedule = 0) %>%
    select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m,
           Year, Quarter, Timepoint, Vaccine_Schedule)
  
  # Manual UTLA check
  missing_utlas_q3 = setdiff(valid_utlas, merged_LA$ONS_Code)
  if (length(missing_utlas_q3) > 0) {
    missing_names_q3 = utla_list %>% filter(UTLA_code %in% missing_utlas_q3)
    la12_extra = la12 %>% filter(UTLA_Name %in% missing_names_q3$UTLA_name)
    la24_extra = la24 %>% filter(UTLA_Name %in% missing_names_q3$UTLA_name)
    
    merged_extra = full_join(la12_extra, la24_extra, by = "UTLA_Name") %>%
      left_join(missing_names_q3, by = c("UTLA_Name" = "UTLA_name")) %>%
      rename(ONS_Code = UTLA_code) %>%
      select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m) %>%
      mutate(Year = "2019/2020", Quarter = "Q3", Timepoint = 2, Vaccine_Schedule = 0)
    
    merged_LA = bind_rows(merged_LA, merged_extra) %>% distinct()
  }
  
  q2_data = fread(file.path(clean_dir, "COVER_2019_Q1_Q2_Cleaned.csv"))
  combined_2019 = bind_rows(q2_data, merged_LA)
  fwrite(combined_2019, file.path(clean_dir, "COVER_2019_Q1_Q2_Q3_Cleaned.csv"))
  
  
  #### 🫧 2019 Q4 🫧 ####
  file_path = file.path(main_dir, "2019_Q4.ods")
  
  la12 = read_ods(file_path, sheet = "12m_UTLA_GOR") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV2%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           UTLA_Name = `Upper Tier LA Name`,
           Population_12m = `12m Denominator`,
           PCV_12m = `12m PCV2%`) %>%
    mutate(Population_12m = as.numeric(Population_12m),
           PCV_12m = as.numeric(PCV_12m)) %>%
    filter(!is.na(ONS_Code))
  
  la24 = read_ods(file_path, sheet = "24m_UTLA_GOR") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           UTLA_Name = `Upper Tier LA Name`,
           Population_24m = `24m Denominator`,
           PCV_24m = `24m PCV Booster%`) %>%
    mutate(Population_24m = as.numeric(Population_24m),
           PCV_24m = as.numeric(PCV_24m)) %>%
    filter(!is.na(ONS_Code))
  
  merged_LA = full_join(la12, la24, by = "ONS_Code") %>%
    filter(ONS_Code %in% valid_utlas) %>%
    mutate(Year = "2019/2020", Quarter = "Q4", Timepoint = 3, Vaccine_Schedule = 0) %>%
    select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m,
           Year, Quarter, Timepoint, Vaccine_Schedule)
  
  # Manual UTLA check
  missing_utlas_q4 = setdiff(valid_utlas, merged_LA$ONS_Code)
  if (length(missing_utlas_q4) > 0) {
    missing_names_q4 = utla_list %>% filter(UTLA_code %in% missing_utlas_q4)
    la12_extra = la12 %>% filter(UTLA_Name %in% missing_names_q4$UTLA_name)
    la24_extra = la24 %>% filter(UTLA_Name %in% missing_names_q4$UTLA_name)
    
    merged_extra = full_join(la12_extra, la24_extra, by = "UTLA_Name") %>%
      left_join(missing_names_q4, by = c("UTLA_Name" = "UTLA_name")) %>%
      rename(ONS_Code = UTLA_code) %>%
      select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m) %>%
      mutate(Year = "2019/2020", Quarter = "Q4", Timepoint = 3, Vaccine_Schedule = 0)
    
    merged_LA = bind_rows(merged_LA, merged_extra) %>% distinct()
  }
  
  q3_data = fread(file.path(clean_dir, "COVER_2019_Q1_Q2_Q3_Cleaned.csv"))
  combined_2019 = bind_rows(q3_data, merged_LA)
  fwrite(combined_2019, file.path(clean_dir, "COVER_2019_Cleaned.csv"))
  
  
#### 🫧 Final check 🫧 ####
combined_2019 %>%
  group_by(Quarter) %>%
  summarise(Unique_UTLAs = n_distinct(ONS_Code))

missing_utlas = setdiff(valid_utlas, combined_2019$ONS_Code)
print(missing_utlas)

utla_list %>% filter(UTLA_code %in% missing_utlas)


#####################################
#### *ੈ✩‧₊˚༺☆༻*ੈ✩‧₊˚ V  A  C  C  I  N  E    C  H  A  N  G  E *ੈ✩‧₊˚༺☆༻*ੈ✩‧₊˚  ####
#####################################

#### ⋆˚࿔ 2 0 2 0 𝜗𝜚˚ ⋆ ####
# Function to map old ONS codes to new ones before filtering
map_old_to_new <- function(df) {
  df %>%
    mutate(ONS_Code = recode(ONS_Code,
                             "E10000009" = "E06000059",   # Dorset old -> new
                             "E06000028" = "E06000058"    # Bournemouth, Christchurch & Poole old -> new
    ))
}

  #### 🫧 2020 Q1 🫧 ####
  file_path = file.path(main_dir, "2020 Q1.ods")
  
  la12 = read_ods(file_path, sheet = "12m_UTLA_GOR") %>%
    select(`ONS Upper Tier LA Code`, `12m Denominator`, `12m PCV2%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           Population_12m = `12m Denominator`,
           PCV_12m = `12m PCV2%`) %>%
    map_old_to_new() %>%
    mutate(Population_12m = as.numeric(Population_12m),
           PCV_12m = as.numeric(PCV_12m)) %>%
    filter(!is.na(ONS_Code))
  
  la24 = read_ods(file_path, sheet = "24m_UTLA_GOR") %>%
    select(`ONS Upper Tier LA Code`, `24m Denominator`, `24m PCV Booster%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           Population_24m = `24m Denominator`,
           PCV_24m = `24m PCV Booster%`) %>%
    map_old_to_new() %>%
    mutate(Population_24m = as.numeric(Population_24m),
           PCV_24m = as.numeric(PCV_24m)) %>%
    filter(!is.na(ONS_Code))
  
  merged_LA = full_join(la12, la24, by = "ONS_Code") %>%
    filter(ONS_Code %in% valid_utlas) %>%
    mutate(Year = "2020/2021", Quarter = "Q1", Timepoint = 0, Vaccine_Schedule = 1)
  
  missing_utlas_q1 = setdiff(valid_utlas, merged_LA$ONS_Code)
  if (length(missing_utlas_q1) > 0) {
    la12_extra = la12 %>% filter(ONS_Code %in% missing_utlas_q1)
    la24_extra = la24 %>% filter(ONS_Code %in% missing_utlas_q1)
    merged_extra = full_join(la12_extra, la24_extra, by = "ONS_Code") %>%
      mutate(Year = "2020/2021", Quarter = "Q1", Timepoint = 0, Vaccine_Schedule = 1)
    merged_LA = bind_rows(merged_LA, merged_extra) %>% distinct()
  }
  fwrite(merged_LA, file.path(clean_dir, "COVER_2020_Q1_Cleaned.csv"))
  
  
  #### 🫧 2020 Q2 🫧 ####
  file_path = file.path(main_dir, "2020 Q2.ods")
  
  la12 = read_ods(file_path, sheet = "12m_UTLA_GOR") %>%
    select(`ONS Upper Tier LA Code`, `12m Denominator`, `12m PCV2%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           Population_12m = `12m Denominator`,
           PCV_12m = `12m PCV2%`) %>%
    map_old_to_new() %>%
    mutate(Population_12m = as.numeric(Population_12m),
           PCV_12m = as.numeric(PCV_12m)) %>%
    filter(!is.na(ONS_Code))
  
  la24 = read_ods(file_path, sheet = "24m_UTLA_GOR") %>%
    select(`ONS Upper Tier LA Code`, `24m Denominator`, `24m PCV Booster%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           Population_24m = `24m Denominator`,
           PCV_24m = `24m PCV Booster%`) %>%
    map_old_to_new() %>%
    mutate(Population_24m = as.numeric(Population_24m),
           PCV_24m = as.numeric(PCV_24m)) %>%
    filter(!is.na(ONS_Code))
  
  merged_LA = full_join(la12, la24, by = "ONS_Code") %>%
    filter(ONS_Code %in% valid_utlas) %>%
    mutate(Year = "2020/2021", Quarter = "Q2", Timepoint = 1, Vaccine_Schedule = 1)
  
  missing_utlas_q2 = setdiff(valid_utlas, merged_LA$ONS_Code)
  if (length(missing_utlas_q2) > 0) {
    la12_extra = la12 %>% filter(ONS_Code %in% missing_utlas_q2)
    la24_extra = la24 %>% filter(ONS_Code %in% missing_utlas_q2)
    merged_extra = full_join(la12_extra, la24_extra, by = "ONS_Code") %>%
      mutate(Year = "2020/2021", Quarter = "Q2", Timepoint = 1, Vaccine_Schedule = 1)
    merged_LA = bind_rows(merged_LA, merged_extra) %>% distinct()
  }
  
  q1_data = fread(file.path(clean_dir, "COVER_2020_Q1_Cleaned.csv"))
  combined_2020 = bind_rows(q1_data, merged_LA)
  fwrite(combined_2020, file.path(clean_dir, "COVER_2020_Q1_Q2_Cleaned.csv"))
  
  
  #### 🫧 2020 Q3 🫧 ####
  file_path = file.path(main_dir, "2020 Q3.ods")
  
  la12 = read_ods(file_path, sheet = "12m_UTLA_GOR") %>%
    select(`ONS Upper Tier LA Code`, `12m Denominator`, `12m PCV2%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           Population_12m = `12m Denominator`,
           PCV_12m = `12m PCV2%`) %>%
    map_old_to_new() %>%
    mutate(Population_12m = as.numeric(Population_12m),
           PCV_12m = as.numeric(PCV_12m)) %>%
    filter(!is.na(ONS_Code))
  
  la24 = read_ods(file_path, sheet = "24m_UTLA_GOR") %>%
    select(`ONS Upper Tier LA Code`, `24m Denominator`, `24m PCV Booster%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           Population_24m = `24m Denominator`,
           PCV_24m = `24m PCV Booster%`) %>%
    map_old_to_new() %>%
    mutate(Population_24m = as.numeric(Population_24m),
           PCV_24m = as.numeric(PCV_24m)) %>%
    filter(!is.na(ONS_Code))
  
  merged_LA = full_join(la12, la24, by = "ONS_Code") %>%
    filter(ONS_Code %in% valid_utlas) %>%
    mutate(Year = "2020/2021", Quarter = "Q3", Timepoint = 2, Vaccine_Schedule = 1)
  
  missing_utlas_q3 = setdiff(valid_utlas, merged_LA$ONS_Code)
  if (length(missing_utlas_q3) > 0) {
    la12_extra = la12 %>% filter(ONS_Code %in% missing_utlas_q3)
    la24_extra = la24 %>% filter(ONS_Code %in% missing_utlas_q3)
    merged_extra = full_join(la12_extra, la24_extra, by = "ONS_Code") %>%
      mutate(Year = "2020/2021", Quarter = "Q3", Timepoint = 2, Vaccine_Schedule = 1)
    merged_LA = bind_rows(merged_LA, merged_extra) %>% distinct()
  }
  
  q2_data = fread(file.path(clean_dir, "COVER_2020_Q1_Q2_Cleaned.csv"))
  combined_2020 = bind_rows(q2_data, merged_LA)
  fwrite(combined_2020, file.path(clean_dir, "COVER_2020_Q1_Q2_Q3_Cleaned.csv"))
  
  
  #### 🫧 2020 Q4 🫧 ####
  file_path = file.path(main_dir, "2020 Q4.ods")
  
  la12 = read_ods(file_path, sheet = "12m_UTLA_GOR") %>%
    select(`ONS Upper Tier LA Code`, `12m Denominator`, `12m PCV1%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           Population_12m = `12m Denominator`,
           PCV_12m = `12m PCV1%`) %>%
    map_old_to_new() %>%
    mutate(Population_12m = as.numeric(Population_12m),
           PCV_12m = as.numeric(PCV_12m)) %>%
    filter(!is.na(ONS_Code))
  
  la24 = read_ods(file_path, sheet = "24m_UTLA_GOR") %>%
    select(`ONS Upper Tier LA Code`, `24m Denominator`, `24m PCV Booster%`) %>%
    rename(ONS_Code = `ONS Upper Tier LA Code`,
           Population_24m = `24m Denominator`,
           PCV_24m = `24m PCV Booster%`) %>%
    map_old_to_new() %>%
    mutate(Population_24m = as.numeric(Population_24m),
           PCV_24m = as.numeric(PCV_24m)) %>%
    filter(!is.na(ONS_Code))
  
  merged_LA = full_join(la12, la24, by = "ONS_Code") %>%
    filter(ONS_Code %in% valid_utlas) %>%
    mutate(Year = "2020/2021", Quarter = "Q4", Timepoint = 3, Vaccine_Schedule = 1)
  
  missing_utlas_q4 = setdiff(valid_utlas, merged_LA$ONS_Code)
  if (length(missing_utlas_q4) > 0) {
    la12_extra = la12 %>% filter(ONS_Code %in% missing_utlas_q4)
    la24_extra = la24 %>% filter(ONS_Code %in% missing_utlas_q4)
    merged_extra = full_join(la12_extra, la24_extra, by = "ONS_Code") %>%
      mutate(Year = "2020/2021", Quarter = "Q4", Timepoint = 3, Vaccine_Schedule = 1)
    merged_LA = bind_rows(merged_LA, merged_extra) %>% distinct()
  }
  
  q3_data = fread(file.path(clean_dir, "COVER_2020_Q1_Q2_Q3_Cleaned.csv"))
  combined_2020 = bind_rows(q3_data, merged_LA)
  fwrite(combined_2020, file.path(clean_dir, "COVER_2020_Cleaned.csv"))
  
#### 🫧 Final check 🫧 ####
combined_2020 %>%
  group_by(Quarter) %>%
  summarise(Unique_UTLAs = n_distinct(ONS_Code))

missing_utlas = setdiff(valid_utlas, combined_2020$ONS_Code)
print(missing_utlas)
utla_list %>% filter(UTLA_code %in% missing_utlas)


#####################################
#####################################

##### ⋆˚࿔ 2 0 2 1 𝜗𝜚˚ ⋆ #####

map_utla_codes <- function(df) {
  df %>%
    left_join(utla_list, by = c("UTLA_Name" = "UTLA_name")) %>%
    mutate(
      ONS_Code = ifelse(ONS_Code %in% valid_utlas, ONS_Code, UTLA_code)
    ) %>%
    select(-UTLA_code)
}

  ##### 🫧 Q1 🫧 #####
  file_path = file.path(main_dir, "2021 Q1.ods")
  
  la12 = read_ods(file_path, sheet = "12m_UTLA_GOR") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `12m Denominator`, `12m PCV1%`) %>%
    rename(
      ONS_Code = `ONS Upper Tier LA Code`,
      UTLA_Name = `Upper Tier LA Name`,
      Population_12m = `12m Denominator`,
      PCV_12m = `12m PCV1%`
    ) %>%
    map_utla_codes() %>%
    mutate(Population_12m = as.numeric(Population_12m),
           PCV_12m = as.numeric(PCV_12m)) %>%
    filter(ONS_Code %in% valid_utlas)
  
  la24 = read_ods(file_path, sheet = "24m_UTLA_GOR") %>%
    select(`ONS Upper Tier LA Code`, `Upper Tier LA Name`, `24m Denominator`, `24m PCV Booster%`) %>%
    rename(
      ONS_Code = `ONS Upper Tier LA Code`,
      UTLA_Name = `Upper Tier LA Name`,
      Population_24m = `24m Denominator`,
      PCV_24m = `24m PCV Booster%`
    ) %>%
    map_utla_codes() %>%
    mutate(Population_24m = as.numeric(Population_24m),
           PCV_24m = as.numeric(PCV_24m)) %>%
    filter(ONS_Code %in% valid_utlas)
  
  merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
    mutate(Year = "2021/2022", Quarter = "Q1", Timepoint = 0, Vaccine_Schedule = 1)
  
  fwrite(merged_LA, file.path(clean_dir, "COVER_2021_Q1_Cleaned.csv"))
  
  ##### 🫧 Q2 🫧 #####
  file_path = file.path(main_dir, "2021 Q2.ods")
  
  la12 = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 4) %>%
    select(`ONS UTLA code`, `UTLA name`, `12m denominator`, `12m PCV1%`) %>%
    rename(
      ONS_Code = `ONS UTLA code`,
      UTLA_Name = `UTLA name`,
      Population_12m = `12m denominator`,
      PCV_12m = `12m PCV1%`
    ) %>%
    map_utla_codes() %>%
    mutate(Population_12m = as.numeric(Population_12m),
           PCV_12m = as.numeric(PCV_12m)) %>%
    filter(ONS_Code %in% valid_utlas)
  
  la24 = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 4) %>%
    select(`ONS UTLA code`, `UTLA name`, `24m denominator`, `24m PCV Booster%`) %>%
    rename(
      ONS_Code = `ONS UTLA code`,
      UTLA_Name = `UTLA name`,
      Population_24m = `24m denominator`,
      PCV_24m = `24m PCV Booster%`
    ) %>%
    map_utla_codes() %>%
    mutate(Population_24m = as.numeric(Population_24m),
           PCV_24m = as.numeric(PCV_24m)) %>%
    filter(ONS_Code %in% valid_utlas)
  
  merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
    mutate(Year = "2021/2022", Quarter = "Q2", Timepoint = 1, Vaccine_Schedule = 1)
  
  q1_data = fread(file.path(clean_dir, "COVER_2021_Q1_Cleaned.csv"))
  combined_2021 = bind_rows(q1_data, merged_LA)
  fwrite(combined_2021, file.path(clean_dir, "COVER_2021_Q1_Q2_Cleaned.csv"))
  
  ##### 🫧 Q3 🫧 #####
  file_path = file.path(main_dir, "2021 Q3.ods")
  
  la12 = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 4) %>%
    select(`ONS UTLA code`, `UTLA name`, `12 month denominator`, `12 month PCV1%`) %>%
    rename(
      ONS_Code = `ONS UTLA code`,
      UTLA_Name = `UTLA name`,
      Population_12m = `12 month denominator`,
      PCV_12m = `12 month PCV1%`
    ) %>%
    map_utla_codes() %>%
    mutate(Population_12m = as.numeric(Population_12m),
           PCV_12m = as.numeric(PCV_12m)) %>%
    filter(ONS_Code %in% valid_utlas)
  
  la24 = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 4) %>%
    select(`ONS UTLA code`, `UTLA name`, `24 month denominator`, `24 month PCV Booster%`) %>%
    rename(
      ONS_Code = `ONS UTLA code`,
      UTLA_Name = `UTLA name`,
      Population_24m = `24 month denominator`,
      PCV_24m = `24 month PCV Booster%`
    ) %>%
    map_utla_codes() %>%
    mutate(Population_24m = as.numeric(Population_24m),
           PCV_24m = as.numeric(PCV_24m)) %>%
    filter(ONS_Code %in% valid_utlas)
  
  merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
    mutate(Year = "2021/2022", Quarter = "Q3", Timepoint = 2, Vaccine_Schedule = 1)
  
  q2_data = fread(file.path(clean_dir, "COVER_2021_Q1_Q2_Cleaned.csv"))
  combined_2021 = bind_rows(q2_data, merged_LA)
  fwrite(combined_2021, file.path(clean_dir, "COVER_2021_Q1_Q2_Q3_Cleaned.csv"))
  
  ##### 🫧 Q4 🫧 #####
  file_path = file.path(main_dir, "2021 Q4.ods")
  
  la12 = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 4) %>%
    select(`ONS UTLA code`, `UTLA name`, `12m denominator`, `12m PCV1%`) %>%
    rename(
      ONS_Code = `ONS UTLA code`,
      UTLA_Name = `UTLA name`,
      Population_12m = `12m denominator`,
      PCV_12m = `12m PCV1%`
    ) %>%
    map_utla_codes() %>%
    mutate(Population_12m = as.numeric(Population_12m),
           PCV_12m = as.numeric(PCV_12m)) %>%
    filter(ONS_Code %in% valid_utlas)
  
  la24 = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 4) %>%
    select(`ONS UTLA code`, `UTLA name`, `24m denominator`, `24m PCV Booster%`) %>%
    rename(
      ONS_Code = `ONS UTLA code`,
      UTLA_Name = `UTLA name`,
      Population_24m = `24m denominator`,
      PCV_24m = `24m PCV Booster%`
    ) %>%
    map_utla_codes() %>%
    mutate(Population_24m = as.numeric(Population_24m),
           PCV_24m = as.numeric(PCV_24m)) %>%
    filter(ONS_Code %in% valid_utlas)
  
  merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
    mutate(Year = "2021/2022", Quarter = "Q4", Timepoint = 3, Vaccine_Schedule = 1)
  
  q3_data = fread(file.path(clean_dir, "COVER_2021_Q1_Q2_Q3_Cleaned.csv"))
  combined_2021 = bind_rows(q3_data, merged_LA)
  fwrite(combined_2021, file.path(clean_dir, "COVER_2021_Cleaned.csv"))
  
##### 🫧 Final check for 2021 🫧 #####
combined_2021 %>%
  group_by(Quarter) %>%
  summarise(Unique_UTLAs = n_distinct(ONS_Code))

missing_utlas = setdiff(valid_utlas, combined_2021$ONS_Code)
print(missing_utlas)
utla_list %>% filter(UTLA_code %in% missing_utlas)

#####################################
#####################################

#### ⋆˚࿔ 2 0 2 2 𝜗𝜚˚ ⋆ ####

# Function to map UTLA names to codes if needed
map_utla_codes <- function(df) {
  df %>%
    left_join(utla_list, by = c("UTLA_Name" = "UTLA_name")) %>%
    mutate(
      ONS_Code = ifelse(ONS_Code %in% valid_utlas, ONS_Code, UTLA_code)
    ) %>%
    select(-UTLA_code)
}

  #### 🫧 Q1 🫧 ####
  file_path = file.path(main_dir, "2022 Q1.ods")
  
  la12 = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 4) %>%
    select(`ONS UTLA code`, `UTLA name`, `12m denominator`, `12m PCV1%`) %>%
    rename(
      ONS_Code = `ONS UTLA code`,
      UTLA_Name = `UTLA name`,
      Population_12m = `12m denominator`,
      PCV_12m = `12m PCV1%`
    ) %>%
    map_utla_codes() %>%
    mutate(
      Population_12m = as.numeric(Population_12m),
      PCV_12m = as.numeric(PCV_12m)
    ) %>%
    filter(ONS_Code %in% valid_utlas)
  
  la24 = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 4) %>%
    select(`ONS UTLA code`, `UTLA name`, `24m denominator`, `24m PCV Booster%`) %>%
    rename(
      ONS_Code = `ONS UTLA code`,
      UTLA_Name = `UTLA name`,
      Population_24m = `24m denominator`,
      PCV_24m = `24m PCV Booster%`
    ) %>%
    map_utla_codes() %>%
    mutate(
      Population_24m = as.numeric(Population_24m),
      PCV_24m = as.numeric(PCV_24m)
    ) %>%
    filter(ONS_Code %in% valid_utlas)
  
  merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
    mutate(Year = "2022/2023", Quarter = "Q1", Timepoint = 0, Vaccine_Schedule = 1)
  
  fwrite(merged_LA, file.path(clean_dir, "COVER_2022_Q1_Cleaned.csv"))
  
  #### 🫧 Q2 🫧 ####
  file_path = file.path(main_dir, "2022 Q2.ods")
  
  la12 = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 4) %>%
    select(`ONS UTLA code`, `UTLA name`, `12m denominator`, `12m PCV1%`) %>%
    rename(
      ONS_Code = `ONS UTLA code`,
      UTLA_Name = `UTLA name`,
      Population_12m = `12m denominator`,
      PCV_12m = `12m PCV1%`
    ) %>%
    map_utla_codes() %>%
    mutate(Population_12m = as.numeric(Population_12m),
           PCV_12m = as.numeric(PCV_12m)) %>%
    filter(ONS_Code %in% valid_utlas)
  
  la24 = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 5) %>%
    select(`ONS UTLA code`, `UTLA name`, `24m denominator`, `24m PCV Booster%`) %>%
    rename(
      ONS_Code = `ONS UTLA code`,
      UTLA_Name = `UTLA name`,
      Population_24m = `24m denominator`,
      PCV_24m = `24m PCV Booster%`
    ) %>%
    map_utla_codes() %>%
    mutate(Population_24m = as.numeric(Population_24m),
           PCV_24m = as.numeric(PCV_24m)) %>%
    filter(ONS_Code %in% valid_utlas)
  
  merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
    mutate(Year = "2022/2023", Quarter = "Q2", Timepoint = 1, Vaccine_Schedule = 1)
  
  q1_data = fread(file.path(clean_dir, "COVER_2022_Q1_Cleaned.csv"))
  combined_2022 = bind_rows(q1_data, merged_LA)
  fwrite(combined_2022, file.path(clean_dir, "COVER_2022_Q1_Q2_Cleaned.csv"))
  
  #### 🫧 Q3 🫧 ####
  file_path = file.path(main_dir, "2022 Q3.ods")
  
  la12 = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 4) %>%
    select(`ONS UTLA code`, `UTLA name`, `12m denominator`, `12m PCV1%`) %>%
    rename(
      ONS_Code = `ONS UTLA code`,
      UTLA_Name = `UTLA name`,
      Population_12m = `12m denominator`,
      PCV_12m = `12m PCV1%`
    ) %>%
    map_utla_codes() %>%
    mutate(Population_12m = as.numeric(Population_12m),
           PCV_12m = as.numeric(PCV_12m)) %>%
    filter(ONS_Code %in% valid_utlas)
  
  la24 = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 5) %>%
    select(`ONS UTLA code`, `UTLA name`, `24m denominator`, `24m PCV Booster%`) %>%
    rename(
      ONS_Code = `ONS UTLA code`,
      UTLA_Name = `UTLA name`,
      Population_24m = `24m denominator`,
      PCV_24m = `24m PCV Booster%`
    ) %>%
    map_utla_codes() %>%
    mutate(Population_24m = as.numeric(Population_24m),
           PCV_24m = as.numeric(PCV_24m)) %>%
    filter(ONS_Code %in% valid_utlas)
  
  merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
    mutate(Year = "2022/2023", Quarter = "Q3", Timepoint = 2, Vaccine_Schedule = 1)
  
  q2_data = fread(file.path(clean_dir, "COVER_2022_Q1_Q2_Cleaned.csv"))
  combined_2022 = bind_rows(q2_data, merged_LA)
  fwrite(combined_2022, file.path(clean_dir, "COVER_2022_Q1_Q2_Q3_Cleaned.csv"))
  
  #### 🫧 Q4 🫧 ####
  file_path = file.path(main_dir, "2022 Q4.ods")
  
  la12 = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 5) %>%
    select(`ONS upper tier local authority code`, `Upper tier local authority name`, `12 month denominator`, `12 month PCV1%`) %>%
    rename(
      ONS_Code = `ONS upper tier local authority code`,
      UTLA_Name = `Upper tier local authority name`,
      Population_12m = `12 month denominator`,
      PCV_12m = `12 month PCV1%`
    ) %>%
    map_utla_codes() %>%
    mutate(Population_12m = as.numeric(Population_12m),
           PCV_12m = as.numeric(PCV_12m)) %>%
    filter(ONS_Code %in% valid_utlas)
  
  la24 = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 5) %>%
    select(`ONS upper tier local authority code`, `Upper tier local authority name`, `24 month denominator`, `24 month PCV Booster%`) %>%
    rename(
      ONS_Code = `ONS upper tier local authority code`,
      UTLA_Name = `Upper tier local authority name`,
      Population_24m = `24 month denominator`,
      PCV_24m = `24 month PCV Booster%`
    ) %>%
    map_utla_codes() %>%
    mutate(Population_24m = as.numeric(Population_24m),
           PCV_24m = as.numeric(PCV_24m)) %>%
    filter(ONS_Code %in% valid_utlas)
  
  merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
    mutate(Year = "2022/2023", Quarter = "Q4", Timepoint = 3, Vaccine_Schedule = 1)
  
  q3_data = fread(file.path(clean_dir, "COVER_2022_Q1_Q2_Q3_Cleaned.csv"))
  combined_2022 = bind_rows(q3_data, merged_LA)
  fwrite(combined_2022, file.path(clean_dir, "COVER_2022_Cleaned.csv"))
  
#### 🫧 Final check for 2022 🫧 ####
combined_2022 %>%
  group_by(Quarter) %>%
  summarise(Unique_UTLAs = n_distinct(ONS_Code))

missing_utlas = setdiff(valid_utlas, combined_2022$ONS_Code)
print(missing_utlas)

utla_list %>% filter(UTLA_code %in% missing_utlas)


#####################################
#####################################

#### ⋆˚࿔ 2 0 2 3 𝜗𝜚˚ ⋆ ####

map_utla_codes <- function(df) {
  df %>%
    left_join(utla_list, by = c("UTLA_Name" = "UTLA_name")) %>%
    mutate(
      ONS_Code = ifelse(ONS_Code %in% valid_utlas, ONS_Code, UTLA_code),
      ONS_Code = trimws(ONS_Code)
    ) %>%
    select(-UTLA_code)
}

  ####🫧 Q1 🫧####
  file_path = file.path(main_dir, "2023 Q1.ods")
  
  la12 = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 5) %>%
    select(`ONS UTLA code`, `UTLA name`, `12m denominator`, `12m PCV1%`) %>%
    rename(ONS_Code = `ONS UTLA code`,
           UTLA_Name = `UTLA name`,
           Population_12m = `12m denominator`,
           PCV_12m = `12m PCV1%`) %>%
    map_utla_codes() %>%
    mutate(Population_12m = as.numeric(Population_12m),
           PCV_12m = as.numeric(PCV_12m)) %>%
    filter(!is.na(ONS_Code))
  
  la24 = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 5) %>%
    select(`ONS UTLA code`, `UTLA name`, `24m denominator`, `24m PCV Booster%`) %>%
    rename(ONS_Code = `ONS UTLA code`,
           UTLA_Name = `UTLA name`,
           Population_24m = `24m denominator`,
           PCV_24m = `24m PCV Booster%`) %>%
    map_utla_codes() %>%
    mutate(Population_24m = as.numeric(Population_24m),
           PCV_24m = as.numeric(PCV_24m)) %>%
    filter(!is.na(ONS_Code))
  
  merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
    mutate(Year = "2023/2024", Quarter = "Q1", Timepoint = 0, Vaccine_Schedule = 1)
  
  fwrite(merged_LA, file.path(clean_dir, "COVER_2023_Q1_Cleaned.csv"))
  message("✅ Q1 saved.")
  
  ####🫧 Q2 🫧####
  file_path = file.path(main_dir, "2023 Q2.ods")
  
  la12 = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 6) %>%
    select(`ONS UTLA code`, `UTLA name`, `12m denominator`, `12m PCV1%`) %>%
    rename(ONS_Code = `ONS UTLA code`,
           UTLA_Name = `UTLA name`,
           Population_12m = `12m denominator`,
           PCV_12m = `12m PCV1%`) %>%
    map_utla_codes() %>%
    mutate(Population_12m = as.numeric(Population_12m),
           PCV_12m = as.numeric(PCV_12m)) %>%
    filter(!is.na(ONS_Code))
  
  la24 = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 5) %>%
    select(`ONS UTLA code`, `UTLA name`, `24m denominator`, `24m PCV Booster%`) %>%
    rename(ONS_Code = `ONS UTLA code`,
           UTLA_Name = `UTLA name`,
           Population_24m = `24m denominator`,
           PCV_24m = `24m PCV Booster%`) %>%
    map_utla_codes() %>%
    mutate(Population_24m = as.numeric(Population_24m),
           PCV_24m = as.numeric(PCV_24m)) %>%
    filter(!is.na(ONS_Code))
  
  merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
    mutate(Year = "2023/2024", Quarter = "Q2", Timepoint = 1, Vaccine_Schedule = 1)
  
  q1_data = fread(file.path(clean_dir, "COVER_2023_Q1_Cleaned.csv"))
  combined_2023 = bind_rows(q1_data, merged_LA)
  fwrite(combined_2023, file.path(clean_dir, "COVER_2023_Q1_Q2_Cleaned.csv"))
  message("✅ Q2 saved.")
  
  ####🫧 Q3 🫧####
  file_path = file.path(main_dir, "2023 Q3.ods")
  
  la12 = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 5) %>%
    select(`ONS UTLA code`, `UTLA name`, `12m denominator`, `12m PCV1%`) %>%
    rename(ONS_Code = `ONS UTLA code`,
           UTLA_Name = `UTLA name`,
           Population_12m = `12m denominator`,
           PCV_12m = `12m PCV1%`) %>%
    map_utla_codes() %>%
    mutate(Population_12m = as.numeric(Population_12m),
           PCV_12m = as.numeric(PCV_12m)) %>%
    filter(!is.na(ONS_Code))
  
  la24 = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 5) %>%
    select(`ONS UTLA code`, `UTLA name`, `24m denominator`, `24m PCV Booster%`) %>%
    rename(ONS_Code = `ONS UTLA code`,
           UTLA_Name = `UTLA name`,
           Population_24m = `24m denominator`,
           PCV_24m = `24m PCV Booster%`) %>%
    map_utla_codes() %>%
    mutate(Population_24m = as.numeric(Population_24m),
           PCV_24m = as.numeric(PCV_24m)) %>%
    filter(!is.na(ONS_Code))
  
  merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
    mutate(Year = "2023/2024", Quarter = "Q3", Timepoint = 2, Vaccine_Schedule = 1)
  
  q2_data = fread(file.path(clean_dir, "COVER_2023_Q1_Q2_Cleaned.csv"))
  combined_2023 = bind_rows(q2_data, merged_LA)
  fwrite(combined_2023, file.path(clean_dir, "COVER_2023_Q1_Q2_Q3_Cleaned.csv"))
  message("✅ Q3 saved.")
  
  ####🫧 Q4 🫧####
  file_path = file.path(main_dir, "2023 Q4.ods")
  
  la12 = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 5) %>%
    select(`ONS UTLA code`, `UTLA name`, `12m denominator`, `12m PCV1%`) %>%
    rename(ONS_Code = `ONS UTLA code`,
           UTLA_Name = `UTLA name`,
           Population_12m = `12m denominator`,
           PCV_12m = `12m PCV1%`) %>%
    map_utla_codes() %>%
    mutate(Population_12m = as.numeric(Population_12m),
           PCV_12m = as.numeric(PCV_12m)) %>%
    filter(!is.na(ONS_Code), ONS_Code != "[z]")
  
  la24 = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 5) %>%
    select(`ONS UTLA code`, `UTLA name`, `24m denominator`, `24m PCV Booster%`) %>%
    rename(ONS_Code = `ONS UTLA code`,
           UTLA_Name = `UTLA name`,
           Population_24m = `24m denominator`,
           PCV_24m = `24m PCV Booster%`) %>%
    map_utla_codes() %>%
    mutate(Population_24m = as.numeric(Population_24m),
           PCV_24m = as.numeric(PCV_24m)) %>%
    filter(!is.na(ONS_Code), ONS_Code != "[z]")
  
  merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
    mutate(Year = "2023/2024", Quarter = "Q4", Timepoint = 3, Vaccine_Schedule = 1)
  
  q3_data = fread(file.path(clean_dir, "COVER_2023_Q1_Q2_Q3_Cleaned.csv"))
  combined_2023 = bind_rows(q3_data, merged_LA)
  fwrite(combined_2023, file.path(clean_dir, "COVER_2023_Cleaned.csv"))
  message("✅ Q4 and full 2023 file saved.")
  
####🫧 Final check for 2023 🫧####
combined_2023 %>%
  group_by(Quarter) %>%
  summarise(Unique_UTLAs = n_distinct(ONS_Code))

missing_utlas = setdiff(valid_utlas, combined_2023$ONS_Code)
print(missing_utlas)

utla_list %>% filter(UTLA_code %in% missing_utlas)

#####################################
#####################################

#### ⋆˚࿔ 2 0 2 4 𝜗𝜚˚ ⋆ ####

map_utla_codes <- function(df) {
  df %>%
    left_join(utla_list, by = c("UTLA_Name" = "UTLA_name")) %>%
    mutate(
      ONS_Code = ifelse(ONS_Code %in% valid_utlas, ONS_Code, UTLA_code),
      ONS_Code = trimws(ONS_Code)
    ) %>%
    select(-UTLA_code)
}

  ####🫧 Q1 🫧####
  file_path = file.path(main_dir, "2024 Q1.ods")
  
  # 12-month sheet
  la12 = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 5) %>%
    select(`ONS UTLA code`, `UTLA name`, `12m denominator`, `12m PCV1 (%)`) %>%
    rename(
      ONS_Code = `ONS UTLA code`,
      UTLA_Name = `UTLA name`,
      Population_12m = `12m denominator`,
      PCV_12m = `12m PCV1 (%)`
    ) %>%
    map_utla_codes() %>%
    mutate(Population_12m = as.numeric(Population_12m),
           PCV_12m = as.numeric(PCV_12m)) %>%
    filter(!is.na(ONS_Code), ONS_Code != "[z]")
  
  # 24-month sheet
  la24 = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 5) %>%
    select(`ONS UTLA code`, `UTLA name`, `24m denominator`, `24m PCV Booster%`) %>%
    rename(
      ONS_Code = `ONS UTLA code`,
      UTLA_Name = `UTLA name`,
      Population_24m = `24m denominator`,
      PCV_24m = `24m PCV Booster%`
    ) %>%
    map_utla_codes() %>%
    mutate(Population_24m = as.numeric(Population_24m),
           PCV_24m = as.numeric(PCV_24m)) %>%
    filter(!is.na(ONS_Code), ONS_Code != "[z]")
  
  # Merge and annotate
  merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
    mutate(Year = "2024/2025", Quarter = "Q1", Timepoint = 0, Vaccine_Schedule = 1)
  
  fwrite(merged_LA, file.path(clean_dir, "COVER_2024_Q1_Cleaned.csv"))
  message("✅ Q1 saved.")
  
  ####🫧 Q2 🫧####
  file_path = file.path(main_dir, "2024 Q2.ods")
  
  la12 = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 6) %>%
    select(`Code`, `Local authority`, `Number of children who reached 12 months in reporting quarter`, `Coverage at 12 months PCV1 (%)`) %>%
    rename(
      ONS_Code = `Code`,
      UTLA_Name = `Local authority`,
      Population_12m = `Number of children who reached 12 months in reporting quarter`,
      PCV_12m = `Coverage at 12 months PCV1 (%)`
    ) %>%
    map_utla_codes() %>%
    mutate(Population_12m = as.numeric(Population_12m),
           PCV_12m = as.numeric(PCV_12m)) %>%
    filter(!is.na(ONS_Code), ONS_Code != "[z]")
  
  la24 = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 6) %>%
    select(`Code`, `Local authority`, `Number of children who reached 24 months in reporting quarter`, `Coverage at 24 months PCV Booster (%)`) %>%
    rename(
      ONS_Code = `Code`,
      UTLA_Name = `Local authority`,
      Population_24m = `Number of children who reached 24 months in reporting quarter`,
      PCV_24m = `Coverage at 24 months PCV Booster (%)`
    ) %>%
    map_utla_codes() %>%
    mutate(Population_24m = as.numeric(Population_24m),
           PCV_24m = as.numeric(PCV_24m)) %>%
    filter(!is.na(ONS_Code), ONS_Code != "[z]")
  
  merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
    mutate(Year = "2024/2025", Quarter = "Q2", Timepoint = 1, Vaccine_Schedule = 1)
  
  q1_data = fread(file.path(clean_dir, "COVER_2024_Q1_Cleaned.csv"))
  combined_2024 = bind_rows(q1_data, merged_LA)
  fwrite(combined_2024, file.path(clean_dir, "COVER_2024_Q1_Q2_Cleaned.csv"))
  message("✅ Q2 saved.")
  
  ####🫧 Q3 🫧####
  file_path = file.path(main_dir, "2024 Q3.ods")
  
  la12 = read_ods(file_path, sheet = "Table5", skip = 5) %>%
    select(`Code`, `Local authority`, `Number of children who reached 12 months in reporting quarter`, `Coverage at 12 months PCV1 (%)`) %>%
    rename(
      ONS_Code = `Code`,
      UTLA_Name = `Local authority`,
      Population_12m = `Number of children who reached 12 months in reporting quarter`,
      PCV_12m = `Coverage at 12 months PCV1 (%)`
    ) %>%
    map_utla_codes() %>%
    mutate(Population_12m = as.numeric(Population_12m),
           PCV_12m = as.numeric(PCV_12m)) %>%
    filter(!is.na(ONS_Code), ONS_Code != "[z]")
  
  la24 = read_ods(file_path, sheet = "Table6", skip = 4) %>%
    select(`Code`, `Local authority`, `Number of children who reached 24 months in reporting quarter`, `Coverage at 24 months PCV Booster (%)`) %>%
    rename(
      ONS_Code = `Code`,
      UTLA_Name = `Local authority`,
      Population_24m = `Number of children who reached 24 months in reporting quarter`,
      PCV_24m = `Coverage at 24 months PCV Booster (%)`
    ) %>%
    map_utla_codes() %>%
    mutate(Population_24m = as.numeric(Population_24m),
           PCV_24m = as.numeric(PCV_24m)) %>%
    filter(!is.na(ONS_Code), ONS_Code != "[z]")
  
  merged_LA = full_join(la12, la24, by = c("ONS_Code", "UTLA_Name")) %>%
    mutate(Year = "2024/2025", Quarter = "Q3", Timepoint = 2, Vaccine_Schedule = 1)
  
  q2_data = fread(file.path(clean_dir, "COVER_2024_Q1_Q2_Cleaned.csv"))
  combined_2024 = bind_rows(q2_data, merged_LA)
  fwrite(combined_2024, file.path(clean_dir, "COVER_2024_Q1_Q2_Q3_Cleaned.csv"))
  message("✅ Q3 saved.")
  
####🫧 Final Check for 2024 🫧####
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
