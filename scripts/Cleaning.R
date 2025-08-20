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
main_dir = "C:/Users/User/OneDrive/HSD MSC/Diss Data" # Replace this with your actual wd
setwd(main_dir)

clean_dir = file.path(main_dir, "Cleaned_COVER_Files")
# dir.create(clean_dir, showWarnings = FALSE)

getwd()





#### ⋆˚࿔ 2 0 1 3 𝜗𝜚˚ ⋆ ####

#### 🫧 2013 Q2 🫧 ####

# set file path
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





#### ⋆˚࿔ 2 0 1 4 𝜗𝜚˚⋆ ####

####🫧 2014 data — Q1 🫧####
# Define file path
file_path = file.path(main_dir, "2014_Q1.xlsx")

# Load full sheet (after skipping metadata)
la12_raw = read_excel(file_path, sheet = "12m_UT LA", range = "F1:M153")

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS Upper Tier LA Code`, `12 month Denominator`, `PCV2 (%)`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_12m = `12 month Denominator`,
    PCV_12m = `PCV2 (%)`
  )


la24_raw = read_excel(file_path, sheet = "24m_UT LA", range = "F1:M153")

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS Upper Tier LA Code`, `24m Denominator`, `24m_PCV Booster (%)`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m_PCV Booster (%)`
  )

# View
View(la12)
View(la24)

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")

# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2014/2015",
    Quarter = "Q1",
    Timepoint = 0,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
save_file_path = file.path(clean_dir, "COVER_2014_Q1_Cleaned.csv")
fwrite(merged_LA, file = save_file_path)

message("Cleaned data saved to: ", save_file_path)


####🫧 2014 data — Q2 🫧####
# Define file path
file_path = file.path(main_dir, "2014_Q2.xlsx")

# Load full sheet (after skipping metadata)
la12_raw = read_excel(file_path, sheet = "12m_UT LA", range = "F1:M153")

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS LA Code`, `Sum of Denom_12m`, `Sum of 12m_PCV%`) %>%
  rename(
    ONS_Code = `ONS LA Code`,
    Population_12m = `Sum of Denom_12m`,
    PCV_12m = `Sum of 12m_PCV%`
  )


la24_raw = read_excel(file_path, sheet = "24m_UT LA", range = "F1:M153")

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS Upper Tier LA Code`, `Sum of Denom_24m`, `Sum of 24m_PCVB%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_24m = `Sum of Denom_24m`,
    PCV_24m = `Sum of 24m_PCVB%`
  )

# View
View(la12)
View(la24)

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")

# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2014/2015",
    Quarter = "Q2",
    Timepoint = 1,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
q1_data = fread(file.path(clean_dir, "COVER_2014_Q1_Cleaned.csv"))
combined_2014 = rbind(q1_data, merged_LA)
fwrite(combined_2014, file.path(clean_dir, "COVER_2014_Q1_Q2_Cleaned.csv"))

message("Cleaned data saved to: ", save_file_path)


####🫧 2014 data — Q3 🫧####
# Define file path
file_path = file.path(main_dir, "2014_Q3.xlsx")

# Load full sheet (after skipping metadata)
la12_raw = read_excel(file_path, sheet = "12m_UT LA", range = "E1:M153")

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS LA Code`, `Sum of Denom_12m`, `Sum of 12m_PCV%`) %>%
  rename(
    ONS_Code = `ONS LA Code`,
    Population_12m = `Sum of Denom_12m`,
    PCV_12m = `Sum of 12m_PCV%`
  )


la24_raw = read_excel(file_path, sheet = "24m_UT LA", range = "E1:M153")

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS LA Code`, `24m Denominator`, `24m PCV Booster %`) %>%
  rename(
    ONS_Code = `ONS LA Code`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster %`
  )

# View
View(la12)
View(la24)

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")

# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2014/2015",
    Quarter = "Q3",
    Timepoint = 2,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
q2_data = fread(file.path(clean_dir, "COVER_2014_Q1_Q2_Cleaned.csv"))
combined_2014 = rbind(q2_data, merged_LA)
fwrite(combined_2014, file.path(clean_dir, "COVER_2014_Q1_Q2_Q3_Cleaned.csv"))

message("Cleaned data saved to: ", save_file_path)



####🫧 2014 data — Q4 🫧####
# Define file path
file_path = file.path(main_dir, "2014_Q4.xlsx")

# Load full sheet (after skipping metadata)
la12_raw = read_excel(file_path, sheet = "12m_UT LA", range = "E1:M154")

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS Upper Tier LA Code`, `12m Denominator`, `12m PCV %`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV %`
  )


la24_raw = read_excel(file_path, sheet = "24m_UT LA", range = "E1:M154")

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS LA Code`, `24m Denominator`, `24m PCV Booster %`) %>%
  rename(
    ONS_Code = `ONS LA Code`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster %`
  )

# View
View(la12)
View(la24)

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")

# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2014/2015",
    Quarter = "Q4",
    Timepoint = 3,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
q3_data = fread(file.path(clean_dir, "COVER_2014_Q1_Q2_Q3_Cleaned.csv"))
combined_2014 = rbind(q3_data, merged_LA)
fwrite(combined_2014, file.path(clean_dir, "COVER_2014_Cleaned.csv"))

message("Cleaned data saved to: ", save_file_path)






#### ⋆˚࿔ 2 0 1 5 𝜗𝜚˚ ⋆ ####

####🫧 2015 data — Q1 🫧####
# Define file path
file_path = file.path(main_dir, "2015_Q1.xlsx")

# Load full sheet (after skipping metadata)
la12_raw = read_excel(file_path, sheet = "12m_UT LA", range = "E1:M154")

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS Upper Tier LA Code`, `12m Denominator`, `12m PCV %`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV %`
  )


la24_raw = read_excel(file_path, sheet = "24m_UT LA", range = "E1:M154")

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS LA Code`, `24m Denominator`, `24m PCV Booster %`) %>%
  rename(
    ONS_Code = `ONS LA Code`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster %`
  )

# View
View(la12)
View(la24)

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")

# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2015/2016",
    Quarter = "Q1",
    Timepoint = 0,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
save_file_path = file.path(clean_dir, "COVER_2015_Q1_Cleaned.csv")
fwrite(merged_LA, file = save_file_path)

message("Cleaned data saved to: ", save_file_path)


####🫧 2015 data — Q2 🫧####
# Define file path
file_path = file.path(main_dir, "2015_Q2.xlsx")

# Load full sheet (after skipping metadata)
la12_raw = read_excel(file_path, sheet = "12m_UT LA", range = "E1:M154")

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS Upper Tier LA Code`, `12m Denominator`, `12m PCV %`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV %`
  )


la24_raw = read_excel(file_path, sheet = "24m_UT LA", range = "E1:M154")

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS LA Code`, `24m Denominator`, `24m PCV Booster %`) %>%
  rename(
    ONS_Code = `ONS LA Code`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster %`
  )

# View
View(la12)
View(la24)

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")

# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2015/2016",
    Quarter = "Q2",
    Timepoint = 1,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
q1_data = fread(file.path(clean_dir, "COVER_2015_Q1_Cleaned.csv"))
combined_2015 = rbind(q1_data, merged_LA)
fwrite(combined_2015, file.path(clean_dir, "COVER_2015_Q1_Q2_Cleaned.csv"))

message("Cleaned data saved to: ", save_file_path)


####🫧 2015 data — Q3 🫧####
# Define file path
file_path = file.path(main_dir, "2015_Q3.xlsx")

# Load full sheet (after skipping metadata)
la12_raw = read_excel(file_path, sheet = "12m_UT LA", range = "E1:M154")

# See the column names
print(names(la12_raw))

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS Upper Tier LA Code`, `12m Denominator`, `12m PCV %`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV %`
  )


la24_raw = read_excel(file_path, sheet = "24m_UT LA", range = "E1:M154")

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS LA Code`, `24m Denominator`, `24m PCV Booster %`) %>%
  rename(
    ONS_Code = `ONS LA Code`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster %`
  )

# View
View(la12)
View(la24)

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")

# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2015/2016",
    Quarter = "Q3",
    Timepoint = 2,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
q2_data = fread(file.path(clean_dir, "COVER_2015_Q1_Q2_Cleaned.csv"))
combined_2015 = rbind(q2_data, merged_LA)
fwrite(combined_2015, file.path(clean_dir, "COVER_2015_Q1_Q2_Q3_Cleaned.csv"))

message("Cleaned data saved to: ", save_file_path)



####🫧 2015 data — Q4 🫧####
# Define file path
file_path = file.path(main_dir, "2015_Q4.xlsx")

# Load full sheet (after skipping metadata)
la12_raw = read_excel(file_path, sheet = "12m_UT LA", range = "E1:M154")

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS Upper Tier LA Code`, `12m Denominator`, `12m PCV %`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV %`
  )


la24_raw = read_excel(file_path, sheet = "24m_UT LA", range = "E1:M154")

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS LA Code`, `24m Denominator`, `24m PCV Booster %`) %>%
  rename(
    ONS_Code = `ONS LA Code`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster %`
  )

# View
View(la12)
View(la24)

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")

# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2015/2016",
    Quarter = "Q4",
    Timepoint = 3,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
q3_data = fread(file.path(clean_dir, "COVER_2015_Q1_Q2_Q3_Cleaned.csv"))
combined_2015 = rbind(q3_data, merged_LA)
fwrite(combined_2015, file.path(clean_dir, "COVER_2015_Cleaned.csv"))

message("Cleaned data saved to: ", save_file_path)


 



#### ⋆˚࿔ 2 0 1 6 𝜗𝜚˚ ⋆ ####



####🫧 2016 data — Q1 🫧####
# Define file path
file_path = file.path(main_dir, "2016_Q1.xlsx")

# Load full sheet (after skipping metadata)
la12_raw = read_excel(file_path, sheet = "12m_UTLA_AT")

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS Upper Tier LA Code`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  )


la24_raw = read_excel(file_path, sheet = "24m_UTLA_AT")

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS Upper Tier LA Code`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  )

# View
View(la12)
View(la24)


# Check for duplicates 
la12 %>% count(ONS_Code) %>% filter(n > 1)
la24 %>% count(ONS_Code) %>% filter(n > 1)

# Rows with missing ONS codes
# Not true missing rows, but subheadings in data for some reason 
la12 = la12 %>% filter(!is.na(ONS_Code))
la24 = la24 %>% filter(!is.na(ONS_Code))

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")


# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2016/2017",
    Quarter = "Q1",
    Timepoint = 0,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# remove redundant rows
merged_LA = merged_LA[1:(nrow(merged_LA) - 2), ]

# Save using fwrite()
save_file_path = file.path(clean_dir, "COVER_2016_Q1_Cleaned.csv")
fwrite(merged_LA, file = save_file_path)

message("Cleaned data saved to: ", save_file_path)


####🫧 2016 data — Q2 🫧####
# Define file path
file_path = file.path(main_dir, "2016_Q2.xlsx")

# Load full sheet (after skipping metadata)
la12_raw = read_excel(file_path, sheet = "12m_UTLA_AT")

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS Upper Tier LA Code`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  )


la24_raw = read_excel(file_path, sheet = "24m_UTLA_AT")

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS Upper Tier LA Code`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  )

# View
View(la12)
View(la24)

# Rows with missing ONS codes
# Not true missing rows, but subheadings in data for some reason 
la12 = la12 %>% filter(!is.na(ONS_Code))
la24 = la24 %>% filter(!is.na(ONS_Code))

# remove redundant rows
la12 = la12[1:(nrow(la12) - 1), ]
la24 = la24[1:(nrow(la24) - 1), ]

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")

# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2016/2017",
    Quarter = "Q2",
    Timepoint = 1,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
q1_data = fread(file.path(clean_dir, "COVER_2016_Q1_Cleaned.csv"))
combined_2016 = rbind(q1_data, merged_LA)
fwrite(combined_2016, file.path(clean_dir, "COVER_2016_Q1_Q2_Cleaned.csv"))

message("Cleaned data saved to: ", save_file_path)


####🫧 2016 data — Q3 🫧####
# Define file path
file_path = file.path(main_dir, "2016_Q3.xlsx")

# Load full sheet (after skipping metadata)
la12_raw = read_excel(file_path, sheet = "12m_UTLA_AT")

# See the column names
print(names(la12_raw))

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS Upper Tier LA Code`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  )


la24_raw = read_excel(file_path, sheet = "24m_UTLA_AT")

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS Upper Tier LA Code`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  )

# View
View(la12)
View(la24)

# Rows with missing ONS codes
# Not true missing rows, but subheadings in data for some reason 
la12 = la12 %>% filter(!is.na(ONS_Code))
la24 = la24 %>% filter(!is.na(ONS_Code))

# remove redundant rows
la12 = la12[1:(nrow(la12) - 1), ]
la24 = la24[1:(nrow(la24) - 1), ]

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")

# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2016/2017",
    Quarter = "Q3",
    Timepoint = 2,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
q2_data = fread(file.path(clean_dir, "COVER_2016_Q1_Q2_Cleaned.csv"))
combined_2016 = rbind(q2_data, merged_LA)
fwrite(combined_2016, file.path(clean_dir, "COVER_2016_Q1_Q2_Q3_Cleaned.csv"))

message("Cleaned data saved to: ", save_file_path)



####🫧 2016 data — Q4 🫧####
# Define file path
file_path = file.path(main_dir, "2016_Q4.xlsx")

# Load full sheet (after skipping metadata)
la12_raw = read_excel(file_path, sheet = "12m_UTLA_AT")

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS Upper Tier LA Code`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  )


la24_raw = read_excel(file_path, sheet = "24m_UTLA_AT")

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS Upper Tier LA Code`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  )

# View
View(la12)
View(la24)

# Rows with missing ONS codes
# Not true missing rows, but subheadings in data for some reason 
la12 = la12 %>% filter(!is.na(ONS_Code))
la24 = la24 %>% filter(!is.na(ONS_Code))

# remove redundant rows
la12 = la12[1:(nrow(la12) - 1), ]
la24 = la24[1:(nrow(la24) - 1), ]

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")

# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2016/2017",
    Quarter = "Q4",
    Timepoint = 3,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
q3_data = fread(file.path(clean_dir, "COVER_2016_Q1_Q2_Q3_Cleaned.csv"))
combined_2016 = rbind(q3_data, merged_LA)
fwrite(combined_2016, file.path(clean_dir, "COVER_2016_Cleaned.csv"))

message("Cleaned data saved to: ", save_file_path)









#### ⋆˚࿔ 2 0 1 7 𝜗𝜚˚ ⋆ ####

####🫧 2017 data — Q1 🫧####

# Define file path
file_path = file.path(main_dir, "2017_Q1.xlsx")

# Load full sheet (after skipping metadata)
la12_raw = read_excel(file_path, sheet = "12m_UTLA_AT")

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS Upper Tier LA Code`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  )


la24_raw = read_excel(file_path, sheet = "24m_UTLA_AT")

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS Upper Tier LA Code`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  )

# View
View(la12)
View(la24)


# Check for duplicates 
la12 %>% count(ONS_Code) %>% filter(n > 1)
la24 %>% count(ONS_Code) %>% filter(n > 1)

# Rows with missing ONS codes
# Not true missing rows, but subheadings in data for some reason 
la12 = la12 %>% filter(!is.na(ONS_Code))
la24 = la24 %>% filter(!is.na(ONS_Code))

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")


# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2017/2018",
    Quarter = "Q1",
    Timepoint = 0,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# remove redundant rows
merged_LA = merged_LA[1:(nrow(merged_LA) - 2), ]

# Save using fwrite()
save_file_path = file.path(clean_dir, "COVER_2017_Q1_Cleaned.csv")
fwrite(merged_LA, file = save_file_path)

message("Cleaned data saved to: ", save_file_path)


####🫧 2017 data — Q2 🫧####
# Define file path
file_path = file.path(main_dir, "2017_Q2.xlsx")

# Load full sheet (after skipping metadata)
la12_raw = read_excel(file_path, sheet = "12m_UTLA_AT")

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS Upper Tier LA Code`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  )


la24_raw = read_excel(file_path, sheet = "24m_UTLA_AT")

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS Upper Tier LA Code`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  )

# View
View(la12)
View(la24)

# Rows with missing ONS codes
# Not true missing rows, but subheadings in data for some reason 
la12 = la12 %>% filter(!is.na(ONS_Code))
la24 = la24 %>% filter(!is.na(ONS_Code))

# remove redundant rows
la12 = la12[1:(nrow(la12) - 1), ]
la24 = la24[1:(nrow(la24) - 1), ]

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")

# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2017/2018",
    Quarter = "Q2",
    Timepoint = 1,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
q1_data = fread(file.path(clean_dir, "COVER_2017_Q1_Cleaned.csv"))
combined_2017 = rbind(q1_data, merged_LA)
fwrite(combined_2017, file.path(clean_dir, "COVER_2017_Q1_Q2_Cleaned.csv"))

message("Cleaned data saved to: ", save_file_path)


####🫧 2017 data — Q3 🫧####
# Define file path
file_path = file.path(main_dir, "2017_Q3.xlsx")

# Load full sheet (after skipping metadata)
la12_raw = read_excel(file_path, sheet = "12m_UTLA_AT")

# See the column names
print(names(la12_raw))

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS Upper Tier LA Code`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  )


la24_raw = read_excel(file_path, sheet = "24m_UTLA_AT")

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS Upper Tier LA Code`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  )

# View
View(la12)
View(la24)

# Rows with missing ONS codes
# Not true missing rows, but subheadings in data for some reason 
la12 = la12 %>% filter(!is.na(ONS_Code))
la24 = la24 %>% filter(!is.na(ONS_Code))

# remove redundant rows
la12 = la12[1:(nrow(la12) - 1), ]
la24 = la24[1:(nrow(la24) - 1), ]

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")

# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2017/2018",
    Quarter = "Q3",
    Timepoint = 2,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
q2_data = fread(file.path(clean_dir, "COVER_2017_Q1_Q2_Cleaned.csv"))
combined_2017 = rbind(q2_data, merged_LA)
fwrite(combined_2017, file.path(clean_dir, "COVER_2017_Q1_Q2_Q3_Cleaned.csv"))

message("Cleaned data saved to: ", save_file_path)



####🫧 2017 data — Q4 🫧####
# Define file path
file_path = file.path(main_dir, "2017_Q4.xlsx")

# Load full sheet (after skipping metadata)
la12_raw = read_excel(file_path, sheet = "12m_UTLA_AT")

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS Upper Tier LA Code`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  )


la24_raw = read_excel(file_path, sheet = "24m_UTLA_AT")

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS Upper Tier LA Code`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  )

# View
View(la12)
View(la24)

# Rows with missing ONS codes
# Not true missing rows, but subheadings in data for some reason 
la12 = la12 %>% filter(!is.na(ONS_Code))
la24 = la24 %>% filter(!is.na(ONS_Code))

# remove redundant rows
la12 = la12[1:(nrow(la12) - 1), ]
la24 = la24[1:(nrow(la24) - 1), ]

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")

# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2017/2018",
    Quarter = "Q4",
    Timepoint = 3,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
q3_data = fread(file.path(clean_dir, "COVER_2017_Q1_Q2_Q3_Cleaned.csv"))
combined_2017 = rbind(q3_data, merged_LA)
fwrite(combined_2017, file.path(clean_dir, "COVER_2017_Cleaned.csv"))

message("Cleaned data saved to: ", save_file_path)












#### ⋆˚࿔ 2 0 1 8 𝜗𝜚˚ ⋆ ####

####🫧 2018 data — Q1 🫧####

# Define file path
file_path = file.path(main_dir, "2018_Q1.xlsx")

# Load full sheet (after skipping metadata)
la12_raw = read_excel(file_path, sheet = "12m_UTLA_GOR")

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS Upper Tier LA Code`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  )


la24_raw = read_excel(file_path, sheet = "24m_UTLA_GOR")

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS Upper Tier LA Code`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  )

# View
View(la12)
View(la24)


# Check for duplicates 
la12 %>% count(ONS_Code) %>% filter(n > 1)
la24 %>% count(ONS_Code) %>% filter(n > 1)

# Rows with missing ONS codes
# Not true missing rows, but subheadings in data for some reason 
la12 = la12 %>% filter(!is.na(ONS_Code))
la24 = la24 %>% filter(!is.na(ONS_Code))

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")


# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2018/2017",
    Quarter = "Q1",
    Timepoint = 0,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
save_file_path = file.path(clean_dir, "COVER_2018_Q1_Cleaned.csv")
fwrite(merged_LA, file = save_file_path)

message("Cleaned data saved to: ", save_file_path)


####🫧 2018 data — Q2 🫧####
# Define file path
file_path = file.path(main_dir, "2018_Q2.xlsx")

# Load full sheet (after skipping metadata)
la12_raw = read_excel(file_path, sheet = "12m_UTLA_GOR")

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS Upper Tier LA Code`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  )


la24_raw = read_excel(file_path, sheet = "24m_UTLA_GOR")

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS Upper Tier LA Code`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  )

# View
View(la12)
View(la24)

# Rows with missing ONS codes
# Not true missing rows, but subheadings in data for some reason 
la12 = la12 %>% filter(!is.na(ONS_Code))
la24 = la24 %>% filter(!is.na(ONS_Code))

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")

# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2018/2019",
    Quarter = "Q2",
    Timepoint = 1,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
q1_data = fread(file.path(clean_dir, "COVER_2018_Q1_Cleaned.csv"))
combined_2018 = rbind(q1_data, merged_LA)
fwrite(combined_2018, file.path(clean_dir, "COVER_2018_Q1_Q2_Cleaned.csv"))

message("Cleaned data saved to: ", save_file_path)


####🫧 2018 data — Q3 🫧####
# Define file path
file_path = file.path(main_dir, "2018_Q3.xlsx")

# Load full sheet (after skipping metadata)
la12_raw = read_excel(file_path, sheet = "12m_UTLA_GOR")

# See the column names
print(names(la12_raw))

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS Upper Tier LA Code`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  )


la24_raw = read_excel(file_path, sheet = "24m_UTLA_GOR")

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS Upper Tier LA Code`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  )

# View
View(la12)
View(la24)

# Rows with missing ONS codes
# Not true missing rows, but subheadings in data for some reason 
la12 = la12 %>% filter(!is.na(ONS_Code))
la24 = la24 %>% filter(!is.na(ONS_Code))

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")

# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2018/2019",
    Quarter = "Q3",
    Timepoint = 2,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
q2_data = fread(file.path(clean_dir, "COVER_2018_Q1_Q2_Cleaned.csv"))
combined_2018 = rbind(q2_data, merged_LA)
fwrite(combined_2018, file.path(clean_dir, "COVER_2018_Q1_Q2_Q3_Cleaned.csv"))

message("Cleaned data saved to: ", save_file_path)



####🫧 2018 data — Q4 🫧####
# Define file path
file_path = file.path(main_dir, "2018_Q4.xlsx")

# Load full sheet (after skipping metadata)
la12_raw = read_excel(file_path, sheet = "12m_UTLA_GOR")

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS Upper Tier LA Code`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  )


la24_raw = read_excel(file_path, sheet = "24m_UTLA_GOR")

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS Upper Tier LA Code`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  )

# View
View(la12)
View(la24)

# Rows with missing ONS codes
# Not true missing rows, but subheadings in data for some reason 
la12 = la12 %>% filter(!is.na(ONS_Code))
la24 = la24 %>% filter(!is.na(ONS_Code))

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")

# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2018/2019",
    Quarter = "Q4",
    Timepoint = 3,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
q3_data = fread(file.path(clean_dir, "COVER_2018_Q1_Q2_Q3_Cleaned.csv"))
combined_2018 = rbind(q3_data, merged_LA)
fwrite(combined_2018, file.path(clean_dir, "COVER_2018_Cleaned.csv"))

message("Cleaned data saved to: ", save_file_path)







#### ⋆˚࿔ 2 0 1 9 𝜗𝜚˚ ⋆ ####

# ODC - ONS code (2028 pull)
# Table 1 only

####🫧 2019 data — Q1 🫧####

# Define file path
file_path = file.path(main_dir, "2019_Q1.ods")

# Load table
data_raw = read_ods(file_path, sheet = "Table_1", skip = 2)
                    
# Preview column names
names(data_raw)


# After confirming, extract only what you want
data = data_raw %>%
  select(`ODS Upper Tier LA Code`, `12m Denominator`, `12m PCV2%`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ODS_Code = `ODS Upper Tier LA Code`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  )

# View
View(data)

# Map ODS to ONS 

lookup_ods_ons_raw = read_excel(file.path(main_dir, "2018_Q1.xlsx"), sheet = "12m_UTLA_GOR")

# View all column names to confirm
print(names(lookup_ods_ons_raw))
View(lookup_ods_ons_raw)

# Step 3: Select relevant columns and rename
lookup_ods_ons = lookup_ods_ons_raw %>%
  select(`ODS Upper Tier LA Code`, `ONS Upper Tier LA Code`) %>%
  rename(
    ODS_Code = `ODS Upper Tier LA Code`,
    ONS_Code = `ONS Upper Tier LA Code`
  ) %>%
  filter(!is.na(ONS_Code)) %>%  # drop rows where ONS is missing
  distinct()

# View to confirm final lookup
View(lookup_ods_ons)

str(data$ODS_Code)
str(lookup_ods_ons$ODS_Code)

# different types so, transform 
data = data %>%
  mutate(ODS_Code = as.character(ODS_Code))

lookup_ods_ons = lookup_ods_ons %>%
  mutate(ODS_Code = as.character(ODS_Code))

# join works
data = left_join(data, lookup_ods_ons, by = "ODS_Code")

# finalilse and save
data = data %>%
  filter(!is.na(ONS_Code)) %>%
  mutate(
    Year = "2019/2020",
    Quarter = "Q1",
    Timepoint = 0,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

fwrite(data, file.path(clean_dir, "COVER_2019_Q1_Cleaned.csv"))

message("Cleaned data saved to: ", save_file_path)




####🫧 2019 data — Q2 🫧####

# Define file path
file_path = file.path(main_dir, "2019_Q2.ods")

# Load table
data_raw = read_ods(file_path, sheet = "Table_1", skip = 2)

# Preview column names
names(data_raw)

# After confirming, extract only what you want
data = data_raw %>%
  select(`ODS Upper Tier LA Code`, `12m Denominator`, `12m PCV2%`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ODS_Code = `ODS Upper Tier LA Code`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  )

# View
View(data)

# different types so, transform 
data = data %>%
  mutate(ODS_Code = as.character(ODS_Code))

# join works
data = left_join(data, lookup_ods_ons, by = "ODS_Code")

# finalilse and save
data = data %>%
  filter(!is.na(ONS_Code)) %>%
  mutate(
    Year = "2019/2020",
    Quarter = "Q2",
    Timepoint = 1,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(data)

# Save using fwrite()
q1_data = fread(file.path(clean_dir, "COVER_2019_Q1_Cleaned.csv"))
combined_2019 = rbind(q1_data, data)
fwrite(combined_2019, file.path(clean_dir, "COVER_2019_Q1_Q2_Cleaned.csv"))

message("Cleaned data saved to: ", save_file_path)









####🫧 2019 data — Q3 🫧####

# Define file path
file_path = file.path(main_dir, "2019_Q3.ods")

# Load table
data_raw = read_ods(file_path, sheet = "Table_1", skip = 2)

# Preview column names
names(data_raw)

# After confirming, extract only what you want
data = data_raw %>%
  select(`ODS Upper Tier LA Code`, `12m Denominator`, `12m PCV2%`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ODS_Code = `ODS Upper Tier LA Code`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  )

# View
View(data)

# different types so, transform 
data = data %>%
  mutate(ODS_Code = as.character(ODS_Code))

# join works
data = left_join(data, lookup_ods_ons, by = "ODS_Code")

# finalilse and save
data = data %>%
  filter(!is.na(ONS_Code)) %>%
  mutate(
    Year = "2019/2020",
    Quarter = "Q3",
    Timepoint = 2,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(data)

# Save using fwrite()
q2_data = fread(file.path(clean_dir, "COVER_2019_Q1_Cleaned.csv"))
combined_2019 = rbind(q1_data, data)
fwrite(combined_2019, file.path(clean_dir, "COVER_2019_Q1_Q2_Q3_Cleaned.csv"))

message("Cleaned data saved to: ", save_file_path)



####🫧 2019 data — Q4 🫧####

# Define file path
file_path = file.path(main_dir, "2019_Q4.ods")

# Load table
data_raw = read_ods(file_path, sheet = "Table_1", skip = 2)

# Preview column names
names(data_raw)

# After confirming, extract only what you want
data = data_raw %>%
  select(`ODS Upper Tier LA Code`, `12m Denominator`, `12m PCV2%`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ODS_Code = `ODS Upper Tier LA Code`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  )

# View
View(data)

# different types so, transform 
data = data %>%
  mutate(ODS_Code = as.character(ODS_Code))

# join works
data = left_join(data, lookup_ods_ons, by = "ODS_Code")

# finalilse and save
data = data %>%
  filter(!is.na(ONS_Code)) %>%
  mutate(
    Year = "2019/2020",
    Quarter = "Q4",
    Timepoint = 3,
    Vaccine_Schedule = 0
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(data)

# Save using fwrite()
q3_data = fread(file.path(clean_dir, "COVER_2019_Q1_Q2_Q3_Cleaned.csv"))
combined_2019 = rbind(q3_data, merged_LA)
fwrite(combined_2019, file.path(clean_dir, "COVER_2019_Cleaned.csv"))

message("Cleaned data saved to: ", save_file_path)








#### *ੈ✩‧₊˚༺☆༻*ੈ✩‧₊˚ V  A  C  C  I  N  E    C  H  A  N  G  E *ੈ✩‧₊˚༺☆༻*ੈ✩‧₊˚  ####



#### ⋆˚࿔ 2 0 2 0 𝜗𝜚˚ ⋆ ####

####🫧 2020 data — Q1 🫧####

# Define file path
file_path = file.path(main_dir, "2020 Q1.ods")

# Load full sheet (after skipping metadata)
la12_raw = read_ods(file_path, sheet = "12m_UTLA_GOR")

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS Upper Tier LA Code`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  )


la24_raw = read_ods(file_path, sheet = "24m_UTLA_GOR")

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS Upper Tier LA Code`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  )

# View
View(la12)
View(la24)


# Check for duplicates 
la12 %>% count(ONS_Code) %>% filter(n > 1)
la24 %>% count(ONS_Code) %>% filter(n > 1)

# Rows with missing ONS codes
# Not true missing rows, but subheadings in data for some reason 
la12 = la12 %>% filter(!is.na(ONS_Code))
la24 = la24 %>% filter(!is.na(ONS_Code))

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")


# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2020/2021",
    Quarter = "Q1",
    Timepoint = 0,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
save_file_path = file.path(clean_dir, "COVER_2020_Q1_Cleaned.csv")
fwrite(merged_LA, file = save_file_path)

message("Cleaned data saved to: ", save_file_path)


####🫧 2020 data — Q2 🫧####

# Define file path
file_path = file.path(main_dir, "2020 Q2.ods")

# Load full sheet (after skipping metadata)
la12_raw = read_ods(file_path, sheet = "12m_UTLA_GOR")

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS Upper Tier LA Code`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  )


la24_raw = read_ods(file_path, sheet = "24m_UTLA_GOR")

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS Upper Tier LA Code`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  )

# View
View(la12)
View(la24)

# Rows with missing ONS codes
# Not true missing rows, but subheadings in data for some reason 
la12 = la12 %>% filter(!is.na(ONS_Code))
la24 = la24 %>% filter(!is.na(ONS_Code))

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")

# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2020/2021",
    Quarter = "Q2",
    Timepoint = 1,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
q1_data = fread(file.path(clean_dir, "COVER_2020_Q1_Cleaned.csv"))
combined_2020 = rbind(q1_data, merged_LA)
fwrite(combined_2020, file.path(clean_dir, "COVER_2020_Q1_Q2_Cleaned.csv"))

message("Cleaned data saved to: ", save_file_path)

####🫧 2020 data — Q3 🫧####

# Define file path
file_path = file.path(main_dir, "2020 Q3.ods")

# Load full sheet (after skipping metadata)
la12_raw = read_ods(file_path, sheet = "12m_UTLA_GOR")

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS Upper Tier LA Code`, `12m Denominator`, `12m PCV2%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV2%`
  )


la24_raw = read_ods(file_path, sheet = "24m_UTLA_GOR")

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS Upper Tier LA Code`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  )

# View
View(la12)
View(la24)

# Rows with missing ONS codes
# Not true missing rows, but subheadings in data for some reason 
la12 = la12 %>% filter(!is.na(ONS_Code))
la24 = la24 %>% filter(!is.na(ONS_Code))

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")

# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2020/2021",
    Quarter = "Q3",
    Timepoint = 2,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
q1_data = fread(file.path(clean_dir, "COVER_2020_Q1_Q2_Cleaned.csv"))
combined_2020 = rbind(q1_data, merged_LA)
fwrite(combined_2020, file.path(clean_dir, "COVER_2020_Q1_Q2_Q3_Cleaned.csv"))

message("Cleaned data saved to: ", save_file_path)



####🫧 2020 data — Q4 🫧####


# Define file path
file_path = file.path(main_dir, "2020 Q4.ods")

# Load full sheet (after skipping metadata)
la12_raw = read_ods(file_path, sheet = "12m_UTLA_GOR")

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS Upper Tier LA Code`, `12m Denominator`, `12m PCV1%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV1%`
  )


la24_raw = read_ods(file_path, sheet = "24m_UTLA_GOR")

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS Upper Tier LA Code`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  )

# View
View(la12)
View(la24)

# Rows with missing ONS codes
# Not true missing rows, but subheadings in data for some reason 
la12 = la12 %>% filter(!is.na(ONS_Code))
la24 = la24 %>% filter(!is.na(ONS_Code))

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")

# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2020/2021",
    Quarter = "Q4",
    Timepoint = 3,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
q1_data = fread(file.path(clean_dir, "COVER_2020_Q1_Q2_Q3_Cleaned.csv"))
combined_2020 = rbind(q1_data, merged_LA)
fwrite(combined_2020, file.path(clean_dir, "COVER_2020_Cleaned.csv"))

message("Cleaned data saved to: ", save_file_path)




#### ⋆˚࿔ 2 0 2 1 𝜗𝜚˚ ⋆ ####

# Note 1 denominator < 5, so suppressed.

####🫧 2021 data — Q1 🫧####

# Define file path
file_path = file.path(main_dir, "2021 Q1.ods")

# Load full sheet (after skipping metadata)
la12_raw = read_ods(file_path, sheet = "12m_UTLA_GOR")

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS Upper Tier LA Code`, `12m Denominator`, `12m PCV1%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_12m = `12m Denominator`,
    PCV_12m = `12m PCV1%`
  )


la24_raw = read_ods(file_path, sheet = "24m_UTLA_GOR")

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS Upper Tier LA Code`, `24m Denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS Upper Tier LA Code`,
    Population_24m = `24m Denominator`,
    PCV_24m = `24m PCV Booster%`
  )

# View
View(la12)
View(la24)


# Check for duplicates 
la12 %>% count(ONS_Code) %>% filter(n > 1)
la24 %>% count(ONS_Code) %>% filter(n > 1)

# Rows with missing ONS codes
# Not true missing rows, but subheadings in data for some reason 
la12 = la12 %>% filter(!is.na(ONS_Code))
la24 = la24 %>% filter(!is.na(ONS_Code))

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")


# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2021/2022",
    Quarter = "Q1",
    Timepoint = 0,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
save_file_path = file.path(clean_dir, "COVER_2021_Q1_Cleaned.csv")
fwrite(merged_LA, file = save_file_path)

message("Cleaned data saved to: ", save_file_path)


####🫧 2021 data — Q2 🫧####

# Define file path
file_path = file.path(main_dir, "2021 Q2.ods")

# Load full sheet (after skipping metadata)
la12_raw = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 4)

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS UTLA code`, `12m denominator`, `12m PCV1%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    Population_12m = `12m denominator`,
    PCV_12m = `12m PCV1%`
  )


la24_raw = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 4)

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS UTLA code`, `24m denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    Population_24m = `24m denominator`,
    PCV_24m = `24m PCV Booster%`
  )

# View
View(la12)
View(la24)

# Rows with missing ONS codes
# Not true missing rows, but subheadings in data for some reason 
la12 = la12 %>% filter(!is.na(ONS_Code))
la24 = la24 %>% filter(!is.na(ONS_Code))

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")

# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2021/2022",
    Quarter = "Q2",
    Timepoint = 1,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
q1_data = fread(file.path(clean_dir, "COVER_2021_Q1_Cleaned.csv"))
combined_2021 = rbind(q1_data, merged_LA)
fwrite(combined_2021, file.path(clean_dir, "COVER_2021_Q1_Q2_Cleaned.csv"))

message("Cleaned data saved to: ", save_file_path)

####🫧 2021 data — Q3 🫧####

# Define file path
file_path = file.path(main_dir, "2021 Q3.ods")

# Load full sheet (after skipping metadata)
la12_raw = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 4)

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS UTLA code`, `12 month denominator`, `12 month PCV1%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    Population_12m = `12 month denominator`,
    PCV_12m = `12 month PCV1%`
  )


la24_raw = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 4)

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS UTLA code`, `24 month denominator`, `24 month PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    Population_24m = `24 month denominator`,
    PCV_24m = `24 month PCV Booster%`
  )

# View
View(la12)
View(la24)

# Rows with missing ONS codes
# Not true missing rows, but subheadings in data for some reason 
la12 = la12 %>% filter(!is.na(ONS_Code))
la24 = la24 %>% filter(!is.na(ONS_Code))

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")

# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2021/2022",
    Quarter = "Q3",
    Timepoint = 2,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
q1_data = fread(file.path(clean_dir, "COVER_2021_Q1_Q2_Cleaned.csv"))
combined_2021 = rbind(q1_data, merged_LA)
fwrite(combined_2021, file.path(clean_dir, "COVER_2021_Q1_Q2_Q3_Cleaned.csv"))

message("Cleaned data saved to: ", save_file_path)



####🫧 2021 data — Q4 🫧####

# Define file path
file_path = file.path(main_dir, "2021 Q4.ods")

# Load full sheet (after skipping metadata)
la12_raw = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 4)

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS UTLA code`, `12m denominator`, `12m PCV1%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    Population_12m = `12m denominator`,
    PCV_12m = `12m PCV1%`
  )


la24_raw = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 4)

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS UTLA code`, `24m denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    Population_24m = `24m denominator`,
    PCV_24m = `24m PCV Booster%`
  )

# View
View(la12)
View(la24)

# Rows with missing ONS codes
# Not true missing rows, but subheadings in data for some reason 
la12 = la12 %>% filter(!is.na(ONS_Code))
la24 = la24 %>% filter(!is.na(ONS_Code))

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")

# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2021/2022",
    Quarter = "Q4",
    Timepoint = 3,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
q1_data = fread(file.path(clean_dir, "COVER_2021_Q1_Q2_Q3_Cleaned.csv"))
combined_2021 = rbind(q1_data, merged_LA)
fwrite(combined_2021, file.path(clean_dir, "COVER_2021_Cleaned.csv"))

message("Cleaned data saved to: ", save_file_path)





#### ⋆˚࿔ 2 0 2 2 𝜗𝜚˚ ⋆ ####

####🫧 2022 data — Q1 🫧####

# Define file path
file_path = file.path(main_dir, "2022 Q1.ods")

# Load full sheet (after skipping metadata)
la12_raw = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 4)

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS UTLA code`, `12m denominator`, `12m PCV1%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    Population_12m = `12m denominator`,
    PCV_12m = `12m PCV1%`
  )


la24_raw = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 4)

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS UTLA code`, `24m denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    Population_24m = `24m denominator`,
    PCV_24m = `24m PCV Booster%`
  )

# View
View(la12)
View(la24)


# Check for duplicates 
la12 %>% count(ONS_Code) %>% filter(n > 1)
la24 %>% count(ONS_Code) %>% filter(n > 1)

# Rows with missing ONS codes
# Not true missing rows, but subheadings in data for some reason 
la12 = la12 %>% filter(!is.na(ONS_Code))
la24 = la24 %>% filter(!is.na(ONS_Code))

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")


# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2022/2023",
    Quarter = "Q1",
    Timepoint = 0,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
save_file_path = file.path(clean_dir, "COVER_2022_Q1_Cleaned.csv")
fwrite(merged_LA, file = save_file_path)

message("Cleaned data saved to: ", save_file_path)


####🫧 2022 data — Q2 🫧####

# Define file path
file_path = file.path(main_dir, "2022 Q2.ods")

# Load full sheet (after skipping metadata)
la12_raw = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 4)

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS UTLA code`, `12m denominator`, `12m PCV1%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    Population_12m = `12m denominator`,
    PCV_12m = `12m PCV1%`
  )


la24_raw = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 5)

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS UTLA code`, `24m denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    Population_24m = `24m denominator`,
    PCV_24m = `24m PCV Booster%`
  )

# View
View(la12)
View(la24)

# Rows with missing ONS codes
# Not true missing rows, but subheadings in data for some reason 
la12 = la12 %>% filter(!is.na(ONS_Code))
la24 = la24 %>% filter(!is.na(ONS_Code))

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")

# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2022/2023",
    Quarter = "Q2",
    Timepoint = 1,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
q1_data = fread(file.path(clean_dir, "COVER_2022_Q1_Cleaned.csv"))
combined_2022 = rbind(q1_data, merged_LA)
fwrite(combined_2022, file.path(clean_dir, "COVER_2022_Q1_Q2_Cleaned.csv"))

message("Cleaned data saved to: ", save_file_path)

####🫧 2022 data — Q3 🫧####

# Define file path
file_path = file.path(main_dir, "2022 Q3.ods")

# Load full sheet (after skipping metadata)
la12_raw = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 4)

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS UTLA code`, `12m denominator`, `12m PCV1%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    Population_12m = `12m denominator`,
    PCV_12m = `12m PCV1%`
  )


la24_raw = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 5)

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS UTLA code`, `24m denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    Population_24m = `24m denominator`,
    PCV_24m = `24m PCV Booster%`
  )

# View
View(la12)
View(la24)

# Rows with missing ONS codes
# Not true missing rows, but subheadings in data for some reason 
la12 = la12 %>% filter(!is.na(ONS_Code))
la24 = la24 %>% filter(!is.na(ONS_Code))

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")

# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2022/2023",
    Quarter = "Q3",
    Timepoint = 2,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
q1_data = fread(file.path(clean_dir, "COVER_2022_Q1_Q2_Cleaned.csv"))
combined_2022 = rbind(q1_data, merged_LA)
fwrite(combined_2022, file.path(clean_dir, "COVER_2022_Q1_Q2_Q3_Cleaned.csv"))

message("Cleaned data saved to: ", save_file_path)



####🫧 2022 data — Q4 🫧####

# Define file path
file_path = file.path(main_dir, "2022 Q4.ods")

# Load full sheet (after skipping metadata)
la12_raw = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 5)

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS upper tier local authority code`, `12 month denominator`, `12 month PCV1%`) %>%
  rename(
    ONS_Code = `ONS upper tier local authority code`,
    Population_12m = `12 month denominator`,
    PCV_12m = `12 month PCV1%`
  )


la24_raw = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 5)

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS upper tier local authority code`, `24 month denominator`, `24 month PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS upper tier local authority code`,
    Population_24m = `24 month denominator`,
    PCV_24m = `24 month PCV Booster%`
  )

# View
View(la12)
View(la24)

# Rows with missing ONS codes
# Not true missing rows, but subheadings in data for some reason 
la12 = la12 %>% filter(!is.na(ONS_Code))
la24 = la24 %>% filter(!is.na(ONS_Code))

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")

# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2022/2023",
    Quarter = "Q4",
    Timepoint = 3,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
q1_data = fread(file.path(clean_dir, "COVER_2022_Q1_Q2_Q3_Cleaned.csv"))
combined_2022 = rbind(q1_data, merged_LA)
fwrite(combined_2022, file.path(clean_dir, "COVER_2022_Cleaned.csv"))

message("Cleaned data saved to: ", save_file_path)







#### ⋆˚࿔ 2 0 2 3 𝜗𝜚˚ ⋆ ####

####🫧 2023 data — Q1 🫧####

# Define file path
file_path = file.path(main_dir, "2023 Q1.ods")

# Load full sheet (after skipping metadata)
la12_raw = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 5)

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS UTLA code`, `12m denominator`, `12m PCV1%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    Population_12m = `12m denominator`,
    PCV_12m = `12m PCV1%`
  )


la24_raw = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 5)

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS UTLA code`, `24m denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    Population_24m = `24m denominator`,
    PCV_24m = `24m PCV Booster%`
  )

# View
View(la12)
View(la24)


# Check for duplicates 
la12 %>% count(ONS_Code) %>% filter(n > 1)
la24 %>% count(ONS_Code) %>% filter(n > 1)

# Rows with missing ONS codes
# Not true missing rows, but subheadings in data for some reason 
la12 = la12 %>% filter(!is.na(ONS_Code))
la24 = la24 %>% filter(!is.na(ONS_Code))

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")


# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2023/2024",
    Quarter = "Q1",
    Timepoint = 0,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
save_file_path = file.path(clean_dir, "COVER_2023_Q1_Cleaned.csv")
fwrite(merged_LA, file = save_file_path)

message("Cleaned data saved to: ", save_file_path)


####🫧 2023 data — Q2 🫧####

# Define file path
file_path = file.path(main_dir, "2023 Q2.ods")

# Load full sheet (after skipping metadata)
la12_raw = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 6)

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS UTLA code`, `12m denominator`, `12m PCV1%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    Population_12m = `12m denominator`,
    PCV_12m = `12m PCV1%`
  )


la24_raw = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 5)

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS UTLA code`, `24m denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    Population_24m = `24m denominator`,
    PCV_24m = `24m PCV Booster%`
  )

# View
View(la12)
View(la24)

# Rows with missing ONS codes
# Not true missing rows, but subheadings in data for some reason 
la12 = la12 %>% filter(!is.na(ONS_Code))
la24 = la24 %>% filter(!is.na(ONS_Code))

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")

# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2023/2024",
    Quarter = "Q2",
    Timepoint = 1,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
q1_data = fread(file.path(clean_dir, "COVER_2023_Q1_Cleaned.csv"))
combined_2023 = rbind(q1_data, merged_LA)
fwrite(combined_2023, file.path(clean_dir, "COVER_2023_Q1_Q2_Cleaned.csv"))

message("Cleaned data saved to: ", save_file_path)

####🫧 2023 data — Q3 🫧####

# Define file path
file_path = file.path(main_dir, "2023 Q3.ods")

# Load full sheet (after skipping metadata)
la12_raw = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 5)

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS UTLA code`, `12m denominator`, `12m PCV1%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    Population_12m = `12m denominator`,
    PCV_12m = `12m PCV1%`
  )


la24_raw = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 5)

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS UTLA code`, `24m denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    Population_24m = `24m denominator`,
    PCV_24m = `24m PCV Booster%`
  )

# View
View(la12)
View(la24)

# Rows with missing ONS codes
# Not true missing rows, but subheadings in data for some reason 
la12 = la12 %>% filter(!is.na(ONS_Code))
la24 = la24 %>% filter(!is.na(ONS_Code))

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")

# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2023/2024",
    Quarter = "Q3",
    Timepoint = 2,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
q1_data = fread(file.path(clean_dir, "COVER_2023_Q1_Q2_Cleaned.csv"))
combined_2023 = rbind(q1_data, merged_LA)
fwrite(combined_2023, file.path(clean_dir, "COVER_2023_Q1_Q2_Q3_Cleaned.csv"))

message("Cleaned data saved to: ", save_file_path)



####🫧 2023 data — Q4 🫧####

# Define file path
file_path = file.path(main_dir, "2023 Q4.ods")

# Load full sheet (after skipping metadata)
la12_raw = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 5)

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS UTLA code`, `12m denominator`, `12m PCV1%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    Population_12m = `12m denominator`,
    PCV_12m = `12m PCV1%`
  )


la24_raw = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 5)

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS UTLA code`, `24m denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    Population_24m = `24m denominator`,
    PCV_24m = `24m PCV Booster%`
  )

# View
View(la12)
View(la24)

# Rows with missing ONS codes
# Not true missing rows, but subheadings in data for some reason 
la12 = la12 %>% filter(!is.na(ONS_Code))
la24 = la24 %>% filter(!is.na(ONS_Code))

la12 = la12 %>%
  mutate(ONS_Code = trimws(ONS_Code)) %>%
  filter(!is.na(ONS_Code) & ONS_Code != "" & ONS_Code != "[z]")

la24 = la24 %>%
  mutate(ONS_Code = trimws(ONS_Code)) %>%
  filter(!is.na(ONS_Code) & ONS_Code != "" & ONS_Code != "[z]")

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")

# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2023/2024",
    Quarter = "Q4",
    Timepoint = 3,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
q1_data = fread(file.path(clean_dir, "COVER_2023_Q1_Q2_Q3_Cleaned.csv"))
combined_2023 = rbind(q1_data, merged_LA)
fwrite(combined_2023, file.path(clean_dir, "COVER_2023_Cleaned.csv"))

message("Cleaned data saved to: ", save_file_path)





#### ⋆˚࿔ 2 0 2 4 𝜗𝜚˚ ⋆ ####
# [z] not applicable
# [x] data excluded due to data quality issues.
# [c] Some figures have been suppressed due to potential disclosure issues associated with small numbers.


####🫧 2024 data — Q1 🫧####

# Define file path
file_path = file.path(main_dir, "2024 Q1.ods")

# Load full sheet (after skipping metadata)
la12_raw = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 5)

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`ONS UTLA code`, `12m denominator`, `12m PCV1 (%)`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    Population_12m = `12m denominator`,
    PCV_12m = `12m PCV1 (%)`
  )


la24_raw = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 5)

print(names(la24_raw))

la24 = la24_raw %>%
  select(`ONS UTLA code`, `24m denominator`, `24m PCV Booster%`) %>%
  rename(
    ONS_Code = `ONS UTLA code`,
    Population_24m = `24m denominator`,
    PCV_24m = `24m PCV Booster%`
  )

# View
View(la12)
View(la24)


# Check for duplicates 
la12 %>% count(ONS_Code) %>% filter(n > 1)
la24 %>% count(ONS_Code) %>% filter(n > 1)

# Rows with missing ONS codes
# Not true missing rows, but subheadings in data for some reason 
la12 = la12 %>% filter(!is.na(ONS_Code))
la24 = la24 %>% filter(!is.na(ONS_Code))

la12 = la12 %>%
  mutate(ONS_Code = trimws(ONS_Code)) %>%
  filter(!is.na(ONS_Code) & ONS_Code != "" & ONS_Code != "[z]")

la24 = la24 %>%
  mutate(ONS_Code = trimws(ONS_Code)) %>%
  filter(!is.na(ONS_Code) & ONS_Code != "" & ONS_Code != "[z]")

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")


# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2024/2025",
    Quarter = "Q1",
    Timepoint = 0,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
save_file_path = file.path(clean_dir, "COVER_2024_Q1_Cleaned.csv")
fwrite(merged_LA, file = save_file_path)

message("Cleaned data saved to: ", save_file_path)


####🫧 2024 data — Q2 🫧####
# Define file path
file_path = file.path(main_dir, "2024 Q2.ods")

# Load full sheet (after skipping metadata)
la12_raw = read_ods(file_path, sheet = "12m_UTLA_GOR", skip = 6)

# See the column names
print(names(la12_raw))  # or just `names(la12_raw)`

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`Code`, `Number of children who reached 12 months in reporting quarter`, `Coverage at 12 months PCV1 (%)`) %>%
  rename(
    ONS_Code = `Code`,
    Population_12m = `Number of children who reached 12 months in reporting quarter`,
    PCV_12m = `Coverage at 12 months PCV1 (%)`
  )


la24_raw = read_ods(file_path, sheet = "24m_UTLA_GOR", skip = 6)

print(names(la24_raw))

la24 = la24_raw %>%
  select(`Code`, `Number of children who reached 24 months in reporting quarter`, `Coverage at 24 months PCV Booster (%)`) %>%
  rename(
    ONS_Code = `Code`,
    Population_24m = `Number of children who reached 24 months in reporting quarter`,
    PCV_24m = `Coverage at 24 months PCV Booster (%)`
  )

# View
View(la12)
View(la24)

# Rows with missing ONS codes
# Not true missing rows, but subheadings in data for some reason 
la12 = la12 %>% filter(!is.na(ONS_Code))
la24 = la24 %>% filter(!is.na(ONS_Code))
la12 = la12 %>%
  mutate(ONS_Code = trimws(ONS_Code)) %>%
  filter(!is.na(ONS_Code) & ONS_Code != "" & ONS_Code != "[z]")

la24 = la24 %>%
  mutate(ONS_Code = trimws(ONS_Code)) %>%
  filter(!is.na(ONS_Code) & ONS_Code != "" & ONS_Code != "[z]")

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")

# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2024/2025",
    Quarter = "Q2",
    Timepoint = 1,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
q1_data = fread(file.path(clean_dir, "COVER_2024_Q1_Cleaned.csv"))
combined_2024 = rbind(q1_data, merged_LA)
fwrite(combined_2024, file.path(clean_dir, "COVER_2024_Q1_Q2_Cleaned.csv"))

message("Cleaned data saved to: ", save_file_path)


####🫧 2018 data — Q3 🫧####
# Define file path
file_path = file.path(main_dir, "2024 Q3.ods")

# Load full sheet (after skipping metadata)
la12_raw = read_ods(file_path, sheet = "Table5", skip = 5)

# See the column names
print(names(la12_raw))

# After confirming, extract only what you want
la12 = la12_raw %>%
  select(`Code`, `Number of children who reached 12 months in reporting quarter`, `Coverage at 12 months PCV1 (%)`) %>%
  rename(
    ONS_Code = `Code`,
    Population_12m = `Number of children who reached 12 months in reporting quarter`,
    PCV_12m = `Coverage at 12 months PCV1 (%)`
  )


la24_raw = read_ods(file_path, sheet = "Table6", skip = 4)

print(names(la24_raw))

la24 = la24_raw %>%
  select(`Code`, `Number of children who reached 24 months in reporting quarter`, `Coverage at 24 months PCV Booster (%)`) %>%
  rename(
    ONS_Code = `Code`,
    Population_24m = `Number of children who reached 24 months in reporting quarter`,
    PCV_24m = `Coverage at 24 months PCV Booster (%)`
  )

# View
View(la12)
View(la24)

# Rows with missing ONS codes
# Not true missing rows, but subheadings in data for some reason 
la12 = la12 %>% filter(!is.na(ONS_Code))
la24 = la24 %>% filter(!is.na(ONS_Code))

# Merge by ONS code
merged_LA = full_join(la12, la24, by = "ONS_Code")

# Add metadata 
merged_LA = merged_LA %>%
  mutate(
    Year = "2024/2024",
    Quarter = "Q3",
    Timepoint = 2,
    Vaccine_Schedule = 1
  ) %>%
  select(ONS_Code, PCV_12m, PCV_24m, Population_12m, Population_24m, Year, Quarter, Timepoint, Vaccine_Schedule)

View(merged_LA)

# Save using fwrite()
q2_data = fread(file.path(clean_dir, "COVER_2024_Q1_Q2_Cleaned.csv"))
combined_2024 = rbind(q2_data, merged_LA)
fwrite(combined_2024, file.path(clean_dir, "COVER_2024_Q1_Q2_Q3_Cleaned.csv"))

message("Cleaned data saved to: ", save_file_path)

