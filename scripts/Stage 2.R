#### 🫧 Load packages ####
library("readxl")
library("readr")
library("dplyr")
library("ggplot2")
library("RColorBrewer")
library("tidyr")
library("ggrepel")

setwd("C:/Users/User/OneDrive/HSD MSC/Diss Data/Stage 2 data") ## CHANGE WD


#######################################
#######################################

### 🫧 Loading UTLA Summary data #### 

# make sure to set working directory

# Check for sheets in file
excel_sheets("UTLA_summaries.xlsx")

# extract the sheet we need
imd_data = read_excel("UTLA_summaries.xlsx", sheet = "IMD") 

# view column names
names(imd_data)

# Extract columns needed
imd_subset = imd_data[, c("Upper Tier Local Authority District code (2019)", "Upper Tier Local Authority District name (2019)", "IMD - Average score")]

# Rename for clarity
colnames(imd_subset) = c("utla_code", "utla_name", "imd_score")

write.csv(imd_subset, "cleaned_imd_data.csv", row.names = FALSE)

#### 🫧 Join all cleaned data ####
files = list.files(pattern = "COVER_\\d{4}.*_Cleaned\\.csv")

cover_all = bind_rows(lapply(files, function(file) {
  # Read everything as character to avoid type mismatch
  df = read_csv(file, col_types = cols(.default = "c"))
  
  return(df)
}))


# check
head(cover_all)

# convert all Nas
cover_all = cover_all %>%
  mutate(across(
    everything(),
    ~ ifelse(grepl("^N", .x), NA, .x)
  ))

# replace anything starting with '['

cover_all = cover_all %>%
  mutate(across(
    everything(),
    ~ ifelse(grepl("^\\[", .x), NA, .x)
  ))

# Fix column characteristics 

cover_all = cover_all%>%
  mutate(
    PCV_12m = as.numeric(PCV_12m),
    PCV_24m = as.numeric(PCV_24m),
    Population_12m = as.numeric(Population_12m),
    Population_24m = as.numeric(Population_24m),
    Timepoint = as.factor(Timepoint),
    Quarter = as.factor(Quarter),
    Vaccine_Schedule = as.factor(Vaccine_Schedule)
  )


# check
str(cover_all)

# check missing values 
sum(is.na(cover_all))
colSums(is.na(cover_all))
round(colMeans(is.na(cover_all)) * 100, 2)

summary(cover_all)


######################### 
#########################

#### 🫧 To imput or not to imput ####
# first save dataset before imputing
write.csv(cover_all, "COVER_All_Years_UNIMPUTED.csv", row.names = FALSE)

cover_all_imputed = cover_all %>%
  group_by(Vaccine_Schedule) %>%
  mutate(
    PCV_12m = ifelse(is.na(PCV_12m), mean(PCV_12m, na.rm = TRUE), PCV_12m),
    PCV_24m = ifelse(is.na(PCV_24m), mean(PCV_24m, na.rm = TRUE), PCV_24m),
    Population_12m = ifelse(is.na(Population_12m), median(Population_12m, na.rm = TRUE), Population_12m),
    Population_24m = ifelse(is.na(Population_24m), median(Population_24m, na.rm = TRUE), Population_24m)
  ) %>%
  ungroup()
# mean for pcv since it's % uptake
# medial for population since it's more likely to be skewed

write.csv(cover_all_imputed, "COVER_All_Years_IMPUTED.csv", row.names = FALSE)




#######################################
#######################################

#### 🫧 Merge imd to cover####
# load saved data and check it
cover = read.csv("COVER_All_Years_IMPUTED.csv")
imd = read.csv("cleaned_imd_data.csv")

head(cover$ONS_Code)
head(imd$utla_code)

class(cover$ONS_Code)
class(imd$utla_code)

#trim white space
cover$ONS_Code = trimws(cover$ONS_Code)
imd$utla_code = trimws(imd$utla_code)

# check mismatch
setdiff(cover$ONS_Code, imd$utla_code)
setdiff(imd$utla_code, cover$ONS_Code) # City of London and Isle of Scilly

# Drop rows that don’t match IMD data
cover = cover %>%
  filter(ONS_Code %in% imd$utla_code)


cover_merged = cover %>%
  left_join(imd, by = c("ONS_Code" = "utla_code")) %>%
  mutate(imd_quintile = ntile(imd_score, 5))

View(cover_merged)

cover_merged %>%
  distinct(ONS_Code, imd_quintile) %>%
  count(imd_quintile)

write.csv(cover_merged, "COVER_All_Years_MERGED_WITH_IMD.csv", row.names = FALSE)


