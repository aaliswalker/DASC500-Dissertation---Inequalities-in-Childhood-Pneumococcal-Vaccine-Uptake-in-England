# DASC500-Dissertation---Inequalities-in-Childhood-Pneumococcal-Vaccine-Uptake-in-England
This repository contains the code, data processing, and analysis for my MSc Health Data Science dissertation:  "Examining trends and inequalities in childhood pneumococcal vaccine uptake in England: schedule changes and implications for protection amongst childhood cohorts."

## 🌸 Aims

🫧 Investigate trends and inequalities in uptake of the pneumococcal conjugate vaccine (PCV) across two immunisation schedules:

  - 2+1 schedule: 8, 16 weeks + booster at 12 months (pre-2020).
  
  - 1+1 schedule: 12 weeks + booster at 12 months (post-2020).

🫧 Estimate and compare disease susceptibility between childhood cohorts under the two schedules by combining vaccine uptake data with published vaccine effectiveness evidence.

## 🌸 Data Sources

🫧 UKHSA COVER Programme – Cover of Vaccination Evaluated Rapidly (uptake data).

🫧 ONS Population Estimates – demographic and population data.

🫧 Published Literature – vaccine effectiveness studies and systematic reviews.

## 🌸 Methods

🫧 Descriptive analysis and statistical modelling of PCV uptake.

🫧 Integration of vaccine effectiveness estimates with uptake trends to assess susceptibility.

🫧 Inequality analysis – exploring variation by geography and deprivation.

## 🌸 Repository Structure
- cleaned_Data/      # Processed data files
- data/              # Data sources (publicly available)
- scripts/           # R scripts for data cleaning, analysis, and plotting
  - 01_clean_and_process_COVER_data.R                    #Clean and process quarterly PCV vaccine uptake data from COVER programme
  - 02_link_IMD_and_create_geographic_plots.R            #Harmonising COVER and IMD datasets, preparing them for analysis
  - 03_descriptive_statistics_and_deprivation_analysis.R #Generate descriptive statistics and visualisations for PCV uptake patterns by deprivation
  - 04_susceptibility_modeling.R                         #Calculate and visualise pneumococcal disease susceptibility patterns
- results/           # Figures, tables, and model outputs
- README.md          # Project documentation

