# DASC500-Dissertation---Inequalities-in-Childhood-Pneumococcal-Vaccine-Uptake-in-England
This repository contains the code, data processing, and analysis for the scientific paper: "Inequalities in childhood pneumococcal conjugate vaccine uptake in England before and after the change from a 2+1 to 1+1 schedule: a longitudinal study" by Praise Ilechukwu, Daniel Hungerford, Neil French and Edward M. Hill.

Preprint details: P Ilechukwu, D Hungerford, N French, EM Hill. (2025) Inequalities in childhood pneumococcal conjugate vaccine uptake in England before and after the change from a 2+1 to 1+1 schedule: a longitudinal study. *medRxiv*. doi: 10.64898/2025.12.05.25341691. URL: [https://www.medrxiv.org/content/10.64898/2025.12.05.25341691v1](https://www.medrxiv.org/content/10.64898/2025.12.05.25341691v1)

Zenodo DOI for the code release associated with the preprint version of the manuscript:
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17854535.svg)](https://doi.org/10.5281/zenodo.17854535)

Peer-reviewed publication details: P Ilechukwu, D Hungerford, N French, EM Hill. (2026) Inequalities in childhood pneumococcal conjugate vaccine uptake in England before and after the change from a 2+1 to 1+1 schedule: a longitudinal study. *The Lancet Regional Health - Europe*. doi: 10.1016/j.lanepe.2026.101667. URL: [https://doi.org/10.1016/j.lanepe.2026.101667](https://doi.org/10.1016/j.lanepe.2026.101667)

Zenodo DOI for the code release associated with the peer-reviewed publication version of the manuscript:
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.19225364.svg)](https://doi.org/10.5281/zenodo.19225364) 

This Zeonodo DOI represents all versions of the archvied code repository and will always resolve to the latest version: 
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17850025.svg)](https://doi.org/10.5281/zenodo.17850025)

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
- output/            # Figures, data processing and model outputs
- README.md          # Project documentation

