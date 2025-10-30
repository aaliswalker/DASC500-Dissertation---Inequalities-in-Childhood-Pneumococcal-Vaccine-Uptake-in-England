# 🌸 COVER Data

The COVER (Cover of Vaccination Evaluated Rapidly) programme is published by the UK Health Security Agency (UKHSA). It provides quarterly reports on childhood vaccination uptake across England, broken down by local authority and other demographic indicators.

For this project, I used the COVER PCV (pneumococcal conjugate vaccine) uptake data from multiple years. 
Since the raw datasets vary slightly between releases (e.g., column names, formatting, and coding), I carried out data cleaning and renaming steps to make them consistent for analysis This will be in scripts.

🫧 Harmonised column names (e.g., PCV_12m, PCV_24m, ONS_Code, utla_name, imd_quintile).
🫧 Standardised local authority codes and names across years to allow merging with demographic data.
🫧 Merged quarterly files into a single, long-format dataset covering all years of interest.

# 🌸 IMD Data

To assess inequalities in vaccine uptake, I linked the COVER dataset with the Index of Multiple Deprivation (IMD).

The IMD is published by the UK Government (Ministry of Housing, Communities & Local Government / ONS) and provides a composite measure of deprivation at small-area level, ranking neighbourhoods in England from most to least deprived. It is widely used in public health research to study social inequalities.

For this project:

🫧 I mapped local authority codes from COVER data to their corresponding IMD quintiles (from most deprived = Quintile 1, to least deprived = Quintile 5).
🫧 Some renaming and recoding was necessary to ensure consistent matching between datasets (e.g., differences in formatting of local authority codes/names across years).
🫧 Once harmonised, IMD quintiles were used to stratify vaccination uptake and highlight inequalities in PCV coverage.
