# README

#---
# Code Files
#---

## Code Files - SAS 

#### 1) 1_Optum_macro_2021_10_03.sas
Macro -- loops through all quarters of Optum data & appends data to each of three data files and saves to three data files to temporary library. 

#### 2) 2_Data_reshape_tee_2021_10_03.sas
reshapes TEE cpt codes & provider type (anesthesiologists and cardiologists) to single row and multiple columns.

#### 3) 3_Data_reshape_srg_2021_10_03.sas
reshapes surgical cpt codes (including VAECMO CPT codes) to single row and multiple columns.

#### 4) 4_Medical_file_merge_2021_10_03.sas
merges processed surgical medical data file & tee medical data file into the confinement source file (single row per patient-hospitalization).

#### 5) 5_Elixhauser_outcomes_2022_07_07.sas 
codes all Elixhausers (with logic), codes outcomes (with logic), combines Icd9/10, collapses into one patient-hospitalizaiton (containing comorbidities and outcomes that meet logical conditions specified).

#### 6) 6_Surgical_categorize_2021_10_03.sas
codes all index cardiac surgeries (with logic), combines Icd9p/10p codes, collapses into one patient-hospitalizaiton (containing index surgeries that meet logical conditions specified).

#### 7) 7_Elixhauser_Surgical_merge_2022_07_07.sas
merges the Elixhauser/outcomes-coded file and the index surgeries-coded file into the confinement source file.

#### 8) 8_Merge_all_2022_07_07.sas
sequentially merges datasets from above steps and outputs entire dataset to a single .csv file. 

## Code Files - STATA

#### 1) 9_working_optum_2023_04_25.do
processes, cleans, develops cohort in preparation for statistical analysis
