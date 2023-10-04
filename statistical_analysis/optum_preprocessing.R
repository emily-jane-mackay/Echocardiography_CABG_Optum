## ---------------------------
##
##
## Optum data preprocessing
##
## Author: Charlotte Talham
##
## Date Created: 2023-04-24
##
##
## ---------------------------
##
## Notes: This file performs data cleaning of Optum dataset and variable
## creation to prepare for statistical matching.
##   
## ---------------------------
library(tidyverse)
library(tableone)

opt_full <- read_csv("Optum_data.csv")

# Cohort Development ------------------------------------------------------
# Starting count 195,826

# Exclude years before 2016 and additional exclusion criteria
# (exclude variable coded in SAS.)
# N = 68,533
opt <- opt_full %>% filter(exclude_1 == 0)


# Variable renaming -------------------------------------------------------

# Create single hypertension and diabetes variables.
opt$elix_htn_b <- ifelse(opt$elix_htn_un_cx_b == 1 | opt$elix_htn_cx_b == 1, 
                         1, 0)
opt$elix_diab_b <- ifelse(opt$elix_diab_un_cx_b == 1 | opt$elix_diab_cx_b == 1, 
                          1, 0)

# Recode gender and race/eth variables.
opt$gender <- ifelse(opt$gender_e == "M", 1, 0)
opt$race_eth <- as.factor(opt$race_e)
opt$asian <- ifelse(opt$race_e == "Asian", 1, 0)
opt$black <- ifelse(opt$race_e == "Black", 1, 0)
opt$hispanic <- ifelse(opt$race_e == "Hispanic", 1, 0)
opt$unknown <- ifelse(opt$race_e == "Unknown", 1, 0)
opt$white <- ifelse(opt$race_e == "White", 1, 0)


# Subset data by surgery type ---------------------------------------------

# Create subset of isolated CABG surgeries. Select variables needed for
# matching and analysis.
iso_cabg <- opt %>% filter(isolated_cabg == 1) %>% 
  select(c(TEE_intraop, TEE, death_30, death_30_or_stroke, complic_stroke, 
           afib, elix_chf_b, elix_arrhy_b, cabg_ima, cabg_four, cabg_three, 
           cabg_two, cabg_one, isolated_cabg, age_years, 
           gender, race_eth, asian, black, hispanic, unknown, white, yr, 
           surgical_volume_npi_2016_2020, elix_pcd_b, elix_pvd_b, elix_htn_b, 
           elix_fluid_elec_b, elix_neurol_b, elix_cpd_b, elix_diab_b, 
           elix_renal_fail_b, elix_liver_b, elix_coag_b, elix_anemia_iron_b, 
           elix_valvular_b, elix_anemia_bld_loss_b)) # N = 42,249

# Create variable for exact match ----------------------------------

# Making no. of vessels mutually exclusive
iso_cabg$ves_four <- ifelse(iso_cabg$cabg_four == 1, 1, 0)
iso_cabg$ves_three <- ifelse(iso_cabg$cabg_three == 1 & 
                               iso_cabg$cabg_four == 0, 1, 0)
iso_cabg$ves_two <- ifelse(iso_cabg$cabg_two == 1 & iso_cabg$cabg_three == 0 & 
                        iso_cabg$cabg_four == 0, 1, 0)
iso_cabg$ves_one <- ifelse(iso_cabg$cabg_one == 1 & iso_cabg$cabg_two == 0 & 
                        iso_cabg$cabg_three == 0 & 
                          iso_cabg$cabg_four == 0, 1, 0)
iso_cabg$ves_three_plus <- ifelse(iso_cabg$ves_three == 1 | 
                                    iso_cabg$ves_four == 1, 1, 0)

# Define risk group
# Low: 1 vessel + IMA #9,350
# High: otherwise #32,899
iso_cabg$low_rsk <- ifelse(iso_cabg$ves_one == 1 & iso_cabg$cabg_ima == 1, 
                           1, 0)

# Make exact match variable
iso_cabg$exact <- paste0(iso_cabg$low_rsk, iso_cabg$elix_chf_b)
iso_cabg$exact_dec <- strtoi(iso_cabg$exact, base = 2)



# Export ------------------------------------------------------------------

write_csv(iso_cabg, "iso_cabg.csv")

