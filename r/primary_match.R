## ---------------------------
##
## Statistical matching for primary analysis
##
## Author: Charlotte Talham
##
## Date Created: 2023-05-14
##
##
## ---------------------------
##
## Notes: Statistical matching procedure for Optum isolated CABG patients
## using match2C.
##   
## ---------------------------

library(match2C)
library(mvtnorm)
library(rigr)
library(tidyverse)
library(tableone)
source("create_list_from_scratch.R")

cabg <- read_csv("iso_cabg.csv")

# 'Exposure' for statistical matching is no intraoperative TEE.
iso_cabg$exposure <- ifelse(iso_cabg$TEE_intraop == 1, 0, 1)

cabg16 <- cabg %>% filter(yr == 2016)
cabg17 <- cabg %>% filter(yr == 2017)
cabg18 <- cabg %>% filter(yr == 2018)
cabg19 <- cabg %>% filter(yr == 2019)


# Examine TEE vs no TEE before matching -----------------------------------

covars <- c("age_years", "gender", "asian", "black", "hispanic", "unknown", 
            "white", "elix_pcd_b", "elix_pvd_b", "elix_htn_b", 
            "elix_fluid_elec_b", "elix_neurol_b", "elix_cpd_b", "elix_diab_b", 
            "elix_renal_fail_b", "elix_liver_b", "elix_coag_b", 
            "elix_anemia_iron_b", "elix_valvular_b", "elix_anemia_bld_loss_b", 
            "elix_chf_b", "elix_arrhy_b", "surgical_volume_npi_2016_2020", 
            "ves_one", "ves_two", "ves_three_plus", "cabg_ima", "low_rsk", 
            "death_30", "death_30_or_stroke", 
            "complic_stroke", "afib", "yr")
catVars <- c("gender", "asian", "black", "hispanic", "unknown", 
             "white", "elix_pcd_b", "elix_pvd_b", "elix_htn_b", 
             "elix_fluid_elec_b", "elix_neurol_b", "elix_cpd_b", "elix_diab_b", 
             "elix_renal_fail_b", "elix_liver_b", "elix_coag_b", 
             "elix_anemia_iron_b", "elix_valvular_b", "elix_anemia_bld_loss_b", 
             "elix_chf_b", "elix_arrhy_b", "ves_one", "ves_two", 
             "ves_three_plus", "cabg_ima", "low_rsk")

tb_before_matching16 = tableone::CreateTableOne(vars = covars, 
                                                strata = 'exposure', 
                                                data = cabg16)
tb_before_matching17 = tableone::CreateTableOne(vars = covars, 
                                                strata = 'exposure', 
                                                data = cabg17)
tb_before_matching18 = tableone::CreateTableOne(vars = covars, 
                                                strata = 'exposure', 
                                                data = cabg18)
tb_before_matching19 = tableone::CreateTableOne(vars = covars, 
                                                strata = 'exposure', 
                                                data = cabg19)
tb_before_matching = tableone::CreateTableOne(vars = covars, 
                                              factorVars = catVars,
                                              strata = 'exposure', 
                                              data = cabg, 
                                              addOverall = TRUE)


# 2016 match -------------------------------------------------------

# Covariates to be matched
X <- as.matrix(cabg16[,c("exact_dec", "age_years",
                         "gender", "asian", "black", "hispanic", "white", 
                         "surgical_volume_npi_2016_2020", "elix_pcd_b",
                         "elix_pvd_b", "elix_htn_b", "elix_fluid_elec_b", 
                         "elix_neurol_b", "elix_cpd_b", "elix_diab_b", 
                         "elix_renal_fail_b", "elix_liver_b", "elix_coag_b", 
                         "elix_anemia_iron_b", "elix_valvular_b", 
                         "elix_anemia_bld_loss_b", "elix_arrhy_b")])

# Exposure is no TEE
Z <- cabg16$exposure

# Fit a propensity score model
attach(cabg16)
propensity = glm(exposure ~ age_years + gender + asian + black + hispanic + 
                   white + surgical_volume_npi_2016_2020 + 
                   elix_pcd_b + elix_pvd_b + elix_htn_b + elix_fluid_elec_b + 
                   elix_neurol_b + elix_cpd_b + elix_diab_b + 
                   elix_renal_fail_b + elix_liver_b + elix_coag_b + 
                   elix_anemia_iron_b + elix_valvular_b + 
                   elix_anemia_bld_loss_b, family = binomial)$fitted.values
detach(cabg16)

cabg16$propensity <- propensity



#Create user-specified distance list for LHS using exact match.
dist_list_left <- create_list_from_scratch(Z, X, exact = c("exact_dec"), 
                                           p = propensity, caliper_low = .3,
                                           k = 300)

dist_list_left$d <- 1e2 * dist_list_left$d

# Create user specified distance list for RHS
dist_list_right = create_list_from_scratch(Z = Z, X = propensity,
                                           p = propensity,
                                           caliper_low = 0.3,
                                           k = 500,
                                           method = 'L1')

dist_list_right_fb = create_list_from_scratch(Z = Z, X = X[, c('elix_arrhy_b')],
                                              p = propensity,
                                              caliper_low = 0.3,
                                              k = 500,
                                              method = '0/1')

dist_list_right$d = 100*(1e2*dist_list_right$d + 1e4*dist_list_right_fb$d)

# Perform 2016 match
set.seed(33)
start_time <- Sys.time()
matching_output16 = match_2C_list(Z = Z, dataset = cabg16,
                                  dist_list_1 = dist_list_left,
                                  dist_list_2 = dist_list_right,
                                  lambda = 100,
                                  controls = 1)
end_time <- Sys.time()
end_time - start_time


# Check match quality
tb_after_match16 = check_balance(Z = Z, matching_output16, 
                                 cov_list = c("age_years", "gender", "asian", 
                                              "black", "hispanic", "unknown", 
                                              "white", "elix_pcd_b", 
                                              "elix_pvd_b", "elix_htn_b", 
                                              "elix_fluid_elec_b", 
                                              "elix_neurol_b", "elix_cpd_b", 
                                              "elix_diab_b", 
                                              "elix_renal_fail_b", 
                                              "elix_liver_b", "elix_coag_b", 
                                              "elix_anemia_iron_b", 
                                              "elix_valvular_b", 
                                              "elix_anemia_bld_loss_b", 
                                              "elix_chf_b", "elix_arrhy_b", 
                                              "surgical_volume_npi_2016_2020",
                                              "low_rsk"), 
                                 plot_propens = T, propens = propensity) 


dt16_matched <- matching_output16$data_with_matched_set_ind[!is.na(matching_output16$data_with_matched_set_ind$matched_set),]




# 2017 Match --------------------------------------------------------------


X <- as.matrix(cabg17[,c("exact_dec", "age_years", 
                         "gender", "asian", "black", "hispanic", "white", 
                         "surgical_volume_npi_2016_2020", "elix_pcd_b",
                         "elix_pvd_b", "elix_htn_b", "elix_fluid_elec_b", 
                         "elix_neurol_b", "elix_cpd_b", "elix_diab_b", 
                         "elix_renal_fail_b", "elix_liver_b", "elix_coag_b", 
                         "elix_anemia_iron_b", "elix_valvular_b", 
                         "elix_anemia_bld_loss_b", "elix_arrhy_b")])

# Exposure is no TEE
Z <- cabg17$exposure

# Fit a propensity score model
attach(cabg17)
propensity = glm(exposure ~ age_years + gender + asian + black + hispanic + 
                   white + surgical_volume_npi_2016_2020 + 
                   elix_pcd_b + elix_pvd_b + elix_htn_b + elix_fluid_elec_b + 
                   elix_neurol_b + elix_cpd_b + elix_diab_b + 
                   elix_renal_fail_b + elix_liver_b + elix_coag_b + 
                   elix_anemia_iron_b + elix_valvular_b + 
                   elix_anemia_bld_loss_b, family = binomial)$fitted.values
detach(cabg17)

cabg17$propensity <- propensity



#Create user-specified distance list for LHS using exact match.
dist_list_left <- create_list_from_scratch(Z, X, exact = c("exact_dec"), 
                                           p = propensity, caliper_low = .3,
                                           k = 300)

dist_list_left$d <- 1e2 * dist_list_left$d

# Create user specified distance list for RHS
dist_list_right = create_list_from_scratch(Z = Z, X = propensity,
                                           p = propensity,
                                           caliper_low = 0.3,
                                           k = 500,
                                           method = 'L1')

dist_list_right_fb = create_list_from_scratch(Z = Z, X = X[, c('elix_arrhy_b')],
                                              p = propensity,
                                              caliper_low = 0.3,
                                              k = 500,
                                              method = '0/1')

dist_list_right$d = 100*(1e2*dist_list_right$d + 1e4*dist_list_right_fb$d)

# Perform 2017 match
set.seed(33)
start_time <- Sys.time()
matching_output17 = match_2C_list(Z = Z, dataset = cabg17,
                                  dist_list_1 = dist_list_left,
                                  dist_list_2 = dist_list_right,
                                  lambda = 100,
                                  controls = 1)
end_time <- Sys.time()
end_time - start_time


# Check match quality
tb_after_match17 = check_balance(Z = Z, matching_output17, 
                                 cov_list = c("age_years", "gender", "asian", 
                                              "black", "hispanic", "unknown", 
                                              "white", "elix_pcd_b", 
                                              "elix_pvd_b", "elix_htn_b", 
                                              "elix_fluid_elec_b", 
                                              "elix_neurol_b", "elix_cpd_b", 
                                              "elix_diab_b", 
                                              "elix_renal_fail_b", 
                                              "elix_liver_b", "elix_coag_b", 
                                              "elix_anemia_iron_b", 
                                              "elix_valvular_b", 
                                              "elix_anemia_bld_loss_b", 
                                              "elix_chf_b", "elix_arrhy_b", 
                                              "surgical_volume_npi_2016_2020",
                                              "low_rsk"), 
                                 plot_propens = T, propens = propensity) 


dt17_matched <- matching_output17$data_with_matched_set_ind[!is.na(matching_output17$data_with_matched_set_ind$matched_set),]



# 2018 Match --------------------------------------------------------------

X <- as.matrix(cabg18[,c("exact_dec", "age_years", 
                         "gender", "asian", "black", "hispanic", "white", 
                         "surgical_volume_npi_2016_2020", "elix_pcd_b",
                         "elix_pvd_b", "elix_htn_b", "elix_fluid_elec_b", 
                         "elix_neurol_b", "elix_cpd_b", "elix_diab_b", 
                         "elix_renal_fail_b", "elix_liver_b", "elix_coag_b", 
                         "elix_anemia_iron_b", "elix_valvular_b", 
                         "elix_anemia_bld_loss_b", "elix_arrhy_b")])

# Exposure variable. Treated is no TEE group.
Z <- cabg18$exposure

# Fit a propensity score model
attach(cabg18)
propensity = glm(exposure ~ age_years + gender + asian + black + hispanic + 
                   white + surgical_volume_npi_2016_2020 + 
                   elix_pcd_b + elix_pvd_b + elix_htn_b + elix_fluid_elec_b + 
                   elix_neurol_b + elix_cpd_b + elix_diab_b + 
                   elix_renal_fail_b + elix_liver_b + elix_coag_b + 
                   elix_anemia_iron_b + elix_valvular_b + 
                   elix_anemia_bld_loss_b, family = binomial)$fitted.values
detach(cabg18)

cabg18$propensity <- propensity



#Create user-specified distance list for LHS using exact match.
dist_list_left <- create_list_from_scratch(Z, X, exact = c("exact_dec"), 
                                           p = propensity, caliper_low = .3,
                                           k = 300)

dist_list_left$d <- 1e2 * dist_list_left$d

# Create user specified distance list for RHS
dist_list_right = create_list_from_scratch(Z = Z, X = propensity,
                                           p = propensity,
                                           caliper_low = 0.3,
                                           k = 500,
                                           method = 'L1')

dist_list_right_fb = create_list_from_scratch(Z = Z, X = X[, c('elix_arrhy_b')],
                                              p = propensity,
                                              caliper_low = 0.3,
                                              k = 500,
                                              method = '0/1') #Fine balance

dist_list_right$d = 100*(1e2*dist_list_right$d + 1e4*dist_list_right_fb$d)

# Perform 2018 match
set.seed(33)
start_time <- Sys.time()
matching_output18 = match_2C_list(Z = Z, dataset = cabg18,
                                  dist_list_1 = dist_list_left,
                                  dist_list_2 = dist_list_right,
                                  lambda = 100,
                                  controls = 1)
end_time <- Sys.time()
end_time - start_time


# Check match quality
tb_after_match18 = check_balance(Z = Z, matching_output18, 
                                 cov_list = c("age_years", "gender", "asian", 
                                              "black", "hispanic", "unknown", 
                                              "white", "elix_pcd_b", 
                                              "elix_pvd_b", "elix_htn_b", 
                                              "elix_fluid_elec_b", 
                                              "elix_neurol_b", "elix_cpd_b", 
                                              "elix_diab_b", 
                                              "elix_renal_fail_b", 
                                              "elix_liver_b", "elix_coag_b", 
                                              "elix_anemia_iron_b", 
                                              "elix_valvular_b", 
                                              "elix_anemia_bld_loss_b", 
                                              "elix_chf_b", "elix_arrhy_b", 
                                              "surgical_volume_npi_2016_2020",
                                              "low_rsk"), 
                                 plot_propens = T, propens = propensity) 


dt18_matched <- matching_output18$data_with_matched_set_ind[!is.na(matching_output18$data_with_matched_set_ind$matched_set),]




# 2019 Match --------------------------------------------------------------

X <- as.matrix(cabg19[,c("exact_dec", "age_years", 
                         "gender", "asian", "black", "hispanic", "white", 
                         "surgical_volume_npi_2016_2020", "elix_pcd_b",
                         "elix_pvd_b", "elix_htn_b", "elix_fluid_elec_b", 
                         "elix_neurol_b", "elix_cpd_b", "elix_diab_b", 
                         "elix_renal_fail_b", "elix_liver_b", "elix_coag_b", 
                         "elix_anemia_iron_b", "elix_valvular_b", 
                         "elix_anemia_bld_loss_b", "elix_arrhy_b")])

# Exposure variable. Treated is no TEE group.
Z <- cabg19$exposure

# Fit a propensity score model
attach(cabg19)
propensity = glm(exposure ~ age_years + gender + asian + black + hispanic + 
                   white + surgical_volume_npi_2016_2020 + 
                   elix_pcd_b + elix_pvd_b + elix_htn_b + elix_fluid_elec_b + 
                   elix_neurol_b + elix_cpd_b + elix_diab_b + 
                   elix_renal_fail_b + elix_liver_b + elix_coag_b + 
                   elix_anemia_iron_b + elix_valvular_b + 
                   elix_anemia_bld_loss_b, family = binomial)$fitted.values
detach(cabg19)

cabg19$propensity <- propensity



# Create user-specified distance list for LHS using exact match.
dist_list_left <- create_list_from_scratch(Z, X, exact = c("exact_dec"),
                                           p = propensity, caliper_low = .3, 
                                           k = 300)

dist_list_left$d <- 1e2 * dist_list_left$d

# Create user specified distance list for RHS
dist_list_right = create_list_from_scratch(Z = Z, X = propensity,
                                           p = propensity,
                                           caliper_low = 0.3,
                                           k = 500,
                                           method = 'L1')

dist_list_right_fb = create_list_from_scratch(Z = Z, X = X[, c('elix_arrhy_b')],
                                              p = propensity,
                                              caliper_low = 0.3,
                                              k = 500,
                                              method = '0/1')

dist_list_right$d = 100*(1e2*dist_list_right$d + 1e4*dist_list_right_fb$d) 

# Perform 2019 match
set.seed(33)
start_time <- Sys.time()
matching_output19 = match_2C_list(Z = Z, dataset = cabg19,
                                  dist_list_1 = dist_list_left,
                                  dist_list_2 = dist_list_right,
                                  lambda = 100,
                                  controls = 1)
end_time <- Sys.time()
end_time - start_time


# Check match quality
tb_after_match19 = check_balance(Z = Z, matching_output19, 
                                 cov_list = c("age_years", "gender", "asian", 
                                              "black", "hispanic", "unknown", 
                                              "white", "elix_pcd_b", 
                                              "elix_pvd_b", "elix_htn_b", 
                                              "elix_fluid_elec_b", 
                                              "elix_neurol_b", "elix_cpd_b", 
                                              "elix_diab_b", 
                                              "elix_renal_fail_b", 
                                              "elix_liver_b", "elix_coag_b", 
                                              "elix_anemia_iron_b", 
                                              "elix_valvular_b", 
                                              "elix_anemia_bld_loss_b", 
                                              "elix_chf_b", "elix_arrhy_b", 
                                              "surgical_volume_npi_2016_2020",
                                              "low_rsk"), 
                                 plot_propens = T, propens = propensity) 


dt19_matched <- matching_output19$data_with_matched_set_ind[!is.na(matching_output19$data_with_matched_set_ind$matched_set),]



# Concatenate -------------------------------------------------------------


#Save matched data
dir.create("./Primary_match_output")
write_csv(dt16_matched, "./Primary_match_output/dt16_matched.csv")
write_csv(dt17_matched, "./Primary_match_output/dt17_matched.csv")
write_csv(dt18_matched, "./Primary_match_output/dt18_matched.csv")
write_csv(dt19_matched, "./Primary_match_output/dt19_matched.csv")
write_csv(tb_after_match16, "./Primary_match_output/tb_after_match16.csv")
write_csv(tb_after_match17, "./Primary_match_output/tb_after_match17.csv")
write_csv(tb_after_match18, "./Primary_match_output/tb_after_match18.csv")
write_csv(tb_after_match19, "./Primary_match_output/tb_after_match19.csv")


