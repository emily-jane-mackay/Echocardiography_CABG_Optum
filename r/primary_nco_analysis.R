## ---------------------------
##
## Primary Optum TEE Analysis
##
## Author: Charlotte Talham
##
## Date Created: 2023-05-28
##
##
## ---------------------------
##
## Notes: This file includes post-match standardized mean
## difference calculations, primary and subgroup analyses, and 
## negative control outcome analyses.
## Requires output from primary_match.R.
##   
## ---------------------------

library(exact2x2)
library(tidyverse)

# Concatenate matching output ---------------------------------------------
cabg <- read_csv("iso_cabg.csv")
dt16_matched <- read_csv("./Primary_match_output/dt16_matched.csv")
dt17_matched <- read_csv("./Primary_match_output/dt17_matched.csv")
dt18_matched <- read_csv("./Primary_match_output/dt18_matched.csv")
dt19_matched <- read_csv("./Primary_match_output/dt19_matched.csv")

# Order concatenated matched data
dt16_matched$matched_pair <- dt16_matched$matched_set
dt17_matched$matched_pair <- dt17_matched$matched_set + dim(dt16_matched)[1]/2
dt18_matched$matched_pair <- dt18_matched$matched_set + dim(dt16_matched)[1]/2 + 
  dim(dt17_matched)[1]/2
dt19_matched$matched_pair <- dt19_matched$matched_set + dim(dt16_matched)[1]/2 +
  dim(dt17_matched)[1]/2 + dim(dt18_matched)[1]/2

dt_matched <- dt16_matched %>% rbind(dt17_matched) %>% rbind(dt18_matched) %>% 
  rbind(dt19_matched)
dt_matched <- dt_matched %>% arrange(matched_pair)

# Treated: TEE_intraop == 1; Control: TEE_intraop == 0
dt_treated <- dt_matched %>% filter(TEE_intraop == 1)
dt_control <- dt_matched %>% filter(TEE_intraop == 0)

# Export ------------------------------------------------------------------
write_csv(dt_matched, "./Primary_match_output/dt_matched.csv")
write_csv(dt_treated, "./Primary_match_output/dt_treated.csv")
write_csv(dt_control, "./Primary_match_output/dt_control.csv")
rm(list = c("dt16_matched", "dt17_matched", "dt18_matched", "dt19_matched"))

# After match balance table -----------------------------------------------

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

tb_after_matching = tableone::CreateTableOne(vars = covars, 
                                             factorVars = catVars,
                                             strata = "TEE_intraop", 
                                             data = dt_matched,
                                             test = FALSE)
print(tb_after_matching)


# Balance table with SMD --------------------------------------------------

dt_treated_before <- cabg %>% filter(TEE_intraop == 1) %>% select(all_of(covars))
dt_control_before <- cabg %>% filter(TEE_intraop == 0) %>% select(all_of(covars))
dt_treated_after <- dt_treated %>% select(all_of(covars))
dt_control_after <- dt_control %>% select(all_of(covars))


mean_treated_before = apply(dt_treated_before, 2, mean)
mean_control_before = apply(dt_control_before, 2, mean)
mean_diff_before = mean_treated_before - mean_control_before

sd_treated_before = apply(dt_treated_before, 2, stats::sd)
sd_control_before = apply(dt_control_before, 2, stats::sd)

pooled_sd = sqrt(sd_treated_before^2 + sd_control_before^2)

std_before = mean_diff_before/pooled_sd

mean_treated_after = apply(dt_treated_after, 2, mean)
mean_control_after = apply(dt_control_after, 2, mean)
mean_diff_after = mean_treated_after - mean_control_after

std_after = mean_diff_after/pooled_sd

# Tabulate all results
balance_table = data.frame(mean_control_before, mean_treated_before, std_before,
                           mean_treated_after, std_after)

rownames(balance_table) <= covars
colnames(balance_table) <- c('Z = 0', 'Z = 1 (Bef)',
                             'Std. Diff (Bef)',
                             'Z = 1 (Aft)',
                             'Std. Diff (Aft)')

# McNemar Test function ---------------------------------------------------
mcnemar_test <- function(dt_treated, dt_control){
  n = dim(dt_treated)[1]
  pp=0
  pn=0
  np=0
  nn=0
  for(i in seq(1,n,1))
  {
    if(dt_treated$death_30[i]==1 & dt_control$death_30[i]==1){pp = pp+1}
    else if(dt_treated$death_30[i]==1 & dt_control$death_30[i]==0){pn = pn+1}
    else if(dt_treated$death_30[i]==0 & dt_control$death_30[i]==1){np = np+1}
    else if(dt_treated$death_30[i]==0 & dt_control$death_30[i]==0){nn = nn+1}
  }
  contingency_mtx <- matrix(c(pp,pn,np,nn),ncol = 2,byrow = T, dimnames = list(c("t_1","t_0"),c("c_1","c_0")))
  return(mcnemar.exact(contingency_mtx))
}

mcnemar_nco <- function(dt_treated, dt_control){
  n = dim(dt_treated)[1]
  pp=0
  pn=0
  np=0
  nn=0
  for(i in seq(1,n,1))
  {
    if(dt_treated$afib[i]==1 & dt_control$afib[i]==1){pp = pp+1}
    else if(dt_treated$afib[i]==1 & dt_control$afib[i]==0){pn = pn+1}
    else if(dt_treated$afib[i]==0 & dt_control$afib[i]==1){np = np+1}
    else if(dt_treated$afib[i]==0 & dt_control$afib[i]==0){nn = nn+1}
  }
  contingency_mtx <- matrix(c(pp,pn,np,nn),ncol = 2,byrow = T, dimnames = list(c("t_1","t_0"),c("c_1","c_0")))
  return(mcnemar.exact(contingency_mtx))
}


# Primary Analysis --------------------------------------------------------

# Primary (main) analysis
dt_treated %>% summarise(mean(death_30), n()) 
dt_control %>% summarise(mean(death_30), n()) 
mcnemar_test(dt_treated, dt_control)

# Subgroup Analysis I
# CHF
dt_treated %>% filter(elix_chf_b == 1) %>% summarise(mean(death_30), n()) 
dt_control %>% filter(elix_chf_b == 1) %>% summarise(mean(death_30), n()) 
mcnemar_test(dt_treated[dt_treated$elix_chf_b == 1,],
             dt_control[dt_control$elix_chf_b == 1,]) 

# No CHF
dt_treated %>% filter(elix_chf_b == 0) %>% summarise(mean(death_30), n()) 
dt_control %>% filter(elix_chf_b == 0) %>% summarise(mean(death_30), n()) 
mcnemar_test(dt_treated[dt_treated$elix_chf_b == 0,],
             dt_control[dt_control$elix_chf_b == 0,]) 


# Subgroup Analysis II
# Low risk + CHF
dt_treated %>% filter(elix_chf_b == 1 & low_rsk == 1) %>% summarise(mean(death_30), n()) 
dt_control %>% filter(elix_chf_b == 1 & low_rsk == 1) %>% summarise(mean(death_30), n()) 
mcnemar_test(dt_treated[dt_treated$elix_chf_b == 1 & dt_treated$low_rsk == 1,],
             dt_control[dt_control$elix_chf_b == 1 & dt_control$low_rsk == 1,]) 

# Low risk + no CHF
dt_treated %>% filter(elix_chf_b == 0 & low_rsk == 1) %>% summarise(mean(death_30), n()) 
dt_control %>% filter(elix_chf_b == 0 & low_rsk == 1) %>% summarise(mean(death_30), n()) 
mcnemar_test(dt_treated[dt_treated$elix_chf_b == 0 & dt_treated$low_rsk == 1,],
             dt_control[dt_control$elix_chf_b == 0 & dt_control$low_rsk == 1,]) 

# High risk + CHF
dt_treated %>% filter(elix_chf_b == 1 & low_rsk == 0) %>% summarise(mean(death_30), n()) 
dt_control %>% filter(elix_chf_b == 1 & low_rsk == 0) %>% summarise(mean(death_30), n()) 
mcnemar_test(dt_treated[dt_treated$elix_chf_b == 1 & dt_treated$low_rsk == 0,],
             dt_control[dt_control$elix_chf_b == 1 & dt_control$low_rsk == 0,]) 

# High risk + no CHF
dt_treated %>% filter(elix_chf_b == 0 & low_rsk == 0) %>% summarise(mean(death_30), n()) 
dt_control %>% filter(elix_chf_b == 0 & low_rsk == 0) %>% summarise(mean(death_30), n()) 
mcnemar_test(dt_treated[dt_treated$elix_chf_b == 0 & dt_treated$low_rsk == 0,],
             dt_control[dt_control$elix_chf_b == 0 & dt_control$low_rsk == 0,]) 


# Negative Control Outcome Analysis ------------------------------------------------

# Primary (main) Analysis
dt_treated %>% summarise(mean(afib), n())
dt_control %>% summarise(mean(afib), n())
mcnemar_nco(dt_treated, dt_control) 

# Subgroup Analysis I
# CHF
dt_treated %>% filter(elix_chf_b == 1) %>% summarise(mean(afib), n()) 
dt_control %>% filter(elix_chf_b == 1) %>% summarise(mean(afib), n()) 
mcnemar_nco(dt_treated[dt_treated$elix_chf_b == 1,],
            dt_control[dt_control$elix_chf_b == 1,]) 

# No CHF
dt_treated %>% filter(elix_chf_b == 0) %>% summarise(mean(afib), n()) #
dt_control %>% filter(elix_chf_b == 0) %>% summarise(mean(afib), n()) #
mcnemar_nco(dt_treated[dt_treated$elix_chf_b == 0,],
            dt_control[dt_control$elix_chf_b == 0,])

# Subgroup Analysis II
# Low risk + CHF
dt_treated %>% filter(elix_chf_b == 1 & low_rsk == 1) %>% summarise(mean(afib), n()) 
dt_control %>% filter(elix_chf_b == 1 & low_rsk == 1) %>% summarise(mean(afib), n()) 
mcnemar_nco(dt_treated[dt_treated$elix_chf_b == 1 & dt_treated$low_rsk == 1,],
            dt_control[dt_control$elix_chf_b == 1 & dt_control$low_rsk == 1,])

# Low risk + no CHF
dt_treated %>% filter(elix_chf_b == 0 & low_rsk == 1) %>% summarise(mean(afib), n()) # 
dt_control %>% filter(elix_chf_b == 0 & low_rsk == 1) %>% summarise(mean(afib), n()) #
mcnemar_nco(dt_treated[dt_treated$elix_chf_b == 0 & dt_treated$low_rsk == 1,],
            dt_control[dt_control$elix_chf_b == 0 & dt_control$low_rsk == 1,]) 

# High risk + CHF
dt_treated %>% filter(elix_chf_b == 1 & low_rsk == 0) %>% summarise(mean(afib), n()) 
dt_control %>% filter(elix_chf_b == 1 & low_rsk == 0) %>% summarise(mean(afib), n()) 
mcnemar_nco(dt_treated[dt_treated$elix_chf_b == 1 & dt_treated$low_rsk == 0,],
            dt_control[dt_control$elix_chf_b == 1 & dt_control$low_rsk == 0,])

# High risk + no CHF
dt_treated %>% filter(elix_chf_b == 0 & low_rsk == 0) %>% summarise(mean(afib), n()) 
dt_control %>% filter(elix_chf_b == 0 & low_rsk == 0) %>% summarise(mean(afib), n()) 
mcnemar_nco(dt_treated[dt_treated$elix_chf_b == 0 & dt_treated$low_rsk == 0,],
            dt_control[dt_control$elix_chf_b == 0 & dt_control$low_rsk == 0,])












