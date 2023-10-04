## ---------------------------
##
## Optum TEE exposure based sensitivity analysis
##
## Author: Charlotte Talham
##
## Date Created: 2023-08-16
##
##
## ---------------------------
##
## Notes: Exposure based sensitivity analysis. Requires output
## from sensitivity_match.R. Output for eAppendix section 4.2; eTable 5 and
## eTable 6.
##   
## ---------------------------

library(exact2x2)
library(tidyverse)

cabg <- read_csv("iso_cabg.csv")
dt16_matched_sa <- read_csv("./Sensitivity_match_output/dt16_matched_sa.csv")
dt17_matched_sa <- read_csv("./Sensitivity_match_output/dt17_matched_sa.csv")
dt18_matched_sa <- read_csv("./Sensitivity_match_output/dt18_matched_sa.csv")
dt19_matched_sa <- read_csv("./Sensitivity_match_output/dt19_matched_sa.csv")

# Order concatenated matched data
dt16_matched_sa$matched_pair <- dt16_matched_sa$matched_set
dt17_matched_sa$matched_pair <- dt17_matched_sa$matched_set + 
  dim(dt16_matched_sa)[1]/2
dt18_matched_sa$matched_pair <- dt18_matched_sa$matched_set + 
  dim(dt16_matched_sa)[1]/2 + dim(dt17_matched_sa)[1]/2
dt19_matched_sa$matched_pair <- dt19_matched_sa$matched_set + 
  dim(dt16_matched_sa)[1]/2 + dim(dt17_matched_sa)[1]/2 + 
  dim(dt18_matched_sa)[1]/2

dt_matched_sa <- dt16_matched_sa %>% rbind(dt17_matched_sa) %>% 
  rbind(dt18_matched_sa) %>% rbind(dt19_matched_sa)
dt_matched_sa <- dt_matched_sa %>% arrange(matched_pair)
rm(list = c("dt16_matched_sa", "dt17_matched_sa", "dt18_matched_sa", 
            "dt19_matched_sa"))


# Treated: TEE == 1; Control: TEE == 0
dt_treated_sa <- dt_matched_sa %>% filter(TEE == 1)
dt_control_sa <- dt_matched_sa %>% filter(TEE == 0)

# After match balance table -----------------------------------------------

covars <- c("age_years", "gender", "asian", "black", "hispanic", "unknown", 
            "white", "elix_pcd_b", "elix_pvd_b", "elix_htn_b", 
            "elix_fluid_elec_b", "elix_neurol_b", "elix_cpd_b", "elix_diab_b", 
            "elix_renal_fail_b", "elix_liver_b", "elix_coag_b", 
            "elix_anemia_iron_b", "elix_valvular_b", "elix_anemia_bld_loss_b", 
            "elix_chf_b", "elix_arrhy_b", "surgical_volume_npi_2016_2020", 
            "ves_one", "ves_two", "ves_three_plus", "cabg_ima", "low_rsk", 
            "high_risk2","death_30", "death_30_or_stroke", 
            "complic_stroke", "afib", "yr")
catVars <- c("gender", "asian", "black", "hispanic", "unknown", 
             "white", "elix_pcd_b", "elix_pvd_b", "elix_htn_b", 
             "elix_fluid_elec_b", "elix_neurol_b", "elix_cpd_b", "elix_diab_b", 
             "elix_renal_fail_b", "elix_liver_b", "elix_coag_b", 
             "elix_anemia_iron_b", "elix_valvular_b", "elix_anemia_bld_loss_b", 
             "elix_chf_b", "elix_arrhy_b",
             "ves_one", "ves_two", "ves_three_plus", "cabg_ima", "low_rsk", 
             "high_risk2")


tb_after_matching = tableone::CreateTableOne(vars = covars, 
                                             factorVars = catVars,
                                             strata = "TEE", 
                                             data = dt_matched,
                                             test = FALSE)
print(tb_after_matching)


# Balance table with SMD --------------------------------------------------

dt_treated_before <- cabg %>% filter(TEE == 1) %>% select(all_of(covars))
dt_control_before <- cabg %>% filter(TEE == 0) %>% select(all_of(covars))
dt_treated_after <- dt_treated_sa %>% select(all_of(covars))
dt_control_after <- dt_control_sa %>% select(all_of(covars))


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

# balance_table = data.frame(mean no TEE, mean TEE before, st. diff before,
#                            mean TEE after, st. diff after)

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

dt_treated_sa %>% summarise(mean(death_30), n()) # TEE: 2.65%
dt_control_sa %>% summarise(mean(death_30), n()) # no TEE: 2.89%
mcnemar_test(dt_treated_sa, dt_control_sa) # OR: 0.912, p < 0.198

# Subgroup Analysis 1 ----------------------------------------------------------------
# CHF
dt_treated_sa %>% filter(elix_chf_b == 1) %>% summarise(mean(death_30), n()) # TEE: 4.35%
dt_control_sa %>% filter(elix_chf_b == 1) %>% summarise(mean(death_30), n()) # no TEE: 4.89%
mcnemar_test(dt_treated_sa[dt_treated_sa$elix_chf_b == 1,],
             dt_control_sa[dt_control_sa$elix_chf_b == 1,]) # OR: 0.882, p = 0.209

# No CHF
dt_treated_sa %>% filter(elix_chf_b == 0) %>% summarise(mean(death_30), n()) # TEE: 1.86%
dt_control_sa %>% filter(elix_chf_b == 0) %>% summarise(mean(death_30), n()) # no TEE: 1.96%
mcnemar_test(dt_treated_sa[dt_treated_sa$elix_chf_b == 0,],
             dt_control_sa[dt_control_sa$elix_chf_b == 0,]) # OR: 0.946, p = 0.616


# Subgroup Analysis 2 -----------------------------------------------------

# Low risk + CHF
dt_treated_sa %>% filter(elix_chf_b == 1 & low_rsk == 1) %>% summarise(mean(death_30), n()) # TEE: 4.28%
dt_control_sa %>% filter(elix_chf_b == 1 & low_rsk == 1) %>% summarise(mean(death_30), n()) # no TEE: 5.01%
mcnemar_test(dt_treated_sa[dt_treated_sa$elix_chf_b == 1 & dt_treated_sa$low_rsk == 1,],
             dt_control_sa[dt_control_sa$elix_chf_b == 1 & dt_control_sa$low_rsk == 1,]) # OR: 0.846, p = 0.475


# Low risk + no CHF
dt_treated_sa %>% filter(elix_chf_b == 0 & low_rsk == 1) %>% summarise(mean(death_30), n()) # TEE: 2.14%
dt_control_sa %>% filter(elix_chf_b == 0 & low_rsk == 1) %>% summarise(mean(death_30), n()) # no TEE: 1.68%
mcnemar_test(dt_treated_sa[dt_treated_sa$elix_chf_b == 0 & dt_treated_sa$low_rsk == 1,],
             dt_control_sa[dt_control_sa$elix_chf_b == 0 & dt_control_sa$low_rsk == 1,]) # OR: 1.289, p = 0.284

# Med + High risk + CHF
dt_treated_sa %>% filter(elix_chf_b == 1 & low_rsk == 0) %>% summarise(mean(death_30), n()) # TEE: 4.37%
dt_control_sa %>% filter(elix_chf_b == 1 & low_rsk == 0) %>% summarise(mean(death_30), n()) # no TEE: 4.86%
mcnemar_test(dt_treated_sa[dt_treated_sa$elix_chf_b == 1 & dt_treated_sa$low_rsk == 0,],
             dt_control_sa[dt_control_sa$elix_chf_b == 1 & dt_control_sa$low_rsk == 0,]) # OR: 0.892, p = 0.324

# Med + High risk + no CHF
dt_treated_sa %>% filter(elix_chf_b == 0 & low_rsk == 0) %>% summarise(mean(death_30), n()) # TEE: 1.77%
dt_control_sa %>% filter(elix_chf_b == 0 & low_rsk == 0) %>% summarise(mean(death_30), n()) # no TEE: 2.04%
mcnemar_test(dt_treated_sa[dt_treated_sa$elix_chf_b == 0 & dt_treated_sa$low_rsk == 0,],
             dt_control_sa[dt_control_sa$elix_chf_b == 0 & dt_control_sa$low_rsk == 0,]) # OR: 0.867, p = 0.233



# eTable 6 -------------------------------------------------------------
rm(list = c("dt_matched_sa", "dt_control_sa", "dt_treated_sa"))

dt_matched <- read_csv("./Primary_match_output/dt_matched.csv")
dt_control <- read_csv("./Primary_match_output/dt_control.csv")
dt_treated <- read_csv("./Primary_match_output/dt_treated.csv")


# Primary Analysis
primary_mp <- dt_control %>% filter(TEE == 1) %>% pull(matched_pair)
dt_matched %>% filter(matched_pair %in% primary_mp) %>% 
  group_by(TEE_intraop) %>% summarize(n(), mean(death_30))

# Subgroup Analysis I

# CHF
chf_mp <- dt_control %>% filter(TEE == 1 & elix_chf_b == 1) %>% 
  pull(matched_pair)
dt_matched %>% filter(matched_pair %in% chf_mp) %>% group_by(TEE_intraop) %>% 
  summarize(n(), mean(death_30))

# no CHF
no_chf_mp <- dt_control %>% filter(TEE == 1 & elix_chf_b == 0) %>% 
  pull(matched_pair)
dt_matched %>% filter(matched_pair %in% no_chf_mp) %>% group_by(TEE_intraop) %>% 
  summarize(n(), mean(death_30))

# Subgroup Analysis II


# Low Risk & CHF
low_chf_mp <- dt_control %>% 
  filter(TEE == 1 & elix_chf_b == 1 & low_rsk == 1) %>% pull(matched_pair)
dt_matched %>% filter(matched_pair %in% low_chf_mp) %>% 
  group_by(TEE_intraop) %>% summarize(n(), mean(death_30))

# Low Risk & No CHF
low_no_chf_mp <- dt_control %>% 
  filter(TEE == 1 & elix_chf_b == 0 & low_rsk == 1) %>% pull(matched_pair)
dt_matched %>% filter(matched_pair %in% low_no_chf_mp) %>% 
  group_by(TEE_intraop) %>% summarize(n(), mean(death_30))

# High Risk & CHF
high_chf_mp <- dt_control %>% 
  filter(TEE == 1 & elix_chf_b == 1 & low_rsk == 0) %>% pull(matched_pair)
dt_matched %>% filter(matched_pair %in% high_chf_mp) %>% 
  group_by(TEE_intraop) %>% summarize(n(), mean(death_30))

# High Risk + No CHF
high_no_chf_mp <- dt_control %>% 
  filter(TEE == 1 & elix_chf_b == 0 & low_rsk == 0) %>% pull(matched_pair)
dt_matched %>% filter(matched_pair %in% high_no_chf_mp) %>% 
  group_by(TEE_intraop) %>% summarize(n(), mean(death_30))








