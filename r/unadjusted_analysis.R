## ---------------------------
##
## Optum TEE Unadjusted Analysis
##
## Author: Charlotte Talham
##
## Date Created: 2023-08-30
##
##
## ---------------------------
##
## Notes: Unadjusted analysis.
## eAppendix 3; eTable 4 output. Requires optum_preprocessing.R.
##
## ---------------------------

library(tidyverse)

cabg <- read_csv("iso_cabg.csv")

# Primary Analysis --------------------------------------------------------

cabg %>% group_by(TEE_intraop) %>% summarise(n = n(), death = mean(death_30))
cabg %>% group_by(death_30, TEE_intraop) %>% tally() %>% 
  spread(TEE_intraop, n) %>% ungroup() %>% select(-death_30) %>% as.matrix %>% 
  fisher.exact()

# Subgroup Analysis I -----------------------------------------------------

#CHF
cabg %>% filter(elix_chf_b == 1) %>% group_by(TEE_intraop) %>% 
  summarise(n(), mean(death_30))
cabg %>% filter(elix_chf_b == 1) %>% group_by(death_30, TEE_intraop) %>% 
  tally() %>% spread(TEE_intraop, n) %>% ungroup() %>% select(-death_30) %>% 
  as.matrix() %>% fisher.exact()

# no CHF
cabg %>% filter(elix_chf_b == 0) %>% group_by(TEE_intraop) %>% 
  summarise(n(), mean(death_30))
cabg %>% filter(elix_chf_b == 0) %>% group_by(death_30, TEE_intraop) %>% 
  tally() %>% spread(TEE_intraop, n) %>% ungroup() %>% select(-death_30) %>% 
  as.matrix() %>% fisher.exact()


# Subgroup Analysis II ----------------------------------------------------

# Low Risk CHF
cabg %>% filter(elix_chf_b == 1 & low_rsk == 1) %>% group_by(TEE_intraop) %>% 
  summarise(n(), mean(death_30))
cabg %>% filter(elix_chf_b == 1 & low_rsk == 1) %>% group_by(death_30, TEE_intraop) %>% 
  tally() %>% spread(TEE_intraop, n) %>% ungroup() %>% select(-death_30) %>% 
  as.matrix() %>% fisher.exact()

# High Risk CHF
cabg %>% filter(elix_chf_b == 1 & low_rsk == 0) %>% group_by(TEE_intraop) %>% 
  summarise(n(), mean(death_30))
cabg %>% filter(elix_chf_b == 1 & low_rsk == 0) %>% group_by(death_30, TEE_intraop) %>% 
  tally() %>% spread(TEE_intraop, n) %>% ungroup() %>% select(-death_30) %>% 
  as.matrix() %>% fisher.exact()

# Low Risk no CHF
cabg %>% filter(elix_chf_b == 0 & low_rsk == 1) %>% group_by(TEE_intraop) %>% 
  summarise(n(), mean(death_30))
cabg %>% filter(elix_chf_b == 0 & low_rsk == 1) %>% group_by(death_30, TEE_intraop) %>% 
  tally() %>% spread(TEE_intraop, n) %>% ungroup() %>% select(-death_30) %>% 
  as.matrix() %>% fisher.exact()

# High Risk no CHF
cabg %>% filter(elix_chf_b == 0 & low_rsk == 0) %>% group_by(TEE_intraop) %>% 
  summarise(n(), mean(death_30))
cabg %>% filter(elix_chf_b == 0 & low_rsk == 0) %>% group_by(death_30, TEE_intraop) %>% 
  tally() %>% spread(TEE_intraop, n) %>% ungroup() %>% select(-death_30) %>% 
  as.matrix() %>% fisher.exact()

