## ---------------------------
##
## Optum TEE Sensitivity Analysis
##
## Author: Charlotte Talham
##
## Date Created: 2023-08-08
##
##
## ---------------------------
##
## Notes: This file includes the calculation of Rosenbaum bounds
## for the Optum TEE sensitivity analysis. Requires primary_nco_analysis.R.
## Output for eAppendix 4.1
## 
## 'rbounds' function 'binarysens' calculates 
## P(X>x) as opposed to P(X>=x) for binomial RV, X.
## Sensitivity analysis below adjusts for this by using
## (max-1, min+1) as discordant pair.
##
##
## ---------------------------

library(tidyverse)
library(rbounds)
library(sensitivitymv)


dt_matched <- read_csv("./Primary_match_output/dt_matched.csv")
dt_control <- read_csv("./Primary_match_output/dt_control.csv")
dt_treated <- read_csv("./Primary_match_output/dt_treated.csv")


# Primary Analysis --------------------------------------------------------

# Find discordant pairs
b_prim_filter <- dt_control %>% filter(death_30 == 1) %>% pull(matched_pair)
b_prim <- dt_treated %>% 
  filter(matched_pair %in% b_prim_filter & death_30 == 0) %>% 
  count() %>% as.numeric()
c_prim_filter <- dt_control %>% filter(death_30 == 0) %>% pull(matched_pair)
c_prim <- dt_treated %>% 
  filter(matched_pair %in% c_prim_filter & death_30 == 1) %>% 
  count() %>% as.numeric()
                      
lambda <- c(1.2, 1.3, 1.4, 1.5, 1.6, 1.7)
# Discordant pair (529, 430).
binarysens(min(b_prim,c_prim)+1, max(b_prim,c_prim)-1, Gamma = 1.12, 
           GammaInc = .01) # Gamma = 1.10
amplify(gamma = 1.1, lambda = lambda)

# Subgroup Analysis 1 ------------------------------------------------------

# CHF

# Find discordant pairs
b_chf_filter <- dt_control %>% filter(death_30 == 1 & elix_chf_b == 1) %>% 
  pull(matched_pair)
b_chf <- dt_treated %>% 
  filter(matched_pair %in% b_chf_filter & death_30 == 0) %>% 
  count() %>% as.numeric()
c_chf_filter <- dt_control %>% filter(death_30 == 0 & elix_chf_b == 1) %>% 
  pull(matched_pair)
c_chf <- dt_treated %>% 
  filter(matched_pair %in% c_chf_filter & death_30 == 1) %>% 
  count() %>% as.numeric()

# Discordant pair (225, 287).
binarysens(min(b_chf,c_chf)+1, max(b_chf,c_chf)-1, Gamma = 1.1, GammaInc = .01) # Gamma = 1.09
amplify(gamma = 1.09, lambda = lambda)

# No CHF
# Not significant.


# Subgroup Analysis 2 -----------------------------------------------------

# Low Risk & CHF
# Not significant.

# Low Risk & No CHF
# Not significant.

# High Risk & CHF

# Find discordant pairs
b_high_chf_filter <- dt_control %>% 
  filter(death_30 == 1 & elix_chf_b == 1 & low_rsk == 0) %>% 
  pull(matched_pair)
b_high_chf <- dt_treated %>% 
  filter(matched_pair %in% b_high_chf_filter & death_30 == 0) %>% 
  count() %>% as.numeric()
c_high_chf_filter <- dt_control %>% 
  filter(death_30 == 0 & elix_chf_b == 1 & low_rsk == 0) %>% pull(matched_pair)
c_high_chf <- dt_treated %>% 
  filter(matched_pair %in% c_high_chf_filter & death_30 == 1) %>% 
  count() %>% as.numeric()

# Discordant pair (179, 225).
binarysens(min(b_high_chf,c_high_chf)+1, 
           max(b_high_chf,c_high_chf)-1, Gamma = 1.1, GammaInc = .01) # Gamma = 1.06
amplify(gamma = 1.06, lambda = lambda)

# High Risk + No CHF
# Not significant.


