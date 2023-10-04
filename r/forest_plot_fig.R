## ---------------------------
##
## Optum TEE Forest Plots
##
## Author: Charlotte Talham
##
## Date Created: 2023-07-10
##
##
## ---------------------------
##
## Notes: Creates table with count, proportions, odds ratios, forest plots,
## and p-values for primary and secondary analyses and NCO analysis.
##
## ---------------------------

# Load packages
library(tidyverse)
library(haven)
library(forestplot)
library(Cairo)
library(exact2x2)
library(scales)

# Load the datasets
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





# Primary analysis --------------------------------------------------------

tab_primary_treat <- dt_treated %>% summarise(mean(death_30), n()) # TEE: 2.63%
tab_primary_cntrl <- dt_control %>% summarise(mean(death_30), n()) # no TEE: 3.20%
mcnemar_primary <- mcnemar_test(dt_treated, dt_control) # OR: 0.813, p < 0.002

# Subgroup analysis ----------------------------------------------------------------
# CHF
tab_chf_treat <- dt_treated %>% filter(elix_chf_b == 1) %>% summarise(mean(death_30), n()) # TEE: 4.20%
tab_chf_cntrl <- dt_control %>% filter(elix_chf_b == 1) %>% summarise(mean(death_30), n()) # no TEE: 5.26%
mcnemar_chf <- mcnemar_test(dt_treated[dt_treated$elix_chf_b == 1,],
             dt_control[dt_control$elix_chf_b == 1,]) # OR: 0.784, p = 0.007

# No CHF
tab_no_chf_treat <- dt_treated %>% filter(elix_chf_b == 0) %>% summarise(mean(death_30), n()) # TEE: 1.83%
tab_no_chf_cntrl <- dt_control %>% filter(elix_chf_b == 0) %>% summarise(mean(death_30), n()) # no TEE: 2.15%
mcnemar_no_chf <- mcnemar_test(dt_treated[dt_treated$elix_chf_b == 0,],
             dt_control[dt_control$elix_chf_b == 0,]) # OR: 0.847, p = 0.089

# Subgroups
# Low risk + CHF
tab_low_chf_treat <- dt_treated %>% filter(elix_chf_b == 1 & low_rsk == 1) %>% summarise(mean(death_30), n()) # TEE: 4.08%
tab_low_chf_cntrl <- dt_control %>% filter(elix_chf_b == 1 & low_rsk == 1) %>% summarise(mean(death_30), n()) # no TEE: 5.33%
mcnemar_low_chf <- mcnemar_test(dt_treated[dt_treated$elix_chf_b == 1 & dt_treated$low_rsk == 1,],
             dt_control[dt_control$elix_chf_b == 1 & dt_control$low_rsk == 1,]) # OR: 0.742, p = 0.149

# Low risk + no CHF
tab_low_no_chf_treat <- dt_treated %>% filter(elix_chf_b == 0 & low_rsk == 1) %>% summarise(mean(death_30), n()) # TEE: 1.88%
tab_low_no_chf_cntrl <- dt_control %>% filter(elix_chf_b == 0 & low_rsk == 1) %>% summarise(mean(death_30), n()) # no TEE: 1.91%
mcnemar_low_no_chf <- mcnemar_test(dt_treated[dt_treated$elix_chf_b == 0 & dt_treated$low_rsk == 1,],
             dt_control[dt_control$elix_chf_b == 0 & dt_control$low_rsk == 1,]) # OR: 0.979, p = 1

# High risk + CHF
tab_high_chf_treat <- dt_treated %>% filter(elix_chf_b == 1 & low_rsk == 0) %>% summarise(mean(death_30), n()) # TEE: 4.23%
tab_high_chf_cntl <-dt_control %>% filter(elix_chf_b == 1 & low_rsk == 0) %>% summarise(mean(death_30), n()) # no TEE: 5.24%
mcnemar_high_chf <- mcnemar_test(dt_treated[dt_treated$elix_chf_b == 1 & dt_treated$low_rsk == 0,],
             dt_control[dt_control$elix_chf_b == 1 & dt_control$low_rsk == 0,]) # OR: 0.796, p = 0.025

# High risk + no CHF
tab_high_no_chf_treat <- dt_treated %>% filter(elix_chf_b == 0 & low_rsk == 0) %>% summarise(mean(death_30), n()) # TEE: 1.82%
tab_high_no_chf_cntrl <- dt_control %>% filter(elix_chf_b == 0 & low_rsk == 0) %>% summarise(mean(death_30), n()) # no TEE: 2.22%
mcnemar_high_no_chf <- mcnemar_test(dt_treated[dt_treated$elix_chf_b == 0 & dt_treated$low_rsk == 0,],
             dt_control[dt_control$elix_chf_b == 0 & dt_control$low_rsk == 0,]) # OR: 0.814, p = 0.062





# Negative Control Outcome ------------------------------------------------

# Afib outcome results

# Primary

tab_afib_treat <- dt_treated %>% summarise(mean(afib), n()) # TEE: 6.46%
tab_afib_cntrl <- dt_control %>% summarise(mean(afib), n()) # no TEE: 6.38%
mcnemar_afib_prim <- mcnemar_nco(dt_treated, dt_control) # OR: 1.01, p = 0.791



# Subgroup
# CHF
tab_afib_chf_treat <- dt_treated %>% filter(elix_chf_b == 1) %>% summarise(mean(afib), n()) 
tab_afib_chf_cntrl <- dt_control %>% filter(elix_chf_b == 1) %>% summarise(mean(afib), n()) 
mcnemar_afib_chf <- mcnemar_nco(dt_treated[dt_treated$elix_chf_b == 1,],
            dt_control[dt_control$elix_chf_b == 1,]) 

# No CHF
tab_afib_no_chf_treat <- dt_treated %>% filter(elix_chf_b == 0) %>% summarise(mean(afib), n()) #
tab_afib_no_chf_cntrl <- dt_control %>% filter(elix_chf_b == 0) %>% summarise(mean(afib), n()) #
mcnemar_afib_no_chf <- mcnemar_nco(dt_treated[dt_treated$elix_chf_b == 0,],
            dt_control[dt_control$elix_chf_b == 0,])

# Subgroups
# Low risk + CHF
tab_afib_low_chf_treat <- dt_treated %>% filter(elix_chf_b == 1 & low_rsk == 1) %>% summarise(mean(afib), n()) 
tab_afib_low_chf_cntrl <- dt_control %>% filter(elix_chf_b == 1 & low_rsk == 1) %>% summarise(mean(afib), n()) 
mcnemar_afib_low_chf <- mcnemar_nco(dt_treated[dt_treated$elix_chf_b == 1 & dt_treated$low_rsk == 1,],
            dt_control[dt_control$elix_chf_b == 1 & dt_control$low_rsk == 1,])

# Low risk + no CHF
tab_afib_low_no_chf_treat <- dt_treated %>% filter(elix_chf_b == 0 & low_rsk == 1) %>% summarise(mean(afib), n()) # 
tab_afib_low_no_chf_cntrl <- dt_control %>% filter(elix_chf_b == 0 & low_rsk == 1) %>% summarise(mean(afib), n()) #
mcnemar_afib_low_no_chf <- mcnemar_nco(dt_treated[dt_treated$elix_chf_b == 0 & dt_treated$low_rsk == 1,],
            dt_control[dt_control$elix_chf_b == 0 & dt_control$low_rsk == 1,]) 

# High risk + CHF
tab_afib_high_chf_treat <- dt_treated %>% filter(elix_chf_b == 1 & low_rsk == 0) %>% summarise(mean(afib), n()) 
tab_afib_high_chf_cntrl <- dt_control %>% filter(elix_chf_b == 1 & low_rsk == 0) %>% summarise(mean(afib), n()) 
mcnemar_afib_high_chf <- mcnemar_nco(dt_treated[dt_treated$elix_chf_b == 1 & dt_treated$low_rsk == 0,],
            dt_control[dt_control$elix_chf_b == 1 & dt_control$low_rsk == 0,])

# High risk + no CHF
tab_afib_high_no_chf_treat <- dt_treated %>% filter(elix_chf_b == 0 & low_rsk == 0) %>% summarise(mean(afib), n()) 
tab_afib_high_no_chf_cntrl <- dt_control %>% filter(elix_chf_b == 0 & low_rsk == 0) %>% summarise(mean(afib), n()) 
mcnemar_afib_high_no_chf <- mcnemar_nco(dt_treated[dt_treated$elix_chf_b == 0 & dt_treated$low_rsk == 0,],
            dt_control[dt_control$elix_chf_b == 0 & dt_control$low_rsk == 0,])




# Make Primary Table --------------------------------------------------------


table_lables = c('\n\n', '\nSubgroup\n', '\nComparison\n', '\nn\n', 
                 '30-day \nMortality', 'Odds \nRatio \n(95% CI)\n',
                 '\nP-value\n')

or_vec <- c(NA, NA, 
            mcnemar_primary$estimate, NA,
            mcnemar_chf$estimate, NA,
            mcnemar_no_chf$estimate, NA,
            mcnemar_low_chf$estimate, NA,
            mcnemar_low_no_chf$estimate, NA,
            mcnemar_high_chf$estimate, NA,
            mcnemar_high_no_chf$estimate)

low_vec <- c(NA, NA, 
            mcnemar_primary$conf.int[1], NA,
            mcnemar_chf$conf.int[1], NA,
            mcnemar_no_chf$conf.int[1], NA,
            mcnemar_low_chf$conf.int[1], NA,
            mcnemar_low_no_chf$conf.int[1], NA,
            mcnemar_high_chf$conf.int[1], NA,
            mcnemar_high_no_chf$conf.int[1])

high_vec <- c(NA, NA, 
             mcnemar_primary$conf.int[2], NA,
             mcnemar_chf$conf.int[2], NA,
             mcnemar_no_chf$conf.int[2], NA,
             mcnemar_low_chf$conf.int[2], NA,
             mcnemar_low_no_chf$conf.int[2], NA,
             mcnemar_high_chf$conf.int[2], NA,
             mcnemar_high_no_chf$conf.int[2])


p_val_vec <- c(NA, NA, 
               round(mcnemar_primary$p.value, digits = 3), NA,
               round(mcnemar_chf$p.value, digits = 3), NA,
               round(mcnemar_no_chf$p.value, digits = 3), NA,
               round(mcnemar_low_chf$p.value, digits = 3), NA,
               round(mcnemar_low_no_chf$p.value, digits = 3), NA,
               round(mcnemar_high_chf$p.value, digits = 3), NA,
               round(mcnemar_high_no_chf$p.value, digits = 3))


# HR and CI
# Keep two digits, i.e., 1.40, 2.25, etc
CI = paste0(format(or_vec, digits = 2), ' [', format(low_vec, digits = 2) , ', ', 
            format(high_vec, digits = 2) , ']')
CI[is.na(or_vec)] = NA
CI[1] = table_lables[6]

# P-value
p_val_vec[1] = table_lables[7]

# Make the table text
tabletext = cbind(c(table_lables[1], 'Primary Analysis', NA,
                    'Subgroup Analysis', NA, NA, NA, NA, NA, 
                    NA, NA, NA, NA, NA, NA),
                  c(table_lables[2], NA, NA, 'CHF', NA,
                    'No CHF', NA, 'Low Risk + CHF', NA,
                    'Low Risk + No CHF', NA, 'High Risk + CHF', 
                    NA, 'High Risk + No CHF', NA),
                  c(table_lables[3], 'No TOE', 'TOE','No TOE', 'TOE',
                    'No TOE', 'TOE', 'No TOE', 'TOE', 'No TOE', 'TOE',
                    'No TOE', 'TOE', 'No TOE', 'TOE'),
                  c(table_lables[4],
                    tab_primary_cntrl$`n()`,     # no. of obs
                    tab_primary_treat$`n()`, 
                    tab_chf_cntrl$`n()`,
                    tab_chf_treat$`n()`, 
                    tab_no_chf_cntrl$`n()`, 
                    tab_no_chf_treat$`n()`,
                    tab_low_chf_cntrl$`n()`, 
                    tab_low_chf_treat$`n()`, 
                    tab_low_no_chf_cntrl$`n()`,
                    tab_low_no_chf_treat$`n()`,
                    tab_high_chf_cntl$`n()`,
                    tab_high_chf_treat$`n()`,
                    tab_high_no_chf_cntrl$`n()`,
                    tab_high_no_chf_treat$`n()`),
                  c(table_lables[5],
                    percent(tab_primary_cntrl$`mean(death_30)`, accuracy = .01),      # proportions
                    percent(tab_primary_treat$`mean(death_30)`, accuracy = .01), 
                    percent(tab_chf_cntrl$`mean(death_30)`, accuracy = .01),
                    percent(tab_chf_treat$`mean(death_30)`, accuracy = .01), 
                    percent(tab_no_chf_cntrl$`mean(death_30)`, accuracy = .01), 
                    percent(tab_no_chf_treat$`mean(death_30)`, accuracy = .01),
                    percent(tab_low_chf_cntrl$`mean(death_30)`, accuracy = .01), 
                    percent(tab_low_chf_treat$`mean(death_30)`, accuracy = .01), 
                    percent(tab_low_no_chf_cntrl$`mean(death_30)`, accuracy = .01),
                    percent(tab_low_no_chf_treat$`mean(death_30)`, accuracy = .01),
                    percent(tab_high_chf_cntl$`mean(death_30)`, accuracy = .01),
                    percent(tab_high_chf_treat$`mean(death_30)`, accuracy = .01),
                    percent(tab_high_no_chf_cntrl$`mean(death_30)`, accuracy = .01),
                    percent(tab_high_no_chf_treat$`mean(death_30)`, accuracy = .01)),
                  CI,
                  p_val_vec
)



# X ticks in the log-scale
xticks <- c(log(0.25), log(0.50), log(.75), log(1), log(2))
attr(xticks, "labels")  = c('0.25', '0.50', '0.75', '1', '2')


# width = 6.5 inch, height = 6 inch, fontsize = 7
cairo_pdf('Figure_primary_british.pdf', width = 6.5, height = 6, pointsize = 7)
forestplot(
  tabletext,
  mean = or_vec,
  low = low_vec,
  upper = high_vec,
  is.summary = FALSE,
  col = fpColors(
    box = "darkblue",
    line = "darkblue",
    summary = "royalblue",
    zero = 'white'
  ),
  graph.pos = 7,
  graphwidth = 'auto',
  hrzl_lines = list("2" = gpar(lty=1)),
  lwd.zero = 0.5,
  lwd.ci = 0.5,
  lwd.xaxis = 0.5,
  xticks = xticks,
  xlog = TRUE,
  boxsize = 0.1,
  lineheight = 'auto',
  grid = structure(c(1), 
                   gp = gpar(lty = 2, col = "#CCCCFF", size = 1.1)), 
  txt_gp = fpTxtGp(
    ticks = gpar(fontfamily = "Arial", cex = 0.75),
    label = gpar(fontfamily = "Arial", cex = 1),
    summary = gpar(fontfamily = "Arial", cex = 1)
  ),
  colgap = unit(2, "mm"),
  align = c("c", "c", "c", "c", "c"),
  mar = unit(c(3,4,5,5), "mm")
)
dev.off()



# Make NCO Table ----------------------------------------------------------

table_lables_nco = c('\n\n', '\nSubgroup\n', '\nComparison\n', '\nn\n', 
                 'Atrial \nFibrillation', 'Odds \nRatio \n(95% CI)\n',
                 '\nP-value\n')

or_vec_nco <- c(NA, NA, 
            mcnemar_afib_prim$estimate, NA,
            mcnemar_afib_chf$estimate, NA,
            mcnemar_afib_no_chf$estimate, NA,
            mcnemar_afib_low_chf$estimate, NA,
            mcnemar_afib_low_no_chf$estimate, NA,
            mcnemar_afib_high_chf$estimate, NA,
            mcnemar_afib_high_no_chf$estimate)

low_vec_nco <- c(NA, NA, 
             mcnemar_afib_prim$conf.int[1], NA,
             mcnemar_afib_chf$conf.int[1], NA,
             mcnemar_afib_no_chf$conf.int[1], NA,
             mcnemar_afib_low_chf$conf.int[1], NA,
             mcnemar_afib_low_no_chf$conf.int[1], NA,
             mcnemar_afib_high_chf$conf.int[1], NA,
             mcnemar_afib_high_no_chf$conf.int[1])

high_vec_nco <- c(NA, NA, 
              mcnemar_afib_prim$conf.int[2], NA,
              mcnemar_afib_chf$conf.int[2], NA,
              mcnemar_afib_no_chf$conf.int[2], NA,
              mcnemar_afib_low_chf$conf.int[2], NA,
              mcnemar_afib_low_no_chf$conf.int[2], NA,
              mcnemar_afib_high_chf$conf.int[2], NA,
              mcnemar_afib_high_no_chf$conf.int[2])


p_val_vec_nco <- c(NA, NA, 
               round(mcnemar_afib_prim$p.value, digits = 3), NA,
               round(mcnemar_afib_chf$p.value, digits = 3), NA,
               round(mcnemar_afib_no_chf$p.value, digits = 3), NA,
               round(mcnemar_afib_low_chf$p.value, digits = 3), NA,
               round(mcnemar_afib_low_no_chf$p.value, digits = 3), NA,
               round(mcnemar_afib_high_chf$p.value, digits = 3), NA,
               round(mcnemar_afib_high_no_chf$p.value, digits = 3))


# OR and CI
# Keep two digits, i.e., 1.40, 2.25, etc
CI_nco = paste0(format(or_vec_nco, digits = 3), ' [', format(low_vec_nco, digits = 2) , ', ', 
            format(high_vec_nco, digits = 3) , ']')
CI_nco[is.na(or_vec_nco)] = NA
CI_nco[1] = table_lables_nco[6]

# P-value
p_val_vec_nco[1] = table_lables_nco[7]

# Make the table text
tabletext_nco = cbind(c(table_lables_nco[1], 'Primary Analysis', NA,
                    'Subgroup Analysis', NA, NA, NA, NA, NA, 
                    NA, NA, NA, NA, NA, NA),
                  c(table_lables_nco[2], NA, NA, 'CHF', NA,
                    'No CHF', NA, 'Low Risk + CHF', NA,
                    'Low Risk + No CHF', NA, 'High Risk + CHF', 
                    NA, 'High Risk + No CHF', NA),
                  c(table_lables_nco[3], 'No TOE', 'TOE','No TOE', 'TOE',
                    'No TOE', 'TOE', 'No TOE', 'TOE', 'No TOE', 'TOE',
                    'No TOE', 'TOE', 'No TOE', 'TOE'),
                  c(table_lables_nco[4],
                    tab_afib_cntrl$`n()`,     # no. of obs
                    tab_afib_treat$`n()`, 
                    tab_afib_chf_cntrl$`n()`,
                    tab_afib_chf_treat$`n()`, 
                    tab_afib_no_chf_cntrl$`n()`, 
                    tab_afib_no_chf_treat$`n()`,
                    tab_afib_low_chf_cntrl$`n()`, 
                    tab_afib_low_chf_treat$`n()`, 
                    tab_afib_low_no_chf_cntrl$`n()`,
                    tab_afib_low_no_chf_treat$`n()`,
                    tab_afib_high_chf_cntrl$`n()`,
                    tab_afib_high_chf_treat$`n()`,
                    tab_afib_high_no_chf_cntrl$`n()`,
                    tab_afib_high_no_chf_treat$`n()`),
                  c(table_lables_nco[5],
                    percent(tab_afib_cntrl$`mean(afib)`, accuracy = .01),      # proportions
                    percent(tab_afib_treat$`mean(afib)`, accuracy = .01), 
                    percent(tab_afib_chf_cntrl$`mean(afib)`, accuracy = .01),
                    percent(tab_afib_chf_treat$`mean(afib)`, accuracy = .01), 
                    percent(tab_afib_no_chf_cntrl$`mean(afib)`, accuracy = .01), 
                    percent(tab_afib_no_chf_treat$`mean(afib)`, accuracy = .01),
                    percent(tab_afib_low_chf_cntrl$`mean(afib)`, accuracy = .01), 
                    percent(tab_afib_low_chf_treat$`mean(afib)`, accuracy = .01), 
                    percent(tab_afib_low_no_chf_cntrl$`mean(afib)`, accuracy = .01),
                    percent(tab_afib_low_no_chf_treat$`mean(afib)`, accuracy = .01),
                    percent(tab_afib_high_chf_cntrl$`mean(afib)`, accuracy = .01),
                    percent(tab_afib_high_chf_treat$`mean(afib)`, accuracy = .01),
                    percent(tab_afib_high_no_chf_cntrl$`mean(afib)`, accuracy = .01),
                    percent(tab_afib_high_no_chf_treat$`mean(afib)`, accuracy = .01)),
                  CI_nco,
                  p_val_vec_nco
)



# X ticks in the log-scale
xticks_nco <- c(log(.5), log(1), log(1.5), log(2))
attr(xticks_nco, "labels")  = c('0.5', '1', '1.5', '2')


# width = 6.5 inch, height = 6 inch, fontsize = 7
cairo_pdf('Figure_nco_british.pdf', width = 6.5, height = 6, pointsize = 7)
forestplot(
  tabletext_nco,
  mean = or_vec_nco,
  low = low_vec_nco,
  upper = high_vec_nco,
  is.summary = FALSE,
  col = fpColors(
    box = "darkblue",
    line = "darkblue",
    summary = "royalblue",
    zero = 'white'
  ),
  graph.pos = 7,
  graphwidth = 'auto',
  hrzl_lines = list("2" = gpar(lty=1)),
  lwd.zero = 0.5,
  lwd.ci = 0.5,
  lwd.xaxis = 0.5,
  xticks = xticks_nco,
  xlog = TRUE,
  boxsize = 0.1,
  lineheight = 'auto',
  grid = structure(c(1), 
                   gp = gpar(lty = 2, col = "#CCCCFF", size = 1.1)), 
  txt_gp = fpTxtGp(
    ticks = gpar(fontfamily = "Arial", cex = 0.75),
    label = gpar(fontfamily = "Arial", cex = 1),
    summary = gpar(fontfamily = "Arial", cex = 1)
  ),
  colgap = unit(2, "mm"),
  align = c("c", "c", "c", "c", "c"),
  mar = unit(c(3,4,5,5), "mm")
)
dev.off()

