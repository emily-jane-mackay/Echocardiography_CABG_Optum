
### Files in Optum_TEE_analysis.Rproj

#### 1) create_list_from_scratch.R
Code for create_list_from_scratch function from R package match2C.

#### 2) optum_preprocessing.R
Optum data preprocessing.

#### 3) primary_match.R
Performs statistical matching for primary analysis. Requires optum_preprocessing.R, create_list_from_scratch.R.

#### 4) primary_nco_analysis.R
Performs post match primary and negative control outcome analyses. Requires primary_match.R.

#### 5) rosenbaum_bounds.R
Calculates Rosenbaum bounds for sensitivity analysis. Requires primary_nco_analysis.R.

#### 6) sensitivity_match.R
Performs statistical matching for sensitivity analysis. Requires optum_preprocessing.R, create_list_from_scratch.R.

#### 7) sensitivity_analysis.R
Performs post match sensitivity analysis. Requires sensitivity_match.R.

#### 8) unadjusted_analysis.R
Performs unadjusted data analysis. Requires optum_preprocessing.R.

#### 9) forest_plot_fig.R
Creates forest plot for primary and negative control outcomes analysis. Requires primary_match.R.





