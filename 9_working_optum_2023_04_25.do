* setup
set more off
set linesize 255

* ensure correct working directory
cd "/path/"

* path
local file_name = "working_optum_2023_04_25"
local path = "/Users/emily2/Dropbox/Echo_Optum/STATA/"
log using "`path'`file_name'.txt", text replace

* import the .dta file  
use "/path/data_complete_2022_07_08.dta", clear 
* change all cases to lowercase 
ren *, lower

// To DO // 

* process date data
* death date
generate date_death = date(ymdod, "YM")

* generate patid_str variable = patid 
tostring patid, generate(patid_str) format(%11.0f)
* generate pat_planid string variable  
tostring pat_planid, generate(pat_planid_str) format(%11.0f)

* admit_date and disch_date are already in Stata-format (because exported from sas in .dta)
tabulate admit_date
tabulate disch_date 

* age 
tostring yrdob_co_e, replace
generate date_of_birth = date(yrdob_co_e, "Y")
gen age_years = (admit_date - date_of_birth) / 365.25
summarize age_years, detail 

* year
gen yr = year(admit_date)

* days between tee and surgery
generate tee_to_srg = fst_dt_srg - fst_dt_tee 

* death with duplicates
* sort the data based on admit_date before tagging duplicates
duplicates report patid_str pat_planid_str conf_id admit_date 
gsort patid_str pat_planid_str admit_date 

* flag duplicate patid and conf_id
duplicates report patid conf_id
duplicates tag patid conf_id, gen(patid_conf_id_dup)

* flag duplicate patid 
duplicates report patid 
duplicates tag patid, gen(patid_dup)

* confinement to death
generate conf_to_death_ind = (date_death - admit_date) 
summarize conf_to_death_ind, detail
* death assumption (1): assume death on 1st of month 
* deal with negative values
browse patid_str conf_id ymdod conf_to_death_ind date_death admit_date disch_date if conf_to_death_ind > -45 & conf_to_death_ind < 0
* death assumption (2): any patid with a negtaive value between -30 & 0 is assumed to have died during the admission for index surgery (e.g. admitted on 12/15/2013 died in "dec 2013" would have a -15 "conf_to_death_ind")
browse patid_str conf_id ymdod conf_to_death_ind date_death admit_date disch_date if conf_to_death_ind > -45 & conf_to_death_ind < -30
* exclude spurious observation with a conf_to_death_ind >-30 (e.g. only one observation with -44 value)

* ECMO CPT codes
generate ecmo_cpt = 0
replace ecmo_cpt = 1 if cpt_33947_srg == 1 | cpt_33948_srg == 1 | cpt_33949_srg == 1 | cpt_33952 == 1 | cpt_33954 == 1 | cpt_33956_srg == 1 | cpt_33958 == 1 | cpt_33962_srg == 1 | cpt_33964_srg == 1 | cpt_33966_srg == 1 | cpt_33984_srg == 1 | cpt_33986_srg == 1 | cpt_33987_srg == 1 | cpt_33988_srg == 1 | cpt_33989_srg == 1 
tab yr ecmo_cpt, miss

* generate drg for ct surgery indicator
generate drg_ct = 0 
replace drg_ct = 1 if drg == "1" | drg == "2" | drg == "3" | drg == "7" | drg == "215" | drg == "216" | drg == "217" | drg == "218" | drg == "219" | drg == "220" | drg == "221" | drg == "222" | drg == "223" | drg == "224" | drg == "225" | drg == "226" | drg == "227" | drg == "228" | drg == "229" | drg == "230" | drg == "231" | drg == "232" | drg == "233" | drg == "234" | drg == "235" | drg == "236" | drg == "237" | drg == "238" | drg == "266" | drg == "267" | drg == "280" | drg == "281" | drg == "282" | drg == "283" | drg == "300" | drg == "302" | drg == "303" | drg == "306" | drg == "307" | drg == "314" | drg == "326" | drg == "853" | drg == "854" | drg == "856" | drg == "857" | drg == "907" | drg == "908" | drg == "909" | drg == "969" | drg == "970" | drg == "981" | drg == "982" | drg == "983"

* tee variable 
generate TEE = 0
replace TEE = 1 if tee_tee == 1 
* tee +/- 2 calendar days of fst_dt_srg 
generate TEE_intraop = 0
replace TEE_intraop = 1 if TEE == 1 & (tee_to_srg >= -2 & tee_to_srg <= +2)

* generate 30-day death indicator variable 
generate death_ind_30 = 0 
replace death_ind_30 = 1 if (conf_to_death_ind <=0 & conf_to_death_ind > -30) | (conf_to_death_ind >=1 & conf_to_death_ind <=30)
summarize conf_to_death if death_ind_30 == 1, detail
* death_ind_30 is 30-day death 
generate death_30 = death_ind_30

* generate 60-day death indicator variable 
generate death_ind_60 = 0 
replace death_ind_60 = 1 if (conf_to_death_ind <=0 & conf_to_death_ind > -30) | (conf_to_death_ind >=1 & conf_to_death_ind <=60)
summarize conf_to_death if death_ind_60 == 1, detail
* death_ind_60 is 60-day death (death within 60 days of admission)
generate death_60 = death_ind_60

* generate death_30 | stroke outcome
generate death_30_or_stroke = 0
replace death_30_or_stroke = 1 if death_30 == 1 | complic_stroke == 1

* generate death_ind | stroke outcome
generate death_ind_or_stroke = 0
replace death_ind_or_stroke = 1 if death_ind_30 == 1 | complic_stroke == 1

* ///////////////////////////////////////////////////////////////////////////////

* cohort development duplicates 

* ///////////////////////////////////////////////////////////////////////////////


* indicator for all surgeries - duplicates (patid & conf_id duplicates) 
tab cv 
* starting count: 195,826
* look at observations with duplicate patid & conf_id 
tab cv patid_conf_id_dup 
* exclude observations with conf_id duplicates
* exclusion count
count if cv == 1 & patid_conf_id_dup == 1 
* 136
* inclusion count 
count if cv == 1 & patid_conf_id_dup == 0
* 195,690
* first exclude 
generate exclude_1 = 0
replace exclude_1 = 1 if patid_conf_id_dup == 1 
* sanity check
tab cv exclude_1, miss

* indicator for all surgeries - duplicates (patid duplicates)
* note: may use these subsequent confinements for additional cardiac surgeries as a reoperation outcome 
* but for preliminary analyses exclude 
tab cv if patid_conf_id_dup == 0 
* look at observations with duplicate patid (but subsequent confinemenets) 
tab patid_dup cv if patid_conf_id_dup == 0 
* exclude at all but first patient-confinement for cardiac surgery (e.g. eliminate subsequent confinements for a given patid)
* exclusion count
count if cv == 1 & patid_conf_id_dup == 0 & patid_dup > 0
* 4,365
* inclusion count
count if cv == 1 & patid_conf_id_dup == 0 & patid_dup == 0
* 191,325 
* second exclude 
replace exclude_1 = 1 if patid_conf_id_dup == 1 | patid_dup > 0
* sanity check 
tab cv exclude_1, miss

* years 2007 - 2019 valves/CABG/both
count if cv == 1 & patid_conf_id_dup == 0 & patid_dup == 0
* 191,325 
* count if yr >=2016
tab yr if cv == 1 & patid_conf_id_dup == 0 & patid_dup == 0
* exclude surgeries occuring prior to 01/01/2016
* inclusion count
count if cv == 1 & patid_conf_id_dup == 0 & patid_dup == 0 & yr >= 2016 
* 76,518
* exclusion count
count if cv == 1 & patid_conf_id_dup == 0 & patid_dup == 0 & yr < 2016
* 114,807
* third exclude 
replace exclude_1 = 1 if patid_conf_id_dup == 1 | patid_dup > 0 | yr < 2016
* sanity check
tab cv exclude_1, miss

* ///////////////////////////////////////////////////////////////////////////////

* cohort development manuscript (2016 - 2020) starting count  

* ///////////////////////////////////////////////////////////////////////////////

* n = 76,518
* starting count for valves/CABG/both AFTER 2016 = 76,518
* look at prov_type distribution 
* note: prov_type indicates npi type & merged to confinement data using prov (& facility level npi if prov_type != 1)
tab prov_type cv if cv == 1 & patid_conf_id_dup == 0 & patid_dup == 0 & yr >= 2016, miss
* exclude all observations with an individual prov_type 
* exclusion count
count if cv == 1 & patid_conf_id_dup == 0 & patid_dup == 0 & yr >= 2016 & prov_type == "1"
* 463
* inclusion count
count if cv == 1 & patid_conf_id_dup == 0 & patid_dup == 0 & yr >= 2016 & prov_type != "1"
* 76,055
* fourth exclude
replace exclude_1 = 1 if patid_conf_id_dup == 1 | patid_dup > 0 | yr < 2016 | prov_type == "1"
* sanity check
tab cv exclude_1, miss

* look at gdr_cd_co_e distribution 
tab gdr_cd_co_e cv if cv == 1 & patid_conf_id_dup == 0 & patid_dup == 0 & yr >= 2016 & prov_type != "1", miss 
* exclude all observations with an unknown gender code
* exclusion count
count if cv == 1 & patid_conf_id_dup == 0 & patid_dup == 0 & yr >= 2016 & prov_type != "1" & gdr_cd_co_e == "U"
* 3
* inclusion count
count if cv == 1 & patid_conf_id_dup == 0 & patid_dup == 0 & yr >= 2016 & prov_type != "1" & gdr_cd_co_e != "U"
* 76,052
* fifth exclude 
replace exclude_1 = 1 if patid_conf_id_dup == 1 | patid_dup > 0 | yr < 2016 | prov_type == "1" | gdr_cd_co_e == "U"
* sanity check 
tab cv exclude_1, miss

* ///////////////////////////////////////////////////////////////////////////////

* deal with continuous enrollment issue  
gsort patid_str pat_planid conf_id admit_date clmid_tee clmid_srg
duplicates report patid_str if exclude_1 != 1  

* sanity check 
list patid_str admit_date eligeff_e fst_dt_srg eligeff_co_e eligend_co_e eligend_e if patid_str == "33003282085" & exclude_1 != 1

* generate day count variable from date of enrollment to admission 
gsort patid_str admit_date 
bysort patid_str: generate enroll_to_admit = eligeff_co_e - admit_date if exclude_1 != 1  

* sanity check 
list patid_str admit_date enroll_to_admit eligeff_e fst_dt_srg eligeff_co_e eligend_co_e eligend_e if patid_str == "33003282085" & exclude_1 != 1

* generate day count variable from date of admission to end of enrollment 
gsort patid_str admit_date 
bysort patid_str: generate admit_to_deenroll = eligend_co_e - admit_date if exclude_1 != 1  

* sanity check 
list patid_str admit_date enroll_to_admit admit_to_deenroll eligeff_e fst_dt_srg eligeff_co_e eligend_co_e eligend_e if patid_str == "33003282085" & exclude_1 != 1

* at least 3 months continuous enrollment prior to admission 
bysort patid_str: generate enroll_90_day_pre_index = enroll_to_admit <= -90 & exclude_1 != 1 
* look at observations in new variable 
tabulate enroll_90_day_pre_index if exclude_1 != 1 
* sanity check
list patid_str admit_date enroll_to_admit admit_to_deenroll eligeff_e fst_dt_srg eligeff_co_e eligend_co_e eligend_e if patid_str == "33003282085" & exclude_1 != 1 & enroll_90_day_pre_index == 1

* at least 60 days continuous enrollment post-admission
bysort patid_str: generate enroll_60_day_post_index = admit_to_deenroll >= 60 & exclude_1 != 1 
* sanity check 
summarize enroll_to_admit if enroll_60_day_post_index == 1 & exclude_1 != 1, detail
* look at observations in new variable
tabulate enroll_60_day_post_index if exclude_1 != 1

* at least 3 months continuous enrollment post-admission
bysort patid_str: generate enroll_90_day_post_index = admit_to_deenroll >= 90 & exclude_1 != 1 
* sanity check 
summarize enroll_to_admit if enroll_90_day_post_index == 1 & exclude_1 != 1, detail
* look at observations in new variable
tabulate enroll_90_day_post_index if exclude_1 != 1

* at least 1 year continuous enrollment after surgery 
bysort patid_str: generate enroll_365_day_post_index = admit_to_deenroll >= 365 & exclude_1 != 1 
* look at observations in new variable
tabulate enroll_365_day_post_index if exclude_1 != 1
* sanity check 
list patid_str admit_date enroll_to_admit admit_to_deenroll eligeff_e fst_dt_srg eligeff_co_e eligend_co_e eligend_e enroll_365_day_post_index enroll_90_day_pre_index if patid_str == "33003282085" & exclude_1 != 1

* at least 2 years continuous enrollment after surgery 
bysort patid_str: generate enroll_760_day_post_index = admit_to_deenroll >= 760 & exclude_1 != 1 
* look at observations in new variable
tabulate enroll_760_day_post_index if exclude_1 != 1
* sanity check 
list patid_str admit_date enroll_to_admit admit_to_deenroll eligeff_e fst_dt_srg eligeff_co_e eligend_co_e eligend_e enroll_365_day_post_index enroll_90_day_pre_index if patid_str == "33003282085" & exclude_1 != 1

* ///////////////////////////////////////////////////////////////////////////////

* additional exclusion criteria - 90 days of continuous enrollment pre-surgery

* ///////////////////////////////////////////////////////////////////////////////

* look at pre-index continuous enrollment 
tab enroll_90_day_pre_index cv if exclude_1 != 1
* exclude observations without 90 days of pre-index enrollment 
* exclusion count
count if cv == 1 & exclude_1 != 1 & enroll_90_day_pre_index == 0 
* 5,661 
* inclusion count 
count if cv == 1 & exclude_1 != 1 & enroll_90_day_pre_index == 1
* 70,391 
* sixth exclude
replace exclude_1 = 1 if patid_conf_id_dup == 1 | patid_dup > 0 | yr < 2016 | prov_type == "1" | gdr_cd_co_e == "U" | enroll_90_day_pre_index == 0 
* sanity check 
tab cv exclude_1, miss

* ///////////////////////////////////////////////////////////////////////////////

* additional exclusion criteria - ECMO 

* ///////////////////////////////////////////////////////////////////////////////

* look at ECMO (cpt codes > 2016)
tab ecmo_cpt cv if exclude_1 != 1
* exclude observations with a cpt code for ECMO 
* exclusion count
count if cv == 1 & exclude_1 != 1 & ecmo_cpt == 1
* 872
* inclusion count
count if cv == 1 & exclude_1 != 1 & ecmo_cpt != 1
* 69,519 
* seventh exclude 
replace exclude_1 = 1 if patid_conf_id_dup == 1 | patid_dup > 0 | yr < 2016 | prov_type == "1" | gdr_cd_co_e == "U" | enroll_90_day_pre_index == 0 | ecmo_cpt == 1
* sanity check
tabulate cv exclude_1, miss

* ///////////////////////////////////////////////////////////////////////////////

* note final cohort n = 69,519 valve/CABG/both 2016 - 2020 

* ///////////////////////////////////////////////////////////////////////////////


* generate tee probability by npi variable - entire cohort 
egen tee_prob_npi_2016_2020 = mean(TEE) if exclude_1 !=1, by(npi) 
* sanity check 
summarize tee_prob_npi_2016_2020 if exclude_1 != 1, detail


* surgical volume 
egen surgical_volume_npi_2016_2020 = count(cv) if exclude_1 != 1, by(npi)
summarize surgical_volume_npi if exclude_1 != 1, detail 

* race
generate race_ethnicity_str = " "
replace race_ethnicity_str = "Asian" if race_co_e == "A"
replace race_ethnicity_str = "Black" if race_co_e == "B"
replace race_ethnicity_str = "Hispanic" if race_co_e == "H"
replace race_ethnicity_str = "White" if race_co_e == "W"
replace race_ethnicity_str = "Unknown" if race_co_e == "" 
* encode race 
encode race_ethnicity_str, gen(race_e)

* encode state 
encode state_e, generate(state_e_encoded)

* encode gender 
encode gdr_cd_co_e, generate(gender_e)

* ///////////////////////////////////////////////////////////////////////////////

* recategorize observations without a surgery using icd10p codes from hospitalization 

* ///////////////////////////////////////////////////////////////////////////////

foreach v of varlist proc1 proc2 proc3 proc4 proc5 {
	icd10p generate cco_`v' = `v', range(5A1221Z)
}

gen cco_10 = cco_proc1 == 1 | cco_proc2 == 1 | cco_proc3 == 1 | cco_proc4 == 1 | cco_proc5 == 1 
drop cco_proc*

foreach v of varlist proc1 proc2 proc3 proc4 proc5 {
	gen cco_9_`v' = cond((`v' == "8963" | `v' == "8964" | `v' == "8966" | `v' == "8967" | `v' == "8968"), 1, 0)
}

gen cco_9 = cco_9_proc1 == 1 | cco_9_proc2 == 1 | cco_9_proc3 == 1 | cco_9_proc4 == 1 | cco_9_proc5 == 1 
drop cco_9_proc*

/* 
note: cpt code "93503" is for PAC but isn't in this dataset because wasn't pulled from anesthesia CPT codes - only surgery cpt codes were pulled from original
*/ 


* ///////////////////////////////////////////////////////////////////////////////

foreach v of varlist proc1 proc2 proc3 proc4 proc5 {
	icd10p generate valve_`v' = `v', range(027F0ZZ 02NF0ZZ 02QF0ZZ 027G0ZZ 02NG0ZZ 02QG0ZZ 027H0ZZ 02NH0ZZ 02QH0ZZ 027J0ZZ 02NJ0ZZ 02QJ0ZZ 02RF07Z 02RF08Z 02RF0KZ 02RF0JZ 02RG07Z 02RG08Z 02RG0KZ 02RG0JZ 02RH07Z 02RH08Z 02RH0KZ 02RH0JZ 02RJ07Z 02RJ08Z 02RJ0KZ 02RJ0JZ 02UF0JZ 02UG0JZ 02UH0JZ 02UJ0JZ)
}

gen valve_srg = valve_proc1 == 1 | valve_proc2 == 1 | valve_proc3 == 1 | valve_proc4 == 1 | valve_proc5 == 1
drop valve_proc*

gen valve_srg_top = valve_srg == 1 | index_valve_1 == 1 

* ///////////////////////////////////////////////////////////////////////////////
/*
027F0ZZ Dilation of Aortic Valve, Open Approach -
027G0ZZ Dilation of Mitral Valve, Open Approach - 
027H0ZZ Dilation of Pulmonary Valve, Open Approach - 
027J0ZZ Dilation of Tricuspid Valve, Open Approach - 
02NF0ZZ Release Aortic Valve, Open Approach -
02NG0ZZ Release Mitral Valve, Open Approach - 
02NH0ZZ Release Pulmonary Valve, Open Approach - 
02NJ0ZZ Release Tricuspid Valve, Open Approach - 
02QF0ZZ Repair Aortic Valve, Open Approach -
02QG0ZZ Repair Mitral Valve, Open Approach - 
02QH0ZZ Repair Pulmonary Valve, Open Approach - 
02QJ0ZZ Repair Tricuspid Valve, Open Approach - 
02RF07Z Replacement of Aortic Valve with Autol Sub, Open Approach -
02RF08Z Replacement of Aortic Valve with Zooplastic, Open Approach -
02RF0JZ Replacement of Aortic Valve with Synth Sub, Open Approach -
02RF0KZ Replacement of Aortic Valve with Nonaut Sub, Open Approach -
02RG07Z Replacement of Mitral Valve with Autol Sub, Open Approach -
02RG08Z Replacement of Mitral Valve with Zooplastic, Open Approach -
02RG0JZ Replacement of Mitral Valve with Synth Sub, Open Approach -
02RG0KZ Replacement of Mitral Valve with Nonaut Sub, Open Approach -
02RH07Z Replacement of Pulmonary Valve with Autol Sub, Open Approach -
02RH08Z Replacement of Pulm Valve with Zooplastic, Open Approach -
02RH0JZ Replacement of Pulmonary Valve with Synth Sub, Open Approach -
02RH0KZ Replacement of Pulm Valve with Nonaut Sub, Open Approach -
02RJ07Z Replacement of Tricuspid Valve with Autol Sub, Open Approach -
02RJ08Z Replacement of Tricusp Valve with Zooplastic, Open Approach -
02RJ0JZ Replacement of Tricuspid Valve with Synth Sub, Open Approach -
02RJ0KZ Replacement of Tricusp Valve with Nonaut Sub, Open Approach -
02UF0JZ Supplement Aortic Valve with Synth Sub, Open Approach - 
02UG0JZ Supplement Mitral Valve with Synth Sub, Open Approach - 
02UH0JZ Supplement Pulmonary Valve with Synth Sub, Open Approach - 
02UJ0JZ Supplement Tricuspid Valve with Synth Sub, Open Approach - 
*/ 

* av repair 
foreach v of varlist proc1 proc2 proc3 proc4 proc5 {
	icd10p generate av_repair_`v' = `v', range(027F0ZZ 02NF0ZZ 02QF0ZZ 02UF0JZ)
}

gen valve_av_repair = av_repair_proc1 == 1 | av_repair_proc2 == 1 | av_repair_proc3 == 1 | av_repair_proc4 == 1 | av_repair_proc5 == 1 | index_av_repair_1 == 1 
drop av_repair_proc*

* av replace
foreach v of varlist proc1 proc2 proc3 proc4 proc5 {
	icd10p generate av_replace_`v' = `v', range(02RF07Z 02RF08Z 02RF0JZ 02RF0KZ)
}
 
gen valve_av_replace = av_replace_proc1 == 1 | av_replace_proc2 == 1 | av_replace_proc3 == 1 | av_replace_proc4 == 1 | av_replace_proc5 == 1 | index_av_replace_1 == 1  
drop av_replace_proc* 

* mv repair 
foreach v of varlist proc1 proc2 proc3 proc4 proc5 {
	icd10p generate mv_repair_`v' = `v', range(027G0ZZ 02NG0ZZ 02QG0ZZ 02UG0JZ)
}

gen valve_mv_repair = mv_repair_proc1 == 1 | mv_repair_proc2 == 1 | mv_repair_proc3 == 1 | mv_repair_proc4 == 1 | mv_repair_proc5 == 1 | index_mv_repair_1 == 1 
drop mv_repair_proc*

* mv replace
foreach v of varlist proc1 proc2 proc3 proc4 proc5 {
	icd10p generate mv_replace_`v' = `v', range(02RG07Z 02RG08Z 02RG0JZ 02RG0KZ)
}

gen valve_mv_replace = mv_replace_proc1 == 1 | mv_replace_proc2 == 1 | mv_replace_proc3 == 1 | mv_replace_proc4 == 1 | mv_replace_proc5 == 1 | index_mv_replace_1 == 1 
drop mv_replace_proc*

* pv repair 
foreach v of varlist proc1 proc2 proc3 proc4 proc5 {
	icd10p generate pv_repair_`v' = `v', range(027H0ZZ 02NH0ZZ 02QH0ZZ 02UH0JZ)
}

gen valve_pv_repair = pv_repair_proc1 == 1 | pv_repair_proc2 == 1 | pv_repair_proc3 == 1 | pv_repair_proc4 == 1 | pv_repair_proc5 == 1 | index_pv_repair_1 == 1 
drop pv_repair_proc*

* pv replace 
foreach v of varlist proc1 proc2 proc3 proc4 proc5 {
	icd10p generate pv_replace_`v' = `v', range(02RH07Z 02RH08Z 02RH0JZ 02RH0KZ)
}

gen valve_pv_replace = pv_replace_proc1 == 1 | pv_replace_proc2 == 1 | pv_replace_proc3 == 1 | pv_replace_proc4 == 1 | pv_replace_proc5 == 1 | index_pv_replace_1 == 1 
drop pv_replace_proc*

* tv repair 
foreach v of varlist proc1 proc2 proc3 proc4 proc5 {
	icd10p generate tv_repair_`v' = `v', range(027J0ZZ 02NJ0ZZ 02QJ0ZZ 02UJ0JZ)
}

gen valve_tv_repair = tv_repair_proc1 == 1 | tv_repair_proc2 == 1 | tv_repair_proc3 == 1 | tv_repair_proc4 == 1 | tv_repair_proc5 == 1 | index_tv_repair_1 == 1 
drop tv_repair_proc*


* tv replace 
foreach v of varlist proc1 proc2 proc3 proc4 proc5 {
	icd10p generate tv_replace_`v' = `v', range(02RJ07Z 02RJ08Z 02RJ0JZ 02RJ0KZ)
}

gen valve_tv_replace = tv_replace_proc1 == 1 | tv_replace_proc2 == 1 | tv_replace_proc3 == 1 | tv_replace_proc4 == 1 | tv_replace_proc5 == 1 | index_tv_replace_1 == 1 
drop tv_replace_proc*

* pv repair/replace
* combine pulmonic repair and replace
gen valve_pvr = valve_pv_repair == 1 | valve_pv_replace == 1 


* ///////////////////////////////////////////////////////////////////////////////

foreach v of varlist proc1 proc2 proc3 proc4 proc5 {
	icd10p generate cabg_`v' = `v', range(0210093 0210098 0210099 021009C 021009F 021009W 02100A3 02100A8 02100A9 02100AC 02100AF 02100AW 02100J3 02100J8 02100J9 02100JC 02100JF 02100JW 02100K3 02100K8 02100K9 02100KC 02100KF 02100KW 02100Z3 02100Z8 02100Z9 02100ZC 02100ZF 0210493 0210498 0210499 021049C 021049F 021049W 02104A3 02104A8 02104A9 02104AC 02104AF 02104AW 02104J3 02104J8 02104J9 02104JC 02104JF 02104JW 02104K3 02104K8 02104K9 02104KC 02104KF 02104KW 02104Z3 02104Z8 02104Z9 02104ZC 02104ZF 0211093 0211098 0211099 021109C 021109F 021109W 02110A3 02110A8 02110A9 02110AC 02110AF 02110AW 02110J3 02110J8 02110J9 02110JC 02110JF 02110JW 02110K3 02110K8 02110K9 02110KC 02110KF 02110KW 02110Z3 02110Z8 02110Z9 02110ZC 02110ZF 0211493 0211498 0211499 021149C 021149F 021149W 02114A3 02114A8 02114A9 02114AC 02114AF 02114AW 02114J3 02114J8 02114J9 02114JC 02114JF 02114JW 02114K3 02114K8 02114K9 02114KC 02114KF 02114KW 02114Z3 02114Z8 02114Z9 02114ZC 02114ZF 0212093 0212098 0212099 021209C 021209F 021209W 02120A3 02120A8 02120A9 02120AC 02120AF 02120AW 02120J3 02120J8 02120J9 02120JC 02120JF 02120JW 02120K3 02120K8 02120K9 02120KC 02120KF 02120KW 02120Z3 02120Z8 02120Z9 02120ZC 02120ZF 0212493 0212498 0212499 021249C 021249F 021249W 02124A3 02124A8 02124A9 02124AC 02124AF 02124AW 02124J3 02124J8 02124J9 02124JC 02124JF 02124JW 02124K3 02124K8 02124K9 02124KC 02124KF 02124KW 02124Z3 02124Z8 02124Z9 02124ZC 02124ZF 0213093 0213098 0213099 021309C 021309F 021309W 02130A3 02130A8 02130A9 02130AC 02130AF 02130AW 02130J3 02130J8 02130J9 02130JC 02130JF 02130JW 02130K3 02130K8 02130K9 02130KC 02130KF 02130KW 02130Z3 02130Z8 02130Z9 02130ZC 02130ZF 0213493 0213498 0213499 021349C 021349F 021349W 02134A3 02134A8 02134A9 02134AC 02134AF 02134AF 02134J3 02134J8 02134J9 02134JC 02134JF 02134JW 02134K3 02134K8 02134K9 02134KC 02134KF 02134KW 02134Z3 02134Z8 02134Z9 02134ZC 02134ZF)
}

gen cabg_srg = cabg_proc1 == 1 | cabg_proc2 == 1 | cabg_proc3 == 1 | cabg_proc4 == 1 | cabg_proc5 == 1 
drop cabg_proc*

gen cabg_srg_top = cabg_srg == 1 | index_cabg_1 == 1

* ///////////////////////////////////////////////////////////////////////////////

* sanity check for missing observations
tabulate cabg_srg_top valve_srg_top if exclude_1 != 1, miss
browse proc1-proc5 if cabg_srg_top == 1 & valve_srg_top == 0 & exclude_1 != 1

/*
 0210093 Bypass 1 Cor Art from Cor Art with Autol Vn, Open Approach
 0210098 Bypass 1 Cor Art from R Int Mammary w Autol Vn, Open
 0210099 Bypass 1 Cor Art from L Int Mammary w Autol Vn, Open
 021009C Bypass 1 Cor Art from Thor Art with Autol Vn, Open Approach
 021009F Bypass 1 Cor Art from Abd Art with Autol Vn, Open Approach
 021009W Bypass 1 Cor Art from Aorta with Autol Vn, Open Approach
 02100A3 Bypass 1 Cor Art from Cor Art with Autol Art, Open Approach
 02100A8 Bypass 1 Cor Art from R Int Mammary w Autol Art, Open
 02100A9 Bypass 1 Cor Art from L Int Mammary w Autol Art, Open
 02100AC Bypass 1 Cor Art from Thor Art with Autol Art, Open Approach
 02100AF Bypass 1 Cor Art from Abd Art with Autol Art, Open Approach
 02100AW Bypass 1 Cor Art from Aorta with Autol Art, Open Approach
 02100J3 Bypass 1 Cor Art from Cor Art with Synth Sub, Open Approach
 02100J8 Bypass 1 Cor Art from R Int Mammary w Synth Sub, Open
 02100J9 Bypass 1 Cor Art from L Int Mammary w Synth Sub, Open
 02100JC Bypass 1 Cor Art from Thor Art with Synth Sub, Open Approach
 02100JF Bypass 1 Cor Art from Abd Art with Synth Sub, Open Approach
 02100JW Bypass 1 Cor Art from Aorta with Synth Sub, Open Approach
 02100K3 Bypass 1 Cor Art from Cor Art with Nonaut Sub, Open Approach
 02100K8 Bypass 1 Cor Art from R Int Mammary w Nonaut Sub, Open
 02100K9 Bypass 1 Cor Art from L Int Mammary w Nonaut Sub, Open
 02100KC Bypass 1 Cor Art from Thor Art w Nonaut Sub, Open
 02100KF Bypass 1 Cor Art from Abd Art with Nonaut Sub, Open Approach
 02100KW Bypass 1 Cor Art from Aorta with Nonaut Sub, Open Approach
 02100Z3 Bypass 1 Cor Art from Cor Art, Open Approach
 02100Z8 Bypass 1 Cor Art from R Int Mammary, Open Approach
 02100Z9 Bypass 1 Cor Art from L Int Mammary, Open Approach
 02100ZC Bypass 1 Cor Art from Thor Art, Open Approach
 02100ZF Bypass 1 Cor Art from Abd Art, Open Approach
 0210493 Bypass 1 Cor Art from Cor Art w Autol Vn, Perc Endo
 0210498 Bypass 1 Cor Art from R Int Mammary w Autol Vn, Perc Endo
 0210499 Bypass 1 Cor Art from L Int Mammary w Autol Vn, Perc Endo
 021049C Bypass 1 Cor Art from Thor Art w Autol Vn, Perc Endo
 021049F Bypass 1 Cor Art from Abd Art w Autol Vn, Perc Endo
 021049W Bypass 1 Cor Art from Aorta w Autol Vn, Perc Endo
 02104A3 Bypass 1 Cor Art from Cor Art w Autol Art, Perc Endo
 02104A8 Bypass 1 Cor Art from R Int Mammary w Autol Art, Perc Endo
 02104A9 Bypass 1 Cor Art from L Int Mammary w Autol Art, Perc Endo
 02104AC Bypass 1 Cor Art from Thor Art w Autol Art, Perc Endo
 02104AF Bypass 1 Cor Art from Abd Art w Autol Art, Perc Endo
 02104AW Bypass 1 Cor Art from Aorta w Autol Art, Perc Endo
 02104J3 Bypass 1 Cor Art from Cor Art w Synth Sub, Perc Endo
 02104J8 Bypass 1 Cor Art from R Int Mammary w Synth Sub, Perc Endo
 02104J9 Bypass 1 Cor Art from L Int Mammary w Synth Sub, Perc Endo
 02104JC Bypass 1 Cor Art from Thor Art w Synth Sub, Perc Endo
 02104JF Bypass 1 Cor Art from Abd Art w Synth Sub, Perc Endo
 02104JW Bypass 1 Cor Art from Aorta w Synth Sub, Perc Endo
 02104K3 Bypass 1 Cor Art from Cor Art w Nonaut Sub, Perc Endo
 02104K8 Bypass 1 Cor Art from R Int Mammary w Nonaut Sub, Perc Endo
 02104K9 Bypass 1 Cor Art from L Int Mammary w Nonaut Sub, Perc Endo
 02104KC Bypass 1 Cor Art from Thor Art w Nonaut Sub, Perc Endo
 02104KF Bypass 1 Cor Art from Abd Art w Nonaut Sub, Perc Endo
 02104KW Bypass 1 Cor Art from Aorta w Nonaut Sub, Perc Endo
 02104Z3 Bypass 1 Cor Art from Cor Art, Perc Endo Approach
 02104Z8 Bypass 1 Cor Art from R Int Mammary, Perc Endo Approach
 02104Z9 Bypass 1 Cor Art from L Int Mammary, Perc Endo Approach
 02104ZC Bypass 1 Cor Art from Thor Art, Perc Endo Approach
 02104ZF Bypass 1 Cor Art from Abd Art, Perc Endo Approach
 0211093 Bypass 2 Cor Art from Cor Art with Autol Vn, Open Approach
 0211098 Bypass 2 Cor Art from R Int Mammary w Autol Vn, Open
 0211099 Bypass 2 Cor Art from L Int Mammary w Autol Vn, Open
 021109C Bypass 2 Cor Art from Thor Art with Autol Vn, Open Approach
 021109F Bypass 2 Cor Art from Abd Art with Autol Vn, Open Approach
 021109W Bypass 2 Cor Art from Aorta with Autol Vn, Open Approach
 02110A3 Bypass 2 Cor Art from Cor Art with Autol Art, Open Approach
 02110A8 Bypass 2 Cor Art from R Int Mammary w Autol Art, Open
 02110A9 Bypass 2 Cor Art from L Int Mammary w Autol Art, Open
 02110AC Bypass 2 Cor Art from Thor Art with Autol Art, Open Approach
 02110AF Bypass 2 Cor Art from Abd Art with Autol Art, Open Approach
 02110AW Bypass 2 Cor Art from Aorta with Autol Art, Open Approach
 02110J3 Bypass 2 Cor Art from Cor Art with Synth Sub, Open Approach
 02110J8 Bypass 2 Cor Art from R Int Mammary w Synth Sub, Open
 02110J9 Bypass 2 Cor Art from L Int Mammary w Synth Sub, Open
 02110JC Bypass 2 Cor Art from Thor Art with Synth Sub, Open Approach
 02110JF Bypass 2 Cor Art from Abd Art with Synth Sub, Open Approach
 02110JW Bypass 2 Cor Art from Aorta with Synth Sub, Open Approach
 02110K3 Bypass 2 Cor Art from Cor Art with Nonaut Sub, Open Approach
 02110K8 Bypass 2 Cor Art from R Int Mammary w Nonaut Sub, Open
 02110K9 Bypass 2 Cor Art from L Int Mammary w Nonaut Sub, Open
 02110KC Bypass 2 Cor Art from Thor Art w Nonaut Sub, Open
 02110KF Bypass 2 Cor Art from Abd Art with Nonaut Sub, Open Approach
 02110KW Bypass 2 Cor Art from Aorta with Nonaut Sub, Open Approach
 02110Z3 Bypass 2 Cor Art from Cor Art, Open Approach
 02110Z8 Bypass 2 Cor Art from R Int Mammary, Open Approach
 02110Z9 Bypass 2 Cor Art from L Int Mammary, Open Approach
 02110ZC Bypass 2 Cor Art from Thor Art, Open Approach
 02110ZF Bypass 2 Cor Art from Abd Art, Open Approach
 0211493 Bypass 2 Cor Art from Cor Art w Autol Vn, Perc Endo
 0211498 Bypass 2 Cor Art from R Int Mammary w Autol Vn, Perc Endo
 0211499 Bypass 2 Cor Art from L Int Mammary w Autol Vn, Perc Endo
 021149C Bypass 2 Cor Art from Thor Art w Autol Vn, Perc Endo
 021149F Bypass 2 Cor Art from Abd Art w Autol Vn, Perc Endo
 021149W Bypass 2 Cor Art from Aorta w Autol Vn, Perc Endo
 02114A3 Bypass 2 Cor Art from Cor Art w Autol Art, Perc Endo
 02114A8 Bypass 2 Cor Art from R Int Mammary w Autol Art, Perc Endo
 02114A9 Bypass 2 Cor Art from L Int Mammary w Autol Art, Perc Endo
 02114AC Bypass 2 Cor Art from Thor Art w Autol Art, Perc Endo
 02114AF Bypass 2 Cor Art from Abd Art w Autol Art, Perc Endo
 02114AW Bypass 2 Cor Art from Aorta w Autol Art, Perc Endo
 02114J3 Bypass 2 Cor Art from Cor Art w Synth Sub, Perc Endo
 02114J8 Bypass 2 Cor Art from R Int Mammary w Synth Sub, Perc Endo
 02114J9 Bypass 2 Cor Art from L Int Mammary w Synth Sub, Perc Endo
 02114JC Bypass 2 Cor Art from Thor Art w Synth Sub, Perc Endo
 02114JF Bypass 2 Cor Art from Abd Art w Synth Sub, Perc Endo
 02114JW Bypass 2 Cor Art from Aorta w Synth Sub, Perc Endo
 02114K3 Bypass 2 Cor Art from Cor Art w Nonaut Sub, Perc Endo
 02114K8 Bypass 2 Cor Art from R Int Mammary w Nonaut Sub, Perc Endo
 02114K9 Bypass 2 Cor Art from L Int Mammary w Nonaut Sub, Perc Endo
 02114KC Bypass 2 Cor Art from Thor Art w Nonaut Sub, Perc Endo
 02114KF Bypass 2 Cor Art from Abd Art w Nonaut Sub, Perc Endo
 02114KW Bypass 2 Cor Art from Aorta w Nonaut Sub, Perc Endo
 02114Z3 Bypass 2 Cor Art from Cor Art, Perc Endo Approach
 02114Z8 Bypass 2 Cor Art from R Int Mammary, Perc Endo Approach
 02114Z9 Bypass 2 Cor Art from L Int Mammary, Perc Endo Approach
 02114ZC Bypass 2 Cor Art from Thor Art, Perc Endo Approach
 02114ZF Bypass 2 Cor Art from Abd Art, Perc Endo Approach
 0212093 Bypass 3 Cor Art from Cor Art with Autol Vn, Open Approach
 0212098 Bypass 3 Cor Art from R Int Mammary w Autol Vn, Open
 0212099 Bypass 3 Cor Art from L Int Mammary w Autol Vn, Open
 021209C Bypass 3 Cor Art from Thor Art with Autol Vn, Open Approach
 021209F Bypass 3 Cor Art from Abd Art with Autol Vn, Open Approach
 021209W Bypass 3 Cor Art from Aorta with Autol Vn, Open Approach
 02120A3 Bypass 3 Cor Art from Cor Art with Autol Art, Open Approach
 02120A8 Bypass 3 Cor Art from R Int Mammary w Autol Art, Open
 02120A9 Bypass 3 Cor Art from L Int Mammary w Autol Art, Open
 02120AC Bypass 3 Cor Art from Thor Art with Autol Art, Open Approach
 02120AF Bypass 3 Cor Art from Abd Art with Autol Art, Open Approach
 02120AW Bypass 3 Cor Art from Aorta with Autol Art, Open Approach
 02120J3 Bypass 3 Cor Art from Cor Art with Synth Sub, Open Approach
 02120J8 Bypass 3 Cor Art from R Int Mammary w Synth Sub, Open
 02120J9 Bypass 3 Cor Art from L Int Mammary w Synth Sub, Open
 02120JC Bypass 3 Cor Art from Thor Art with Synth Sub, Open Approach
 02120JF Bypass 3 Cor Art from Abd Art with Synth Sub, Open Approach
 02120JW Bypass 3 Cor Art from Aorta with Synth Sub, Open Approach
 02120K3 Bypass 3 Cor Art from Cor Art with Nonaut Sub, Open Approach
 02120K8 Bypass 3 Cor Art from R Int Mammary w Nonaut Sub, Open
 02120K9 Bypass 3 Cor Art from L Int Mammary w Nonaut Sub, Open
 02120KC Bypass 3 Cor Art from Thor Art w Nonaut Sub, Open
 02120KF Bypass 3 Cor Art from Abd Art with Nonaut Sub, Open Approach
 02120KW Bypass 3 Cor Art from Aorta with Nonaut Sub, Open Approach
 02120Z3 Bypass 3 Cor Art from Cor Art, Open Approach
 02120Z8 Bypass 3 Cor Art from R Int Mammary, Open Approach
 02120Z9 Bypass 3 Cor Art from L Int Mammary, Open Approach
 02120ZC Bypass 3 Cor Art from Thor Art, Open Approach
 02120ZF Bypass 3 Cor Art from Abd Art, Open Approach
 0212493 Bypass 3 Cor Art from Cor Art w Autol Vn, Perc Endo
 0212498 Bypass 3 Cor Art from R Int Mammary w Autol Vn, Perc Endo
 0212499 Bypass 3 Cor Art from L Int Mammary w Autol Vn, Perc Endo
 021249C Bypass 3 Cor Art from Thor Art w Autol Vn, Perc Endo
 021249F Bypass 3 Cor Art from Abd Art w Autol Vn, Perc Endo
 021249W Bypass 3 Cor Art from Aorta w Autol Vn, Perc Endo
 02124A3 Bypass 3 Cor Art from Cor Art w Autol Art, Perc Endo
 02124A8 Bypass 3 Cor Art from R Int Mammary w Autol Art, Perc Endo
 02124A9 Bypass 3 Cor Art from L Int Mammary w Autol Art, Perc Endo
 02124AC Bypass 3 Cor Art from Thor Art w Autol Art, Perc Endo
 02124AF Bypass 3 Cor Art from Abd Art w Autol Art, Perc Endo
 02124AW Bypass 3 Cor Art from Aorta w Autol Art, Perc Endo
 02124J3 Bypass 3 Cor Art from Cor Art w Synth Sub, Perc Endo
 02124J8 Bypass 3 Cor Art from R Int Mammary w Synth Sub, Perc Endo
 02124J9 Bypass 3 Cor Art from L Int Mammary w Synth Sub, Perc Endo
 02124JC Bypass 3 Cor Art from Thor Art w Synth Sub, Perc Endo
 02124JF Bypass 3 Cor Art from Abd Art w Synth Sub, Perc Endo
 02124JW Bypass 3 Cor Art from Aorta w Synth Sub, Perc Endo
 02124K3 Bypass 3 Cor Art from Cor Art w Nonaut Sub, Perc Endo
 02124K8 Bypass 3 Cor Art from R Int Mammary w Nonaut Sub, Perc Endo
 02124K9 Bypass 3 Cor Art from L Int Mammary w Nonaut Sub, Perc Endo
 02124KC Bypass 3 Cor Art from Thor Art w Nonaut Sub, Perc Endo
 02124KF Bypass 3 Cor Art from Abd Art w Nonaut Sub, Perc Endo
 02124KW Bypass 3 Cor Art from Aorta w Nonaut Sub, Perc Endo
 02124Z3 Bypass 3 Cor Art from Cor Art, Perc Endo Approach
 02124Z8 Bypass 3 Cor Art from R Int Mammary, Perc Endo Approach
 02124Z9 Bypass 3 Cor Art from L Int Mammary, Perc Endo Approach
 02124ZC Bypass 3 Cor Art from Thor Art, Perc Endo Approach
 02124ZF Bypass 3 Cor Art from Abd Art, Perc Endo Approach
 0213093 Bypass 4+ Cor Art from Cor Art with Autol Vn, Open Approach
 0213098 Bypass 4+ Cor Art from R Int Mammary w Autol Vn, Open
 0213099 Bypass 4+ Cor Art from L Int Mammary w Autol Vn, Open
 021309C Bypass 4+ Cor Art from Thor Art with Autol Vn, Open Approach
 021309F Bypass 4+ Cor Art from Abd Art with Autol Vn, Open Approach
 021309W Bypass 4+ Cor Art from Aorta with Autol Vn, Open Approach
 02130A3 Bypass 4+ Cor Art from Cor Art with Autol Art, Open Approach
 02130A8 Bypass 4+ Cor Art from R Int Mammary w Autol Art, Open
 02130A9 Bypass 4+ Cor Art from L Int Mammary w Autol Art, Open
 02130AC Bypass 4+ Cor Art from Thor Art w Autol Art, Open
 02130AF Bypass 4+ Cor Art from Abd Art with Autol Art, Open Approach
 02130AW Bypass 4+ Cor Art from Aorta with Autol Art, Open Approach
 02130J3 Bypass 4+ Cor Art from Cor Art with Synth Sub, Open Approach
 02130J8 Bypass 4+ Cor Art from R Int Mammary w Synth Sub, Open
 02130J9 Bypass 4+ Cor Art from L Int Mammary w Synth Sub, Open
 02130JC Bypass 4+ Cor Art from Thor Art w Synth Sub, Open
 02130JF Bypass 4+ Cor Art from Abd Art with Synth Sub, Open Approach
 02130JW Bypass 4+ Cor Art from Aorta with Synth Sub, Open Approach
 02130K3 Bypass 4+ Cor Art from Cor Art w Nonaut Sub, Open
 02130K8 Bypass 4+ Cor Art from R Int Mammary w Nonaut Sub, Open
 02130K9 Bypass 4+ Cor Art from L Int Mammary w Nonaut Sub, Open
 02130KC Bypass 4+ Cor Art from Thor Art w Nonaut Sub, Open
 02130KF Bypass 4+ Cor Art from Abd Art w Nonaut Sub, Open
 02130KW Bypass 4+ Cor Art from Aorta with Nonaut Sub, Open Approach
 02130Z3 Bypass 4+ Cor Art from Cor Art, Open Approach
 02130Z8 Bypass 4+ Cor Art from R Int Mammary, Open Approach
 02130Z9 Bypass 4+ Cor Art from L Int Mammary, Open Approach
 02130ZC Bypass 4+ Cor Art from Thor Art, Open Approach
 02130ZF Bypass 4+ Cor Art from Abd Art, Open Approach
 0213493 Bypass 4+ Cor Art from Cor Art w Autol Vn, Perc Endo
 0213498 Bypass 4+ Cor Art from R Int Mammary w Autol Vn, Perc Endo
 0213499 Bypass 4+ Cor Art from L Int Mammary w Autol Vn, Perc Endo
 021349C Bypass 4+ Cor Art from Thor Art w Autol Vn, Perc Endo
 021349F Bypass 4+ Cor Art from Abd Art w Autol Vn, Perc Endo
 021349W Bypass 4+ Cor Art from Aorta w Autol Vn, Perc Endo
 02134A3 Bypass 4+ Cor Art from Cor Art w Autol Art, Perc Endo
 02134A8 Bypass 4+ Cor Art from R Int Mammary w Autol Art, Perc Endo
 02134A9 Bypass 4+ Cor Art from L Int Mammary w Autol Art, Perc Endo
 02134AC Bypass 4+ Cor Art from Thor Art w Autol Art, Perc Endo
 02134AF Bypass 4+ Cor Art from Abd Art w Autol Art, Perc Endo
 02134J3 Bypass 4+ Cor Art from Cor Art w Synth Sub, Perc Endo
 02134J8 Bypass 4+ Cor Art from R Int Mammary w Synth Sub, Perc Endo
 02134J9 Bypass 4+ Cor Art from L Int Mammary w Synth Sub, Perc Endo
 02134JC Bypass 4+ Cor Art from Thor Art w Synth Sub, Perc Endo
 02134JF Bypass 4+ Cor Art from Abd Art w Synth Sub, Perc Endo
 02134JW Bypass 4+ Cor Art from Aorta w Synth Sub, Perc Endo
 02134K3 Bypass 4+ Cor Art from Cor Art w Nonaut Sub, Perc Endo
 02134K8 Bypass 4+ Cor Art from R Int Mammary w Nonaut Sub, Perc Endo
 02134K9 Bypass 4+ Cor Art from L Int Mammary w Nonaut Sub, Perc Endo
 02134KC Bypass 4+ Cor Art from Thor Art w Nonaut Sub, Perc Endo
 02134KF Bypass 4+ Cor Art from Abd Art w Nonaut Sub, Perc Endo
 02134KW Bypass 4+ Cor Art from Aorta w Nonaut Sub, Perc Endo
 02134Z3 Bypass 4+ Cor Art from Cor Art, Perc Endo Approach
 02134Z8 Bypass 4+ Cor Art from R Int Mammary, Perc Endo Approach
 02134Z9 Bypass 4+ Cor Art from L Int Mammary, Perc Endo Approach
 02134ZC Bypass 4+ Cor Art from Thor Art, Perc Endo Approach
 02134ZF Bypass 4+ Cor Art from Abd Art, Perc Endo Approach
*/ 

* cabg one 
foreach v of varlist proc1 proc2 proc3 proc4 proc5 {
	icd10p generate cabg_one_`v' = `v', range(0210093 0210098 0210099 021009C 021009F 021009W 02100A3 02100A8 02100A9 02100AC 02100AF 02100AW 02100J3 02100J8 02100J9 02100JC 02100JF 02100JW 02100K3 02100K8 02100K9 02100KC 02100KF 02100KW 02100Z3 02100Z8 02100Z9 02100ZC 02100ZF 0210493 0210498 0210499 021049C 021049F 021049W 02104A3 02104A8 02104A9 02104AC 02104AF 02104AW 02104J3 02104J8 02104J9 02104JC 02104JF 02104JW 02104K3 02104K8 02104K9 02104KC 02104KF 02104KW 02104Z3 02104Z8 02104Z9 02104ZC 02104ZF)
}

gen cabg_one = cabg_one_proc1 == 1 | cabg_one_proc2 == 1 | cabg_one_proc3 == 1 | cabg_one_proc4 == 1 | cabg_one_proc5 == 1 | index_cabgx1_1 == 1
drop cabg_one_proc*


* cabg two 
foreach v of varlist proc1 proc2 proc3 proc4 proc5 {
	icd10p generate cabg_two_`v' = `v', range(0211093 0211098 0211099 021109C 021109F 021109W 02110A3 02110A8 02110A9 02110AC 02110AF 02110AW 02110J3 02110J8 02110J9 02110JC 02110JF 02110JW 02110K3 02110K8 02110K9 02110KC 02110KF 02110KW 02110Z3 02110Z8 02110Z9 02110ZC 02110ZF 0211493 0211498 0211499 021149C 021149F 021149W 02114A3 02114A8 02114A9 02114AC 02114AF 02114AW 02114J3 02114J8 02114J9 02114JC 02114JF 02114JW 02114K3 02114K8 02114K9 02114KC 02114KF 02114KW 02114Z3 02114Z8 02114Z9 02114ZC 02114ZF)
}

gen cabg_two = cabg_two_proc1 == 1 | cabg_two_proc2 == 1 | cabg_two_proc3 == 1 | cabg_two_proc4 == 1 | cabg_two_proc5 == 1 | index_cabgx2_1 == 1 
drop cabg_two_proc*

* cabg three 
foreach v of varlist proc1 proc2 proc3 proc4 proc5 {
	icd10p generate cabg_three_`v' = `v', range(0212093 0212098 0212099 021209C 021209F 021209W 02120A3 02120A8 02120A9 02120AC 02120AF 02120AW 02120J3 02120J8 02120J9 02120JC 02120JF 02120JW 02120K3 02120K8 02120K9 02120KC 02120KF 02120KW 02120Z3 02120Z8 02120Z9 02120ZC 02120ZF 0212493 0212498 0212499 021249C 021249F 021249W 02124A3 02124A8 02124A9 02124AC 02124AF 02124AW 02124J3 02124J8 02124J9 02124JC 02124JF 02124JW 02124K3 02124K8 02124K9 02124KC 02124KF 02124KW 02124Z3 02124Z8 02124Z9 02124ZC 02124ZF)
}

gen cabg_three = cabg_three_proc1 == 1 | cabg_three_proc2 == 1 | cabg_three_proc3 == 1 | cabg_three_proc4 == 1 | cabg_three_proc5 == 1 | index_cabgx3 == 1 
drop cabg_three_proc*

* cabg four + 
foreach v of varlist proc1 proc2 proc3 proc4 proc5 {
	icd10p generate cabg_four_`v' = `v', range(0213093 0213098 0213099 021309C 021309F 021309W 02130A3 02130A8 02130A9 02130AC 02130AF 02130AW 02130J3 02130J8 02130J9 02130JC 02130JF 02130JW 02130K3 02130K8 02130K9 02130KC 02130KF 02130KW 02130Z3 02130Z8 02130Z9 02130ZC 02130ZF 0213493 0213498 0213499 021349C 021349F 021349W 02134A3 02134A8 02134A9 02134AC 02134AF 02134AF 02134J3 02134J8 02134J9 02134JC 02134JF 02134JW 02134K3 02134K8 02134K9 02134KC 02134KF 02134KW 02134Z3 02134Z8 02134Z9 02134ZC 02134ZF)
}

gen cabg_four = cabg_four_proc1 == 1 | cabg_four_proc2 == 1 | cabg_four_proc3 == 1 | cabg_four_proc4 == 1 | cabg_four_proc5 == 1 | index_cabgx4_more_1 == 1 
drop cabg_four_proc*

* cabg_ima 
foreach v of varlist proc1 proc2 proc3 proc4 proc5 {
	icd10p generate cabg_ima_`v' = `v', range(0210098 0210099 02100A8 02100A9 02100J8 02100J9 02100K8 02100K9 02100Z8 02100Z9 0210498 0210499 02104A8 02104A9 02104J8 02104J9 02104K8 02104K9 02104Z8 02104Z9 0211098 0211099 02110A8 02110A9 02110J8 02110J9 02110K8 02110K9 02110Z8 02110Z9 0211498 0211499 02114A8 02114A9 02114J8 02114J9 02114K8 02114K9 02114Z8 02114Z9 02114ZC 0212098 0212099 02120A8 02120A9 02120J8 02120J9 02120K8 02120K9 02120Z8 02120Z9 0212498 0212499 02124A8 02124A9 02124J8 02124J9 02124K8 02124K9 02124Z8 02124Z9 0213098 0213099 02130A8 02130A9 02130J8 02130J9 02130K8 02130K9 02130Z8 02130Z9 0213498 0213499 02134A8 02134A9 02134J8 02134J9 02134K8 02134K9 02134Z8 02134Z9)
}

gen cabg_ima = cabg_ima_proc1 == 1 | cabg_ima_proc2 == 1 | cabg_ima_proc3 == 1 | cabg_ima_proc4 == 1 | cabg_ima_proc5 == 1 | index_cabg_ima_1 == 1
drop cabg_ima_proc* 

* ///////////////////////////////////////////////////////////////////////////////

* surgical covariates

* ///////////////////////////////////////////////////////////////////////////////

tab cabg_srg_top valve_srg_top if exclude_1 != 1, miss 

* generate isolated cabg
generate isolated_cabg = cabg_srg_top == 1 & valve_srg_top != 1 
tab cabg_srg_top isolated_cabg if exclude_1 != 1, miss
tab valve_srg_top isolated_cabg if exclude_1 != 1, miss

* check to ensure all valve obs have a surgery 
count if (valve_av_repair == 1 | valve_av_replace == 1 | valve_mv_repair == 1 | valve_mv_replace == 1 | valve_pv_repair == 1 | valve_pv_replace == 1 | valve_tv_repair == 1 |valve_tv_replace == 1) & exclude_1 != 1 
tab valve_srg_top if exclude_1 != 1, miss
list proc1-proc5 if valve_srg_top == 1 & (valve_av_repair != 1 & valve_av_replace != 1 & valve_mv_repair != 1 & valve_mv_replace != 1 & valve_pv_repair != 1 & valve_pv_replace != 1 & valve_tv_repair != 1 & valve_tv_replace != 1) & exclude_1 != 1 
* two obs. have the following codes 
 /*
02HA0QZ Insertion of Implant Heart Assist into Heart, Open Approach
02L70ZK Occlusion of Left Atrial Appendage, Open Approach
02PA0QZ Removal of Implant Heart Assist from Heart, Open Approach
02UG08Z Supplement Mitral Valve with Zooplastic, Open Approach
03HY32Z Insertion of Monitoring Device into Up Art, Perc Approach
3E033XZ Introduction of Vasopressor into Periph Vein, Perc Approach
4A133B1 Monitoring of Arterial Pressure, Peripheral, Perc Approach
5A09357 Assistance with Respiratory Ventilation, <24 Hrs, CPAP
5A1221Z Performance of Cardiac Output, Continuous
*/ 
* generate variable capturing those two observations
generate other_ct_srg = valve_srg_top == 1 & (valve_av_repair != 1 & valve_av_replace != 1 & valve_mv_repair != 1 & valve_mv_replace != 1 & valve_pv_repair != 1 & valve_pv_replace != 1 & valve_tv_repair != 1 & valve_tv_replace != 1) 
* sanity check 
tab valve_srg_top other_ct_srg if exclude_1 != 1

* ///////////////////////////////////////////////////////////////////////////////

* after recateogization - one additional exclusion criteria - other ct surgeries  

* ///////////////////////////////////////////////////////////////////////////////

* look at two obs. from valve cohort without a classified valve 
tab other_ct_srg if exclude_1 !=1, miss 
* exclude those two observations 
count if cv == 1 & exclude_1 !=1 & other_ct_srg == 1 
* exclusion count
* 2
* inclusion count
count if cv == 1 & exclude_1 != 1 & other_ct_srg != 1 
* 69,517 
* eighth exclude 
replace exclude_1 = 1 if patid_conf_id_dup == 1 | patid_dup > 0 | yr < 2016 | prov_type == "1" | gdr_cd_co_e == "U" | enroll_90_day_pre_index == 0 | ecmo_cpt == 1 | other_ct_srg == 1 
* sanity check
tabulate cv exclude_1, miss

* age exclusion 
summarize age_years if exclude_1 != 1, detail
* exclude obs. <18 years of age_years
count if cv == 1 & exclude_1 !=1 & age_years < 18
* 984
* inclusion count 
count if cv == 1 & exclude_1 !=1 & age_years >= 18
* 68,533
* ninth exclude 
replace exclude_1 = 1 if patid_conf_id_dup == 1 | patid_dup > 0 | yr < 2016 | prov_type == "1" | gdr_cd_co_e == "U" | enroll_90_day_pre_index == 0 | ecmo_cpt == 1 | other_ct_srg == 1 | age_years < 18
* sanity check 
tabulate cv exclude_1, miss 


* ///////////////////////////////////////////////////////////////////////////////

* note final cohort n = 68,533 valve/CABG/both 2016 - 2020 

* ///////////////////////////////////////////////////////////////////////////////

* check to ensure all cabg surgeries have a surgery 
tab cabg_srg_top isolated_cabg if exclude_1 !=1, miss 
count if (cabg_ima == 1 | cabg_four == 1 | cabg_three == 1 | cabg_two == 1 | cabg_one == 1) & exclude_1 != 1 & isolated_cabg == 1

tab cabg_srg_top isolated_cabg if exclude_1 != 1, miss
tab valve_srg_top isolated_cabg if exclude_1 != 1, miss
tab valve_srg_top cabg_srg_top if exclude_1 != 1, miss

* all 68,662 observations have a surgery 

* ///////////////////////////////////////////////////////////////////////////////

* negative control outcome options 

* ///////////////////////////////////////////////////////////////////////////////


* DVT/PE 
foreach v of varlist diag1 diag2 diag3 diag4 diag5 {
	icd10 generate dvt_pe_`v' = `v', range(I26 I260 I269 I80 I800 I801 I802 I803 I808 I809 I82 I822 I823 I828 I829)
}

gen dvt_pe = dvt_pe_diag1 == 1 | dvt_pe_diag2 == 1 | dvt_pe_diag3 == 1 | dvt_pe_diag4 == 1 | dvt_pe_diag5 == 1 
drop dvt_pe_diag*

* afib 
foreach v of varlist diag1 diag2 diag3 diag4 diag5 {
	icd10 generate afib_cx_`v' = `v', range(I48 I480 I481 I483 I484 I489)
}

gen afib = afib_cx_diag1 == 1 | afib_cx_diag2 == 1 | afib_cx_diag3 == 1 | afib_cx_diag4 == 1 | afib_cx_diag5 == 1 
drop afib_cx_diag*
tab afib if exclude_1 != 1, miss 

* ///////////////////////////////////////////////////////////////////////////////

* code out TEE provider 
generate tee_provider = " " 
replace tee_provider = "anesthesiologist" if (provcat_tee == "0388" | provcat_tee == "0390" | provcat_tee == "0391" | provcat_tee == "0392") & TEE == 1 
replace tee_provider = "cardiologist" if (provcat_tee == "0407" | provcat_tee == "0408" | provcat_tee == "0410" | provcat_tee == "0411") & TEE == 1 
replace tee_provider = "other" if tee_provider != "anesthesiologist" & tee_provider != "cardiologist" & TEE == 1 
replace tee_provider = "no tee" if tee_provider != "anesthesiologist" & tee_provider != "cardiologist" & tee_provider != "other" & TEE == 0
* encode
encode tee_provider, generate(tee_provider_e)

* ///////////////////////////////////////////////////////////////////////////////

* if do a trends paper: multiple lines for TEE use by year - CABG & valve plotted on same graph 

* ///////////////////////////////////////////////////////////////////////////////

* clean data 
generate elix_chf_b = cond((elix_chf_1 == 1), 1, 0) 
generate elix_arrhy_b = cond((elix_arrhy_1 == 1), 1, 0) 
generate elix_pcd_b = cond((elix_pcd_1 == 1), 1, 0) 
generate elix_pvd_b = cond((elix_pvd_1 == 1), 1, 0) 
generate elix_fluid_elec_b = cond((elix_fluid_elec_1 == 1), 1, 0) 
generate elix_htn_un_cx_b = cond((elix_htn_un_cx_1 == 1), 1, 0)
generate elix_htn_cx_b = cond((elix_htn_cx_1 == 1), 1, 0) 
generate elix_neurol_b = cond((elix_neurol_1 == 1), 1, 0) 
generate elix_cpd_b = cond((elix_cpd_1 == 1), 1, 0) 
generate elix_diab_un_cx_b = cond((elix_diab_un_cx_1 == 1), 1, 0) 
generate elix_diab_cx_b = cond((elix_diab_cx_1 == 1), 1, 0) 
generate elix_renal_fail_b = cond((elix_renal_fail_1 == 1), 1, 0) 
generate elix_liver_b = cond((elix_liver_1 == 1), 1, 0) 
generate elix_coag_b = cond((elix_coag_1 == 1), 1, 0) 
generate elix_anemia_iron_b = cond((elix_anemia_iron_1 == 1), 1, 0)
generate elix_anemia_bld_loss_b = cond((elix_anemia_bld_loss_1 == 1), 1, 0)
generate elix_hypoth_b = cond((elix_hypoth_1 == 1), 1, 0)
generate elix_valvular_b = cond((elix_valvular_1 == 1), 1, 0) 
 

* ///////////////////////////////////////////////////////////////////////////////

* all surgeries 

* ///////////////////////////////////////////////////////////////////////////////

* baseline characteristics
ttest age_years if exclude_1 != 1, by(TEE_intraop)
tabulate gender_e TEE_intraop if exclude_1 != 1, chi2 column 
tabulate race_e TEE_intraop if exclude_1 != 1, chi2 column
tabulate yr TEE_intraop if exclude_1 != 1, chi2 column
ttest surgical_volume_npi if exclude_1 != 1, by(TEE_intraop)
tabulate valve_srg_top TEE_intraop if exclude_1 != 1, chi2 column
tabulate cabg_srg_top TEE_intraop if exclude_1 != 1, chi2 column
tabulate isolated_cabg TEE_intraop if exclude_1 != 1, chi2 column
tabulate cabg_ima TEE_intraop if exclude_1 != 1, chi2 column
tabulate cabg_one TEE_intraop if exclude_1 != 1, chi2 column
tabulate cabg_two TEE_intraop if exclude_1 != 1, chi2 column
tabulate cabg_three TEE_intraop if exclude_1 != 1, chi2 column
tabulate cabg_four TEE_intraop if exclude_1 != 1, chi2 column
tabulate valve_av_repair TEE_intraop if exclude_1 != 1, chi2 column
tabulate valve_av_replace TEE_intraop if exclude_1 != 1, chi2 column
tabulate valve_mv_repair TEE_intraop if exclude_1 != 1, chi2 column
tabulate valve_mv_replace TEE_intraop if exclude_1 != 1, chi2 column
tabulate valve_pv_repair TEE_intraop if exclude_1 != 1, chi2 column
tabulate valve_pv_replace TEE_intraop if exclude_1 != 1, chi2 column
tabulate valve_pvr TEE_intraop if exclude_1 != 1, chi2 column
tabulate valve_tv_repair TEE_intraop if exclude_1 != 1, chi2 column
tabulate valve_tv_replace TEE_intraop if exclude_1 != 1, chi2 column
tabulate elix_chf_b TEE_intraop if exclude_1 != 1, chi2 column
tabulate elix_arrhy_b TEE_intraop if exclude_1 != 1, chi2 column
tabulate elix_pcd_b TEE_intraop if exclude_1 != 1, chi2 column
tabulate elix_pvd_b TEE_intraop if exclude_1 != 1, chi2 column
tabulate elix_htn_un_cx_b TEE_intraop if exclude_1 != 1, chi2 column
tabulate elix_htn_cx_b TEE_intraop if exclude_1 != 1, chi2 column
tabulate elix_fluid_elec_b TEE_intraop if exclude_1 != 1, chi2 column
tabulate elix_cpd_b TEE_intraop if exclude_1 != 1, chi2 column
tabulate elix_diab_un_cx_b TEE_intraop if exclude_1 != 1, chi2 column
tabulate elix_diab_cx_b TEE_intraop if exclude_1 != 1, chi2 column
tabulate elix_renal_fail_b TEE_intraop if exclude_1 != 1, chi2 column
tabulate elix_liver_b TEE_intraop if exclude_1 != 1, chi2 column
tabulate elix_coag_b TEE_intraop if exclude_1 != 1, chi2 column
tabulate elix_anemia_iron_b TEE_intraop if exclude_1 != 1, chi2 column
tabulate elix_anemia_bld_loss_b TEE_intraop if exclude_1 != 1, chi2 column
tabulate elix_hypoth_b TEE_intraop if exclude_1 != 1, chi2 column
tabulate elix_valvular_b TEE_intraop if exclude_1 != 1, chi2 column
* outcome(s) raw TEE_intraop 
tabulate death_ind_30 TEE_intraop if exclude_1 != 1, chi2 column 
tabulate death_ind_or_stroke TEE_intraop if exclude_1 != 1, chi2 column
ttest los if exclude_1 != 1, by(TEE_intraop)
* outcome(s) raw TEE 
tabulate death_ind_30 TEE if exclude_1 != 1, chi2 column 
tabulate death_ind_or_stroke TEE if exclude_1 != 1, chi2 column
ttest los if exclude_1 != 1, by(TEE)
* negative control 
* additional exclusion criteria if negative control is afib (i.e. exclude elix_arrh_b?)
tabulate afib TEE_intraop if exclude_1 != 1 & elix_arrhy_b != 1, chi2 column 

* ///////////////////////////////////////////////////////////////////////////////

* isolated CABG: n = 42,249

* ///////////////////////////////////////////////////////////////////////////////

* cohort count for isolated CABG
tabulate isolated_cabg if exclude_1 != 1

* baseline characteristics
ttest age_years if exclude_1 != 1 & isolated_cabg  == 1, by(TEE_intraop)
tabulate race_ethnicity_str TEE_intraop if exclude_1 != 1 & isolated_cabg  == 1, chi2 column
tabulate yr TEE_intraop if exclude_1 != 1 & isolated_cabg  == 1, chi2 column
ttest surgical_volume_npi if exclude_1 != 1 & isolated_cabg  == 1, by(TEE_intraop)
tabulate cabg_ima TEE_intraop if exclude_1 != 1 & isolated_cabg  == 1, chi2 column
tabulate cabg_one TEE_intraop if exclude_1 != 1 & isolated_cabg  == 1, chi2 column
tabulate cabg_two TEE_intraop if exclude_1 != 1 & isolated_cabg  == 1, chi2 column
tabulate cabg_three TEE_intraop if exclude_1 != 1 & isolated_cabg  == 1, chi2 column
tabulate cabg_four TEE_intraop if exclude_1 != 1 & isolated_cabg  == 1, chi2 column
tabulate elix_chf_b TEE_intraop if exclude_1 != 1 & isolated_cabg  == 1, chi2 column
tabulate elix_arrhy_b TEE_intraop if exclude_1 != 1 & isolated_cabg  == 1, chi2 column
tabulate elix_valvular_b TEE_intraop if exclude_1 != 1, chi2 column
tabulate elix_pvd_b TEE_intraop if exclude_1 != 1 & isolated_cabg  == 1, chi2 column
tabulate elix_htn_un_cx_b TEE_intraop if exclude_1 != 1 & isolated_cabg  == 1, chi2 column
tabulate elix_cpd_b TEE_intraop if exclude_1 != 1 & isolated_cabg  == 1, chi2 column
tabulate elix_diab_cx_b TEE_intraop if exclude_1 != 1 & isolated_cabg  == 1, chi2 column
tabulate elix_renal_fail_b TEE_intraop if exclude_1 != 1 & isolated_cabg  == 1, chi2 column
tabulate elix_liver_b TEE_intraop if exclude_1 != 1 & isolated_cabg  == 1, chi2 column
* outcome(s) raw TEE_intraop 
tabulate death_ind_30 TEE_intraop if exclude_1 != 1 & isolated_cabg  == 1, chi2 column 
tabulate death_ind_or_stroke TEE_intraop if exclude_1 != 1 & isolated_cabg  == 1, chi2 column
ttest los if exclude_1 != 1 & isolated_cabg  ==1, by(TEE_intraop)
* outcome(s) raw TEE 
tabulate death_ind_30 TEE if exclude_1 != 1 & isolated_cabg  == 1, chi2 column 
tabulate death_ind_or_stroke TEE if exclude_1 != 1 & isolated_cabg  == 1, chi2 column
ttest los if exclude_1 != 1 & isolated_cabg  == 1, by(TEE)
* negative control outcome 
tabulate afib TEE if exclude_1 != 1 & isolated_cabg == 1 & elix_arrhy_b != 1, chi2 column 

* multivariable logistic regression  
* outcome: 30-day mortality 
* exposure: TEE_intraop 
logistic death_ind_30 TEE_intraop age_years i.gender_e i.race_e i.yr surgical_volume_npi i.cabg_ima i.cabg_one i.cabg_two i.cabg_three i.cabg_four i.cabg_ima i.elix_chf_b i.elix_arrhy_b i.elix_valvular_b i.elix_pvd_b i.elix_htn_un_cx_b i.elix_cpd_b i.elix_diab_cx_b i.elix_renal_fail_b i.elix_liver_b if isolated_cabg == 1 & exclude_1 != 1
* exposure: TEE 
logistic death_ind_30 TEE age_years i.gender_e i.race_e i.yr surgical_volume_npi i.cabg_ima i.cabg_one i.cabg_two i.cabg_three i.cabg_four i.cabg_ima i.elix_chf_b i.elix_arrhy_b i.elix_valvular_b i.elix_pvd_b i.elix_htn_un_cx_b i.elix_cpd_b i.elix_diab_cx_b i.elix_renal_fail_b i.elix_liver_b if isolated_cabg == 1 & exclude_1 != 1

* outcome: death | stroke 
* exposure: TEE_intraop 
logistic death_ind_or_stroke TEE_intraop age_years i.gender_e i.race_e i.yr surgical_volume_npi i.cabg_ima i.cabg_one i.cabg_two i.cabg_three i.cabg_four i.cabg_ima i.elix_chf_b i.elix_arrhy_b i.elix_valvular_b i.elix_pvd_b i.elix_htn_un_cx_b i.elix_cpd_b i.elix_diab_cx_b i.elix_renal_fail_b i.elix_liver_b if isolated_cabg == 1 & exclude_1 != 1
* exposure: TEE 
logistic death_ind_or_stroke TEE age_years i.gender_e i.race_e i.yr surgical_volume_npi i.cabg_ima i.cabg_one i.cabg_two i.cabg_three i.cabg_four i.cabg_ima i.elix_chf_b i.elix_arrhy_b i.elix_valvular_b i.elix_pvd_b i.elix_htn_un_cx_b i.elix_cpd_b i.elix_diab_cx_b i.elix_renal_fail_b i.elix_liver_b if isolated_cabg == 1 & exclude_1 != 1


* ///////////////////////////////////////////////////////////////////////////////

* valves +/- cabg n = 26,284 

* ///////////////////////////////////////////////////////////////////////////////

* baseline characteristics
ttest age_years if exclude_1 != 1 & isolated_cabg != 1, by(TEE_intraop)
tabulate gender_e TEE_intraop if exclude_1 != 1 & isolated_cabg != 1, chi2 column
tabulate race_e TEE_intraop if exclude_1 != 1 & isolated_cabg != 1, chi2 column
tabulate yr TEE_intraop if exclude_1 != 1 & isolated_cabg != 1, chi2 column
ttest surgical_volume_npi if exclude_1 != 1 & isolated_cabg != 1, by(TEE_intraop)
tabulate valve_av_repair TEE_intraop if exclude_1 != 1 & isolated_cabg != 1, chi2 column
tabulate valve_av_replace TEE_intraop if exclude_1 != 1 & isolated_cabg != 1, chi2 column
tabulate valve_mv_replace TEE_intraop if exclude_1 != 1 & isolated_cabg != 1, chi2 column
tabulate valve_mv_replace TEE_intraop if exclude_1 != 1 & isolated_cabg != 1, chi2 column
tabulate valve_pv_repair TEE_intraop if exclude_1 != 1 & isolated_cabg != 1, chi2 column
tabulate valve_pv_replace TEE_intraop if exclude_1 != 1 & isolated_cabg != 1, chi2 column
tabulate valve_pvr TEE_intraop if exclude_1 != 1 & isolated_cabg != 1, chi2 column
tabulate valve_tv_repair TEE_intraop if exclude_1 != 1 & isolated_cabg != 1, chi2 column
tabulate valve_tv_replace TEE_intraop if exclude_1 != 1 & isolated_cabg != 1, chi2 column
tabulate cabg_ima TEE_intraop if exclude_1 != 1 & isolated_cabg != 1, chi2 column
tabulate cabg_one TEE_intraop if exclude_1 != 1 & isolated_cabg != 1, chi2 column
tabulate cabg_two TEE_intraop if exclude_1 != 1 & isolated_cabg != 1, chi2 column
tabulate cabg_three TEE_intraop if exclude_1 != 1 & isolated_cabg != 1, chi2 column
tabulate cabg_four TEE_intraop if exclude_1 != 1 & isolated_cabg != 1, chi2 column
tabulate elix_chf_b TEE_intraop if exclude_1 != 1 & isolated_cabg != 1, chi2 column
tabulate elix_arrhy_b TEE_intraop if exclude_1 != 1 & isolated_cabg != 1, chi2 column
tabulate elix_pvd_b TEE_intraop if exclude_1 != 1 & isolated_cabg != 1, chi2 column
tabulate elix_valvular_b TEE_intraop if exclude_1 != 1 & isolated_cabg != 1, chi2 column
tabulate elix_htn_un_cx_b TEE_intraop if exclude_1 != 1 & isolated_cabg != 1, chi2 column
tabulate elix_cpd_b TEE_intraop if exclude_1 != 1 & isolated_cabg != 1, chi2 column
tabulate elix_diab_cx_b TEE_intraop if exclude_1 != 1 & isolated_cabg != 1, chi2 column
tabulate elix_renal_fail_b TEE_intraop if exclude_1 != 1 & isolated_cabg != 1, chi2 column
tabulate elix_liver_b TEE_intraop if exclude_1 != 1 & isolated_cabg != 1, chi2 column
* outcome(s) raw TEE_intraop  
tabulate death_ind_30 TEE_intraop if exclude_1 != 1 & isolated_cabg != 1, chi2 column 
tabulate death_ind_or_stroke TEE_intraop if exclude_1 != 1 & isolated_cabg != 1, chi2 column
ttest los if exclude_1 != 1 & isolated_cabg != 1, by(TEE_intraop)
* outcome(s) raw TEE 
tabulate death_ind_30 TEE if exclude_1 != 1 & isolated_cabg != 1, chi2 column 
tabulate death_ind_or_stroke TEE if exclude_1 != 1 & isolated_cabg != 1, chi2 column
ttest los if exclude_1 != 1 & isolated_cabg != 1, by(TEE)

* multivariable logistic regression  
* outcome: 30-day mortality 
* exposure raw 
logistic death_ind_30 i.TEE_intraop age_years i.gender_e i.race_e i.yr surgical_volume_npi valve_av_repair valve_av_replace valve_mv_repair valve_mv_replace valve_pvr valve_tv_repair valve_tv_replace cabg_ima cabg_one cabg_two cabg_three cabg_four i.elix_chf_b i.elix_arrhy_b i.elix_valvular_b i.elix_pvd_b i.elix_htn_un_cx_b i.elix_cpd_b i.elix_diab_cx_b i.elix_renal_fail_b i.elix_liver_b if isolated_cabg != 1 & exclude_1 != 1

* outcome: death | stroke 
logistic death_ind_or_stroke i.TEE_intraop age_years i.gender_e i.race_e i.yr surgical_volume_npi valve_av_repair valve_av_replace valve_mv_repair valve_mv_replace valve_pvr valve_tv_repair valve_tv_replace cabg_ima cabg_one cabg_two cabg_three cabg_four i.elix_chf_b i.elix_arrhy_b i.elix_valvular_b i.elix_pvd_b i.elix_htn_un_cx_b i.elix_cpd_b i.elix_diab_cx_b i.elix_renal_fail_b i.elix_liver_b if isolated_cabg != 1 & exclude_1 != 1

* multivariable logistic regression  
* outcome: 30-day mortality 
* exposure raw 
logistic death_ind_30 i.TEE age_years i.gender_e i.race_e i.yr surgical_volume_npi valve_av_repair valve_av_replace valve_mv_repair valve_mv_replace valve_pvr valve_tv_repair valve_tv_replace cabg_ima cabg_one cabg_two cabg_three cabg_four i.elix_chf_b i.elix_arrhy_b i.elix_valvular_b i.elix_pvd_b i.elix_htn_un_cx_b i.elix_cpd_b i.elix_diab_cx_b i.elix_renal_fail_b i.elix_liver_b if isolated_cabg != 1 & exclude_1 != 1

* outcome: death | stroke 
logistic death_ind_or_stroke i.TEE age_years i.gender_e i.race_e i.yr surgical_volume_npi valve_av_repair valve_av_replace valve_mv_repair valve_mv_replace valve_pvr valve_tv_repair valve_tv_replace cabg_ima cabg_one cabg_two cabg_three cabg_four i.elix_chf_b i.elix_arrhy_b i.elix_valvular_b i.elix_pvd_b i.elix_htn_un_cx_b i.elix_cpd_b i.elix_diab_cx_b i.elix_renal_fail_b i.elix_liver_b if isolated_cabg != 1 & exclude_1 != 1


* ///////////////////////////////////////////////////////////////////////////////

* export data to .csv 

* ///////////////////////////////////////////////////////////////////////////////

/*

// export & save as .csv file
// change directory to harddrive
cd "/path/"
// export .csv to the changed directory
export delimited Optum_data_tee_2023_04_28.csv, replace 
*/ 

/*
// export & save as .dta file
save "<filename>.dta", replace
*/ 

* log close
log close _all









