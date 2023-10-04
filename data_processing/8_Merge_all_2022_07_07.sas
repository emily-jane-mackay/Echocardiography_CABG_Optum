/* Merge all files */

libname Optum 'path-redacted';
libname Op_death 'path-redacted';
libname Tee_lib 'path-redacted';

/* merge confinement_medical_processed into confinement_elixhauser file */
/* (1) set confinement file with elixhauser / complications / surgeries coded */
data temp_a;
  set Tee_lib.confinement_elix_srg;
run;

proc sort data = temp_a;
  by patid pat_planid admit_date conf_id;
run;

/* (2) set confinement file with medical claims processed (tee and srg physician claims processed) */
data temp_b;
  set Tee_lib.confinement_medical_processed;
run;

proc sort data = temp_b;
  by patid pat_planid admit_date conf_id;
run;

/* (3) merge file with medical claims (both surgical and tee) processed into source file */
data merge_1;
  merge temp_a temp_b;
  by patid pat_planid admit_date conf_id;
run;

/* sequential merge to collect death information into source file */
/* (1) set & sort source file from above steps */
data temp_s;
  set merge_1;
run;

proc sort data = temp_s;
  by patid;
run;

/* (2) set & sort member death data */
data temp_d;
  set op_death.Dod_mbrwdeath (keep = patid Ymdod Mbr_match_type);
run;

proc sort data = temp_d;
  by patid;
run;

/* merge death data into source file */
data merge_2;
  merge temp_s (in = in_temp_s) temp_d;
  by patid;
  if in_temp_s = 1 then output;
run;

/* sequential merge to collect member enrollment data */
/* (1) set & sort source file from above step(s) */
data temp_s;
  set merge_2;
run;

proc sort data = temp_s;
  by patid admit_date;
run;

/* (2) set & sort member enrollment data (co) */
data temp_e_c;
  set op_death.Dod_mbr_co_enroll_r (keep = patid Eligeff Eligend Gdr_Cd race Yrdob);
run;

proc sort data = temp_e_c;
  by Patid;
run;

/* (3) add suffix to end of columns */
/* This code creates a macro variable &list with the list of variables in the form */
/* This format could be used to add a suffix to all the variables */
proc sql noprint;
   select cats(name,'=',name,'_co_e')
          into :list
          separated by ' '
          from dictionary.columns
          where libname = 'WORK' and memname = 'TEMP_E_C';
quit;

proc datasets library = work nolist;
   modify temp_e_c;
   rename &list;
quit;

/* revert key variables to original name */
data temp_e_c;
  set temp_e_c (rename = (
  Patid_co_e = Patid));
run;

proc sort data = temp_e_c;
  by patid eligeff_co_e eligend_co_e;
run;

/* merge enroll_co into temp_source file */
/* logic condition: admit_date between eligeff_co_e & eligend_co_e */
data merge_3;
  merge temp_s(in = temp_s) temp_e_c;
  by patid;
  if (temp_s = 1 and admit_date >= eligeff_co_e and admit_date <= eligend_co_e) then output;
run;

proc sort data = merge_3;
  by patid pat_planid admit_date;
run;

/* (4) set & sort member enrollment data */
/* get member plan id and state data */
data temp_e;
  set op_death.Dod_mbr_enroll_r (keep = patid pat_planid eligeff eligend state);
run;

proc sort data = temp_e;
  by Patid Pat_planid;
run;

/* (5) add suffix to end of columns */
/* This code creates a macro variable &list with the list of variables in the form */
/* This format could be used to add a suffix to all the variables */
proc sql noprint;
   select cats(name,'=',name,'_e')
          into :list
          separated by ' '
          from dictionary.columns
          where libname = 'WORK' and memname = 'TEMP_E';
quit;

proc datasets library = work nolist;
   modify temp_e;
   rename &list;
quit;

/* revert key variables to original name */
data temp_e;
  set temp_e (rename = (
  Patid_e = Patid
  Pat_planid_e = Pat_planid));
run;

/* sort the data */
proc sort data = temp_e;
  by patid pat_planid eligeff_e eligend_e;
run;

/* merge enroll_co into temp_source file */
/* logic condition: admit_date between eligeff_e & eligend_e */
data merge_4;
  merge merge_3 (in = merge_3) temp_e;
  by patid pat_planid;
  if (merge_3 = 1 and admit_date >= eligeff_e and admit_date <= eligend_e) then output;
run;

/* obtain provider bridge data and merge into source file */
/* (1) set and sort provider data: provider bridge file */
data temp_b;
  set op_death.Dod_provider_bridge (keep = prov_unique dea npi prov);
run;

proc sort data = temp_b;
  by prov prov_unique;
run;

/* (2) resort source file by prov */
proc sort data = merge_4;
  by prov;
run;

/* (3) merge provider bridge file into source file */
data merge_5;
  merge merge_4 (in = merge_4) temp_b;
  by prov;
  if merge_4 = 1 then output;
run;

/* (4) sort by patid to look at data */
proc sort data = merge_5;
  by patid;
run;

/* obtain provider data and merge into source file */
/* (1) set & sort provider data: provider file */
data temp_pr;
  set op_death.Dod_provider;
run;

proc sort data = temp_pr
  (keep = prov_unique grp_practice hosp_affil Prov_State
  prov_type provcat Taxonomy1 Taxonomy2);
  by prov_unique;
run;

/* (2) sort source file by prov_unique */
proc sort data = merge_5;
  by prov_unique;
run;

/* (3) merge provider file into source file */
data merge_6;
  merge merge_5 (in = merge_5) temp_pr;
  by prov_unique;
  if merge_5 = 1 then output;
run;

/* (4) sort by patid to look at data */
proc sort data = merge_6;
  by patid conf_id admit_date;
run;

/* save merged  */
data Tee_lib.data_processed_complete;
  set merge_6;
run;

/* sort by patid */
proc sort data = Tee_lib.data_processed_complete;
  by Patid Conf_Id;
run;

/* issue with opening previously-exported .csv file in stata */
/* Exported to .dta format using "export wizard" (code saved in unknown location) */
/* saved as "data_complete_2022_07_08.dta" */

/* Export to .csv data file */
proc export data = Tee_lib.data_processed_complete
  outfile = '/project/MacKay_Optum_TEE/data_processed_complete_2022_07_08.csv';
run;
