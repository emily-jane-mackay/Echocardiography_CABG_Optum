/* deal with reshaping CPT claims to single row from medical files */

libname Optum 'path-redacted';
libname Op_death 'path-redacted';
libname Tee_lib 'path-redacted';

/* load in the concatenated confinement file */
/* sort the concatenated confinement file */

/* patid to find */
/* set confinement file and keep only patid */
data temp_patid;
  set Tee_lib.Cv_confine_21 (keep = patid conf_id);
run;

/* sort the data */
/* confirm no duplicates */
proc sort data = temp_patid nodupkey;
  by patid conf_id;
run;

/* set medical physician claims files with TEE CPT codes */
data temp_tee;
  set Tee_lib.Tee_medical_21;
run;

/* sort the data */
proc sort data = temp_tee;
  by patid conf_id;
run;

/* merge medical physician claims files with TEE CPT codes into confinement source file */
data temp_tee_medical;
  merge temp_patid (in = b) temp_tee (in = l);
  by patid conf_id;
  if b = 1 and l = 1 then output;
run;

/* create a temporary dataset that contains only CPT codes for TEE */
data temp_tee_cpt;
  set temp_tee_medical (keep = patid conf_id charge proc_cd clmseq charge TEE);
run;

/* delete duplicates */
proc sort data = temp_tee_cpt nodupkey;
  by patid conf_id proc_cd;
run;

/* create column that counts all rows */
data step_1;
  set temp_tee_cpt;
  row+1;
run;

/* create column that counts rows by patid */
data step_1;
  set step_1;
  count + 1;
  by patid;
  if first.patid then count = 1;
run;

data step_1;
  set step_1 (rename = (
  count = count_patid));
run;

/* proc transpose */
proc transpose data = step_1 out = step_2 prefix = cpt_;
  by patid conf_id row count_patid;
  id proc_cd;
  var TEE;
run;

/* collapse to single row */
data step_3;
 update step_2(obs=0) step_2;
 by patid;
run;

/* prepare data for merge */
data step_4;
  set step_3 (drop =
  _NAME_ row);
run;

/* sort */
proc sort data = step_4;
  by patid Conf_Id;
run;

/* create a temporary dataset that contains only tos_ext codes */
data temp_prov;
  set temp_tee_medical;
run;

data temp_prov;
  set temp_prov (keep =
  patid conf_id prov provcat tos_ext TEE);
run;

/* delete duplicates */
proc sort data = temp_prov nodupkey;
  by patid conf_id prov provcat;
run;

/* create column that counts all rows */
data step_1_p;
  set temp_prov;
  row+1;
run;

/* create column that counts rows by patid */
data step_1_p;
  set step_1_p;
  count + 1;
  by patid;
  if first.patid then count = 1;
run;

data step_1_p;
  set step_1_p (rename = (
  count = count_patid));
run;

/* proc transpose */
proc transpose data = step_1_p out = step_2_p prefix = prov_;
  by patid conf_id row count_patid;
  id tos_ext;
  var TEE;
run;

/* collapse to single row */
data step_3_p;
 update step_2_p(obs=0) step_2_p;
 by patid;
run;

/* prepare data for merge */
data step_4_p;
  set step_3_p (drop =
  _NAME_ row count_patid
  prov_fac_ip_acute_acute_acute_ac
  prov_fac_op_fo_dia_card_card_ech);
run;

/* sort */
proc sort data = step_4_p;
  by patid conf_id
  descending prov_prof_diagts_cardvs_echocr_e
  descending prov_prof_anesth_abdom_upper_upp;
run;

proc sort data = step_4;
  by patid conf_id
  descending cpt_93312
  descending cpt_93313
  descending cpt_93325
  descending cpt_93320
  descending cpt_93321
  descending cpt_93314
  descending cpt_93318
  descending cpt_93316
  descending cpt_93317
  descending cpt_93315;
run;

/* sequential merge */
/* decide on rows to keep in source data file */
data temp_tee_medical;
  set temp_tee_medical (keep = Patid Pat_Planid Conf_Id
    Charge Clmid Clmseq Drg
    Dstatus Fst_Dt Hccc Icd_Flag Loc_Cd
    Lst_Dt Paid_Dt Paid_Status Pos Proc_Cd
    Procmod Prov Prov_Par Provcat Rvnu_Cd
    Std_Cost Std_Cost_Yr Tos_Cd Op_Visit_Id
    Procmod2 Procmod3 Procmod4 Tos_Ext TEE);
run;

/* drop duplicates in source file */
proc sort data = temp_tee_medical nodupkey;
  by patid conf_id;
run;

/* sequential merge */
data merge_1;
  merge temp_tee_medical (in = CTS) step_4;
  by patid conf_id;
  if CTS = 1;
run;


data merge_2;
  merge merge_1 (in = CTS) step_4_p;
  by patid conf_id;
  if CTS = 1;
run;

/* rename colums that are too long to columns in dataset */
data merge_2;
  set merge_2 (rename = (
  prov_prof_diagts_cardvs_echocr_e = prov_prof_cardvs
  prov_prof_anesth_abdom_upper_upp = prov_prof_anesth));
run;

/* This code creates a macro variable &list with the list of variables in the form */
/* This format could be used to add a suffix to all the variables */
proc sql noprint;
   select cats(name,'=',name,'_tee')
          into :list
          separated by ' '
          from dictionary.columns
          where libname = 'WORK' and memname = 'MERGE_2';
quit;

proc datasets library = work nolist;
   modify merge_2;
   rename &list;
quit;

/* revert key merge variables to original name */
data merge_2;
  set merge_2 (rename = (
  Patid_tee = Patid
  Conf_Id_tee = Conf_Id));
run;


/* save data to library */
data Tee_lib.tee_processed;
  set work.merge_2;
run;
