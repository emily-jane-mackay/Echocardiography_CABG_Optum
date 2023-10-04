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

/* set medical physician claims files with surgical CPT codes */
data temp_srg;
  set Tee_lib.srg_medical_21;
run;

/* sort the data */
proc sort data = temp_srg;
  by patid conf_id;
run;

/* merge medical physician claims files with surgical CPT codes into confinement source file */
data temp_srg_medical;
  merge temp_patid (in = b) temp_srg (in = l);
  by patid conf_id;
  if b = 1 and l = 1 then output;
run;

/* create a temporary dataset that contains only CPT codes for surgery */
data temp_srg_cpt;
  set temp_srg_medical (keep = patid conf_id charge proc_cd clmseq charge SRG);
run;

/* delete duplicates */
proc sort data = temp_srg_cpt nodupkey;
  by patid conf_id proc_cd;
run;

/* create column that counts all rows */
data step_1;
  set temp_srg_cpt;
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
  var SRG;
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
  by patid conf_id;
run;

/* sequential merge */
/* drop duplicates in source file */
proc sort data = temp_srg_medical nodupkey;
  by patid conf_id;
run;

/* decide on rows to keep in source data file */
data temp_srg_medical;
  set temp_srg_medical (keep = Patid Pat_Planid Conf_Id
    Charge Clmid Clmseq Drg
    Dstatus Fst_Dt Hccc Icd_Flag Loc_Cd
    Lst_Dt Paid_Dt Paid_Status Pos Proc_Cd
    Procmod Prov Prov_Par Provcat Rvnu_Cd
    Std_Cost Std_Cost_Yr Tos_Cd Op_Visit_Id
    Procmod2 Procmod3 Procmod4 Tos_Ext SRG);
run;

/* sequential merge */
data merge_1;
  merge temp_srg_medical (in = CTS) step_4;
  by patid conf_id;
  if CTS = 1;
run;

/* This code creates a macro variable &list with the list of variables in the form */
/* This format could be used to add a suffix to all the variables */
proc sql noprint;
   select cats(name,'=',name,'_srg')
          into :list
          separated by ' '
          from dictionary.columns
          where libname = 'WORK' and memname = 'MERGE_1';
quit;

proc datasets library = work nolist;
   modify merge_1;
   rename &list;
quit;

/* revert key merge variables to original name */
data merge_1;
  set merge_1 (rename = (
  Patid_srg = Patid
  Conf_Id_srg = Conf_Id));
run;

/* save dataset to library */
data Tee_lib.srg_processed;
  set work.merge_1;
run;
