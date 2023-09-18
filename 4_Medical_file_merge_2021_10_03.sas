/* merge: 1:1 processed surgical medical file with tee medical file */

libname Optum 'path-redacted';
libname Op_death 'path-redacted';
libname Tee_lib 'path-redacted';

/* confinement file with no duplicates is source file */
data temp_patid;
  set Tee_lib.Cv_confine_21;
run;

/* sort the data */
/* confirm no duplicates */
proc sort data = temp_patid nodupkey;
  by patid conf_id;
run;

/* prepare processed tee medical file for merge by sorting */
proc sort data = Tee_lib.Tee_processed;
  by patid conf_id;
run;

/* first merge */
data merge_1;
  merge temp_patid (in = CTS) Tee_lib.Tee_processed;
  by patid conf_id;
  if CTS = 1;
run;

/* prepare processed tee medical file for merge by sorting */
proc sort data = Tee_lib.srg_processed;
  by patid conf_id;
run;

/* second merge */
data merge_2;
  merge merge_1 (in = CTS) Tee_lib.srg_processed;
  by patid conf_id;
  if CTS = 1;
run;

/* save file with both medical and surgical physician claims merged into confinement */
/* save dataset to library */
data Tee_lib.confinement_medical_processed;
  set work.merge_2;
run;
