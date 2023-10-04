/* Merging Elixahuser / Outcomes file with Surgical Categories file */

libname Optum 'path-redacted';
libname Op_death 'path-redacted';
libname Tee_lib 'path-redacted';

/* merge Elixhauser diagnoses file into source confinement file */
/* 1) set confinement file as source file */
data temp_conf;
  set Tee_lib.Cv_confine_21;
run;

proc sort data = temp_conf;
  by patid pat_planid admit_date;
run;

/* 2) set Elixhauser file as file to merge */
data temp_elix;
  set Tee_lib.Elixhauser_outcomes_coded;
run;

proc sort data = temp_elix;
  by patid pat_planid;
run;

/* 3) left merge: confinement as source file & Elixhauser/outcomes as merge file */
data merge_1;
  merge temp_conf temp_elix;
  by patid pat_planid;
run;

/* 4) set Surgical categories file as file to merge */
data temp_srg;
  set Tee_lib.surgeries_index_coded;
run;

proc sort data = temp_srg;
  by patid pat_planid;
run;

/* 5) left merge: merge_1 as source file & surgical categories as merge file */
data merge_2;
  merge merge_1 temp_srg;
  by patid pat_planid;
run;

/* 4) save confinement merged with Elixhauser to Tee_lib */
data Tee_lib.confinement_elix_srg;
  set merge_2;
run;
