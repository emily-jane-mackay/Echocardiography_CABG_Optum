/* Categorizing Surgeries from Index */

libname Optum 'path-redacted';
libname Op_death 'path-redacted';
libname Tee_lib 'path-redacted';

/* load in the concatenated confinement file */
/* sort the concatenated confinement file */
/* loop through every year */
/* begin macro loop */

/* note: 2007 (quarter 1) - 2015 (quarter 3) icd-9 procedures */

%macro CV_proc_9(begin_yr, begin_qtr, end_yr, end_qtr);
  %do year=&begin_yr %to &end_yr;
    %if &year = &begin_yr %then %let q_start = &begin_qtr;
    %else %let q_start = 1;
    %if &year = &end_yr %then %let q_end = &end_qtr;
    %else %let q_end = 4;

    %do q=&q_start %to &q_end;
      data temp_p;
        set Tee_lib.Cv_confine_21
        (keep = patid pat_planid admit_date disch_date);
      run;

      proc sort data=temp_p nodupkey;
        by patid pat_planid;
      run;

      data temp_d;
        set op_death.Dod_proc&year.q&q;
      run;

      proc sort data = temp_d
        (keep = patid pat_planid proc proc_position fst_dt);
        by patid pat_planid;
      run;

      data merge_1_many_3;
        merge temp_p (in = p) temp_d (in = d);
        by patid pat_planid;
        if p = 1 and d = 1 then output;
      run;

      proc sort data = merge_1_many_3;
        by patid;
      run;

    /* save files to the created library m_lib */

    %if &year = &begin_yr and &q = &begin_qtr %then %do;
      data Tee_lib.surgeries_9; set merge_1_many_3; run;
    %end

    %else %do;
      data Tee_lib.surgeries_9;
        set Tee_lib.surgeries_9 merge_1_many_3; run;
    %end;
  %end;

    /* end macro look commands below */

%mend;

%CV_proc_9(2007, 1, 2015, 3);

/* note: 2015 (quarter 4) - 2019 (quarter 4) icd-10 procedures */

%macro CV_proc_10(begin_yr, begin_qtr, end_yr, end_qtr);
  %do year=&begin_yr %to &end_yr;
    %if &year = &begin_yr %then %let q_start = &begin_qtr;
    %else %let q_start = 1;
    %if &year = &end_yr %then %let q_end = &end_qtr;
    %else %let q_end = 4;

    %do q=&q_start %to &q_end;
      data temp_p;
        set Tee_lib.Cv_confine_21
        (keep = patid pat_planid admit_date disch_date);
      run;

      proc sort data=temp_p nodupkey;
        by patid pat_planid;
      run;

      data temp_d;
        set op_death.Dod_proc&year.q&q;
      run;

      proc sort data = temp_d
        (keep = patid pat_planid proc proc_position fst_dt);
        by patid pat_planid;
      run;

      data merge_1_many_3;
        merge temp_p (in = p) temp_d (in = d);
        by patid pat_planid;
        if p = 1 and d = 1 then output;
      run;

      proc sort data = merge_1_many_3;
        by patid;
      run;

    /* save files to the created library m_lib */

    %if &year = &begin_yr and &q = &begin_qtr %then %do;
      data Tee_lib.surgeries_10; set merge_1_many_3; run;
    %end

    %else %do;
      data Tee_lib.surgeries_10;
        set Tee_lib.surgeries_10 merge_1_many_3; run;
    %end;
  %end;

    /* end macro look commands below */

%mend;

%CV_proc_10(2015, 4, 2019, 4);


/* Surgical 9 Categories */
/* set procedures data file */
data temp_surg_9;
  set Tee_lib.surgeries_9
  (keep = patid pat_planid admit_date disch_date proc fst_dt);
run;

data surg_9;
  set temp_surg_9;

  /* fix logic */


  /* need - second command "is_reop" to capture surgeries performed after discharge from index admission */

  %let is_index = (
    (fst_dt >= admit_date and (fst_dt <= disch_date or fst_dt < intnx('day', disch_date, 7)))
  );

  /*
  %let is_reop = (
    fst_dt > disch_date and fst_dt < intnx('day', disch_date, 365)
  );
  */

  /* All heart valve surgeries (icd9p) */
  %let valve_codes = (
    '3510','3511','3512','3513','3514','3520','3521','3522','3523','3524','3525',
    '3526','3527','3528','3533','3599'
  );
  if proc in &valve_codes and &is_index then index_valve = 1;
  else index_valve = 0;
  /*
  if proc in &valve_codes and &is_reop then reop_valve = 1;
  else reop_valve = 0;
  */

  /* All CABG surgeries (icd9p) */
  %let cabg_codes = (
    '3610','3611','3612','3613','3614','3615','3616','3617','3619'
  );
  if proc in &cabg_codes and &is_index then index_cabg = 1;
  else index_cabg = 0;
  /*
  if proc in &cabg_codes and &is_reop then reop_cabg = 1;
  else reop_cabg = 0;
  */

  /* CABGx1 surgeries (icd9p) */
  if proc in
    ('3611') and &is_index then index_cabgx1 = 1;
  else index_cabgx1 = 0;

  /* CABGx2 surgeries (icd9p) */
  if proc in
    ('3612') and &is_index then index_cabgx2 = 1;
  else index_cabgx2 = 0;

  /* CABGx3 surgeries (icd9p) */
  if proc in
    ('3613') and &is_index then index_cabgx3 = 1;
  else index_cabgx3 = 0;

  /* CABGx4 surgeries (icd9p) */
  if proc in
    ('3614') and &is_index then index_cabgx4_more = 1;
  else index_cabgx4_more = 0;

  /* CABG IMA single / double surgeries (icd9p) */
  if proc in
    ('3615','3616') and &is_index then index_cabg_ima = 1;
  else index_cabg_ima = 0;

  /* aortic valve repair (icd9p) */
  if proc in
    ('3511') and &is_index then index_av_repair = 1;
  else index_av_repair = 0;

  /* aortic valve replace (icd9p) */
  if proc in
    ('3521','3522') and &is_index then index_av_replace = 1;
  else index_av_replace = 0;

  /* mitral valve repair (icd9p) */
  if proc in
    ('3512') and &is_index then index_mv_repair = 1;
  else index_mv_repair = 0;

  /* mitral valve replace (icd9p) */
  if proc in
    ('3523','3524') and &is_index then index_mv_replace = 1;
  else index_mv_replace = 0;

  /* pulmonic valve repair (icd9p) */
  if proc in
    ('3513') and &is_index then index_pv_repair = 1;
  else index_pv_repair = 0;

  /* pulmonic valve replace (icd9p) */
  if proc in
    ('3525','3526') and &is_index then index_pv_replace = 1;
  else index_pv_replace = 0;

  /* tricuspid valve repair (icd9p) */
  if proc in
    ('3514') and &is_index then index_tv_repair = 1;
  else index_tv_repair = 0;

  /* tricuspid valve replace (icd9p) */
  if proc in
    ('3527','3528') and &is_index then index_tv_replace = 1;
  else index_tv_replace = 0;

  /* ECMO index */
  if proc in
    ('3961','3965') and &is_index then index_ecmo = 1;
  else index_ecmo = 0;
run;

  /* Surgical 10 categories */
  data temp_surg_10;
    set Tee_lib.surgeries_10
    (keep = patid pat_planid admit_date disch_date proc fst_dt);
  run;

  data surg_10;
    set temp_surg_10;

    /* need - second command "is_reop" to capture surgeries performed after discharge from index admission */

    %let is_index = (
      (fst_dt >= admit_date and (fst_dt <= disch_date or fst_dt < intnx('day', disch_date, 7)))
    );

    /*
    %let is_reop = (
      fst_dt > disch_date and fst_dt < intnx('day', disch_date, 365)
    );
    */

    /* All heart valve surgeries (icd10p) */
    %let valve_codes = (
      '027F0ZZ','02NF0ZZ','02QF0ZZ','027G0ZZ','02NG0ZZ','02QG0ZZ','027H0ZZ',
      '02NH0ZZ','02QH0ZZ','027J0ZZ','02NJ0ZZ','02QJ0ZZ','02RF07Z','02RF08Z',
      '02RF0KZ','02RF0JZ','02RG07Z','02RG08Z','02RG0KZ','02RG0JZ','02RH07Z',
      '02RH08Z','02RH0KZ','02RH0JZ','02RJ07Z','02RJ08Z','02RJ0KZ','02RJ0JZ',
      '02UF0JZ','02UG0JZ','02UH0JZ','02UJ0JZ'
    );
    if proc in &valve_codes and &is_index then index_valve = 1;
    else index_valve = 0;
    /*
    if proc in &valve_codes and &is_reop then reop_valve = 1;
    else reop_valve = 0;
    */

    /* All CABG surgeries (icd10p) */
    %let cabg_codes = (
      '0210093','0210098','0210099','021009C','021009F','021009W','02100A3','02100A8','02100A9','02100AC',
      '02100AF','02100AW','02100J3','02100J8','02100J9','02100JC','02100JF','02100JW','02100K3','02100K8',
      '02100K9','02100KC','02100KF','02100KW','02100Z3','02100Z8','02100Z9','02100ZC','02100ZF','0210493',
      '0210498','0210499','021049C','021049F','021049W','02104A3','02104A8','02104A9','02104AC','02104AF',
      '02104AW','02104J3','02104J8','02104J9','02104JC','02104JF','02104JW','02104K3','02104K8','02104K9',
      '02104KC','02104KF','02104KW','02104Z3','02104Z8','02104Z9','02104ZC','02104ZF','0211093','0211098',
      '0211099','021109C','021109F','021109W','02110A3','02110A8','02110A9','02110AC','02110AF','02110AW',
      '02110J3','02110J8','02110J9','02110JC','02110JF','02110JW','02110K3','02110K8','02110K9','02110KC',
      '02110KF','02110KW','02110Z3','02110Z8','02110Z9','02110ZC','02110ZF','0211493','0211498','0211499',
      '021149C','021149F','021149W','02114A3','02114A8','02114A9','02114AC','02114AF','02114AW','02114J3',
      '02114J8','02114J9','02114JC','02114JF','02114JW','02114K3','02114K8','02114K9','02114KC','02114KF',
      '02114KW','02114Z3','02114Z8','02114Z9','02114ZC','02114ZF','0212093','0212098','0212099','021209C',
      '021209F','021209W','02120A3','02120A8','02120A9','02120AC','02120AF','02120AW','02120J3','02120J8',
      '02120J9','02120JC','02120JF','02120JW','02120K3','02120K8','02120K9','02120KC','02120KF','02120KW',
      '02120Z3','02120Z8','02120Z9','02120ZC','02120ZF','0212493','0212498','0212499','021249C','021249F',
      '021249W','02124A3','02124A8','02124A9','02124AC','02124AF','02124AW','02124J3','02124J8','02124J9',
      '02124JC','02124JF','02124JW','02124K3','02124K8','02124K9','02124KC','02124KF','02124KW','02124Z3',
      '02124Z8','02124Z9','02124ZC','02124ZF','0213093','0213098','0213099','021309C','021309F','021309W',
      '02130A3','02130A8','02130A9','02130AC','02130AF','02130AW','02130J3','02130J8','02130J9','02130JC',
      '02130JF','02130JW','02130K3','02130K8','02130K9','02130KC','02130KF','02130KW','02130Z3','02130Z8',
      '02130Z9','02130ZC','02130ZF','0213493','0213498','0213499','021349C','021349F','021349W','02134A3',
      '02134A8','02134A9','02134AC','02134AF','02134AF','02134J3','02134J8','02134J9','02134JC','02134JF',
      '02134JW','02134K3','02134K8','02134K9','02134KC','02134KF','02134KW','02134Z3','02134Z8','02134Z9',
      '02134ZC','02134ZF'
    );
    if proc in &cabg_codes and &is_index then index_cabg = 1;
    else index_cabg = 0;
    /*
    if proc in &cabg_codes and &is_reop then reop_cabg = 1;
    else reop_cabg = 0;
    */


    /* CABGx1 surgeries (icd10p) */
    if proc in
      ('0210093','0210098','0210099','021009C','021009F','021009W','02100A3','02100A8','02100A9','02100AC',
      '02100AF','02100AW','02100J3','02100J8','02100J9','02100JC','02100JF','02100JW','02100K3','02100K8',
      '02100K9','02100KC','02100KF','02100KW','02100Z3','02100Z8','02100Z9','02100ZC','02100ZF','0210493',
      '0210498','0210499','021049C','021049F','021049W','02104A3','02104A8','02104A9','02104AC','02104AF',
      '02104AW','02104J3','02104J8','02104J9','02104JC','02104JF','02104JW','02104K3','02104K8','02104K9',
      '02104KC','02104KF','02104KW','02104Z3','02104Z8','02104Z9','02104ZC','02104ZF') and &is_index then index_cabgx1 = 1;
    else index_cabgx1 = 0;

    /* CABGx2 surgeries (icd10p) */
    if proc in
      ('0211093','0211098','0211099','021109C','021109F','021109W','02110A3','02110A8','02110A9','02110AC',
      '02110AF','02110AW','02110J3','02110J8','02110J9','02110JC','02110JF','02110JW','02110K3','02110K8',
      '02110K9','02110KC','02110KF','02110KW','02110Z3','02110Z8','02110Z9','02110ZC','02110ZF','0211493',
      '0211498','0211499','021149C','021149F','021149W','02114A3','02114A8','02114A9','02114AC','02114AF',
      '02114AW','02114J3','02114J8','02114J9','02114JC','02114JF','02114JW','02114K3','02114K8','02114K9',
      '02114KC','02114KF','02114KW','02114Z3','02114Z8','02114Z9','02114ZC','02114ZF') and &is_index then index_cabgx2 = 1;
    else index_cabgx2 = 0;

    /* CABGx3 surgeries (icd10p) */
    if proc in
      ('0212093','0212098','0212099','021209C','021209F','021209W','02120A3','02120A8','02120A9','02120AC',
      '02120AF','02120AW','02120J3','02120J8','02120J9','02120JC','02120JF','02120JW','02120K3','02120K8',
      '02120K9','02120KC','02120KF','02120KW','02120Z3','02120Z8','02120Z9','02120ZC','02120ZF','0212493',
      '0212498','0212499','021249C','021249F','021249W','02124A3','02124A8','02124A9','02124AC','02124AF',
      '02124AW','02124J3','02124J8','02124J9','02124JC','02124JF','02124JW','02124K3','02124K8','02124K9',
      '02124KC','02124KF','02124KW','02124Z3','02124Z8','02124Z9','02124ZC','02124ZF') and &is_index then index_cabgx3 = 1;
    else index_cabgx3 = 0;

    /* CABGx4 surgeries (icd10p) */
    if proc in
      ('0213093','0213098','0213099','021309C','021309F','021309W','02130A3','02130A8','02130A9','02130AC',
      '02130AF','02130AW','02130J3','02130J8','02130J9','02130JC','02130JF','02130JW','02130K3','02130K8',
      '02130K9','02130KC','02130KF','02130KW','02130Z3','02130Z8','02130Z9','02130ZC','02130ZF','0213493',
      '0213498','0213499','021349C','021349F','021349W','02134A3','02134A8','02134A9','02134AC','02134AF',
      '02134AF','02134J3','02134J8','02134J9','02134JC','02134JF','02134JW','02134K3','02134K8','02134K9',
      '02134KC','02134KF','02134KW','02134Z3','02134Z8','02134Z9','02134ZC','02134ZF') and &is_index then index_cabgx4_more = 1;
    else index_cabgx4_more = 0;

    /* CABG IMA single / double surgeries (icd10p) */
    if proc in
      ('0210098','0210099','02100A8','02100A9','02100J8','02100J9','02100K8','02100K9','02100Z8','02100Z9',
      '0210498','0210499','02104A8','02104A9','02104J8','02104J9','02104K8','02104K9','02104Z8','02104Z9',
      '0211098','0211099','02110A8','02110A9','02110J8','02110J9','02110K8','02110K9','02110Z8','02110Z9',
      '0211498','0211499','02114A8','02114A9','02114J8','02114J9','02114K8','02114K9','02114Z8','02114Z9',
      '02114ZC','0212098','0212099','02120A8','02120A9','02120J8','02120J9','02120K8','02120K9','02120Z8',
      '02120Z9','0212498','0212499','02124A8','02124A9','02124J8','02124J9','02124K8','02124K9','02124Z8',
      '02124Z9','0213098','0213099','02130A8','02130A9','02130J8','02130J9','02130K8','02130K9','02130Z8',
      '02130Z9','0213498','0213499','02134A8','02134A9','02134J8','02134J9','02134K8','02134K9','02134Z8',
      '02134Z9') and &is_index then index_cabg_ima = 1;
    else index_cabg_ima = 0;

    /* aortic valve repair (icd10p) */
    if proc in
      ('02UF0JZ','027F0ZZ','02NF0ZZ','02QF0ZZ') and &is_index then index_av_repair = 1;
    else index_av_repair = 0;

    /* aortic valve replace (icd10p) */
    if proc in
      ('02RF07Z','02RF08Z','02RF0KZ','02RF0JZ') and &is_index then index_av_replace = 1;
    else index_av_replace = 0;

    /* mitral valve repair (icd10p) */
    if proc in
      ('02UG0JZ','027G0ZZ','02NG0ZZ','02QG0ZZ') and &is_index then index_mv_repair = 1;
    else index_mv_repair = 0;

    /* mitral valve replace (icd10p) */
    if proc in
      ('02RG07Z','02RG08Z','02RG0KZ','02RG0JZ') and &is_index then index_mv_replace = 1;
    else index_mv_replace = 0;

    /* pulmonic valve repair (icd10p) */
    if proc in
      ('02UH0JZ','027H0ZZ','02NH0ZZ','02QH0ZZ') and &is_index then index_pv_repair = 1;
    else index_pv_repair = 0;

    /* pulmonic valve replace (icd10p) */
    if proc in
      ('02UJ0JZ','02RH07Z','02RH08Z','02RH0KZ','02RH0JZ') and &is_index then index_pv_replace = 1;
    else index_pv_replace = 0;

    /* tricuspid valve repair (icd10p) */
    if proc in
      ('027J0ZZ','02NJ0ZZ','02QJ0ZZ') and &is_index then index_tv_repair = 1;
    else index_tv_repair = 0;

    /* tricuspid valve replace (icd10p) */
    if proc in
      ('02RJ07Z','02RJ08Z','02RJ0KZ','02RJ0JZ') and &is_index then index_tv_replace = 1;
    else index_tv_replace = 0;

    /* ECMO index */
    if proc in
      ('5A0','5A05121','5A0512C','5A05221','5A0522C','5A1','5A1522F','5A1522G','5A1522H',
      '5A15A2F','5A15A2G','5A15A2H') and &is_index then index_ecmo = 1;
    else index_ecmo = 0;
run;

/* append proc9 and proc10 procedure files here */
data surgical_output; set Surg_9; run;
proc append base = surgical_output data = work.Surg_10; run;


Proc sql;
  create table surgical_output_edited as
  select patid, pat_planid,
      max(index_valve) as index_valve_1,
      max(index_cabg) as index_cabg_1,
      max(index_cabgx1) as index_cabgx1_1,
      max(index_cabgx2) as index_cabgx2_1,
      max(index_cabgx3) as index_cabgx3_1,
      max(index_cabgx4_more) as index_cabgx4_more_1,
      max(index_cabg_ima) as index_cabg_ima_1,
      max(index_av_repair) as index_av_repair_1,
      max(index_av_replace) as index_av_replace_1,
      max(index_mv_repair) as index_mv_repair_1,
      max(index_mv_replace) as index_mv_replace_1,
      max(index_pv_repair) as index_pv_repair_1,
      max(index_pv_replace) as index_pv_replace_1,
      max(index_tv_repair) as index_tv_repair_1,
      max(index_tv_replace) as index_tv_replace_1,
      max(index_ecmo) as index_ecmo_1
  from surgical_output
  group by patid, pat_planid;



/* save dataset to library */
data Tee_lib.surgeries_index_coded;
    set work.surgical_output_edited;
run;
