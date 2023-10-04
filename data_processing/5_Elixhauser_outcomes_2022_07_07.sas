/* Elixhauser Coding (from medical files) */

libname Optum 'path-redacted';
libname Op_death 'path-redacted';
libname Tee_lib 'path-redacted';


/* load in the concatenated confinement file */
/* sort the concatenated confinement file */
/* loop through every year */
/* begin macro loop */

/* note: 2007 (quarter 1) - 2015 (quarter 3) icd-9 */

%macro CV_diag_9(begin_yr, begin_qtr, end_yr, end_qtr);
  %do year=&begin_yr %to &end_yr;
    %if &year = &begin_yr %then %let q_start = &begin_qtr;
    %else %let q_start = 1;
    %if &year = &end_yr %then %let q_end = &end_qtr;
    %else %let q_end = 4;

    %do q=&q_start %to &q_end;
      data temp_p;
        set Tee_lib.Cv_confine_21
        (keep = patid pat_planid admit_date);
      run;

      proc sort data=temp_p nodupkey;
        by patid pat_planid;
      run;

      data temp_d;
        set op_death.Dod_diag&year.q&q;
      run;

      proc sort data = temp_d
        (keep = patid pat_planid diag diag_position loc_cd poa fst_dt);
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
      data Tee_lib.diagnoses_9; set merge_1_many_3; run;
    %end

    %else %do;
      data Tee_lib.diagnoses_9;
        set Tee_lib.diagnoses_9 merge_1_many_3; run;
    %end;
  %end;

    /* end macro look commands below */

%mend;

%CV_diag_9(2007, 1, 2015, 3);

/* note: 2015 (quarter 4) - 2019 (quarter 4) icd-10 */

%macro CV_diag_10(begin_yr, begin_qtr, end_yr, end_qtr);
  %do year=&begin_yr %to &end_yr;
    %if &year = &begin_yr %then %let q_start = &begin_qtr;
    %else %let q_start = 1;
    %if &year = &end_yr %then %let q_end = &end_qtr;
    %else %let q_end = 4;

    %do q=&q_start %to &q_end;
      data temp_p;
        set Tee_lib.Cv_confine_21
        (keep = patid pat_planid admit_date);
      run;

      proc sort data=temp_p nodupkey;
        by patid pat_planid;
      run;

      data temp_d;
        set op_death.Dod_diag&year.q&q;
      run;

      proc sort data = temp_d
        (keep = patid pat_planid diag diag_position loc_cd poa fst_dt);
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
      data Tee_lib.diagnoses_10; set merge_1_many_3; run;
    %end

    %else %do;
      data Tee_lib.diagnoses_10;
        set Tee_lib.diagnoses_10 merge_1_many_3; run;
    %end;
  %end;

    /* end macro look commands below */

%mend;

%CV_diag_10(2015, 4, 2019, 4);

/* Elixhauser 9 Categories */
/* set diagnoses data file */
data temp_elix_9;
  set Tee_lib.Diagnoses_9
  (keep = patid pat_planid admit_date diag poa fst_dt);
run;

data elix_9;
  set temp_elix_9;

  %let is_preexisting = (
    (fst_dt < admit_date and fst_dt > intnx('day', admit_date, -365)) or
    ((fst_dt >= admit_date and fst_dt <= intnx('day', admit_date, 1)) and (poa = 'Y'))
  );

  %let is_complication = (
    fst_dt >= admit_date and fst_dt < intnx('day', admit_date, 30) and (poa = 'U')
  );

  /* Congestive Heart Failure */
  if diag in
    ('39891','40201','40211','40291','40401','40403','40411', '40413',
    '40491', '40493','4254','4255','4257','4258','4259','428') and &is_preexisting then elix_chf = 1;
  else elix_chf = 0;

  /* Cardiac Arrhythmia */
  if diag in
    ('4260','42613','4267','4269','42610','42612','4270','4271','4272','4273',
    '4274','4276','4278','4279','7850','99601','99604','V450','V533') and &is_preexisting then elix_arrhy = 1;
  else elix_arrhy = 0;

  /* Valvular Disease */
  if diag in
    ('0932','394','395','396','397','424','7463','7464','7465','7466',
    'V422','V433') and &is_preexisting then elix_valvular = 1;
  else elix_valvular = 0;

  /* Pulmonary Circulation Disorders */
  if diag in
    ('4150','4151','416','4170','4178',
    '4179') and &is_preexisting then elix_pcd = 1;
  else elix_pcd = 0;

  /* Peripheral Vascular Disorders */
  if diag in
    ('0930','4373','440','441','4431','4432','4438','4439','4471','5571',
    '5579','V434') and &is_preexisting then elix_pvd = 1;
  else elix_pvd = 0;

  /* Hypertension Uncomplicated */
  if diag in
    ('401') and &is_preexisting then elix_htn_un_cx = 1;
  else elix_htn_un_cx = 0;

  /* Hypertension Complicated */
  if diag in
    ('402','403','404','405') and &is_preexisting then elix_htn_cx = 1;
  else elix_htn_cx = 0;

  /* Paralysis */
  if diag in
    ('3341','342','343','3440','3441','3442','3443','3444','3445','3446',
    '3449') and &is_preexisting then elix_paraly = 1;
  else elix_paraly = 0;

  /* Other Neurological Disorders */
  if diag in
    ('3319','3320','3321','3334','3335','33392','334','335','3362','340','341',
    '345','3481','3483','7803','7843') and &is_preexisting then elix_neurol = 1;
  else elix_neurol = 0;

  /* Chronic Pulmonary Disease */
  if diag in
    ('4168','4169','490','491','492','493','494','495','496','500','501','502',
    '503','504','505','5064','5081','5088') and &is_preexisting then elix_cpd = 1;
  else elix_cpd = 0;

  /* Diabetes Uncomplicated */
  if diag in
    ('2500','2501','2502','2503') and &is_preexisting then elix_diab_un_cx = 1;
  else elix_diab_un_cx = 0;

  /* Diabetes Complicated */
  if diag in
    ('2504','2505','2506','2507','2508','2509') and &is_preexisting then elix_diab_cx = 1;
  else elix_diab_cx = 0;

  /* Hypothyroidism */
  if diag in
    ('2409','243','244','2461','2468') and &is_preexisting then elix_hypoth = 1;
  else elix_hypoth = 0;

  /* Renal Failure */
  if diag in
    ('40301','40311','40391','40402','40403','40412','40413','40492','40493',
    '585','586','5880','V420','V451','V56') and &is_preexisting then elix_renal_fail = 1;
  else elix_renal_fail = 0;

  /* Liver Disease */
  if diag in
    ('07022','07023','07032','07033','07044','07054','0706','0709','4560','4561',
    '4562','570','571','5722','5723','5724','5728','5733','5734','5738','5739',
    'V427') and &is_preexisting then elix_liver = 1;
  else elix_liver = 0;

  /* Peptic Ulcer Disease excluding bleeding */
  if diag in
    ('5317','5319','5327','5329','5337','5339','5347','5349') and &is_preexisting then elix_peptic = 1;
  else elix_peptic = 0;

  /* AIDS/HIV */
  if diag in
    ('042','043','044') and &is_preexisting then elix_aids_hiv = 1;
  else elix_aids_hiv = 0;

  /* Lymphoma */
  if diag in
    ('200','201','202','2030','2386') and &is_preexisting then elix_lymphoma = 1;
  else elix_lymphoma = 0;

  /* Metastatic Cancer */
  if diag in
    ('196','197','198','199') and &is_preexisting then elix_metastatic = 1;
  else elix_metastatic = 0;

  /* Solid Tumor without Metastasis */
  if diag in
    ('140','141','142','143','144','145','146','147','148','149','150','151','152',
    '153','154','155','156','157','158','159','160','161','162','163','164','165',
    '166','167','168','169','170','171','172','174','175','176','177','178','179',
    '180','181','182','183','184','185','186','187','188','189','190','191','192',
    '193','194','195') and &is_preexisting then elix_solid_tum = 1;
  else elix_solid_tum = 0;

  /* Rheumatoid Arthritis/collagen */
  if diag in
    ('446','7010','7100','7101','7102','7103','7104','7108','7109','7112','714',
    '7193','720','725','7285','72889','72930') and &is_preexisting then elix_rheuma = 1;
  else elix_rheuma = 0;

  /* Coagulopathy */
  if diag in
    ('286','2871','2873','2874','2875') and &is_preexisting then elix_coag = 1;
  else elix_coag = 0;

  /* Obesity */
  if diag in
    ('2780') and &is_preexisting then elix_obesity = 1;
  else elix_obesity = 0;

  /* Weight Loss */
  if diag in
    ('260','261','262','263','7832','7994') and &is_preexisting then elix_wt_loss = 1;
  else elix_wt_loss = 0;

  /* Fluid and Electrolyte Disorders */
  if diag in
    ('2536','276') and &is_preexisting then elix_fluid_elec = 1;
  else elix_fluid_elec = 0;

  /* Blood Loss Anemia */
  if diag in
    ('2800') and &is_preexisting then elix_anemia_bld_loss = 1;
  else elix_anemia_bld_loss = 0;

  /* Deficiency Anemia */
  if diag in
    ('2801','2808','2809','281') and &is_preexisting then elix_anemia_iron = 1;
  else elix_anemia_iron = 0;

  /* Alcohol Abuse */
  if diag in
    ('2652','2911','2912','2913','2915','2918','2919','3030','3039','3050','3575',
    '4255','5353','5710','5711','5712','5713','980','V113') and &is_preexisting then elix_alcohol = 1;
  else elix_alcohol = 0;

  /* Drug Abuse */
  if diag in
    ('292','304','3052','3053','3054','3055','3056','3057','3058','3059',
    'V6542') and &is_preexisting then elix_drugs = 1;
  else elix_drugs = 0;

  /* Psychoses */
  if diag in
    ('2938','295','29604','29614','29644','29654','297','298') and &is_preexisting then elix_phsycho = 1;
  else elix_phsycho = 0;

  /* Depression */
  if diag in
    ('2962','2963','2965','3004','309','311') and &is_preexisting then elix_depress = 1;
  else elix_depress = 0;

  /* complication stroke */
  if diag in
    ('431','432','433.01','43311','43321','43331','43381','43391','434','4340',
    '43401','4341','43411','4349','43491','436','99702','438','4380','4381',
    '43810','43811','43812','43819','4382','43820','43821','43822','4383',
    '43830','43831','43832','4384','43840','43841','43842','4385','43850',
    '43851','43852','43853','4386','4387','4388','43881','43882','43883',
    '43884','43885','43889','4389','V1259') and &is_complication then complic_stroke = 1;
  else complic_stroke = 0;
run;

/* ICD-10 */

data temp_elix_10;
  set Tee_lib.Diagnoses_10
  (keep = patid pat_planid admit_date diag poa fst_dt);
run;

data elix_10;
  set temp_elix_10;

  %let is_preexisting = (
    (fst_dt < admit_date and fst_dt > intnx('day', admit_date, -365)) or
    ((fst_dt >= admit_date and fst_dt <= intnx('day', admit_date, 1)) and (poa = 'Y'))
  );

  %let is_complication = (
    fst_dt >= admit_date and fst_dt < intnx('day', admit_date, 30) and (poa = 'U')
  );

  /* Congestive Heart Failure */
  if diag in
    ('I099','I110','I130','I132','I255','I420','I425','I426','I427','I428',
    'I429','I43','I50','P290') and &is_preexisting then elix_chf = 1;
  else elix_chf = 0;

  /* Cardiac Arrhythmia */
  if diag in
    ('I441','I442','I443','I456','I459','I47','I48','I49','R000','R001',
    'R008','T821','Z450','Z950') and &is_preexisting then elix_arrhy = 1;
  else elix_arrhy = 0;

  /* Valvular Disease */
  if diag in
    ('A520','I05','I06','I07','I08','I091','I098','I34','I35','I36','I37',
    'I38','I39','Q230','Q231','Q232','Q233','Z952','Z953','Z954') and &is_preexisting then elix_valvular = 1;
  else elix_valvular = 0;

  /* Pulmonary Circulation Disorders */
  if diag in
    ('I26','I27','I280','I288','I289') and &is_preexisting then elix_pcd = 1;
  else elix_pcd = 0;

  /* Peripheral Vascular Disorders */
  if diag in
    ('I70','I71','I731','I738','I739','I771','I790','I792','K551','K558',
    'K559','Z958','Z959') and &is_preexisting then elix_pvd = 1;
  else elix_pvd = 0;

  /* Hypertension Uncomplicated */
  if diag in
    ('I10') and &is_preexisting then elix_htn_un_cx = 1;
  else elix_htn_un_cx = 0;

  /* Hypertension Complicated */
  if diag in
    ('I11','I12','I13','I15') and &is_preexisting then elix_htn_cx = 1;
  else elix_htn_cx = 0;

  /* Paralysis */
  if diag in
    ('G041','G114','G801','G802','G81','G82','G830','G831','G832','G833',
    'G834','G839') and &is_preexisting then elix_paraly = 1;
  else elix_paraly = 0;

  /* Other Neurological Disorders */
  if diag in
    ('G10','G11','G12','G13','G20','G21','G22','G254','G255','G312','G318',
    'G319','G32','G35','G36','G37','G40','G41','G931','G934','R470','R56') and &is_preexisting then elix_neurol = 1;
  else elix_neurol = 0;

  /* Chronic Pulmonary Disease */
  if diag in
    ('I278','I279','J40','J41','J42','J43','J44','J45','J46','J47','J60','J61',
    'J62','J63','J64','J65','J66','J67','J684','J701','J703') and &is_preexisting then elix_cpd = 1;
  else elix_cpd = 0;

  /* Diabetes Uncomplicated */
  if diag in
    ('E100','E101','E109','E110','E111','E119','E120','E121','E129','E130',
    'E131','E139','E140','E141','E149') and &is_preexisting then elix_diab_un_cx = 1;
  else elix_diab_un_cx = 0;

  /* Diabetes Complicated */
  if diag in
    ('E102','E103','E104','E105','E106','E107','E108','E112','E113','E114','E115',
    'E116','E117','E118','E122','E123','E124','E125','E126','E127','E128','E132',
    'E133','E134','E135','E136','E137','E138','E142','E143','E144','E145','E146',
    'E147','E148')  and &is_preexisting then elix_diab_cx = 1;
  else elix_diab_cx = 0;

  /* Hypothyroidism */
  if diag in
    ('E00','E01','E02','E03','E890') and &is_preexisting then elix_hypoth = 1;
  else elix_hypoth = 0;

  /* Renal Failure */
  if diag in
    ('I120','I131','N18','N19','N250','Z490','Z491','Z492','Z940','Z992') and &is_preexisting then elix_renal_fail = 1;
  else elix_renal_fail = 0;

  /* Liver Disease */
  if diag in
    ('B18','I85','I864','I982','K70','K711','K713','K714','K715','K717','K72','K73',
    'K74','K760','K762','K763','K764','K765','K766','K767','K768','K769','Z944') and &is_preexisting then elix_liver = 1;
  else elix_liver = 0;

  /* Peptic Ulcer Disease excluding bleeding */
  if diag in
    ('K257','K259','K267','K269','K277','K279','K287','K289') and &is_preexisting then elix_peptic = 1;
  else elix_peptic = 0;

  /* AIDS/HIV */
  if diag in
    ('B20','B21','B22','B24') and &is_preexisting then elix_aids_hiv = 1;
  else elix_aids_hiv = 0;

  /* Lymphoma */
  if diag in
    ('C81','C82','C83','C84','C85','C88','C96','C900','C902') and &is_preexisting then elix_lymphoma = 1;
  else elix_lymphoma = 0;

  /* Metastatic Cancer */
  if diag in
    ('C77','C78','C79','C80') and &is_preexisting then elix_metastatic = 1;
  else elix_metastatic = 0;

  /* Solid Tumor without Metastasis */
  if diag in
    ('C00','C01','C02','C03','C04','C05','C06','C07','C08','C09','C10','C11','C12',
    'C13','C14','C15','C16','C17','C18','C19','C20','C21','C22','C23','C24','C25',
    'C26','C30','C31','C32','C33','C34','C37','C38','C39','C40','C41','C43','C45',
    'C46','C47','C48','C49','C50','C51','C52','C53','C54','C55','C56','C57','C58',
    'C60','C61','C62','C63','C64','C65','C66','C67','C68','C69','C70','C71','C72',
    'C73','C74','C75','C76','C97') and &is_preexisting then elix_solid_tum = 1;
  else elix_solid_tum = 0;

  /* Rheumatoid Arthritis/collagen */
  if diag in
    ('L940','L941','L943','M05','M06','M08','M120','M123','M30','M310','M311','M312','M313',
    'M32','M33','M34','M35','M45','M461','M468','M469') and &is_preexisting then elix_rheuma = 1;
  else elix_rheuma = 0;

  /* Coagulopathy */
  if diag in
    ('D65','D66','D67','D68','D691','D693','D694','D695','D696') and &is_preexisting then elix_coag = 1;
  else elix_coag = 0;

  /* Obesity */
  if diag in
    ('E66') and &is_preexisting then elix_obesity = 1;
  else elix_obesity = 0;

  /* Weight Loss */
  if diag in
    ('E40','E41','E42','E43','E44','E45','E46','R634','R64') and &is_preexisting then elix_wt_loss = 1;
  else elix_wt_loss = 0;

  /* Fluid and Electrolyte Disorders */
  if diag in
    ('E222','E86','E87') and &is_preexisting then elix_fluid_elec = 1;
  else elix_fluid_elec = 0;

  /* Blood Loss Anemia */
  if diag in
    ('D500') and &is_preexisting then elix_anemia_bld_loss = 1;
  else elix_anemia_bld_loss = 0;

  /* Iron Deficiency Anemia */
  if diag in
    ('D508','D509','D51','D52','D53') and &is_preexisting then elix_anemia_iron = 1;
  else elix_anemia_iron = 0;

  /* Alcohol Abuse */
  if diag in
    ('F10','E52','G621','I426','K292','K700','K703','K709','T51','Z502','Z714',
    'Z721') and &is_preexisting then elix_alcohol = 1;
  else elix_alcohol = 0;

  /* Drug Abuse */
  if diag in
    ('F11','F12','F13','F14','F15','F16','F18','F19','Z715','Z722') and &is_preexisting then elix_drugs = 1;
  else elix_drugs = 0;

  /* Psychoses */
  if diag in
    ('F20','F22','F23','F24','F25','F28','F29','F302','F312','F315') and &is_preexisting then elix_phsycho = 1;
  else elix_phsycho = 0;

  /* Depression */
  if diag in
    ('F204','F313','F314','F315','F32','F33','F341','F412','F432') and &is_preexisting then elix_depress = 1;
  else elix_depress = 0;

  /* complication stroke */
  if diag in
    ('G45','G458','G459','G460','G461','G462','G464','I601','I61','I610',
    'I611','I612','I613','I614','I615','I616','I618','I619','I63','I630',
    'I631','I632','I633','I634','I635','I636','I638','I639','I64','I65I653',
    'I658','I659','I66','I660','I661','I662','I663','I664','I668','I669',
    'I67','I670','I691','I693','I694') and &is_complication then complic_stroke = 1;
  else complic_stroke = 0;
run;



/* append icd 9 and icd 10 diagnoses files here */
data Elixhauser_output; set Elix_9; run;
proc append base = Elixhauser_output data = work.Elix_10; run;


Proc sql;
  create table Elix_output_edited as
  select patid, pat_planid,
    max(elix_chf) as elix_chf_1,
    max(elix_arrhy) as elix_arrhy_1,
    max(elix_valvular) as elix_valvular_1,
    max(elix_pcd) as elix_pcd_1,
    max(elix_pvd) as elix_pvd_1,
    max(elix_htn_un_cx) as elix_htn_un_cx_1,
    max(elix_htn_cx) as elix_htn_cx_1,
    max(elix_paraly) as elix_paraly_1,
    max(elix_neurol) as elix_neurol_1,
    max(elix_cpd) as elix_cpd_1,
    max(elix_diab_un_cx) as elix_diab_un_cx_1,
    max(elix_diab_cx) as elix_diab_cx_1,
    max(elix_hypoth) as elix_hypoth_1,
    max(elix_renal_fail) as elix_renal_fail_1,
    max(elix_liver) as elix_liver_1,
    max(elix_peptic) as elix_peptic_1,
    max(elix_aids_hiv) as elix_aids_hiv_1,
    max(elix_lymphoma) as elix_lymphoma_1,
    max(elix_metastatic) as elix_metastatic_1,
    max(elix_solid_tum) as elix_solid_tum_1,
    max(elix_rheuma) as elix_rheuma_1,
    max(elix_coag) as elix_coag_1,
    max(elix_obesity) as elix_obesity_1,
    max(elix_wt_loss) as elix_wt_loss_1,
    max(elix_fluid_elec) as elix_fluid_elec_1,
    max(elix_anemia_bld_loss) as elix_anemia_bld_loss_1,
    max(elix_anemia_iron) as elix_anemia_iron_1,
    max(elix_alcohol) as elix_alcohol_1,
    max(elix_drugs) as elix_drugs_1,
    max(elix_phsycho) as elix_phsycho_1,
    max(elix_depress) as elix_depress_1,
    max(complic_stroke) as complic_stroke
  from Elixhauser_output
  group by patid, pat_planid;

  /* save dataset to library */
  data Tee_lib.Elixhauser_outcomes_coded;
    set work.Elix_output_edited;
  run;
