/***************************************************************************************************************************************************
 *Goal: Create unplanned hospital readmission in 30/90 days after SNF discharge indicators based on CMS unplanned hospital readmission definition.

 *Required raw data: Medicare Part A Claim, Medicare Beneficiary Summary File

 *Last updated on: 12/08/2019
***************************************************************************************************************************************************/

OPTIONS COMPRESS=YES;

/*SET MEASURE VARIABLE  MI HF PN CD SK HW HKC HKR*/
%LET DX = HW;

/*ASSIGN VALUES FOR MAX NUMBER OF DIAGNOSIS AND PROCEDURE CODES*/
* Keep only first 10 diagnosis codes, even for 2011-2015 MedPAR;
%LET DXN = 10;
%LET PRN = 6;

/*ASSIGN FIRST YEAR OF REPORTING PERIOD 12 FOR HW*/
%LET YY = 09;

/*ASSIGN LAST YEAR OF REPORTING PERIOD*/
%LET YYE = 16;

/* NUMBER OF YEARS IN THE REPORTING PERIOD */
%LET NYEAR = 8;

/*END MONTH FOR COVERAGE DATA*/
%LET MM=12;

/* START AND END YEAR */
%LET YEAR=0916;
%LET CONDITION=HW; 

**********************************************************************
* INPUT DATASET NAMES
**********************************************************************;
%LET stay_dataset 		  =stay_dataset;
%LET bene_dataset 		  =bene_dataset;
%LET coverage_dataset 	  =coverage&YYE._&MM.;
%LET pta_in_base_dataset  =pta_in_base_dataset;
%LET pta_in_line_dataset  =pta_in_line_dataset;  /*ONLY USED IN STAY CREATION*/
%LET pta_out_base_dataset =pta_out_base_dataset;
%LET pta_out_line_dataset =pta_out_line_dataset;
%LET ptb_line_dataset 	  =ptb_line_dataset;


***************************************************************
* MACROS FOR HOSPITAL WIDE READMISSION - 2013             	  *
***************************************************************;
/* 1) Revised GLIMMIX by removing Random = _Residual_ line  
   2) Updated Labels for Risk Variables */

%MACRO RETRIEVE_HX(INDEXFILE,  OUT_DIAG);
/* IDENTIFYING ADMISSIONS THAT BELONG TO A TRANSFER BUNDLE AND KEEPING
	ONLY ADMISSIONS BEFORE THE LAST ONE OF A TRANSFER BUNDLE */
PROC SQL;
CREATE TABLE BUNDLE AS
SELECT HICNO, CASEID, HISTORY_CASE, COUNT(HICNO) AS MAXCASE
FROM &INDEXFILE (RENAME=(CASE=CASEID))
GROUP BY HICNO, HISTORY_CASE
HAVING MAXCASE > 1;
QUIT;

DATA BUNDLE;
SET BUNDLE;
HICNO_HXCASE=HICNO||'_'||HISTORY_CASE;
RUN;

PROC SORT DATA=BUNDLE;
BY HICNO_HXCASE CASEID;
RUN;

DATA BUNDLE;
SET BUNDLE;
BY HICNO_HXCASE;
IF LAST.HICNO_HXCASE THEN DELETE;
RUN;

PROC SORT DATA=BUNDLE;
BY HICNO CASEID;
RUN;

PROC SORT DATA=&INDEXFILE out=index;
BY HICNO CASE;
RUN;

/* add e codes from index - jng 2012 */
DATA &OUT_DIAG;
MERGE BUNDLE (IN=A RENAME=(HISTORY_CASE=CASE) DROP=MAXCASE HICNO_HXCASE) 
	index (KEEP=HICNO CASE ADMIT DISCH DIAG1-DIAG10 EDGSCD01-EDGSCD12 EVRSCD01-EVRSCD12 YEAR DVRSND01-DVRSND10  /* changed 06/27/2018 MQ*/
	RENAME=(CASE=CASEID ADMIT=FDATE DISCH=TDATE));
BY HICNO CASEID;
IF A;
diag11 = EDGSCD01; diag12 = EDGSCD02; diag13 = EDGSCD03; diag14 = EDGSCD04;
diag15 = EDGSCD05; diag16 = EDGSCD06; diag17 = EDGSCD07; diag18 = EDGSCD08;
diag19 = EDGSCD09; diag20 = EDGSCD10; diag21 = EDGSCD11; diag22 = EDGSCD12;

attrib diag length=$7.;   /* CHANGE to 7 JNG - 5010 update */
ARRAY ICD(1:22) $ DIAG1-DIAG22; /* for 2011 mm with VA data 1/24/2011 zq - more added 12/11 jng */
ARRAY DVRSND_(1:22) DVRSND01-DVRSND10 EVRSCD01-EVRSCD12;
DO I=1 TO 22;
	IF I=1 THEN DO;
	SOURCE='0.0.1.0';
		DIAG=ICD(I);
		DVRSND = DVRSND_(I);
	OUTPUT;
	END;
	ELSE DO;
	SOURCE='0.0.2.0';
		DIAG=ICD(I);
		DVRSND = DVRSND_(I);
	OUTPUT;
	END;
END;
KEEP HICNO CASE DIAG FDATE TDATE SOURCE YEAR DVRSND;
RUN;
DATA &OUT_DIAG;
SET &OUT_DIAG;
	*IF &CDIAG THEN DIAG='';
	IF DIAG IN ('', ' ') THEN DELETE;
RUN;


%MEND RETRIEVE_HX;



OPTIONS SYMBOLGEN MPRINT;  
*****************************************************************************;
* SPECIFY VARIOUS FILE PATHES, DISEASE CONDITION, AND YEAR                  *;
*****************************************************************************;
*****************************************************************************;
LIBNAME RAW "&PATH1";    *Where raw data is stored;
LIBNAME R "&PATH2";      *Where results are stored;
LIBNAME C "&PATH4";	 *Where AHRQ CCS formats for ICD10 procedure codes are stored;
LIBNAME D "&PATH5";	 *Where CC formats for ICD10 (2015 version) are stored;
LIBNAME E "&PATH6";	 *Where CC formats for ICD9 (2015 version) are stored;
LIBNAME F "&PATH7";	 *Where CCS formats for ICD10 (2015 version) are stored;
LIBNAME G "&PATH8";	 *Where CCS formats for ICD9 (2015 version) are stored;
OPTIONS FMTSEARCH=(C D E F G) SYMBOLGEN MPRINT;

%LET ADMISSION=RAW.index_&CONDITION._&YEAR; 
%LET POST=RAW.POSTINDEX_&CONDITION._&YEAR; 
%LET HXDIAG=RAW.DIAGHISTORY_&CONDITION._&YEAR;
%LET ALL=R.&CONDITION._READM_ALL;
%LET ANALYSIS=R.&CONDITION._READM_ANALYSIS;
%LET HWR_RSRR=R.&CONDITION._READM_RSRR;
%LET RESULTS=R.&CONDITION._RSRR_BS; /* pls note keep this name short; */
%LET EST=R.&CONDITION._READM_EST;

/* Hospital Wide READMISSION MODEL VARIABLES */
%LET MODEL_VAR=AGE_65 MetaCancer SevereCancer OtherCancer Hematological Coagulopathy 
	IronDeficiency LiverDisease PancreaticDisease OnDialysis RenalFailure Transplants
	HxInfection OtherInfectious Septicemia CHF CADCVD Arrhythmias CardioRespiratory 
	COPD LungDisorder Malnutrition MetabolicDisorder Arthritis Diabetes Ulcers
	MotorDisfunction Seizure RespiratorDependence  Alcohol Psychological HipFracture;   

%let TRANS_DIAG=R.DIAGHISTORY_&CONDITION._&YEAR._TRANS;

%RETRIEVE_HX(&ADMISSION,&TRANS_DIAG);

data INDEX;
set &ADMISSION (WHERE=(PARA=1)); 
*Drop variables that we are not going to use to speed up processing;
drop ADMSOUR CCUIND COUNTY DISST   EDBVDETH  GROUP ICUIND LOS MSCD NPI_AT NPI_OP  
     POANCD1-POANCD25 POAEND01-POAEND12 POSTMO POSTMOD POSTMO_A PREMO PROCDT1-PROCDT6 
     TYPEADM UNRELDMG UNRELDTH UPIN_AT UPIN_OP;  
RUN;

proc sql;
create table INDEX_2 as 
select a.dschrgdt as snf_dschrgdt, b.*
from snf_bpt.SNF_Bene_Period_1216_s_37 as a
inner join INDEX as b
on a.BENE_ID=b.HICNO & a.hosp_admsndt>=b.ADMIT & a.hosp_dschrgdt<=b.DISCH;
quit; *7,088,301;

/* ELIMINATE ADMISSIONS THAT APPEAR TWICE (ACROSS YEARS) */
PROC SORT DATA=INDEX_2 NODUPKEY DUPOUT=QA_DupOut EQUALS;
	BY HICNO ADMIT DISCH PROVID;
RUN; 

/* IDENTIFY AND COMBINE TWO ADJACENT AMI ADMISSIONS (disch1=admit2), USE DISCHARGE DATE OF 2ND ADMISSION 
   TO REPLACE DISCHARGE DATE OF 1ST ADMISSION (disch1=disch2), SAME FOR DISCHARGE STATUS, TRANS_FIRST, 
   TRANS_MID, POSTMOD. ALSO, CREATE CASE_P TO BE USED FOR FINDING READMISSION. THIS WORKS WHEN THERE ARE 
   MORE THAN TWO ADJACENT AMI ADMISSIONS. */
DATA TEMP; 
	SET INDEX_2;
	BY HICNO;  
if (admit <= lag(disch) <= disch) and lag(provid)=provid
	and lag(hicno)=hicno and lag(diag1) = diag1
	 then combine0=1;
else combine0=0;
RUN;

/* Revised on 02/17/2017 by Yihao Yuan */
proc sort data=TEMP;
	by hicno DESCENDING admit DESCENDING disch;
run;

data TEMP2 QA_CombOut_mid;
set TEMP;
by hicno;
if (admit <= lag(admit) <= disch) and 
	lag(provid)=provid
	and lag(hicno)=hicno and lag(diag1) = diag1
	then combine=1;
else combine=0;
if combine0 and combine then output QA_CombOut_mid;
else output TEMP2;
run;

data TEMP3 QA_CombOut_last;
set TEMP2;
disch_2=lag(disch);
case_2=lag(case);
ddest_2=lag(ddest);
trans_first_2=lag(trans_first);
trans_mid_2=lag(trans_mid);
postmod_a2=lag(postmod_a);  
 
if lag(provid)=provid and lag(hicno)=hicno and lag(combine0)=1 then do;  
	disch=disch_2;
	case_p=case_2;
	ddest=ddest_2;
	trans_first=trans_first_2;
	trans_mid=trans_mid_2;
	postmod_a=postmod_a2;
end;
else case_p=case;

drop disch_2 case_2 ddest_2 trans_first_2 trans_mid_2 postmod_a2;

if combine0 ^=1 then output TEMP3;
else output QA_CombOut_last;

run;

PROC SORT DATA=TEMP3;
	BY HICNO DESCENDING ADMIT  DESCENDING DISCH PROVID;
RUN;

/* APPLY THE FOLLOWING INCLUSION AND EXCLUSION CRITERIA:
   AGE >=65,   DEAD=0, PREMO=12, POSTMOD=1, 2, 3, TRANS_COMBINE=0, AMA=0 */
DATA ALL; 
SET TEMP3 (DROP=COMBINE0);
BY HICNO;
ATTRIB TRANSFER_OUT LABEL='TRANSFER OUT' LENGTH=3.;
ATTRIB TRANS_COMBINE LABEL='Equals to 1 if it is a combined discharge' LENGTH=3.;
ATTRIB DD30 LABEL='30-DAY MORTALITY FROM DISCHARGE' LENGTH=3.;
ATTRIB AGE_65 LABEL='YEARS OVER 65' LENGTH=3.;
ATTRIB AGE65 LABEL='AGE GE 65' LENGTH=3.;
ATTRIB DEAD LABEL='IN HOSPITAL DEATH' LENGTH=3.;
ATTRIB sample LABEL='MEET INC & EXL CRITERIA' LENGTH=3.;
LENGTH ADDXG $7. DCGDIAG $7. proccc1-proccc6 $7. proc1 - proc6 $7. rehabexcl 3.;

array ahrqcc{6} $ proccc1 - proccc6;
array surg{10} ophtho vascular ortho gen ct uro neuro obgyn plastic ent;

DCGDIAG = diag1;

if DVRSND01='0' then do;
	ADDXG = PUT(DCGDIAG,$CCS10CD.);
if addxg = "" then delete;
	 proccc1 = put(proc1,$ICD10_PRCCS.);
	 proccc2 = put(proc2,$ICD10_PRCCS.); 
	 proccc3 = put(proc3,$ICD10_PRCCS.);  
	 proccc4 = put(proc4,$ICD10_PRCCS.); 
	 proccc5 = put(proc5,$ICD10_PRCCS.); 
	 proccc6 = put(proc6,$ICD10_PRCCS.); 

rehabexcl=0;
if addxg in ('254') then rehabexcl=1; /* excluding rehab ccs*/

******PSYCH CODES as Exclusion****************;
psychexcl = 0;   
if addxg in ('650', '651', '652','654', '655', '656', '657', '658', '659', '662', '670') then psychexcl = 1;

***********Subset Surgeries into Catergories - not used for reporting
	*******  Remove CCS 61 from Surgical Cohort   *****;
do i =1 to 6;
	if ahrqcc(i) in ('20', '15', '16', '14', '13', '17') then ophtho=1;
	if ahrqcc(i) in ('51', '55', '52', '60', '59', '56', '53') then vascular=1;
	if ahrqcc(i) in ('153', '146', '152', '158', '3', '161', '142', '147', 
		'162', '148', '154', '145', '150' /*'151', '143'*/) then ortho=1;  ****REMOVE 143, 151 FOR ICD-10***;
	if ahrqcc(i) in ('78', '157', '84', '90', '96', '75', '86', '105', '72', 
		'80', '73', '85', '164', '74', '167', '176', '89', '166', '99', '94',
		'67', '66', '79') then gen=1;
	if ahrqcc(i) in ('44', '43', '49', '36', '42') then ct=1;
	if ahrqcc(i) in ('101', '112', '103', '104', '114', '118', '113', '106','109') then uro=1;**ADD 109 FOR ICD-10; 
	if ahrqcc(i) in ('1', '9', '2') then neuro=1;
	if ahrqcc(i) in ('172', '175', '28', '144', '160') then plastic=1;
	if ahrqcc(i) in ('33', '10', '12', '26', '21', '23','30','24', '22') then ent=1; 
		if ahrqcc(i) in ('124', '119', '132', '129', '134','139', '137', '125','131', '120', 
                         '123', /*'140',*/ '136', '127', '135', '121', '141', '126', '133','122') then obgyn=1;  * REMOVE 140 FOR ICD-10;
end;

do j=1 to 10;
	if surg(j)=. then surg(j)=0;
end;
surg_sum=sum(ophtho, vascular, ortho, gen, ct, uro, neuro, obgyn, plastic, ent);

attrib category length=$10.;
if ophtho or vascular or ortho or gen or ct or uro or neuro or plastic or ent or obgyn then
category='Surgical';
else category='Medical';

attrib subcategory length=$18.;

	if addxg in ('11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', 
				 '23', '24', '29', '30', '31', '32', '33', '34', '35', '36', '37', '38', '39', '40', 
				 '41', '42', '43', '44', '45','25','26','27','28') then subcategory='Cancer';
	else if addxg in ('56','103', '108', '122', '125', '127', '128', '131') then subcategory='Cardiorespiratory';
	else if addxg in ('96', '97', '100', '101', '102', '104', '105', '106', '107', '114', '115', '116', '117', '213') then subcategory='CV';
	else if addxg in ('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '47', '48', '49', 
					  '50', '51', '52', '53',	'54', '55', '57', '58', '59', '60', '61', '62', '63', 
					  '64', '76', '77', '84', '86', '87', '88', '89',	'90', '91', '92', '93', '94', '98', 
					  '99', '118', '119', '120', '121', '123', '124', '126', '129',
					  '130', '132', '133', '134', '135', '136', '137', '138', '139', '140', '141', '142', 
					  '143', '144', '145', '146', '147', '148', '149', '151', '152', '153', '154', '155', 
					  '156', '157', '158', '159', '160', '161', '162', '163', '164', '165', '166', '167', 
					  '173', '175', '197', '198', '199', '200', '201', '202',	'203', '204', '205', '206',
					  '207', '208', '209', '210', '211', '212', '214', '215', '217', '225', '226',
					  '228', '229','230', '231', '232', '234', '235', '236', '237', '238', '239', '240', 
					  '242', '243', '244', '245', '246', '247', '248', '249', '250', '251', '252', '253', 
					  '255', '256', '257', '258', '259',  '241', '168','170','172','46','171','169','174',
				      '653','661','660','663','2617' ) then subcategory='Medicine';    ****ADD 2617 TO MEDICAL COHORT IN ICD-10;   
	else if addxg in ('78', '79', '80', '81', '82', '83', '85', '95', '109', '110', '111', 
	                  '112', '113', '216', '227', '233') then subcategory='Neurology';
end;

if DVRSND01='9' then do;

 ADDXG = PUT(DCGDIAG,$CCS.);
 if addxg = "" then delete;
 proccc1 = put(proc1,$ccsproc.);
 proccc2 = put(proc2,$ccsproc.); 
 proccc3 = put(proc3,$ccsproc.);  
 proccc4 = put(proc4,$ccsproc.); 
 proccc5 = put(proc5,$ccsproc.); 
 proccc6 = put(proc6,$ccsproc.); 
************************************************************************************************;

rehabexcl=0;
if addxg in ('254') then rehabexcl=1; /* excluding rehab ccs*/

******PSYCH CODES as Exclusion****************;
psychexcl = 0;   
if addxg in ('650', '651', '652','654', '655', '656', '657', '658', '659', 
	'662',  '664', '665', '666','667','668','669','670') then psychexcl = 1;

***********Subset Surgeries into Catergories - not used for reporting
	*******  Remove CCS 61 from Surgical Cohort   *****;
do i =1 to 6;
	if ahrqcc(i) in ('20', '15', '16', '14', '13', '17') then ophtho=1;
	if ahrqcc(i) in ('51', '55', '52', '60', '59', '56', '53') then vascular=1;
	if ahrqcc(i) in ('153', '146', '152', '158', '3', '161', '142', '147', 
		'162', '148', '154', '145', '150', '151', '143') then ortho=1;
	if ahrqcc(i) in ('78', '157', '84', '90', '96', '75', '86', '105', '72', 
		'80', '73', '85', '164', '74', '167', '176', '89', '166', '99', '94',
		'67', '66', '79') then gen=1;
	if ahrqcc(i) in ('44', '43', '49', '36', '42') then ct=1;
	if ahrqcc(i) in ('101', '112', '103', '104', '114', '118', '113', '106') then uro=1;
	if ahrqcc(i) in ('1', '9', '2') then neuro=1;
	if ahrqcc(i) in ('172', '175', '28', '144', '160') then plastic=1;
	if ahrqcc(i) in ('33', '10', '12', '26', '21', '23','30','24', '22') then ent=1; 
		if ahrqcc(i) in ('124', '119', '132', '129', '134','139', '137', '125',
		'131', '120', '123', '140', '136', '127', '135', '121', '141', '126', '133',
		'122') then obgyn=1;
end;

do j=1 to 10;
	if surg(j)=. then surg(j)=0;
end;
surg_sum=sum(ophtho, vascular, ortho, gen, ct, uro, neuro, obgyn, plastic, ent);
attrib category length=$10.;


if ophtho or vascular or ortho or gen or ct or uro or neuro or plastic or ent or obgyn then
	category='Surgical';
else category='Medical';
*********************************;
attrib subcategory length=$18.;

************************* Put CF (ccs 56) in CR cohort instead of Med   11/6/12  ****************************;
	if addxg in ('11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', 
		         '23', '24', '29', '30', '31', '32', '33', '34', '35', '36', '37', '38', '39', '40', 
		         '41', '42', '43', '44', '45','25','26','27','28') then subcategory='Cancer';
	else if addxg in ('56','103', '108', '122', '125', '127', '128', '131') then subcategory='Cardiorespiratory';
	else if addxg in ('96', '97', '100', '101', '102', '104', '105','106', '107', '114', '115', '116', '117', '213') then subcategory='CV';                
	else if addxg in ('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '47', '48', '49', 
					  '50', '51', '52', '53',	'54', '55', '57', '58', '59', '60', '61', '62', '63', 
					  '64', '76', '77', '84', '86', '87', '88', '89',	'90', '91', '92', '93', '94', '98', 
					  '99', '118', '119', '120', '121', '123', '124', '126', '129',
					  '130', '132', '133', '134', '135', '136', '137', '138', '139', '140', '141', '142', 
					  '143', '144', '145', '146', '147', '148', '149', '151', '152', '153', '154', '155', 
					  '156', '157', '158', '159', '160', '161', '162', '163', '164', '165', '166', '167', 
					  '173', '175', '197', '198', '199', '200', '201', '202',	'203', '204', '205', '206',
					  '207', '208', '209', '210', '211', '212', '214', '215', '217', '225', '226',
					  '228', '229','230', '231', '232', '234', '235', '236', '237', '238', '239', '240', 
					  '242', '243', '244', '245', '246', '247', '248', '249', '250', '251', '252', '253', 
					  '255', '256', '257', '258', '259',  '241', '168','170','172','46','171','169','174',
				      '653','661','660','663') then subcategory='Medicine';    
	else if addxg in ('78', '79', '80', '81', '82', '83', '85', '95', '109', '110', '111', '112', '113', '216', '227', '233') then subcategory='Neurology';
end;

if category='Surgical' then subcategory='Surgical';

******************************************************************************************************************;
**** exclusion of PPS-Exempt cancer hopitals  *******************;
if provid in ('050146','050660','100079','100271','220162','330354','360242','390196','450076','330154','500138') 
then cancer_hosp=1;
else cancer_hosp = 0;
******************************************************************************************************************;
all = 1;
  
if rehabexcl=1 then subcategory =('Rehab excl');
if psychexcl=1 then subcategory =('Psych excl');

/* TRANSFER OUT INDICATOR, SAME PTS, DIFFERENT HOSP, 0 OR 1 DAY IN BETWEEN, NOT V CODE VISIT */
TRANSFER_OUT=(ADMIT <= LAG(ADMIT) <= DISCH +1) AND (HICNO=LAG(HICNO)) AND (PROVID ^=LAG(PROVID));

/* add post_flag to account for possible transfer that is outside of study period. 
	1/11/2010. ZQ */

TRANS_COMBINE=(TRANSFER_OUT OR TRANS_FIRST OR TRANS_MID or post_flag); 

MALE=(SEX='1');
AGE=INT((ADMIT - BIRTH)/365.25);
AGE65=(AGE >=65);
AGE_65=AGE - 65;

DEAD=(ddest=20);
AMA=(ddest=7);

IF DEATH ^=. THEN DO;
	IF 0 < (DEATH - DISCH) <=30 THEN DD30=1;
	ELSE IF (DEATH - DISCH) > 30 THEN DD30=0;
	ELSE IF (DEATH - DISCH) <= 0 THEN DD30=0;
	END;
ELSE DD30=0;
obs30_mort=dd30;

/*IF (DISCH - ADMIT) = 0 THEN SameDayDischarge=1;	  ***NOT USED IN HWR Exclusions;
ELSE SameDayDischarge=0;*/

PRIOR12=(PREMO_A=12);    **PREMO_A is PART A enrollment only;

IF DD30=1 THEN POSTMOD_A=1;   *If pt dies in 30day period they are eligible;
POST1=(POSTMOD_A=1);


/* INCLUSION CRITERIA: FFS,AGE GE 65,IN HOSPITAL DEATH,WITHOUT >= 1 MOTNH POST,
   EXCLUSION  CRITERIA: TRANSFER OUT, WITH 12-MONTH HISTORY,PPS Cancer Hosp, Rehab,
                        CANCER Medical,AMA, psych removed pre-dryrun-PUT IN AGAIN */
if dead=0 and age65 and post1=1 and trans_combine=0 and PRIOR12=1 and ama = 0 and 
  rehabexcl=0 and cancer_hosp = 0  and subcategory not in ('Cancer') and psychexcl=0
/* SAMPLE is used in PAC Get after discharge info.sas */
then sample = 1; else sample=0 ;
RUN;

PROC SORT DATA=ALL; BY HICNO CASE_P; RUN; 
 
data postindex;
set &post;
length i j k 3.;

***** determine the Proc CCS group each procedure falls into ******;
ATTRIB procccp_1-procccp_6  LENGTH=$3.;
ARRAY procccp_ (1:6) procccp_1-procccp_6;
ARRAY procccsp_(1:6) $  PROC1 - PROC6;
ARRAY PVSNCD_(1:10) $ PVSNCD01-PVSNCD10;
ARRAY DVRSND_(1:10) $ DVRSND01-DVRSND10;

***** ASSIGN PROC CCS TO PROCEDURES  **********;
DO k=1 TO 6;
	if PVSNCD_(K)= "0" then do;
		procccp_(k) = put(procccsp_(k),$ICD10_PRCCS.); 
	end;
	else if PVSNCD_(K)= "9" then do;
		procccp_(k) = put(procccsp_(k),$ccsproc.);
	end; 
end;

****** Categorize the CCS Diagnosis Claims for the potential readmissions *******;
DCGDIAG = diag1;

if DVRSND01='0' then do;
	ADDXG_p = PUT(DCGDIAG,$CCS10CD.);

	*****THIS SECTION UPDATED WITH FINAL PLANNED ALGORITHM *****************;

	***** Create a variable for the AHRQ CCS acute diagnosis based exclusions for planned ****;
	***** Some diagnosis groups are split by ICD-9 diagnosis codes                        ****;
	** added on 11/2 Version 2.1: 
	 CCS 129 to acute list, CCS 224 and 170 to planned list , remove diagnosis codes 410.x2
	 from acute list CCS 100

	REVISED VERSION 3.0: ADD a split for Biliary tract disease  9/2013  add Acute Pancreatitis and HTN w/ Comp
	******************************************************************************************;

	if ADDXG_p in ('1','2','3','4','5','7','8','9','54','55','60','61','63','76','77','78','82'
	,'83','84','85','87','89','90','91','92','93', '102','104','107','109','112',
	'116','118','120','122','123','99',
	'124','125','126','127','128','129','130','131','135',
	'137','139','140','142','145','146','148',
	'153','154','157','159','165','168','172','197','198','225','226','227','228','229','230',
	'232','233','234','235','237','238','239','240','241','242','243','244','245','246','247',
	'249','250','251','252','253','259','650','651','652','653','656','658','660','661','662','663','670')
	OR 
	(addxg_p in ('97') and  diag1 in 
	('A3681','A3950','A3953','A3951','A3952','B3320','B3323','B3321','B3322','B376',
	'I32','I39','B5881','I010','I011','I012','I018','I019','I020','I090',
	'I099','I0989','I309','I301','I300','I308','I330','I339','I41','I409',
	'I400','I401','I408','I312','I310','I311','I314','I514')) 
	OR
	(addxg_p in ('105') and  diag1 in
	('I442','I4430','I440','I441','I4469','I444','I445','I4460','I447','I4510',
	'I450','I4519','I4439','I454','I452','I453','I455','I456','I4581','I459'))
	OR
	(addxg_p in ('106') and  diag1 in
	('I479','R000','I498','R001','I499','I4949','I493'))
	OR
	(addxg_p in ('108') and  diag1 in 
	('I0981','I509','I501','I5020','I5021','I5023','I5030','I5031','I5033','I5040','I5041','I5043','I509')) 
	OR
	( addxg_p in ('100') and  (DIAG1 in ('I2109','I2101','I2102','I2119','I2111','I214','I2129','I213','I2121' )))
	OR
	( addxg_p in ('149') and diag1 in ('K8000','K8012','K8001','K8013','K8042','K8046','K8043','K8047','K8062','K8063',
	'K8066','K8067','K810','K812','K830','K8030','K8031','K8032','K8033','K8036','K8037')) 
	OR
	( addxg_p in ('152') and diag1 in ('K859','K850','K851','K852','K853','K858')) 
	then excldx = 1; else excldx = 0;
end;

if DVRSND01='9' then do;
	ADDXG_p = PUT(DCGDIAG,$CCS.);
	if ADDXG_p in ('1','2','3','4','5','7','8','9','54','55','60','61','63','76','77','78','82'
	,'83','84','85','87','89','90','91','92','93', '102','104','107','109','112',
	'116','118','120','122','123','99',
	'124','125','126','127','128','129','130','131','135',
	'137','139','140','142','145','146','148',
	'153','154','157','159','165','168','172','197','198','225','226','227','228','229','230',
	'232','233','234','235','237','238','239','240','241','242','243','244','245','246','247',
	'249','250','251','252','253','259','650','651','652','653','656','658','660','661','662','663','670')
	OR
	( addxg_p in ('105','106') and  diag1
	 in ('4260','42610','42611','42612','42613','4262',
	'4263','4264','42650','42651','42652','42653','42654','4266','4267','42681','42682',
	'4269','4272','7850','42789','4279','42769') )
	OR
	(addxg_p in ('97') and  diag1 in 
	('03282','03640','03641','03642','03643','07420','07421','07422','07423',
	'11281','11503','11504','11513','11514','11593','11594',
	'1303','3910','3911','3912','3918','3919','3920','3980',
	'39890','39899','4200','42090','42091','42099','4210','4211',
	'4219','4220','42290','42291','42292','42293','42299','4230',
	'4231','4232','4233','4290'))
	OR
	(addxg_p in ('108') and  diag1 in 
	('39891','4280','4281','42820','42821','42823','42830','42831',
	'42833','42840','42841','42843','4289')) 
	OR
	( addxg_p in ('100') and  (DIAG1=:'410' AND SUBSTR(DIAG1, 5, 1)^='2'))
	OR
	( addxg_p in ('149') and diag1 in ('5740','57400','57401','5743','57430','57431',
	 '5746','57460','57461','5748','57480','57481','5750','57512','5761')) 
	OR
	( addxg_p in ('152') and diag1 in ('5770')) 
	then excldx = 1; else excldx = 0;
end; 

ARRAY PROCCS(6) $  PROCCCP_1 - PROCCCP_6;
planned_1 = 0; planned_2=0;

****CREATE ALWAYS PLANNED PROCEDURE VARIABLE*******;
DO I=1 TO 6;
	IF proccs(I) IN 
	('64','105','176','134','135')THEN do; 
	   proc_2  = proccs(I);
	   planned_2 = 1; 
	end;
end;

***Determine if Planned Procedure Occurred:  Version 3.0 REVISED SEP 2013
REMOVE 211 and 224 per valdiation results  ****; 
DO I=1 TO 6;  
	IF proccs(I) IN 
	('1','3','5','9','10','12','33','36','38','40','43','44','45','47','48','49',
	 '51','52','53','55','56','59','62','66','67','74','78','79','84','85','86','99',
     '104','106','107','109','112','113','114','119','120','124','129',
     '132','142','152','153','154','157','158','159',
	 '166','167','169','172','175','170')
	then do;
	   procnum  = proccs(I);
	   planned_1 = 1; 
	end;
end;

**********ADD ICD_10_CM Proc code level Planned Procedures *****;
ARRAY pproc(6) $   PROC1 -  PROC6;

DO J=1 TO 6;
	if PVSNCD_(I)='0' then do;
		if  pproc(J) in ('GZB4ZZZ','GZB0ZZZ','GZB1ZZZ','GZB2ZZZ','GZB3ZZZ') then do;
			procnum  = '990';
			planned_1 = 1; 
		end;
		if  pproc(J) in ('0CBS4ZZ','0CBS7ZZ','0CBS8ZZ', '0BW10FZ', '0BW13FZ', '0BW14FZ',
		                 '0B5N0ZZ','0B5N3ZZ','0B5N4ZZ','0B5P0ZZ','0B5P3ZZ','0B5P4ZZ') then do;
			procnum  = '991';
			planned_1 = 1; 
		end;
		if  pproc(J) in ('0TC03ZZ','0TC04ZZ','0TC13ZZ','0TC14ZZ','0TC33ZZ','0TC34ZZ','0TC43ZZ','0TC44ZZ','0T9030Z','0T9130Z') then do;
			procnum  = '992';
			planned_1 = 1; 
		end;
	end;
end;

DO J=1 TO 6;
	IF PVSNCD_(J)= "9" THEN DO;
		if  pproc(J) in ('9426','9427') then do;
			procnum  = '990';
			planned_1 = 1; 
		end;
		if  pproc(J) in ('304','3174','346','301','3029','303') then do;
			procnum  = '991';
			planned_1 = 1; 
		end;
		if  pproc(J) in ('5503','5504') then do;
			procnum  = '992';
			planned_1 = 1; 
		end;
		if  pproc(J) in ('3818') then do;
			procnum  = '993';
			planned_1 = 1; 
		end;
	end;
end;

/*
if  pproc(J) in ('04CK0ZZ','04CK3ZZ','04CK4ZZ','04CL0ZZ','04CL3ZZ','04CL4ZZ','04CM0ZZ','04CM3ZZ','04CM4ZZ','04CN0ZZ','04CN3ZZ','04CN4ZZ','04CP0ZZ','04CP3ZZ','04CP4ZZ',
                 '04CQ0ZZ','04CQ3ZZ','04CQ4ZZ','04CR0ZZ', '04CR3ZZ','04CR4ZZ','04CS0ZZ','04CS3ZZ','04CS4ZZ','04CT0ZZ','04CT3ZZ','04CT4ZZ','04CU0ZZ','04CU3ZZ','04CU4ZZ',
                 '04CV0ZZ','04CV3ZZ','04CV4ZZ','04CW0ZZ','04CW3ZZ','04CW4ZZ','04CY0ZZ','04CY3ZZ','04CY4ZZ') then do;
*procnum  = '993';
planned_1 = 1; 
*/

planned = 0;

/*step1: Always Planned Procedures*/
if planned_2 = 1 then  planned = 1; *procnum = proc_2;

/*step2: Always Planned Diagnoses*/ ****** Maintenance Chemo Therapy  ******;  /****** Rehabilitation Therapy  ******;*/
else if ADDXG_p = '45' then planned = 1;  * procnum = '999' ;                        

else if ADDXG_p = '254' then planned = 1;    *procnum = '998' ;                      

else if ADDXG_p = '194' then do;
planned = 1;   procnum = '997' ;                        
end;
else if ADDXG_p = '196' then do;
planned = 1;   procnum = '996' ;                      
end;

/*step3: All Other Planned */
else if planned_1 =1 and excldx = 1 then planned = 0;
else if planned_1 =1  and excldx = 0 then planned = 1;

run;

proc sort data=postindex; by hicno case admit; run;

data readm1 QA_DupIndex; 
merge ALL (IN=A)
	  postindex (IN=B KEEP=HICNO ADMIT DISCH PROVID DIAG1 PROCCCP_1 - PROCCCP_6 CASE planned proc1-proc6 DVRSND01
				    RENAME=(DIAG1=_DIAG1 ADMIT=_ADMIT DISCH=_DISCH PROVID=_PROVID proc1=_proc1	proc2=_proc2
                            proc3=_proc3 proc4=_proc4 proc5=_proc5 proc6=_proc6	CASE=CASE_P DVRSND01=_DVRSND01));        
by HICNO CASE_P;
IF A;


IF NOT B THEN RADM90SNFALL=0;
ELSE IF 0 <= _ADMIT - snf_dschrgdt <=90 THEN RADM90SNFALL=1;
ELSE IF _ADMIT - snf_dschrgdt > 90 THEN RADM90SNFALL=0;

INTERVAL=_ADMIT - snf_dschrgdt;
SAME=(PROVID=_PROVID); /* SAME HOSPITAL READMISSION */
RADM90SNF=RADM90SNFALL;

RADM90SNFp=0;
if planned =1 and RADM90SNF=1 then do;
RADM90SNF = 0;
RADM90SNFp = 1;
end;

if _DVRSND01='0' then do;
	/* any readmission with principal diagnosis eq V57  is not counted as readmission, added 1/11/2010. ZQ */
	if upcase(_diag1) in ('Z5189')  then Radm_rehab=1;
	else Radm_rehab=0;

	/* any readmission with psych principal diagnosis eq in range of 290-319 that was within 1 day of the discharge date of index admission with discharge dispostion
		eq 65 is not counted as readmission, added 1/11/2010. ZQ */ 

	/*(1) the admission being evaluated as a potential readmission has a psychiatric principal discharge diagnosis code (ICD-9-CM codes beginning with ‘29’, ‘30’ or ‘31’, 
	                 for discharges prior to October 1, 2015, or ICD-10-CM codes beginning with ‘F’, for discharges on or after October 1, 2015),  added 05/31/2018. MQ*/
	if _diag1=:'F' and (interval in (0,1))  and
		ddest=65 then Radm_psy=1;
	else Radm_psy=0; 
end;

if _DVRSND01='9' then do;
	if upcase(_diag1)=:'V57'  then Radm_rehab=1;
	else Radm_rehab=0;
	if (_diag1=:'29' or _diag1=:'30' or _diag1=:'31') and (interval in (0,1))  and
		ddest=65 then Radm_psy=1;
	else Radm_psy=0;
end;

****** WILL NOT INCLUDE ANY REHAB OR POSSIBLE PSYCH TRANSFERS IN OUR PLANNED READMISSION LOGIC 12/17/12 ****;
if radm_rehab=1 and (RADM90SNF=1 or RADM90SNFp = 1) then do; RADM90SNF=0; RADM90SNFp = 0; interval = 999;  end;
if radm_psy=1 and (RADM90SNF=1 or RADM90SNFp = 1) then do; RADM90SNF=0; RADM90SNFp = 0; interval = 999;	end;

hicno_case=strip(hicno)||strip(case_p);

* PART OF TRANS BUNDLE, SHOULD HAVE BEEN EXCLUDED - *Updated on 06/19/18;
If RADM90SNF=1 and interval=0 and same=1 and diag1=_diag1 then do;
Bundle=1;
Sample=0;
end;

DROP I;

IF ADMIT=_ADMIT AND DISCH=_DISCH AND PROVID=_PROVID AND DIAG1=_DIAG1 THEN OUTPUT QA_DupIndex; 
ELSE OUTPUT readm1;

run; 

proc sort data=readm1; by  hicno_case interval _admit; run;

data snf_bpt.readm90_after_snf(drop=hicno_case);
set readm1;
by  hicno_case interval _admit;
if first.hicno_case;
if 0<=INTERVAL<=30 & RADM90SNF=1 then RADM30SNF=1; else RADM30SNF=0;
label RADM30SNF = 'Eligible Readmission in 30 Days after SNF Discharge'
      RADM90SNF = 'Eligible Readmission in 90 Days after SNF Discharge';
run; 

data temp;
set snf_bpt.readm90_after_snf;
if 0<=INTERVAL<=30 & RADM90SNF=1 then RADM30SNF=1; else RADM30SNF=0;
if RADM90SNF=. then RADM30SNF=.;
run;

data snf_bpt.readm90_after_snf;
set temp;
run;


