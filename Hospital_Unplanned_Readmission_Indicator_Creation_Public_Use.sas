**********************************************************************************
* Name: Hospital Wide 30-Day Readmission Measure Using Medicare Claims Data      *
* Creator: YALE/YNHH CORE                                                        *
* Modifier: Mingyu Qi                                                            *
* Last updated on: last revised on 12/08/2019                 						        
**********************************************************************************;

**********************************************************************************
* PART I: Build index hospital stay data set, define transfer chains, check for  *
*         Medicare coverage history and build post-index hospital stay data set	 *
**********************************************************************************;
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
**********************************************************************;

/*/*************************************************************************************************************/
/*                               SAS Program Description*/
/**/
/*PROGRAM NAME: 00_build_stay_dataset*/
/**/
/*FOR USE WITH: ALL MEASURES*/
/**/
/*PURPOSE: Build Stay level dataset */
/**/
/*OVERVIEW:     */
/**/
/*INPUT DATA: */
/*	data_pta.&pta_in_base_dataset.*/
/*	data_pta.&pta_in_line_dataset.*/
/**/
/*OUTPUT FILES: */
/*	data_sty.&stay_dataset.*/
/**/
/*************************************************************************************************************/
data data_sty.stay_dataset;
	set medpar.mp100mod_2009 medpar.mp100mod_2010 medpar.mp100mod_2011 medpar.mp100mod_2012 medpar.mp100mod_2013 medpar.mp100mod_2014 medpar.mp100mod_2015 medpar.mp100mod_2016
	(drop=EHR_PYMT_ADJSTMT_AMT PPS_STD_VAL_PYMT_AMT FINL_STD_AMT IPPS_FLEX_PYMT_7_AMT PTNT_ADD_ON_PYMT_AMT HAC_PGM_RDCTN_IND_SW PA_IND_CD UNIQ_TRKNG_NUM STAY_2_IND_SW);

	ADMIT=ADMSNDT;
	CASE_TYPE="CMS";
	HSE_UNIQUE_ID=MEDPAR_ID;
	MDCL_REC=BENE_ID;
	NOBS = _N_;
	NPI_AT=.;
	NPI_OP=.;
	pstate_alpha=state_cd;
	REC1STAY=1;
	UPIN_AT=.;
	UPIN_OP=.;
	
	/* dstntncd is the discharge destination code variable.
	02: short-term general hospital for inpatient care; 
	66: critical access hospital;
	20: expired
	*/
	if dstntncd in('02','66') then TXFLAG=-1; else
	if dstntncd='20' then TXFLAG=1; else TXFLAG=0;
	
* Chang the initial variable name when inputing different dataset *;	
	rename
	SRC_ADMS=ADMSOUR
	MEDPAR_ID=BENE_CLM_NUM
	CRNRY_CD=CCUIND
	ADMSNDT=CLM_ADMSN_DT
	DSTNTNCD=DDEST
	DGNS_CD01-DGNS_CD25=DIAG1-DIAG25
	DGNSCNT=DIAG_COUNT
	DSCHRGDT=DISCH
	DSCHRGCD=DISST
	DRG_CD=DRGCD
	DGNS_VRSN_CD_1-DGNS_VRSN_CD_25=DVRSND01-DVRSND25
	DGNS_E_1_CD=EDGSCD01
	DGNS_E_2_CD=EDGSCD02
	DGNS_E_3_CD=EDGSCD03
	DGNS_E_4_CD=EDGSCD04
	DGNS_E_5_CD=EDGSCD05
	DGNS_E_6_CD=EDGSCD06
	DGNS_E_7_CD=EDGSCD07
	DGNS_E_8_CD=EDGSCD08
	DGNS_E_9_CD=EDGSCD09
	DGNS_E_10_CD=EDGSCD10
	DGNS_E_11_CD=EDGSCD11
	DGNS_E_12_CD=EDGSCD12
	DGNS_E_VRSN_CD_1-DGNS_E_VRSN_CD_12=EVRSCD01-EVRSCD12
	BENE_ID=HICNO
	ICUINDCD=ICUIND
	LOSCNT=LOS
	MS_CD=MSCD
	CLM_TYPE=NCH_CLM_TYPE_CD
	POA_DGNS_E_1_IND_CD=POAEND01
	POA_DGNS_E_2_IND_CD=POAEND02
	POA_DGNS_E_3_IND_CD=POAEND03
	POA_DGNS_E_4_IND_CD=POAEND04
	POA_DGNS_E_5_IND_CD=POAEND05
	POA_DGNS_E_6_IND_CD=POAEND06
	POA_DGNS_E_7_IND_CD=POAEND07
	POA_DGNS_E_8_IND_CD=POAEND08
	POA_DGNS_E_9_IND_CD=POAEND09
	POA_DGNS_E_10_IND_CD=POAEND10
	POA_DGNS_E_11_IND_CD=POAEND11
	POA_DGNS_E_12_IND_CD=POAEND12
	
	POA_DGNS_1_IND_CD=POANCD1
	POA_DGNS_2_IND_CD=POANCD2
	POA_DGNS_3_IND_CD=POANCD3
	POA_DGNS_4_IND_CD=POANCD4
	POA_DGNS_5_IND_CD=POANCD5
	POA_DGNS_6_IND_CD=POANCD6
	POA_DGNS_7_IND_CD=POANCD7
	POA_DGNS_8_IND_CD=POANCD8
	POA_DGNS_9_IND_CD=POANCD9
	POA_DGNS_10_IND_CD=POANCD10
	POA_DGNS_11_IND_CD=POANCD11
	POA_DGNS_12_IND_CD=POANCD12
	POA_DGNS_13_IND_CD=POANCD13
	POA_DGNS_14_IND_CD=POANCD14
	POA_DGNS_15_IND_CD=POANCD15
	POA_DGNS_16_IND_CD=POANCD16
	POA_DGNS_17_IND_CD=POANCD17
	POA_DGNS_18_IND_CD=POANCD18
	POA_DGNS_19_IND_CD=POANCD19
	POA_DGNS_20_IND_CD=POANCD20
	POA_DGNS_21_IND_CD=POANCD21
	POA_DGNS_22_IND_CD=POANCD22
	POA_DGNS_23_IND_CD=POANCD23
	POA_DGNS_24_IND_CD=POANCD24
	POA_DGNS_25_IND_CD=POANCD25
	
	prcdr_cd1-prcdr_cd25=proc1-proc25
	prcdr_dt1-prcdr_dt25=procdt1-procdt25
	prvdr_num=provid
	state_cd=pstate
	srgcl_prcdr_vrsn_cd_1-srgcl_prcdr_vrsn_cd_25=pvsncd01-pvsncd25
	TYPE_ADM=TYPEADM;	
	
	/* KEEP INPATIENT SHORT-STAYS ONLY; */
	IF (substr(PRVDR_NUM,3,1) in ('0','M','R','S','T') or substr(prvdr_num,3,2)<="13") & SPCLUNIT not in ('M','R','S','T') then output;
run;

*Replace missing diagnosis version code for 2009 and 2010 MedPAR data;
data data_sty.stay_dataset;
set  data_sty.stay_dataset;
array DVCD{25} DVRSND01-DVRSND25;
array DIAG{25} DIAG1-DIAG25;
array EDCD{12} EDGSCD01-EDGSCD12;
array EVCD{12} EVRSCD01-EVRSCD12;

do I=1 to 25;
	if DIAG{I}^=' ' and DVCD{I}=' ' then DVCD{I}='9';
end;

do I=1 to 12;
	if EDCD{I}^=' ' and EVCD{I}=' ' then EVCD{I}='9';
end;
run;

proc sort data=data_sty.stay_dataset;
	by HICNO ADMIT DISCH TXFLAG PROVID;
run;
	
data data_sty.stay_dataset;
  set data_sty.stay_dataset (DROP=DIAG11-DIAG25 proc7-proc25 procdt7-procdt25);
  nobs + 1;
run;


/************************************************************************************************************
                               SAS Program Description

PROGRAM NAME: 0A_create_flag_transfers_dataset

PURPOSE:      Prep data for transfer check.  To be used for all measures using the same source data. 

OVERVIEW: 	  identify all transfer stays in stay dataset.  

INPUT DATA: 
	data_sty.&stay_dataset.  *PRE-SORTED BY HICNO ADMIT DISCH TXFLAG PROVID  -WITH nobs created after sort

OUTPUT FILES: 
	data_an.flag_transfers

************************************************************************************************************/
/* FLAG CLAIMS THAT MAKE UP A TRANSFER CHAIN */
DATA flag_transfers
	(KEEP=hicno trans_caseno newobs counts newdiag1 newdisch newadmit newprovid newtxflag
	 RENAME=(newobs=nobs newdiag1 = diag1 newdisch = disch newadmit = admit 
			 newprovid = provid newtxflag = txflag));

	/*CREATE LAGGED VARIABLES*/
	LENGTH mark $1 hicno2 $15 prvid2 $6 diag1_2 $7;
	RETAIN mark '';

	/*ASSIGN FIELDS FROM PRIOR RECORD TO 2 VARIABLES*/
	prvid2=provid;
	hicno2=hicno;
	admdate2=admit;
	disdate2=disch;
	nobs2=nobs;
	txflag2=txflag;
	diag1_2 = diag1;

	SET data_sty.stay_dataset (KEEP=hicno admit disch nobs txflag provid diag1 DVRSND01 case_type
							   WHERE=(case_type='CMS'));

	/*SELECT TRANSFER CLAIMS: DIFFERENT PROVIDER, SAME OR NEXT DAY ADMIT AFTER DISCHARGE AND TXFLAG OF 0 OR -1*/
	IF hicno2 = hicno AND prvid2 NE provid AND (disdate2 <= admit <= disdate2+1)
	  AND txflag2<1 THEN DO;
		IF mark='' THEN DO;  /*IF FIRST IDENTIFIED THEN OUTPUT CURRENT AND PREVIOUS STAYS*/
			/*OUTPUT PREVIOUS STAY*/
			trans_caseno+1;
			counts+1;
			mark='1';
			newobs=nobs2;
			newdiag1 = diag1_2;
			newdisch = disdate2;
			newadmit = admdate2;
			newprovid = prvid2;
			newtxflag = txflag2;
			OUTPUT;
			/*OUTPUT CURRENT STAY*/
			counts+1;
			newobs=nobs;
			newdiag1 = diag1;
			newdisch = disch;
			newadmit = admit;
			newprovid = provid;
			newtxflag = txflag;
			OUTPUT;
		END;
		ELSE IF mark='1' THEN DO;  /*IF PREVIOUS STAY WAS OUTPUT THEN ONLY OUTPUT CURRENT STAY*/
			counts+1;
			newobs=nobs;
			newdiag1 = diag1;
			newdisch = disch;
			newadmit = admit;
			newprovid = provid;
			newtxflag = txflag;
			OUTPUT;
		END;
	END;
	ELSE DO;  /*RESET COUNTER AND MARK IF NOT A TRANSFER STAY*/
		counts=0;
		mark='';
	END;
RUN;

**********************************************************************
* ADD IN PROC CODES FOR H/K AND CB
**********************************************************************;
PROC SORT DATA=flag_transfers;
	BY nobs;
RUN;

proc sort data=data_sty.&stay_dataset;
	by nobs;
run;

DATA data_an.flag_transfers;
	MERGE flag_transfers(IN=a)
		  data_sty.&stay_dataset.(KEEP=nobs proc1-proc&PRN.);
	BY nobs;
	IF a;
RUN;

PROC SORT DATA=data_an.flag_transfers;
	BY hicno trans_caseno;
RUN;

TITLE 'TRANSFER CHAIN COUNTS';
PROC FREQ DATA=data_an.flag_transfers;
	TABLE counts 
		  disch*counts
		/LIST MISSING;
	FORMAT disch MONYY.;
RUN;

TITLE 'PRINT TRANSFER CHAIN SAMPLE TO VALIDATE LOGIC';
PROC PRINT DATA=data_an.flag_transfers (OBS=200);
RUN;
TITLE;

/*DROP ADMIT, PROVID AND TXFLAG AFTER VALIDATING LOGIC*/
DATA data_an.flag_transfers;
	SET data_an.flag_transfers(DROP=admit provid txflag);
RUN;


/************************************************************************************************************
                               SAS Program Description

PROGRAM NAME: 1_Create_initial_index_with_transfers

PURPOSE: Identify cases for the readmission index and define the transfer chains 

OVERVIEW: Select cases that have a qualifying diagnosis in reporting period and select those cases that are related to the selected cases by transfering in or out.

INPUT DATA: 
	        data_sty.&stay_dataset.        *PRE-SORTED BY HICNO ADMIT DISCH TXFLAG PROVID  -WITH NOBS CREATED AFTER SORT
	        data_an.flag_transfers         *PRE-SORTED BY HICNO TRANS_CASENO

OUTPUT FILES: 
	          dx_data.index01

************************************************************************************************************/
/* CHECK MACRO VARIABLES */
%PUT DX (MEASURE VARIABLE)= &DX.;
%PUT DXN (MAX NUMBER OF DIAGNOSIS)= &DXN.;
%PUT PRN (MAX NUMBER OF PROCEDURE)= &PRN.;
%PUT YY (FIRST YEAR OF SPLIT YEAR PERIOD)= &YY.;
%PUT YYE (LAST YEAR OF SPLIT YEAR PERIOD)= &YYE.;
RUN;

**********************************************************************
*GET STARTING DATASET OF CLAIMS IN REPORTING PERIOD AND SIX MONTHS 
*PRIOR FOR BENES WITH QUALIFYING DIAGNOSIS IN REPORTING PERIOD
**********************************************************************;
/*GET LIST OF HICNOS FOR BENES WITH DISCHARGES IN REPORTING PERIOD*/
DATA dx_data.&DX._benes(KEEP=hicno);
SET data_sty.&stay_dataset.(KEEP=hicno disch);
WHERE disch >= MDY(01,01,20&YY) AND disch < MDY(12,31,20&YYE);
RUN;

PROC SORT DATA=dx_data.&DX._benes NODUPKEY; BY hicno; RUN;

proc sort data=data_sty.&stay_dataset.; by hicno; run;

/*SELECT ALL STAYS FOR IDENTIFIED BENES WITHIN REPORTING PERIOD AND SIX MONTHS PRIOR*/
DATA dx_data.startingclaims;
	MERGE dx_data.&DX._benes(IN=A)
	data_sty.&stay_dataset.(WHERE=(disch >= MDY(07,01,20&YY-1) AND disch < MDY(12,31,20&YYE))
					        KEEP= admit admsour ccuind ddest diag1-diag&dxn disch disst 
						          drgcd dvrsnd01-dvrsnd25 edgscd01-edgscd12 evrscd01-evrscd12 hicno icuind 
						          los mdcl_rec mdcl_rec mscd npi_at npi_op poaend01-poaend12 poancd1-poancd25 
						          proc1-proc&prn procdt1-procdt&prn provid pstate pvsncd01-pvsncd25 txflag
						          typeadm upin_at upin_op nobs);
	BY hicno;
	IF A;
RUN;


***************************************************************************
* FLAG TRANSFER CHAINS WHERE At least one stay in the transfer chain with a 
* Discharge Date within the report period 
***************************************************************************;
/*CREATE TABLE WITH ONE RECORD PER TRANSFER CHAIN*/
DATA flag_chains_hosp_wide;
	SET data_an.flag_transfers;
	BY hicno trans_caseno;
	RETAIN hosp_wide_transfer index_stay_ct;

	/*FIRST DISCHARGE - RESET COUNTERS*/
	IF FIRST.trans_caseno THEN DO;
		index_stay_ct = 0;
		hosp_wide_transfer = 0;
	END;

	/*COUNT OF STAYS IN INDEX PERIOD*/
	IF disch >= MDY(07,01,20&YY-1) AND disch < MDY(12,31,20&YYE) THEN index_stay_ct +1;

	/*AT LEAST ONE STAY IN TRANSFER CHAIN MUST MEET TIME PERIOD*/
	IF disch >= MDY(07,01,20&YY-1) AND disch < MDY(12,31,20&YYE) THEN hosp_wide_transfer = 1;

	IF LAST.trans_caseno THEN OUTPUT;
RUN;


**********************************************************************
* ADD SEQUENTIAL GROUP NUMBERS
**********************************************************************;
DATA count_trans;
	SET flag_chains_hosp_wide;
	BY hicno;

	LENGTH group 3;
	IF FIRST.hicno THEN group=0;
	/*ITERATE GROUP NUMBERS FOR TRANSFER CHAINS THAT HAVE MORE THAN ONE QUALIFYING STAY IN THE INDEX PERIOD*/	
	IF index_stay_ct > 1 AND hosp_wide_transfer = 1 THEN group+1;  
RUN;

DATA tx_with_group (KEEP=nobs group trans_caseno   
						 hosp_wide_transfer index_stay_ct);
	MERGE data_an.flag_transfers 
		  count_trans(KEEP=hicno trans_caseno group   
						   hosp_wide_transfer index_stay_ct);
	BY hicno trans_caseno;
RUN;

PROC SORT; BY nobs; RUN;

proc sort data=dx_data.startingclaims; by nobs; run;


**********************************************************************
* MERGE THESE TRANSFER CLAIMS ONTO UNBUNDLED CLAIMS AND FLAG ALL
* CLAIMS WITHIN A TRANSFER CHAIN.
**********************************************************************;
DATA transfers_flagged 
	 dx_data.dropped_cases;
	MERGE dx_data.startingclaims(IN=SC) tx_with_group(IN=T);
	BY nobs;
	IF SC;

	IF index_stay_ct>1 AND hosp_wide_transfer = 1 THEN trans=1;
	ELSE DO;
		group=0;
		trans=0;
	END;

	FORMAT dropreason $60.;
	dropreason='';

	/*DROP NON TRANSFER STAYS FROM BEFORE REPORTING PERIOD*/
	IF trans=0 AND DISCH < MDY(01,01,20&YY)
		THEN dropreason = '1. non trans stay from before reporting period';

	IF dropreason='' THEN OUTPUT transfers_flagged;
	  ELSE OUTPUT dx_data.dropped_cases;
RUN;

TITLE 'CASES BEFORE DROPS';
PROC SQL;
SELECT COUNT(1)
FROM dx_data.startingclaims;
QUIT;

TITLE 'CASE DROP REASONS';
PROC FREQ DATA=dx_data.dropped_cases;
	TABLE dropreason /list missing;
RUN;

TITLE 'CASES INCLUDED IN READMISSION INDEX AFTER DROPS';
PROC SQL;
SELECT COUNT(1)
FROM transfers_flagged;
QUIT;

TITLE 'NON TRANS STAY FROM BEFORE REPORTING PERIOD';
PROC FREQ DATA=dx_data.dropped_cases;
	TABLE trans*DISCH /list missing;
	WHERE dropreason = '1. non trans stay from before reporting period';
	FORMAT disch MONYY.;
RUN;


**********************************************************************
* ADD CASE NUMBERS 
**********************************************************************;
PROC SORT DATA=transfers_flagged;
	BY hicno admit disch;
RUN;

DATA transfers_flagged_cases;
	SET transfers_flagged;
	BY HICNO;
	LENGTH case 3;
	IF FIRST.hicno THEN case=0;
	                    case+1;
RUN;


**********************************************************************
* FLAG FIRST STAY IN A TRANSFER, LAST STAY IN A TRANFSER, AND TRANSFER
* CLAIMS THAT ARE NEITHER FIRST NOR LAST 
**********************************************************************;
PROC SORT DATA=transfers_flagged_cases; BY hicno group case; RUN;

DATA dx_data.index01 (KEEP= admit admsour case history_case ccuind ddest diag1-diag&dxn   
						    disch disst drgcd dvrsnd01-dvrsnd25 edgscd01-edgscd12 evrscd01-evrscd12
						    group hicno icuind los mdcl_rec mdcl_rec mscd npi_at npi_op 
						    poaend01-poaend12 poancd1-poancd25 proc1-proc&prn procdt1-procdt&prn provid 
						    pstate pvsncd01-pvsncd25 trans trans_first trans_last trans_mid txflag
						    typeadm upin_at upin_op year SORTEDBY=hicno group case);
	SET transfers_flagged_cases;
	BY hicno group case;

	/*CREATE YEAR VARIABLE - missing for prior to period*/
	LENGTH year $4;

    IF disch <= "30jun20&yy"d THEN year = "&yy"||"&yy";
	ELSE IF "1jul20&yy"d <= disch <= "30jun%eval(20&yy+1)"d               
	  THEN year = "&yy"||SUBSTR("%EVAL(20&yy+1)",3,2);             
	ELSE IF "1jul%eval(20&yy+1)"d <= disch <= "30jun%eval(20&yy+2)"d 
	  THEN year = SUBSTR("%EVAL(20&yy+1)",3,2)||SUBSTR("%EVAL(20&yy+2)",3,2);                     
	ELSE IF "1jul%eval(20&yy+2)"d <= disch <= "30jun%eval(20&yy+3)"d 
	  THEN year = SUBSTR("%EVAL(20&yy+2)",3,2)||SUBSTR("%EVAL(20&yy+3)",3,2);
	ELSE IF "1jul%eval(20&yy+3)"d <= disch <= "30jun%eval(20&yy+4)"d 
	  THEN year = SUBSTR("%EVAL(20&yy+3)",3,2)||SUBSTR("%EVAL(20&yy+4)",3,2);
	ELSE IF "1jul%eval(20&yy+4)"d <= disch <= "30jun%eval(20&yy+5)"d 
	  THEN year = SUBSTR("%EVAL(20&yy+4)",3,2)||SUBSTR("%EVAL(20&yy+5)",3,2);
	ELSE IF "1jul%eval(20&yy+5)"d <= disch <= "30jun%eval(20&yy+6)"d 
	  THEN year = SUBSTR("%EVAL(20&yy+5)",3,2)||SUBSTR("%EVAL(20&yy+6)",3,2);
	ELSE IF "1jul%eval(20&yy+6)"d <= disch <= "30jun%eval(20&yy+7)"d 
	  THEN year = SUBSTR("%EVAL(20&yy+6)",3,2)||SUBSTR("%EVAL(20&yy+7)",3,2);
	ELSE IF "1jul%eval(20&yy+7)"d <= disch <= "30jun%eval(20&yy+8)"d 
	  THEN year = SUBSTR("%EVAL(20&yy+7)",3,2)||SUBSTR("%EVAL(20&yy+8)",3,2);
	ELSE IF "1jul%eval(20&yy+8)"d <= disch <= "30jun%eval(20&yy+9)"d 
	  THEN year = SUBSTR("%EVAL(20&yy+8)",3,2)||SUBSTR("%EVAL(20&yy+9)",3,2);
	ELSE IF "1jul%eval(20&yy+9)"d <= disch <= "30jun%eval(20&yy+10)"d 
	  THEN year = SUBSTR("%EVAL(20&yy+9)",3,2)||SUBSTR("%EVAL(20&yy+10)",3,2);
	ELSE IF "1jul%eval(20&yy+10)"d <= disch <= "30jun%eval(20&yy+11)"d 
	  THEN year = SUBSTR("%EVAL(20&yy+10)",3,2)||SUBSTR("%EVAL(20&yy+11)",3,2);
	ELSE IF "1jul%eval(20&yy+11)"d <= disch <= "30jun%eval(20&yy+12)"d 
	  THEN year = SUBSTR("%EVAL(20&yy+11)",3,2)||SUBSTR("%EVAL(20&yy+12)",3,2);
	ELSE IF "1jul%eval(20&yy+12)"d <= disch <= "30jun%eval(20&yy+13)"d 
	  THEN year = SUBSTR("%EVAL(20&yy+12)",3,2)||SUBSTR("%EVAL(20&yy+13)",3,2);
	  
	  
	/*DEFINE TRANSFER FLAGS*/
	trans_first=0;
	trans_last=0;
	trans_mid=0;
	IF trans=1 THEN DO;
		IF FIRST.group THEN trans_first=1;
		IF LAST.group THEN trans_last=1;
		IF trans_first=0 AND trans_last=0 THEN trans_mid=1;
	END;

	LENGTH history_case 3;
	RETAIN history_case;

	/*DEFINE HISTORY_CASE*/
	IF group=0 THEN history_case=case;
	ELSE IF FIRST.group THEN history_case=case;

RUN;

/*DELETE WORK AND INITIAL DATASET TO CLEAN UP THE EG PROCESS FLOW*/
PROC DELETE DATA= flag_chains_hosp_wide count_trans 
				  transfers_flagged tx_with_group transfers_flagged_cases;
RUN;


/************************************************************************************************************
                               SAS Program Description

PROGRAM NAME: 2_add_Bene_data

FOR USE WITH: ALL MEASURES

PURPOSE:      add bene demographic fields and calculated fields 

OVERVIEW:    merge in bene demographic data and create calculated fields.  
			 run some data checks.

INPUT DATA: 
	dx_data.index01          *PRE-SORTED BY HICNO

OUTPUT FILES: 
	dx_data.index02

************************************************************************************************************/
%macro combine_bene_dataset;

data data_ben.bene_dataset (keep=birth bstate county death edbvdeth hicno race sex zip rfrnc_yr);
set
	%do i = 2009 %to 2016; /* Need to change based on the years you would like to include. */
    	denom.dn100mod_&i. (in=B rename=(bene_dob=birth
										  state_cd=bstate
								          cnty_cd=county
		                                  death_dt=death
		                                  v_dod_sw=edbvdeth
		                                  bene_id=hicno
 	                                      bene_zip=zip))
	%end;
;
run;

%mend;

%combine_bene_dataset;

proc sort data=data_ben.bene_dataset;
	by hicno DESCENDING rfrnc_yr;
run;

proc sort data=data_ben.bene_dataset NODUPKEY;
	by hicno;
run;


********************************************************************; 
*ADD BENE DATA
********************************************************************; 
DATA index02;
	MERGE dx_data.index01 (IN=A)
		  data_ben.&bene_dataset. (IN=B KEEP=hicno birth death edbvdeth sex race 
											  county zip bstate
									    RENAME=(death=cms_death));
	BY hicno;
	IF A;

	/*CREATE FLAG FOR DIED IN HOSPITAL TO USE TO FLAG UNRELIABLE DEATHS*/          
	hospdead=(ddest=20);   

	/*ASSIGN CMS DATA - ONLY USE VERIFIED DEATH*/
	*USED TO BE "Y" BUT DENOMINATOR FILE SHOWS "V" AS VERIFIED DEATH INDICATOR*;
	IF death=. AND edbvdeth='V' THEN death=cms_death;

	/*ONLY KEEP CMS CASES IF THEY HAVE BENE DATA*/
	IF (A AND B) THEN OUTPUT index02;
RUN;


********************************************************************; 
*CREATE UNRELIABLE DEATH AND DEMOGRAPHICS INDICATORS
********************************************************************; 
PROC SORT DATA=index02                                                          
	OUT=index02_hosp;                                                              
	BY hicno case hospdead provid;                                                 
RUN;  

 
DATA index02_U(DROP=UNREL);                                           
	SET index02_hosp;                                                              
	BY hicno;                                                                      
	RETAIN unrel;                                                                  
	IF FIRST.hicno THEN unrel=0;                                                   
	IF hospdead=1 & NOT (LAST.hicno) THEN unrel=1;                                 
	                                                                            
	/*UNRELIABLE DEATH INDICATOR - USES EDB DEATH*/                                
	unreldth=0;                                                                    
	IF (unrel=1 & death=.) OR (death NE . AND death<admit) THEN unreldth=1;        
	ELSE IF (death>=admit AND death<disch AND hospdead=0) THEN unreldth=2;         
	                                                                            
	/*UNRELIABLE DEMOGRAPHICS INDICATOR*/                                          
	unreldmg=0;                                                                    
	IF (INT((admit-birth)/365.25)>115) THEN unreldmg=1;                            
	ELSE IF (sex NE '1' AND sex NE '2') THEN unreldmg=2;                           
RUN;  

                                                                                
********************************************************************; 
*CHECK FOR DUPLICATES BY ADMIT, DISCH, PROVID
********************************************************************; 
PROC SORT DATA=index02_U;                                                       
	BY hicno admit disch provid trans_first;
RUN;                                                                            

DATA dx_data.index02(DROP=hospdead) dup;
	SET index02_u;
	BY hicno admit disch provid;
	IF LAST.provid THEN OUTPUT dx_data.index02;
	ELSE OUTPUT dup;
RUN;


********************************************************************; 
*SORT FOR MERGE IN NEXT STEP
********************************************************************; 
PROC SORT DATA= dx_data.index02;
	BY hicno case;
RUN;

/*DELETE WORK AND INITIAL DATASET TO CLEAN UP THE EG PROCESS FLOW*/
PROC DELETE DATA=index02 index02_hosp index02_U dup; 
RUN;


/************************************************************************************************************
                               SAS Program Description

PROGRAM NAME: 3_add_coverage_and_hospice

FOR USE WITH: ALL MEASURES

PURPOSE:      Calculate completeness fields and hospice fields 

OVERVIEW:     Check each hospice occurence tiemframe for meeting criteria for hospice flags.
			  Calculate each of the completeness fields using formatted coverage data. label
			  index fields.

INPUT DATA: 
	dx_data.index02             *PRE-SORTED BY HICNO CASE
	data_an.coverage&YYE._&MM.  *PRE-SORTED BY HICNO

OUTPUT FILES: 
	dx_data.index03
	dx_data.missing_coverage

************************************************************************************************************/
%macro rename_bene_covg;

	%do i = 2007 %to 2016; /* Change input dataset when redoing jobs. Need denominator data two years ahead of the first reporting year */
			 %let j=%eval(12*(&i.-2007)+1);
			 %let k=%eval(12*(&i.-2007)+12);
			 data data_an.dn100mod_&i. (keep=hicno hmo: buy: drop= HMO_MO BUYIN_MO);
			 set denom.dn100mod_&i. (rename=(bene_id=hicno hmoind01-hmoind12=hmo&j. - hmo&k
					                         buyin01-buyin12=buy&j.-buy&k));
			 if hicno='' then delete;
			run;

	%end;
;
run;

%mend;

%rename_bene_covg;

data data_ben.coverage&YYE._&MM.;
	merge data_an.dn100mod_2007 data_an.dn100mod_2008 data_an.dn100mod_2009 data_an.dn100mod_2010 data_an.dn100mod_2011 
          data_an.dn100mod_2012 data_an.dn100mod_2013 data_an.dn100mod_2014 data_an.dn100mod_2015 data_an.dn100mod_2016;
	by hicno;
run;

proc sort data=data_ben.coverage&YYE._&MM. nodupkey; by hicno; run;


**********************************************************************
* CALCULATE AND VERIFY MACRO VARIABLES
**********************************************************************;
/*DEFINE MACRO VARIABLE FOR NUMBER OF MONTHS FOR COVERAGE FIELDS*/
DATA _NULL_;
	* Change needed here: Frstday is the January 1st of two years before the reporting period;
	frstday = MDY(1,1,2007); 
	lstmonth = MDY(12,31,2016); 
	mthCt = INTCK('MONTH',frstday,lstmonth)+1;
	CALL SYMPUT('mthCt',COMPRESS(mthCt));
run;

/*OUTPUT YEAR AND END MONTH MACRO VARIABLES TO LOG*/
%PUT YEAR AND MONTH MACRO VARIABLES:;
%PUT YYE=&YYE YP1=&YP1 YP2=&YP2 YP3=&YP3 YP4=&YP4 YP5=&YP5 MM=&MM mthCt=&mthCt;
%PUT MAX NUMBER OF DIAGNOSIS AND PROCEDURE CODES:;
%PUT DXN=&DXN PRN=&PRN;
RUN;
**********************************************************************;

********************************************************************
* CALCULATE COMPLETENESS FIELDS AND HOSPICE FIELDS 
**********************************************************************;
DATA data_ben.coverage&YYE._&MM.;
	set data_ben.coverage&YYE._&MM.;
run;
/*	Date Calculator
data _null_;
d='01Jan2009'd;
put d;
run; *17898;
*/
DATA dx_data.index02;
	set dx_data.index02;
	* Change needed here. We adjust all admit dates before Jan 01 2009 to Jan 01 2009;
	if ADMIT le 17898 then do; ADMIT = 17898; end;
run;

%LET PERIOD = %EVAL(%EVAL(12*&NYEAR.)+24);

DATA index03  dx_data.missing_coverage;
	MERGE dx_data.index02(IN=INI) data_an.coverage&YYE._&MM.(IN=COV /*DROP=hspc_ct*/);
	BY hicno;
	IF INI;
	LENGTH para para_b postmo postmo_a postmod postmod_a premo premo_a 
	       admmo dismo begin begind post postd 3;
	
	/*Change is needed: DEFINE STARTING POINT FOR COUNTING MONTHS*/
	frstday = MDY(1,1,2008);

	*CALCULATE COMPLETENESS VARIABLES
	********************************************************************; 
	/*COUNT # OF MONTHS FROM FRSTDAY TO ADMIT AND DISCH DATE*/
	admmo = INTCK('MONTH',frstday,admit) + 1; /*NUMBER OF MTHS FROM FIRST MTH*/
	dismo = INTCK('MONTH',frstday,disch) + 1; /*NUMBER OF MTHS FROM FIRST MTH*/

	/*PARA: HMO=0 AND PART A ENROLLMENT FOR MONTH OF ADMIT*/
	*para =HMO(admmo)='0' AND BUY(admmo) IN ('A','C');
	array hmo_a{&PERIOD.} hmo1-hmo&PERIOD.;
	array buy_a{&PERIOD.} buy1-buy&PERIOD.;
	
	do i=1 to &PERIOD.;
	
	if i=admmo then do;
	if /*HMO_a{i} in (' ','0','4') AND*/ buy_a{i} IN ('A','C','1','3') then para=1; else para=0;
	if /*HMO_a{i} in (' ','0','4') AND*/ buy_a{i} in ('C','A') then para_b=1; else para_b=0;
	if HMO_a{i} not in (' ','0','4') then ma=1;
	end;
	end;

	/*POSTMO AND POSTMO_A ENROLLMENT FOR 3 MONTHS FOLLOWING ADMISSION*/
		postmo=0;
		postmo_a=0;
		begin=admmo+1;
		post =admmo+1;
	
	/*POSTMOD AND POSTMOD_A ENROLLMENT FOR 3 MONTHS FOLLOWING DISCHARGE*/
	    postmod=0;
	    postmod_a=0;
	    BEGIND=dismo+1;
	    POSTD =dismo+1;
	
	IF dismo < &mthCt. THEN DO; /*NOT ENOUGH DATA TO CHECK POSTMO FOR POST INDEX RECORDS*/

		DO I=BEGIN TO POST;
			postmo =SUM(postmo,
					   (postmo=(I-BEGIN))* /*count only if continuously enrld*/
					   (/*HMO_a{I} in (' ','0','4') AND */BUY_a{I} in('C','3')));
	        /*SAME CODING AS POSTMO BUT WITH ADDITIONAL 'A' BUYIN CODE*/
			postmo_a =SUM(postmo_a,
						 (postmo_a=(I-BEGIN))*
						 (/*HMO_a{I} in (' ','0','4') AND*/ (BUY_a{I} IN ('A','C','1','3'))));
		END;

	    DO I=BEGIND TO POSTD;
	        postmod =SUM(postmod,
	                     (postmod=(I-BEGIND))* /*count only if continuously enrld*/
	                     (/*HMO_a{I} in (' ','0','4') AND*/ BUY_a{I} in('C','3')));
	        /*SAME CODING AS POSTMOD BUT WITH ADDITIONAL 'A' BUYIN CODE*/
	        postmod_a =SUM(postmod_a,
	                       (postmod_a=(I-BEGIND))*
						   (/*HMO_a{I} in (' ','0','4') AND*/ (BUY_a{I} IN ('A','C','1','3'))));
	    END;

		/*CREATE CHECK VARIABLES TO CONFIRM THAT POSTMO AND POSTMOD ARE EQUAL WHEN ADMIT AND */
		/*DISCHARGE ARE IN THE SAME MONTH*/
		IF BEGIN=BEGIND THEN DO;
			flag_unequal   = (postmo NE postmod);
			flag_unequal_a = (postmo_a NE postmod_a);
		END;
	END;

	/*PREMO AND PREMO_A CHECK ENROLLMENT IN PREVIOUS 12 MTHS*/
	premo=0;
	premo_a=0;
	* CHECK FOR ENROLLMENT IN 12 MONTHS PRIOR TO ADMISSION;
	BEGIN=ADMMO-12;

	IF BEGIN>=1 THEN DO I=BEGIN TO ADMMO-1;
		premo =SUM(premo,(/*HMO_a{I} in (' ','0','4') AND*/ BUY_a{I} in('C','3')));
		premo_a=SUM(premo_a,(/*HMO_a{I} in (' ','0','4') AND*/ (BUY_a{I}) IN ('A','C','1','3')));
	END;

	label MA="Medicare Advantage";

	IF (INI AND COV) THEN OUTPUT index03;
	ELSE OUTPUT dx_data.missing_coverage;
RUN;


********************************************************************
*VALIDATE CREATED FIELDS
********************************************************************; 
TITLE 'COVERAGE FREQS CHECK RANGES';
PROC FREQ DATA=index03;
	TABLES 
	para para_b premo premo_a postmo postmod postmo_a postmod_a postmo postmo_a
	admit*(premo para postmo postmo_a)
	disch*(postmod postmod_a)
	para*premo_a

	/* hsp_enr_prior_admit
	hsp_enr_on_admit
	hsp_enr_prior_disch
	hsp_enr_on_disch */
	/LIST MISSING;
	FORMAT admit disch YEAR4.;
RUN;

TITLE "VALIDATE COMPLETENESS VARIABLES";
/*PROC PRINT DATA = index03 (OBS=100);
  VAR hicno admit disch postmo postmod postmo_a postmod_a
      begin post begind postd para para_b premo premo_a death edbvdeth
  FORMAT admit disch MMDDYY10.;
RUN;*/

PROC SQL NOPRINT;
SELECT "'"||hicno||"'" INTO: coveragesample
	SEPARATED BY ","
FROM index03(OBS=100);
QUIT;
%PUT coverage sample hicnos:;
%PUT &coveragesample.;

TITLE "ETL COVERAGE DATA FOR COVERAGE SAMPLE RECORDS";
PROC PRINT DATA=data_ben.&coverage_dataset.;
	WHERE hicno IN (&coveragesample.);
RUN;

/*VALIDATE COVERAGE FOR PART A=0 AND PREMO_A>0************************/
DATA parta0;
	SET index03(OBS=50);
	WHERE para=0 and premo_a>0;
RUN;

TITLE "VALIDATE COMPLETENESS VARIABLES WHERE PARA=0 AND PREMO_A>0";
/*
PROC PRINT DATA = parta0;
  VAR hicno admit disch postmo postmod postmo_a postmod_a
      begin post begind postd para para_b premo premo_a death edbvdeth
  FORMAT admit disch MMDDYY10.;
RUN; */
PROC SQL NOPRINT;
SELECT "'"||hicno||"'" INTO: parta0
	SEPARATED BY ","
FROM parta0;
QUIT;
%PUT parta0 coverage sample hicnos:;
%PUT &parta0.;
TITLE "ETL COVERAGE DATA FOR PARTA0 COVERAGE SAMPLE RECORDS";
PROC PRINT DATA=data_ben.&coverage_dataset.;
	WHERE hicno IN (&parta0.);
RUN;



TITLE "IF ADMIT/DISCH ARE IN THE SAME MONTH, POSTMO SHOULD EQUAL POSTMOD";
PROC FREQ DATA = index03;
 TABLES flag_unequal flag_unequal_a /LIST MISSING;
RUN;


 TITLE "CHECK OF HOSPICE INDICATORS";                                           
 /*PROC PRINT DATA=index03 (OBS=20);                                               
 VAR hicno case admit disch 
 hsp_enr_prior_admit hsp_enr_on_admit                
        hsp_enr_prior_disch hsp_enr_on_disch hspbeg1-hspbeg30                   
        hspend1-hspend30;                                                       
 WHERE hspbeg1 NE .;                                                            
RUN;  */                                                                          

/*****DROP COVERAGE AND HOSPICE CALCULATING VARIABLES******/
DATA dx_data.index03(SORTEDBY=hicno case);
 SET index03;
 /*DROP I admmo begin post begind postd dismo frstday 
     y&yp5.buy1-y&yp5.buy12 y&yp5.hmo1-y&yp5.hmo12
     y&yp4.buy1-y&yp4.buy12 y&yp4.hmo1-y&yp4.hmo12 
     y&yp3.buy1-y&yp3.buy12 y&yp3.hmo1-y&yp3.hmo12 
     y&yp2.buy1-y&yp2.buy12 y&yp2.hmo1-y&yp2.hmo12
     y&yp1.buy1-y&yp1.buy12 y&yp1.hmo1-y&yp1.hmo12
     y&YYE.buy1-y&YYE.buy&mm y&YYE.hmo1-y&YYE.hmo&mm
	 hspbeg1-hspbeg30 hspend1-hspend30
     flag_unequal flag_unequal_a
     ;*/
RUN;
**********************************************************************;

/*DELETE WORK AND INITIAL DATASET TO CLEAN UP THE EG PROCESS FLOW*/
PROC DELETE DATA=index03 parta0; 
RUN;
TITLE;


/************************************************************************************************************
                               SAS Program Description

PROGRAM NAME: 4_bundled_index_cases

FOR USE WITH: ALL MEASURES

PURPOSE: create a bundled index file for use in defining the history and post index files 

OVERVIEW: create an index file that includes one record per bene with arrayed fields for 
			  each admit, discharge and provider to use in finding records for the history 
			  and post files

INPUT DATA: 
	        dx_data.index03  *PRE-SORTED BY HICNO

OUTPUT FILES: 
	          dx_data.indexMatch
	          index_max_case

************************************************************************************************************/
OPTIONS COMPRESS=YES symbolgen mprint;

**********************************************************************
* DETERMINE THE MAXIMUM NUMBER OF CASES TO USE TO BUNDLE CLAIMS.
**********************************************************************;
PROC MEANS DATA=dx_data.index03 MAX;                                                
	VAR case;                                                                
	OUTPUT OUT=index_max_case MAX=index_max_case;                                             
RUN;  
 
DATA _NULL_;
	SET index_max_case;
	CALL SYMPUT("MC", LEFT(PUT(index_max_case, 3.)));
RUN;
%PUT MAX CASE FOR INDEX: &MC.;
**********************************************************************;


**********************************************************************
* BUNDLE CLAIMS FOR CREATING PRE AND POST FILES
**********************************************************************;
 DATA dx_data.indexMatch (KEEP=hicno admdt1-admdt&MC disdt1-disdt&MC provid1-provid&MC index_max_case MA
						       SORTEDBY=hicno);
	SET dx_data.index03;
	BY hicno;

	INFORMAT provid1-provid&MC $6.;
	FORMAT admdt1-admdt&MC disdt1-disdt&MC MMDDYY10.;
	RETAIN admdt1-admdt&MC disdt1-disdt&MC provid1-provid&MC;
	ARRAY ADM(&MC) admdt1-admdt&MC;
	ARRAY DIS(&MC) disdt1-disdt&MC;
	ARRAY PVD(&MC) $provid1-provid&MC;

	* INITIALIZE ADMISSIONS, DISCHARGES, PROVID & CASE/TRANS TO MISSING;
	IF (FIRST.hicno) THEN DO;
		DO I=1 TO &MC;
			ADM(I)=.;
			DIS(I)=.;
			PVD(I)='';
		END;
	END;

	index_max_case = &MC.;

	* CREATE A STRING OF INDEX FIELDS FOR EACH BENE;
	ADM(CASE)=admit;
	DIS(CASE)=disch;
	PVD(CASE)=provid;

	* OUTPUT IF LAST INDEX ADMISSION FOR THIS BENE;
	IF (LAST.hicno) THEN OUTPUT;
RUN;


/************************************************************************************************************
                               SAS Program Description

PROGRAM NAME: 5_Post_Index

FOR USE WITH: HW

PURPOSE: Create post index file 

OVERVIEW FROM SPECS: Select stays that are up to 365 days after each index stay. create dataset
					 for creating POST_FLAG and POST_INDEX_STAY flags in the index file.  
					 Remove same day and next day transfers and format data for post index file.

INPUT DATA: 
	        data_sty.&stay_dataset.  *PRE-SORTED BY HICNO ADMIT DISCH TXFLAG PROVID
	        dx_data.indexMatch       *PRE-SORTED BY HICNO

OUTPUT FILES: 
	          dx_data.pstindex -for creating POST_FLAG and POST_INDEX_STAY flags in the index file.
	          An_files.postindex_&DX._&YY.&YYE.

************************************************************************************************************/
/* CHECK MACRO VARIABLES */
%PUT DX (MEASURE VARIABLE)= &DX.;
%PUT DXN (MAX NUMBER OF DIAGNOSIS)= &DXN.;
%PUT PRN (MAX NUMBER OF PROCEDURE)= &PRN.;
%PUT YY (FIRST YEAR OF SPLIT YEAR PERIOD)= &YY.;
%PUT YYE (LAST YEAR OF SPLIT YEAR PERIOD)= &YYE.;
**********************************************************************;


**********************************************************************
* DETERMINE THE MAXIMUM NUMBER OF CASES IN BUNDLED CLAIMS.
**********************************************************************;
OPTIONS OBS=1;
DATA _NULL_;
	SET dx_data.indexMatch;
	CALL SYMPUT("MC", LEFT(PUT(index_max_case, 3.)));
RUN;
OPTIONS OBS=MAX;
%PUT MAX CASE FOR INDEX: &MC.;
**********************************************************************;


**********************************************************************
* SELECT POST INDEX CLAIMS
**********************************************************************;
DATA pstindex (KEEP=admdiff admit admsour case ccuind ddest dvrsnd01-dvrsnd&DXN diag1-diag&DXN disch
					disst drgcd edgscd01-edgscd12 evrscd01-evrscd12 hicno
					icuind los mdcl_rec poaend01-poaend12 pvsncd01-pvsncd0&PRN proc1-proc&PRN
					procdt1-procdt&PRN provid typeadm 
					txflag year same_provid MA);
	MERGE data_sty.&stay_dataset.(IN=INA WHERE = (admit > MDY(01,01,20&YY.)and case_type='CMS')
								  KEEP=admit admsour case_type ccuind ddest dvrsnd01-dvrsnd&DXN diag1-diag&DXN 
										disch disst drgcd edgscd01-edgscd12 evrscd01-evrscd12 
										hicno icuind los mdcl_rec 
										poaend01-poaend12 pvsncd01-pvsncd0&PRN proc1-proc&PRN 
										procdt1-procdt&PRN provid typeadm txflag) 
		  dx_data.indexMatch(IN=INX KEEP=hicno MA disdt1-disdt&MC provid1-provid&MC); 
	BY hicno;
	IF (INX AND INA);

	LENGTH case 3 year $4;

	ARRAY DIS(&MC) disdt1-disdt&MC;
	ARRAY PVD(&MC) $provid1-provid&MC;

	DO I=1 TO &MC;

		/*DAYS BETWEEN INDEX DISCHARGE AND CLAIM ADMISSION*/
		admdiff=admit-DIS(I);

		/*CHECK FOR POST ADMISSION WITHIN 90 DAYS*/
		/*SAME OR NEXT DAY TRANSFERS ARE REMOVED IN A LATER STEP SO THIS FILE*/
		/*CAN BE USED TO SET THE POST_FLAG AND POST_INDEX_STAY IN THE INDEX*/
		IF DIS(I)>0 AND (0<=admdiff<=90) THEN DO;

			case = I;
			IF provid = PVD(I) THEN same_provid=1;
			ELSE same_provid=0;

			/*CREATE YEAR VARIABLE - missing for prior to period*/
            IF DIS(I) <= "30jun20&yy"d THEN year = "&yy"||"&yy";
			ELSE IF "1jul20&yy"d <= DIS(I) <= "30jun%eval(20&yy+1)"d               
			  THEN year = "&yy"||SUBSTR("%EVAL(20&yy+1)",3,2);             
			ELSE IF "1jul%eval(20&yy+1)"d <= DIS(I) <= "30jun%eval(20&yy+2)"d 
			  THEN year = SUBSTR("%EVAL(20&yy+1)",3,2)||SUBSTR("%EVAL(20&yy+2)",3,2);                     
			ELSE IF "1jul%eval(20&yy+2)"d <= DIS(I) <= "30jun%eval(20&yy+3)"d 
			  THEN year = SUBSTR("%EVAL(20&yy+2)",3,2)||SUBSTR("%EVAL(20&yy+3)",3,2);                    
			ELSE IF "1jul%eval(20&yy+3)"d <= DIS(I) <= "30jun%eval(20&yy+4)"d 
			  THEN year = SUBSTR("%EVAL(20&yy+3)",3,2)||SUBSTR("%EVAL(20&yy+4)",3,2);
	        ELSE IF "1jul%eval(20&yy+4)"d <= DIS(I) <= "30jun%eval(20&yy+5)"d 
			  THEN year = SUBSTR("%EVAL(20&yy+4)",3,2)||SUBSTR("%EVAL(20&yy+5)",3,2);   
		  	ELSE IF "1jul%eval(20&yy+5)"d <= disch <= "30jun%eval(20&yy+6)"d 
		  	  THEN year = SUBSTR("%EVAL(20&yy+5)",3,2)||SUBSTR("%EVAL(20&yy+6)",3,2);
			OUTPUT pstindex;
		END;
	END;  /*END CASE NUMBER LOOP*/
RUN;


**********************************************************************
* CREATE DATASET FOR CREATING POST_FLAG AND POST_INDEX_STAY FLAGS IN THE INDEX FILE
* ONLY NEED STAYS WITH ADMIT IN INDEX PERIOD OR DAY AFTER AND DISCH AFTER INDEX PERIOD
* ONLY KEEP NEEDED FIELDS AND RECORDS
**********************************************************************;
 /*SORT STAYS BY HICNO, ADMIT, DISCH, TXFLAG AND PROVID*/
PROC SORT DATA=pstindex;
	BY hicno case admit disch txflag provid;
RUN;

/*FLAG INDEX STAYS*/
DATA pstindexIS;
	SET pstindex(KEEP=hicno admit disch txflag provid MA);
	WHERE admit <= mdy(12,31,20&YYE) <= disch;
RUN;

/*ONLY NEED FIRST CASE PER HICNO*/
PROC SORT 
	DATA=pstindexIS(KEEP=hicno admit provid
					RENAME=(admit=postadmit provid=postprovid))
	OUT=dx_data.pstindex
	NODUPKEY;
	BY hicno;
RUN;
**********************************************************************;


**********************************************************************
* FLAG TRANSFERS WITHIN POST INDEX FILE AND FORMAT FIELDS
**********************************************************************;
 /*ASSIGN AN OBSERVATION NUMBER TO STAYS AND THEN SORT BY THE OBS NUMBER*/
DATA pstindex_nobs
	 deleteIndexTransfer
	 deleteDuplicates;
	SET pstindex;

	/*CREATE CLAIM OBSERVATION NUMBER TO USE BELOW TO FLAG
	 CLAIMS IN A TRANSFER CHAIN*/
	nobs=_N_;

	/*REMOVE TRANSFERS TO POST INDEX FROM READMISSION INDEX*/
	IF same_provid = 0 AND admdiff <= 1 THEN OUTPUT deleteIndexTransfer;

	/*REMOVE DUPLICATES OF ONE DAY STAYS IN THE INDEX*/
	ELSE IF same_provid = 1 AND admit = disch AND admdiff = 0 THEN OUTPUT deleteDuplicates;

	ELSE OUTPUT pstindex_nobs;
RUN;


/*FORMAT FIELDS FOR OUTPUT*/
DATA An_files.postindex_&DX._&YY.&YYE.
		(KEEP = admdiff admit admsour case ccuind ddest dvrsnd01-dvrsnd&DXN. diag1-diag&DXN. disch disst 
				drgcd edgscd01-edgscd12 evrscd01-evrscd12 hicno icuind los mdcl_rec poaend01-poaend12 
				proc1-proc&PRN. pvsncd01-pvsncd0&PRN. procdt1-procdt&PRN. provid typeadm year MA);
	SET pstindex_nobs;

	/*REFORMAT VARS*/
	LENGTH	ccuind2 icuind2 3 ddest2 drgcd2 8 provid2 $6. nprocdt1-nprocdt&PRN. $8.;

	ddest2 = input(ddest, BEST12.);
	ccuind2 = ccuind;
	drgcd2 = input(drgcd, BEST12.);
	icuind2 = icuind;
	provid2 = substr(left(provid), 1, 6);

	ARRAY PROC procdt1-procdt&PRN.;
	ARRAY NPRC nprocdt1-nprocdt&PRN.;

	DO I = 1 TO &PRN.; 
		IF PROC(I) NE . THEN NPRC(I)=put(PROC(I),YYMMDDN8.);
	END;

	DROP ddest ccuind drgcd icuind provid procdt1-procdt&PRN.;

	RENAME	ddest2		= ddest
			ccuind2		= ccuind
			drgcd2		= drgcd 
			icuind2		= icuind 
			provid2		= provid
			nprocdt1-nprocdt&PRN. = procdt1-procdt&PRN.;

	LABEL
		admdiff     = 'Days between index discharge and claim admission'
		admit       = 'Admission date'
		admsour     = 'Source of admission'
		case        = 'Case number from Index Event Record'
		ccuind2     = 'Coronary Care Unit indicator'
		ddest2      = 'Discharge destination code'
		disch       = 'Discharge date'
		drgcd2      = 'Diagnosis Related Group'
		disst       = 'Medicare discharge status code'
		icuind2     = 'Intensive Care Unit indicator'
		mdcl_rec    = 'Medical record number'
		typeadm     = 'Type of admission'
		provid2     = 'Medicare provider number'
		hicno       = 'CLEAN XREFD ID'
		los         = 'Length of stay'
		year		= 'Year cohort'
	%MACRO labelcounts();
		%DO I=1 %TO &DXN;
			diag&I   = "Diagnosis code #&I"
		%END;

		%DO J=1 %TO &PRN;
			proc&J   = "Procedure code #&J"
			nprocdt&J = "Procedure date #&J"
		%END;

		%DO M=1 %TO 12;
			%LET N=%SYSFUNC(PUTN(&M,Z2.));
			edgscd&N = "External cause of injury diagnosis code #&N"
			poaend&N = "External cause of injury POA indicator #&N"
		%END;
	%MEND labelcounts;
	%labelcounts;;
RUN;

/*SORT POST INDEX*/
PROC SORT DATA= An_files.postindex_&DX._&YY.&YYE.;
	BY hicno case;
RUN;


/************************************************************************************************************
                               SAS Program Description

PROGRAM NAME: 6_Final_Index

FOR USE WITH: HW

PURPOSE:      create final readmission and mortality index files 

OVERVIEW:     use pstindex to create POST_FLAG and in readmission index file.
			  use readmission index to add year to post index. 

INPUT DATA: 
	dx_data.index03   *PRE-SORTED BY HICNO CASE
	dx_data.pstindex  *PRE-SORTED BY HICNO -for creating POST_FLAG in the index file.

OUTPUT FILES: 
	An_files.index_&DX._&YY.&YYE.

************************************************************************************************************/
/* CHECK MACRO VARIABLES */
%PUT DX (MEASURE VARIABLE)= &DX.;
%PUT DXN (MAX NUMBER OF DIAGNOSIS)= &DXN.;
%PUT PRN (MAX NUMBER OF PROCEDURE)= &PRN.;
%PUT YY (FIRST YEAR OF SPLIT YEAR PERIOD)= &YY.;
%PUT YYE (LAST YEAR OF SPLIT YEAR PERIOD)= &YYE.;
**********************************************************************;


**********************************************************************
* CREATE FINAL READMISSION INDEX DATASET
* SELECT MORTALITY INDEX RECORDS AND SPLIT OUT TRANSFERS TO CHECK ELIGIBILITY
* ASSIGN POST FLAGS, RECODE TRANS FIELDS AND DEFINE VA_OBS_STAY
**********************************************************************;
DATA An_files.index_&DX._&YY.&YYE.
		(KEEP = admit admsour birth bstate case ccuind  
			county ddest death dvrsnd01-dvrsnd&DXN. diag1-diag&DXN. disch disst
			drgcd edbvdeth edgscd01-edgscd12 evrscd01-evrscd12 
			group hicno history_case icuind los mdcl_rec mscd npi_at npi_op para para_b 
			poaend01-poaend12 poancd1-poancd25 post_flag postmo postmo_a
			postmod postmod_a premo premo_a pvsncd01-pvsncd0&PRN. proc1-proc&PRN. procdt1-procdt&PRN. provid pstate
			race sex trans trans_first trans_last trans_mid txflag
			typeadm unreldmg unreldth upin_at upin_op 
			year zip MA SORTEDBY=hicno case)
	 post_flag_check (KEEP = post_flag ptrans trans ptrans_first trans_first 
						     ptrans_mid trans_mid ptrans_last trans_last);
	MERGE dx_data.index03(IN=I)
		  dx_data.pstindex(IN=pst);
	BY hicno;
	IF I;

	/*INITIALIZE POST FLAGS*/
	post_flag = 0;

	/*CREATE COMPARISON FLAGS TO CHECK CHANGES*/
	ptrans = trans;
	ptrans_first = trans_first;
	ptrans_mid = trans_mid;
	ptrans_last = trans_last;

	/*FLAG IF SAME OR NEXT DAY ADMIT AND DIFFERENT PROVIDER*/
	IF pst AND 0 <= postadmit - disch <= 1
		   AND provid NE postprovid 
	  THEN DO;
		post_flag = 1;
		trans = 1;

		/*CHANGE TRANS_LAST CASE TO TRANS_MID*/
		IF trans_last = 1 THEN DO;
			trans_mid = 1;
			trans_last = 0;
		END;

		/*CHANGE NON TRANSFER CASE TO TRANS_FIRST*/
		ELSE IF ptrans=0 THEN trans_first = 1;

		/*OUTPUT CHECK DATASET*/
		OUTPUT post_flag_check;
	END;


	/*REFORMAT VARIABLES TO MATCH FINAL LAYOUT*/
	LENGTH	ddest2 ccuind2 drgcd2 icuind2 los2 mscd2 race2 3 provid2 pstate2 upin_at2 upin_op2 $6.
			nprocdt1-nprocdt&PRN. $8.;

	ddest2 = input(ddest, BEST12.);
	ccuind2 = ccuind;
	drgcd2 = input(drgcd, BEST12.);
	icuind2 = icuind;
	los2 = los;
	mscd2 = input(mscd, 3.);
	provid2 = substr(left(provid), 1, 6);
	pstate2 = substr(left(pstate), 1, 6);
	upin_at2 = substr(left(upin_at),1,6);
	upin_op2 = substr(left(upin_op),1,6);
	race2 = input(race, 3.);

	ARRAY PROC procdt1-procdt&PRN.;
	ARRAY NPRC nprocdt1-nprocdt1&PRN.;

	DO I = 1 TO &PRN.; 
		IF PROC(I) = . THEN NPRC(I)='';
		ELSE NPRC(I) = put(PROC(I),YYMMDDN8.);
	END;

	DROP ddest ccuind drgcd icuind los mscd provid pstate upin_at upin_op race 
		 procdt1-procdt&PRN.;

	RENAME	ddest2		= ddest
			ccuind2		= ccuind
			drgcd2		= drgcd 
			icuind2		= icuind 
			los2		= los
			mscd2		= mscd
			provid2		= provid
			pstate2		= pstate
			upin_at2	= upin_at
			upin_op2	= upin_op
			race2		= race
			nprocdt1-nprocdt&PRN. = procdt1-procdt&PRN.;


	LABEL
	/*DEFINE LABELS FOR INDEX FIELDS*/
		admit				='Admission date'
		admsour				='Source of admission'
		birth				='EDB date of birth'
		bstate				='Beneficiary State of Residence'
		case				='Case number marker'
		ccuind2				='Coronary Care Unit indicator'
		county				='Beneficiary County of Residence'
		ddest2				='Discharge destination code'
		death				='EDB date of death verified' 
		disch				='Discharge date'
		disst				='Medicare discharge status code'
		drgcd2				='Diagnosis Related Group'
		edbvdeth			='EDB Verified DOD Switch'
		group				='Transfer bundle indicator'
		hicno  				='Clean xrefd ID'
		history_case		='Revised case number for merging index and history files'
		icuind2				='Intensive Care Unit indicator'
		los2				='Length of stay'
		mdcl_rec			='Medical record number'
		mscd2				='Denom medicare status code'
		npi_at				='NPI of attending physician'
		npi_op				='NPI of operating physician'
		para				='Part A FFS enrollee at admission'
		para_b				='Part A and Part B FFS enrollee at admission'
		post_flag       	='Flag for transfers out at end of period'
		postmo				='Post-admission completeness indicator'
		postmo_a			='Post-admission completeness indicator - PTA only'
		postmod				='Post-discharge completenes indicator'
		postmod_a			='Post-discharge completeness indicator - PTA only'
		premo				='Pre-admission completeness indicator'
		premo_a				='Pre-admission completeness indicator - PTA only'
		provid2				='Medicare provider number'
		pstate2				='Medicare Provider state code'
		race2				='Bene race'	
		sex					='Beneficiary sex from edb'                                         
		trans				='1=stay is part of a transfer chain'
		trans_first			='First stay in a transfer chain'
		trans_last			='Last stay in a transfer chain'
		trans_mid			='Middle stay in a transfer chain'
		txflag 				="-1=transfer, 1=died, 0=otherwise"
		typeadm				='Type of admission'
		unreldmg 			='Unreliable age/gender indicator'                                  
		unreldth 			='Unreliable death indicator'                                       
		upin_at2			='UPIN of attending physician'
		upin_op2			='UPIN of operating physician'
		year				='Year cohort'
		zip					='Beneficiary Zipcode of Residence'
		%MACRO LABELCOUNTS(); /*USE LOOPS TO CREATE LABELS FOR SEQUENTIAL VARIABLES*/
			%DO I=1 %TO &DXN.;
				diag&I		="Diagnosis code #&I"
			%END;

			%DO I=1 %TO 25;
				poancd&I	="Diagnosis code present on admission indicator"
			%END;

			%DO I=1 %TO 12;
				%LET N=%SYSFUNC(PUTN(&I,Z2.));
				edgscd&N	="External cause of injury diagnosis code"
				poaend&N	="External cause of injury POA indicator"
			%END;

			%DO I=1 %TO &PRN.;
				proc&I		="Procedure code #&I"
				nprocdt&I	="Procedure date #&I"
			%END;
		%MEND LABELCOUNTS;
		%LABELCOUNTS;
	;

/*OUTPUT ALL RECORDS FOR THE READMISSION INDEX*/
	OUTPUT An_files.index_&DX._&YY.&YYE.;
RUN;

TITLE 'CHECK POST FLAG CHANGES';
PROC FREQ DATA=post_flag_check;
	TABLE post_flag*ptrans*trans*ptrans_first*trans_first*ptrans_mid*trans_mid*ptrans_last*trans_last /LIST MISSING;
RUN;

TITLE "An_files.index_&DX._&YY.&YYE. CONTENTS";
PROC CONTENTS DATA=An_files.index_&DX._&YY.&YYE.;
RUN;

 /*DELETE WORK DATASET TO CLEAN UP THE EG PROCESS FLOW*/
PROC DELETE DATA=post_flag_check; 
RUN;
TITLE;


/************************************************************************************************************
                               SAS Program Description

PROGRAM NAME: 7_CMS_IP_pre_diag

FOR USE WITH: HW

PURPOSE:      Identify CMS inpatient diag and proc history claims 

OVERVIEW: 	  select cases with a discharge up to 365 days before each index admit. Output one
			  record for each diagnosis code or procedure code in each case.

INPUT DATA: 
	data_pta.&pta_in_base_dataset.  *PRE-SORTED BY HICNO
	dx_data.indexMatch              *PRE-SORTED BY HICNO

OUTPUT FILES: 
	An_files.diaghistory_&DX._&YY.&YYE.

************************************************************************************************************/
/* CHECK MACRO VARIABLES */
%PUT DX (MEASURE VARIABLE)= &DX.;
%PUT DXN (MAX NUMBER OF DIAGNOSIS)= &DXN.;


**********************************************************************
* DETERMINE THE MAXIMUM NUMBER OF CASES IN BUNDLED CLAIMS.
**********************************************************************;
OPTIONS OBS=1;
DATA _NULL_;
	SET dx_data.indexMatch;
	CALL SYMPUT("MC", LEFT(PUT(index_max_case, 3.)));
RUN;
OPTIONS OBS=MAX;
%PUT MAX CASE FOR INDEX: &MC.;
**********************************************************************;


**********************************************************************
* MERGE INITIAL_INDEX_BUNDLED WITH CLAIMS AND OUTPUT PREVIOUS DIAGNOSES 
* TO PREDIAG FILE AND PREVIOUS PROCEDURES AND DATES TO PREPROC FILE.
**********************************************************************;
DATA prediag_ip (KEEP = hicno case diag DVRSND source fdate tdate MA);

	LENGTH hicno $15 case 3 source $7 diag $7;
	FORMAT fdate tdate DATE9.;

	MERGE dx_data.indexMatch(IN=I)
		  data_sty.&stay_dataset.(IN=B);
		/*data_pta.&pta_in_base_dataset.(IN=B); CHANGED THIS TO THE STAY DB */
	BY hicno;
	IF I AND B;

	ARRAY ADM(&MC)	admdt1-admdt&MC;
	ARRAY DIS(&MC)  disdt1-disdt&MC;
	ARRAY PVD(&MC)  provid1-provid&MC;

	ARRAY DXC(%eval(&DXN. + 12)) diag1-diag&DXN. edgscd01-edgscd12;
	ARRAY DXCFLAG(%eval(&DXN. + 12)) $  DVRSND01-DVRSND&DXN. EVRSCD01-EVRSCD12;

	LABEL 
			case			= "CASE Number"
			diag			= "Diagnosis code"
			fdate			= "Admission date (IP), Claim FROM_DT (OPD), Line First Expense Date (PTB)"
			hicno			= "CLEAN XREFD ID"
			source			= "4 digit source code"
			tdate			= "Discharge date (IP), Claim THRU DT (OPD), Line Last Expense Date (PTB)";


	DO I = 1 TO &MC;
		/*DAYS BETWEEN INDEX ADMISSION AND CLAIM DISCHARGE*/
		admdiff = ADM(I) - disch;

		/*CHECK FOR PRE DIAGNOSIS - ENSURE THAT THEY ARE NOT FROM THE INDEX ADMISSION*/
		IF (0 < admdiff <= 365 
			OR (admdiff=0 
				AND DIS(I)>0 
				AND (admit NE ADM(I) OR DIS(I) NE disch OR PVD(I) NE provid)))
		  THEN DO;
	        case = I;
	        fdate = admit;
	        tdate = disch;

	        /*OUTPUT ALL DIAGNOSIS*/
	        DO J = 1 TO %EVAL(&DXN. + 12);
				IF (DXC(J) NE '') THEN DO;
					DIAG = DXC(J);
					DVRSND = DXCFLAG(J);
					IF (J = 1) THEN DO;
						source = '0.0.1.0';
					END;
					ELSE DO;
						source = '0.0.2.0';
					END;
					OUTPUT prediag_ip;
				END;  /*DIAGNOSIS CHECK*/
			END;  /*DIAGNOSIS LOOP*/

	        /*OUTPUT ALL PROCEDURES*/

		END;  /*END PRE-ADM CHECK*/
	END;  /*END CASE NUMBER LOOP*/
RUN;

**********************************************************************
* DELETE DUPLICATED RECORDS FROM PREPROC FILE AS THERE ARE DUPLICATED
* PROCEDURE CODES ENTERED IN A MEDPAR RECORD.
**********************************************************************;
PROC SORT DATA = prediag_ip OUT = An_files.diaghistory_&DX._&YY.&YYE. NODUPKEY;
	BY hicno case diag source tdate;  
RUN;


**********************************************************************************
* PART II: Merge index hospital stay data set with post-index hospital stay data *
*          set, create readmission indicators and comorbidity variables          *
**********************************************************************************;
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

%MACRO CMS_HCC_GET(INDSN, OUTDSN, prefix); /* ZQ: add prefix 2/7/09 */
	DATA TEMP1;
    SET &INDSN;
	length nhic $18.;
	nhic=strip(hicno) || '_' || strip(put(case,$5.)); /* ZQ: changed to $5. on 2/7/09*/
    LENGTH ADDXG $6.;

    if DVRSND='0' then do;
	    ADDXG = PUT(ICD,$CCASS10V.);
		IF ICD in ('R0902', 'R0901') then ADDXG='84';
		IF ICD in ('I132') then ADDXG = '85';
	end;

	else if DVRSND='9' then do;
	    ADDXG = PUT(ICD,$CCASS22V.);
		IF ICD in ('79901','79902') then ADDXG = '84';
		IF ICD IN ('40403','40413','40493') THEN ADDXG = '85';
	end;
   
    KEEP HICno CASE ADDXG SOURCE nhic ICD AGE SEX DVRSND; /* ZQ: add age and sex */
	RUN;

    PROC SORT DATA=TEMP1 NODUP;
			 BY nHIC ADDXG ICD; 
	Run;

  DATA &OUTDSN(KEEP=HICno CASE &prefix.CC1-&prefix.CC201 nhic DVRSND) ERR;
    SET TEMP1;
	by nhic;
    length cc $6.;
    cc=left(addxg);

 IF DVRSND='9' THEN DO;
		  /* ES - NEW LOGIC TO CONFORM TO 2006 HCC FORMATS 2/23 */
	    _ICD_3 = SUBSTR(ICD,1,3);    *first 3 characters from ICD;
	    _ICD_4 = SUBSTR(ICD,1,4);    *first 4 characters from ICD;

	     * age restrictions;
	     IF AGE < 18 THEN DO;
	        /*emphysema chronic bronchitis */
	        IF _ICD_3 IN ('491','492','496') OR ICD IN ('5181','5182')
	        THEN CC='112';
	        ELSE
	        IF  _ICD_4 = '4932' THEN  CC='113';
	        /*chronic obstructive asthma */
	     END;

	      *esophageal atresia/stenosis, oth cong GI anomalies age<2;
	      IF AGE<2 THEN
	         IF ICD IN ('7503', '7504', '7507', '7508', '7509', '751',
	                      '7515', '7516', '75160','75162','75169','7517',
	                      '7518','7519')
	         THEN CC='182';

	      * age/sex restrictions;
	      SELECT;
	        /* males only  */
	         WHEN (( '185'<= _ICD_3 <='187'
	                 or _ICD_3 = '257'
	                 or '600'<= _ICD_3 <='608'
	                 or '7525'<=_ICD_4<='7526'
	                 or ICD='7528')
	               & SEX='2')                                  CC='-1.0';

	         /* females only */
	         WHEN ( (ICD='1121' OR _ICD_3='131'
	                             OR '179'<=_ICD_3<='184'
	                             OR _ICD_3='256'
	                             OR '614'<=_ICD_3<='627'
	                             OR _ICD_3='629'
	                             OR ICD='677')
	               & SEX='1')                                  CC='-1.0';


	        /*Infertility, Pregnancy DXGs Restricted to Females
	          Between Ages 8 and 59 */
	         WHEN ( (_ICD_3='628' OR
	               '630'<=_ICD_3<='676' OR
				   '678' <= _ICD_3 <='679' OR /*ZQ: add per RTI 2/7/09 */
	                _ICD_3 IN ('V22','V23','V24','V27','V28'))
	                & (SEX='1' OR AGE<8 OR AGE>59))          CC='-1.0';

	        /* newborns */
	         WHEN((ICD IN ('0900 ','0901 ','0902 ','7485 '
	                       '7505 ','7506 ','7511 ','7512 '
	                       '7513 ','7514 ','75161','7566 ') OR
	              '760' <=_ICD_3<='770' OR
	              '771 ' <=_ICD_4<='7717' OR
	              ICD IN ('7718','77182','77183','77189') OR
	              '772' <=_ICD_3<='779'  OR
	              _ICD_4='V213' OR
	              'V29' <=_ICD_3<='V39')
	                &  AGE>=2)                                CC='-1.0';
	         OTHERWISE;
	      END; *SELECT;
	END;


    RETAIN &prefix.CC1-&prefix.CC201 0 ;
    ATTRIB &prefix.CC1-&prefix.CC201  LENGTH=3.;
    ARRAY C(201)  &prefix.CC1-&prefix.CC201;

    IF CC NOT IN ('0.0 ',' 0.0','-1.0',' -1.') THEN DO;
      *- to find index for the array for current PHCC -;
      IND = INPUT(CC,8.);
      IF 1<= IND <= 201 THEN C(IND)=1;
      ELSE OUTPUT ERR;
    END;

	/* The following code was added by MPR on early 2007 to address the changes 
	in HCC between the old and new mapping/format algorithms. */;
   if DVRSND='0' then do;
   		IF ICD='I132'  THEN &prefix.CC85=1;
   end;
 
   if DVRSND='9' then do;
   		IF ICD IN ('40403','40413','40493') THEN &prefix.CC85=1;
   end;
   
/*----------------------End of codes from MPR -------------------------*/;
IF LAST.nHIC THEN DO;
   OUTPUT &OUTDSN;
  DO I=1 TO 201;
    C(I)=0; 
  END;
END;

*V22 REVISION 2016;
    LABEL
			&PREFIX.CC1=	"HIV/AIDS"
			&PREFIX.CC2=	"Septicemia, Sepsis, Systemic Inflammatory Response Syndrome/Shock"
			&PREFIX.CC3=	"Bacterial, Fungal, and Parasitic Central Nervous System Infections"
			&PREFIX.CC4=	"Viral and Late Effects Central Nervous System Infections"
			&PREFIX.CC5=	"Tuberculosis"
			&PREFIX.CC6=	"Opportunistic Infections"
			&PREFIX.CC7=	"Other Infectious Diseases"
			&PREFIX.CC8=	"Metastatic Cancer and Acute Leukemia"
			&PREFIX.CC9=	"Lung and Other Severe Cancers"
			&PREFIX.CC10=	"Lymphoma and Other Cancers"
			&PREFIX.CC11=	"Colorectal, Bladder, and Other Cancers"
			&PREFIX.CC12=	"Breast, Prostate, and Other Cancers and Tumors"
			&PREFIX.CC13=	"Other Respiratory and Heart Neoplasms"
			&PREFIX.CC14=	"Other Digestive and Urinary Neoplasms"
			&PREFIX.CC15=	"Other Neoplasms"
			&PREFIX.CC16=	"Benign Neoplasms of Skin, Breast, Eye"	
			&PREFIX.CC17=	"Diabetes with Acute Complications"
			&PREFIX.CC18=	"Diabetes with Chronic Complications"
			&PREFIX.CC19=	"Diabetes without Complication"
			&PREFIX.CC20=	"Type I Diabetes Mellitus"
			&PREFIX.CC21=	"Protein-Calorie Malnutrition"
			&PREFIX.CC22=	"Morbid Obesity"
			&PREFIX.CC23=	"Other Significant Endocrine and Metabolic Disorders"
			&PREFIX.CC24=	"Disorders of Fluid/Electrolyte/Acid-Base Balance"
			&PREFIX.CC25=	"Disorders of Lipoid Metabolism"
			&PREFIX.CC26=	"Other Endocrine/Metabolic/Nutritional Disorders"
			&PREFIX.CC27=	"End-Stage Liver Disease"
			&PREFIX.CC28=	"Cirrhosis of Liver"
			&PREFIX.CC29=	"Chronic Hepatitis"
			&PREFIX.CC30=	"Acute Liver Failure/Disease"
			&PREFIX.CC31=	"Other Hepatitis and Liver Disease"
			&PREFIX.CC32=	"Gallbladder and Biliary Tract Disorders"
			&PREFIX.CC33=	"Intestinal Obstruction/Perforation"
			&PREFIX.CC34=	"Chronic Pancreatitis"
			&PREFIX.CC35=	"Inflammatory Bowel Disease"
			&PREFIX.CC36=	"Peptic Ulcer, Hemorrhage, Other Specified Gastrointestinal Disorders"
			&PREFIX.CC37=	"Appendicitis"
			&PREFIX.CC38=	"Other Gastrointestinal Disorders"
			&PREFIX.CC39=	"Bone/Joint/Muscle Infections/Necrosis"
			&PREFIX.CC40=	"Rheumatoid Arthritis and Inflammatory Connective Tissue Disease"
			&PREFIX.CC41=	"Disorders of the Vertebrae and Spinal Discs"
			&PREFIX.CC42=	"Osteoarthritis of Hip or Knee"
			&PREFIX.CC43=	"Osteoporosis and Other Bone/Cartilage Disorders"
			&PREFIX.CC44=	"Congenital/Developmental Skeletal and Connective Tissue Disorders"
			&PREFIX.CC45=	"Other Musculoskeletal and Connective Tissue Disorders"
			&PREFIX.CC46=	"Severe Hematological Disorders"
			&PREFIX.CC47=	"Disorders of Immunity"
			&PREFIX.CC48=	"Coagulation Defects and Other Specified Hematological Disorders"
			&PREFIX.CC49=	"Iron Deficiency and Other/Unspecified Anemias and Blood Disease"
			&PREFIX.CC50=	"Delirium and Encephalopathy"
			&PREFIX.CC51=	"Dementia With Complications"
			&PREFIX.CC52=	"Dementia Without Complication"
			&PREFIX.CC53=	"Nonpsychotic Organic Brain Syndromes/Conditions"
			&PREFIX.CC54=	"Drug/Alcohol Psychosis"
			&PREFIX.CC55=	"Drug/Alcohol Dependence"
			&PREFIX.CC56=	"Drug/Alcohol Abuse, Without Dependence"
			&PREFIX.CC57=	"Schizophrenia"
			&PREFIX.CC58=	"Major Depressive, Bipolar, and Paranoid Disorders"
			&PREFIX.CC59=	"Reactive and Unspecified Psychosis"
			&PREFIX.CC60=	"Personality Disorders"
			&PREFIX.CC61=	"Depression"
			&PREFIX.CC62=	"Anxiety Disorders"
			&PREFIX.CC63=	"Other Psychiatric Disorders"
			&PREFIX.CC64=	"Profound Intellectual Disability/Developmental Disorder"
			&PREFIX.CC65=	"Severe Intellectual Disability/Developmental Disorder"
			&PREFIX.CC66=	"Moderate Intellectual Disability/Developmental Disorder"
			&PREFIX.CC67=	"Mild Intellectual Disability, Autism, Down Syndrome"
			&PREFIX.CC68=	"Other Developmental Disorders"
			&PREFIX.CC69=	"Attention Deficit Disorder"
			&PREFIX.CC70=	"Quadriplegia"
			&PREFIX.CC71=	"Paraplegia"
			&PREFIX.CC72=	"Spinal Cord Disorders/Injuries"
			&PREFIX.CC73=	"Amyotrophic Lateral Sclerosis and Other Motor Neuron Disease"
			&PREFIX.CC74=	"Cerebral Palsy"
			&PREFIX.CC75=	"Myasthenia Gravis/Myoneural Disorders and Guillain-Barre Syndrome/Inflammatory and Toxic Neuropathy"
			&PREFIX.CC76=	"Muscular Dystrophy"
			&PREFIX.CC77=	"Multiple Sclerosis"
			&PREFIX.CC78=	"Parkinson's and Huntington's Diseases"
			&PREFIX.CC79=	"Seizure Disorders and Convulsions"
			&PREFIX.CC80=	"Coma, Brain Compression/Anoxic Damage"
			&PREFIX.CC81=	"Polyneuropathy, Mononeuropathy, and Other Neurological Conditions/Injuries"
			&PREFIX.CC82=	"Respirator Dependence/Tracheostomy Status"
			&PREFIX.CC83=	"Respiratory Arrest"
			&PREFIX.CC84=	"Cardio-Respiratory Failure and Shock"
			&PREFIX.CC85=	"Congestive Heart Failure"
			&PREFIX.CC86=	"Acute Myocardial Infarction"
			&PREFIX.CC87=	"Unstable Angina and Other Acute Ischemic Heart Disease"
			&PREFIX.CC88=	"Angina Pectoris"
			&PREFIX.CC89=	"Coronary Atherosclerosis/Other Chronic Ischemic Heart Disease"
			&PREFIX.CC90=	"Heart Infection/Inflammation, Except Rheumatic"
			&PREFIX.CC91=	"Valvular and Rheumatic Heart Disease"
			&PREFIX.CC92=	"Major Congenital Cardiac/Circulatory Defect"
			&PREFIX.CC93=	"Other Congenital Heart/Circulatory Disease"
			&PREFIX.CC94=	"Hypertensive Heart Disease"
			&PREFIX.CC95=	"Hypertension"
			&PREFIX.CC96=	"Specified Heart Arrhythmias"
			&PREFIX.CC97=	"Other Heart Rhythm and Conduction Disorders"
			&PREFIX.CC98=	"Other and Unspecified Heart Disease"
			&PREFIX.CC99=	"Cerebral Hemorrhage"
			&PREFIX.CC100=	"Ischemic or Unspecified Stroke"
			&PREFIX.CC101=	"Precerebral Arterial Occlusion and Transient Cerebral Ischemia"
			&PREFIX.CC102=	"Cerebrovascular Atherosclerosis, Aneurysm, and Other Disease"
			&PREFIX.CC103=	"Hemiplegia/Hemiparesis"
			&PREFIX.CC104=	"Monoplegia, Other Paralytic Syndromes"
			&PREFIX.CC105=	"Late Effects of Cerebrovascular Disease, Except Paralysis"
			&PREFIX.CC106=	"Atherosclerosis of the Extremities with Ulceration or Gangrene"
			&PREFIX.CC107=	"Vascular Disease with Complications"
			&PREFIX.CC108=	"Vascular Disease"
			&PREFIX.CC109=	"Other Circulatory Disease"
			&PREFIX.CC110=	"Cystic Fibrosis"
			&PREFIX.CC111=	"Chronic Obstructive Pulmonary Disease"
			&PREFIX.CC112=	"Fibrosis of Lung and Other Chronic Lung Disorders"
			&PREFIX.CC113=	"Asthma"
			&PREFIX.CC114=	"Aspiration and Specified Bacterial Pneumonias"
			&PREFIX.CC115=	"Pneumococcal Pneumonia, Empyema, Lung Abscess"
			&PREFIX.CC116=	"Viral and Unspecified Pneumonia, Pleurisy"
			&PREFIX.CC117=	"Pleural Effusion/Pneumothorax"
			&PREFIX.CC118=	"Other Respiratory Disorders"
			&PREFIX.CC119=	"Legally Blind"
			&PREFIX.CC120=	"Major Eye Infections/Inflammations"
			&PREFIX.CC121=	"Retinal Detachment"
			&PREFIX.CC122=	"Proliferative Diabetic Retinopathy and Vitreous Hemorrhage"
			&PREFIX.CC123=	"Diabetic and Other Vascular Retinopathies"
			&PREFIX.CC124=	"Exudative Macular Degeneration"
			&PREFIX.CC125=	"Other Retinal Disorders"
			&PREFIX.CC126=	"Glaucoma"
			&PREFIX.CC127=	"Cataract"
			&PREFIX.CC128=	"Other Eye Disorders"
			&PREFIX.CC129=	"Significant Ear, Nose, and Throat Disorders"
			&PREFIX.CC130=	"Hearing Loss"
			&PREFIX.CC131=	"Other Ear, Nose, Throat, and Mouth Disorders"
			&PREFIX.CC132=	"Kidney Transplant Status"
			&PREFIX.CC133=	"End Stage Renal Disease"
			&PREFIX.CC134=	"Dialysis Status"
			&PREFIX.CC135=	"Acute Renal Failure"
			&PREFIX.CC136=	"Chronic Kidney Disease, Stage 5"
			&PREFIX.CC137=	"Chronic Kidney Disease, Severe (Stage 4)"
			&PREFIX.CC138=	"Chronic Kidney Disease, Moderate (Stage 3)"
			&PREFIX.CC139=	"Chronic Kidney Disease, Mild or Unspecified (Stages 1-2 or Unspecified)"
			&PREFIX.CC140=	"Unspecified Renal Failure"
			&PREFIX.CC141=	"Nephritis"
			&PREFIX.CC142=	"Urinary Obstruction and Retention"
			&PREFIX.CC143=	"Urinary Incontinence"
			&PREFIX.CC144=	"Urinary Tract Infection"
			&PREFIX.CC145=	"Other Urinary Tract Disorders"
			&PREFIX.CC146=	"Female Infertility"
			&PREFIX.CC147=	"Pelvic Inflammatory Disease and Other Specified Female Genital Disorders"
			&PREFIX.CC148=	"Other Female Genital Disorders"
			&PREFIX.CC149=	"Male Genital Disorders"
			&PREFIX.CC150=	"Ectopic and Molar Pregnancy"
			&PREFIX.CC151=	"Miscarriage/Terminated Pregnancy"
			&PREFIX.CC152=	"Completed Pregnancy With Major Complications"
			&PREFIX.CC153=	"Completed Pregnancy With Complications"
			&PREFIX.CC154=	"Completed Pregnancy With No or Minor Complications"
			&PREFIX.CC155=	"Uncompleted Pregnancy With Complications"
			&PREFIX.CC156=	"Uncompleted Pregnancy With No or Minor Complications"
			&PREFIX.CC157=	"Pressure Ulcer of Skin with Necrosis Through to Muscle, Tendon, or Bone"
			&PREFIX.CC158=	"Pressure Ulcer of Skin with Full Thickness Skin Loss"
			&PREFIX.CC159=	"Pressure Ulcer of Skin with Partial Thickness Skin Loss"
			&PREFIX.CC160=	"Pressure Pre-Ulcer Skin Changes or Unspecified Stage"
			&PREFIX.CC161=	"Chronic Ulcer of Skin, Except Pressure"
			&PREFIX.CC162=	"Severe Skin Burn or Condition"
			&PREFIX.CC163=	"Moderate Skin Burn or Condition"
			&PREFIX.CC164=	"Cellulitis, Local Skin Infection"
			&PREFIX.CC165=	"Other Dermatological Disorders"
			&PREFIX.CC166=	"Severe Head Injury"
			&PREFIX.CC167=	"Major Head Injury"
			&PREFIX.CC168=	"Concussion or Unspecified Head Injury"
			&PREFIX.CC169=	"Vertebral Fractures without Spinal Cord Injury"
			&PREFIX.CC170=	"Hip Fracture/Dislocation"
			&PREFIX.CC171=	"Major Fracture, Except of Skull, Vertebrae, or Hip"
			&PREFIX.CC172=	"Internal Injuries"
			&PREFIX.CC173=	"Traumatic Amputations and Complications"
			&PREFIX.CC174=	"Other Injuries"
			&PREFIX.CC175=	"Poisonings and Allergic and Inflammatory Reactions"
			&PREFIX.CC176=	"Complications of Specified Implanted Device or Graft"
			&PREFIX.CC177=	"Other Complications of Medical Care"
			&PREFIX.CC178=	"Major Symptoms, Abnormalities"
			&PREFIX.CC179=	"Minor Symptoms, Signs, Findings"
			&PREFIX.CC180=	"Extremely Immature Newborns, Including Birthweight < 1000 Grams"
			&PREFIX.CC181=	"Premature Newborns, Including Birthweight 1000-1499 Grams"
			&PREFIX.CC182=	"Serious Perinatal Problem Affecting Newborn"
			&PREFIX.CC183=	"Other Perinatal Problems Affecting Newborn"
			&PREFIX.CC184=	"Term or Post-Term Singleton Newborn, Normal or High Birthweight"
			&PREFIX.CC185=	"Major Organ Transplant (procedure)"
			&PREFIX.CC186=	"Major Organ Transplant or Replacement Status"
			&PREFIX.CC187=	"Other Organ Transplant Status/Replacement"
			&PREFIX.CC188=	"Artificial Openings for Feeding or Elimination"
			&PREFIX.CC189=	"Amputation Status, Lower Limb/Amputation Complications"
			&PREFIX.CC190=	"Amputation Status, Upper Limb"
			&PREFIX.CC191=	"Post-Surgical States/Aftercare/Elective"
			&PREFIX.CC192=	"Radiation Therapy"
			&PREFIX.CC193=	"Chemotherapy"
			&PREFIX.CC194=	"Rehabilitation"
			&PREFIX.CC195=	"Screening/Observation/Special Exams"
			&PREFIX.CC196=	"History of Disease"
			&PREFIX.CC197=	"Supplemental Oxygen"
			&PREFIX.CC198=	"CPAP/IPPB/Nebulizers"
			&PREFIX.CC199=	"Patient Lifts, Power Operated Vehicles, Beds"
			&PREFIX.CC200=	"Wheelchairs, Commodes"
			&PREFIX.CC201=	"Walkers";
RUN;

%MEND CMS_HCC_GET;

%macro HWR_model_variables();
***changed to reflect V22 CC new mappings **************;
Attrib HxInfection length=8. label='Severe Infection (CC 1, 3-6)';
Attrib OtherInfectious length=8. label='Other infectious disease & pneumonias (CC 7, 114-116)';
Attrib MetaCancer length=8. label='Metastatic cancer/acute leukemia (CC 8)';
Attrib SevereCancer length=8. label='Severe Cancer (CC 9, 10)';
Attrib OtherCancer length=8. label='Other Cancers (CC 11-14)';
Attrib Diabetes length=8. label='Diabetes mellitus (CC 17-19, 122, 123)';
Attrib Malnutrition length=8. label='Protein-calorie malnutrition (CC 21)';
Attrib LiverDisease length=8. label='End-stage liver disease (CC 27, 28)';
Attrib Hematological length=8. label='Severe Hematological Disorders (CC 46)';
Attrib Alcohol length=8. label='Drug/alcohol psychosis or dependence (CC 54-55)';
Attrib Psychological length=8. label='Psychiatric comorbidity (CC 57-59, 61, 63) ';
Attrib MotorDisfunction length=8. label='Hemiplegia, paraplegia, paralysis, functional disability (CC 70-74 103,104,189,190)';
Attrib Seizure length=8. label='Seizure disorders and convulsions (CC 79)';
Attrib CHF length=8. label='Congestive heart failure (CC 85)';
Attrib CADCVD length=8. label='Coronary atherosclerosis or angina, cerebrovascular disease (CC 86-89,102,105-109)';
Attrib Arrhythmias length=8. label='Specified arrhythmias and other heart rhythm disorders (CC 96-97)';
Attrib COPD length=8. label='Coronary obstructive pulmonary disease (COPD) (CC 111) ';
Attrib LungDisorder length=8. label='Fibrosis of lung or other chronic lung disorders (CC 112) ';
Attrib OnDialysis length=8. label='Dialysis status (CC 134)';
Attrib Ulcers length=8. label='Decubitus Ulcer or Chronic Skin Ulcer (CC 157-161) ';
Attrib Septicemia length=8. label='Septicemia, sepsis, systemic inflammatory response syndrome/shock (CC 2) ';
Attrib MetabolicDisorder length=8. label='Other significant endocrine and metabolic disorders; disorders of fluid/electrolyte/acid-base balance (CC 23-24)';
Attrib IronDeficiency length=8. label='Iron deficiency or Other Unspecified Anemias and Blood Disease (CC 49)';
Attrib CardioRespiratory length=8. label='Cardio-respiratory failure and shock (CC 84), plus ICD-10-CM codes R09.01 and R09.02';
Attrib RenalFailure length=8. label='Renal failure (CC 135-140)';
Attrib PancreaticDisease length=8. label='Pancreatic disease; peptic ulcer, hemorrhage, other specified gastrointestinal disorders (CC 34, 36)';
Attrib Arthritis length=8. label='Rheumatoid arthritis and inflammatory connective tissue disease (CC 40) ';
Attrib RespiratorDependence length=8. label='Respirator dependence/tracheostomy status (CC 82) ';
Attrib Transplants length=8. label='Transplants (CC 132, 186)';
Attrib Coagulopathy length=8. label='Coagulation defects and other specified hematological disorders (CC 48)';
Attrib HipFracture length=8. label='Hip fracture/dislocation (CC 170)';

If CC1 or CC3 or CC4 or CC5 or CC6 Then HxInfection=1;
	Else HxInfection=0;
If CC7 or CC114 or CC115 or CC116 Then OtherInfectious=1;
	Else OtherInfectious=0;
If CC8 Then MetaCancer=1;
	Else MetaCancer=0; 
If CC9 or CC10 Then SevereCancer=1;
	Else SevereCancer=0; 
If CC11 or CC12 or CC13 or CC14 Then OtherCancer=1;
	Else OtherCancer=0; 
If CC17 or CC18 or CC19 or CC122 or CC123 Then Diabetes=1;
	Else Diabetes=0;
If CC21 Then Malnutrition=1;
	Else Malnutrition=0;  
If CC27 or CC28 Then LiverDisease=1;
	Else LiverDisease=0; 
If CC46 Then Hematological=1;
	Else Hematological=0; 
If CC54 or CC55 Then Alcohol=1;
	Else Alcohol=0; 
If CC57 or CC58 or CC59 or CC61 or CC63 Then Psychological=1;
	Else Psychological=0;  
If CC70 or CC71 or CC72 or CC73 or CC74 or CC103 or CC104 or CC189 or CC190 Then MotorDisfunction=1;
	Else MotorDisfunction=0; 
If CC79 Then Seizure=1;
	Else Seizure=0;
If CC85 Then CHF=1;
	Else CHF=0; 
If CC86 or CC87 or CC88 or CC89 or CC102 or CC105 or CC106 or CC107 or CC108 or CC109 Then CADCVD=1;
	Else CADCVD=0; 
If CC96 or CC97 Then Arrhythmias=1;
	Else Arrhythmias=0; 
If CC111 Then COPD=1;
	Else COPD=0; 
If CC112 Then LungDisorder=1;
	Else LungDisorder=0; 
If CC134 Then OnDialysis=1;
	Else OnDialysis=0; 
If CC157 or CC158 or CC159 or CC160 or CC161 Then Ulcers=1;
	Else Ulcers=0; 
If CC2 Then Septicemia=1;
	Else Septicemia=0;	
If CC23 or CC24 Then MetabolicDisorder=1;
	Else MetabolicDisorder=0;  
If CC49 Then IronDeficiency=1;
	Else IronDeficiency=0;
If CC84 Then CardioRespiratory=1;
	Else CardioRespiratory=0; 
If CC135 or CC136 or CC137 or CC138 or CC139 or CC140 Then RenalFailure=1;
	Else RenalFailure=0; 
If CC34 or CC36 Then PancreaticDisease=1;
	Else PancreaticDisease=0; 
If CC40 Then Arthritis=1;
	Else Arthritis=0; 
If CC82 Then RespiratorDependence=1;
	Else RespiratorDependence=0; 
If CC132 or CC186 Then Transplants=1;
	Else Transplants=0;  
If CC48 Then Coagulopathy=1;
	Else Coagulopathy=0; 
If CC170 Then HipFracture=1;
	Else HipFracture=0; 
%mend HWR_model_variables;

%macro HWR_model_Condition_Indicator();

ATTRIB CONDITION LENGTH=$4.;
condition = addxg;

/* MEDICINE */
IF COHORT = 'MEDICINE' THEN DO;
	IF ADDXGnum IN (&med_lfaddxg) THEN CONDITION='000';
	ELSE CONDITION=ADDXG;
END;

/* SURGICAL */
ELSE IF COHORT = 'SURGICAL' THEN DO;
	IF ADDXGnum IN (&surg_lfaddxg) THEN CONDITION='000';
	ELSE CONDITION=ADDXG;
END;

/* CARDIOSRESPIRATORY */
ELSE IF COHORT = 'CARDIORESPIRATORY' THEN DO;
	IF ADDXGnum IN (&cardio_lfaddxg) THEN CONDITION='000';
	ELSE CONDITION=ADDXG;
END;

/* CV */
ELSE IF COHORT = 'CV' THEN DO;
	IF ADDXGnum IN (&CV_lfaddxg) THEN CONDITION='000';
	ELSE CONDITION=ADDXG;
END;

/* NEUROLOGY */
ELSE IF COHORT ='NEUROLOGY' THEN DO;
	IF ADDXGnum IN (&neuro_lfaddxg) THEN CONDITION='000';
	ELSE CONDITION=ADDXG;
END;

%mend HWR_model_Condition_Indicator;

%MACRO HCCPAI(INDSN, OUTDSN);
  DATA &OUTDSN;
    SET &INDSN;
	length ICD $7;
    AGE=INT((ADMIT-BIRTH)/365.25);
	SEX="" || CSEX;
    SOURCE='0.0.2.0'; /*ZQ: source changed 2/7/09 */
	ARRAY ICDCODE{*} $ DIAG1-DIAG10 EDGSCD01-EDGSCD12;
 	ARRAY ICDFLAG{1:22} $  DVRSND01-DVRSND10 EVRSCD01-EVRSCD12;
    DO I=2 TO dim(ICDCODE);
      ICD=ICDCODE[I];
      DVRSND=ICDFLAG[I];
      if ICD not in ('', ' ') then output;
    END;
 
    KEEP HICno CASE AGE SEX ICD SOURCE DVRSND;
  RUN;
%MEND HCCPAI;


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

/* ELIMINATE ADMISSIONS THAT APPEAR TWICE (ACROSS YEARS) */
PROC SORT DATA=INDEX NODUPKEY DUPOUT=QA_DupOut EQUALS;
	BY HICNO ADMIT DISCH PROVID;
RUN;

/* IDENTIFY AND COMBINE TWO ADJACENT AMI ADMISSIONS (disch1=admit2), USE DISCHARGE DATE OF 2ND ADMISSION 
   TO REPLACE DISCHARGE DATE OF 1ST ADMISSION (disch1=disch2), SAME FOR DISCHARGE STATUS, TRANS_FIRST, 
   TRANS_MID, POSTMOD. ALSO, CREATE CASE_P TO BE USED FOR FINDING READMISSION. THIS WORKS WHEN THERE ARE 
   MORE THAN TWO ADJACENT AMI ADMISSIONS. */
DATA TEMP; 
	SET INDEX;
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

/* RADM30ALL: ANY READMISSION WITHIN 30 DAYS */
IF NOT B THEN RADM30ALL=0;
ELSE IF 0 <= _ADMIT - DISCH <=30 THEN RADM30ALL=1;
ELSE IF _ADMIT - DISCH > 30 THEN RADM30ALL=0;

INTERVAL=_ADMIT - DISCH;
SAME=(PROVID=_PROVID); /* SAME HOSPITAL READMISSION */
RADM30=RADM30ALL;

radm30p=0;
if planned =1 and RADM30=1 then do;
RADM30 = 0;
radm30p = 1;
end;

if _DVRSND01='0' then do;
	/* any readmission with principal diagnosis eq V57  is not counted as readmission, added 1/11/2010. ZQ */
	if upcase(_diag1) in ('Z5189')  then Radm_rehab=1;
	else Radm_rehab=0;

	/* any readmission with psych principal diagnosis eq in range of 290-319 that was within 1 day of the discharge date of index admission with discharge dispostion
		eq 65 is not counted as readmission, added 1/11/2010. ZQ */ 

	/*(1) the admission being evaluated as a potential readmission has a psychiatric principal discharge diagnosis code (ICD-9-CM codes beginning with 29, 30 or 31, 
	                 for discharges prior to October 1, 2015, or ICD-10-CM codes beginning with F, for discharges on or after October 1, 2015),  added 05/31/2018. MQ*/
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
if radm_rehab=1 and (radm30=1 or radm30p = 1) then do; radm30=0; radm30p = 0; interval = 999;  end;
if radm_psy=1 and (radm30=1 or radm30p = 1) then do; radm30=0; radm30p = 0; interval = 999;	end;

hicno_case=strip(hicno)||strip(case_p);

* PART OF TRANS BUNDLE, SHOULD HAVE BEEN EXCLUDED - *Updated on 06/19/18;
If RADM30=1 and interval=0 and same=1 and diag1=_diag1 then do;
Bundle=1;
Sample=0;
end;

DROP I;

IF ADMIT=_ADMIT AND DISCH=_DISCH AND PROVID=_PROVID AND DIAG1=_DIAG1 THEN OUTPUT QA_DupIndex; 
ELSE OUTPUT readm1;

run;

proc sort data=readm1; by  hicno_case interval _disch; run;

data readm1data(drop=hicno_case);
set readm1;
by hicno_case;
if first.hicno_case;
run;

proc sort data=readm1data; by hicno case; run;

DATA sample;
set readm1data /*(where=(sample=1))*/;
csex=put(sex, $1.);
attrib addxgnum length=8.; /* added 4/30, zq */
addxgnum=input(addxg, 8.);
drop sex;
RUN;
  
**********DETERMINE GRAB BAG CATEGORIES _ ZQ MAY 2012 ***************************************************;
proc sql;
create table temp_indicator as
select distinct subcategory, addxg, count(hicno) as addxgvol
from sample
group by subcategory, addxg
having addxgvol < 1000;
quit;

proc sql noprint;
select unique(addxg)
into:  med_lfaddxg separated by ','
from temp_indicator
where subcategory='Medicine';

select unique(addxg)
into:  surg_lfaddxg separated by ','
from temp_indicator
where subcategory='Surgical';

select unique(addxg)
into:  cardio_lfaddxg separated by ','
from temp_indicator
where subcategory='Cardiorespiratory';

select unique(addxg)
into:  cv_lfaddxg separated by ','
from temp_indicator
where subcategory='CV';

select unique(addxg)
into:  neuro_lfaddxg separated by ','
from temp_indicator
where subcategory='Neurology';

quit;

DATA _NULL_;

%LET MFILLER=0;

	IF SYMEXIST('MED_LFADDXG')=0 THEN DO;
		CALL SYMPUT('MED_LFADDXG', '&MFILLER');
		END;
	IF SYMEXIST('SURG_LFADDXG')=0 THEN DO;
		CALL SYMPUT('SURG_LFADDXG', '&MFILLER');
		END;
	IF SYMEXIST('CV_LFADDXG')=0 THEN DO;
		CALL SYMPUT('CV_LFADDXG', '&MFILLER');
		END;
	IF SYMEXIST('CARDIO_LFADDXG')=0 THEN DO;
		CALL SYMPUT('CARDIO_LFADDXG', '&MFILLER');
		END;
	IF SYMEXIST('NEURO_LFADDXG')=0 THEN DO;
		CALL SYMPUT('NEURO_LFADDXG', '&MFILLER');
		END;

RUN;

*******************************************************************************************;
DATA HXDIAG;
SET &HXDIAG &TRANS_DIAG(IN=B);
if source in ('0.0.1.0','0.0.2.0'); /* only ip history available */
RUN;

PROC SQL;
CREATE TABLE HXDIAG_2 AS
SELECT SAMPLE.HICNO, SAMPLE.CASE, SAMPLE.AGE, SAMPLE.CSEX AS SEX,
	HXDIAG.DIAG, HXDIAG.FDATE,
	HXDIAG.SOURCE,
	HXDIAG.TDATE, HXDIAG.YEAR, HXDIAG.DVRSND
FROM SAMPLE LEFT JOIN HXDIAG
	ON SAMPLE.HICNO=HXDIAG.HICNO AND SAMPLE.HISTORY_CASE=HXDIAG.CASE;
QUIT;

%HCCPAI(sample, &CONDITION._PA0);   

data  &CONDITION._PA2 &CONDITION._PA1;
set hxdiag_2 (keep=hicno case AGE SEX diag source DVRSND where=(diag^=''));
attrib icd length=$7.;  *change to 7 for new diag 5010 format - JNG 1/4/12;

icd=diag;

if source in ('0.0.1.0') then output &CONDITION._PA1;
else if source in ('0.0.2.0') then output &CONDITION._PA2;
Run;

PROC DELETE DATA=all bundle; RUN;

%cms_hcc_get(&CONDITION._PA0, &CONDITION._PA0_CC, PA0);
%cms_hcc_get(&CONDITION._PA1, &CONDITION._PA1_CC, PA1);
%cms_hcc_get(&CONDITION._PA2, &CONDITION._PA2_CC, PA2);

PROC SORT DATA=&CONDITION._PA0_CC; BY HICNO CASE; RUN;
PROC SORT DATA=&CONDITION._PA2_CC; BY HICNO CASE; RUN;
PROC SORT DATA=&CONDITION._PA1_CC; BY HICNO CASE; RUN;

data &ANALYSIS;
merge 	sample (in=a)
		&CONDITION._PA2_CC
		&CONDITION._PA1_CC
		&CONDITION._PA0_CC;
by hicno case;
if a;

ARRAY PA0{1:201} PA0CC1-PA0CC201;
ARRAY PA2{1:201} PA2CC1-PA2CC201;
ARRAY PA1{1:201} PA1CC1-PA1CC201;
ARRAY CC{1:201}  CC1 - CC201;

/* HF readmission approach for Complication Derivation */
ARRAY COMP{1:56} PA0CC2   PA0CC7   PA0CC17  PA0CC24  PA0CC30 PA0CC33 PA0CC36 
				 PA0CC48  PA0CC50  PA0CC80  PA0CC82  PA0CC83 PA0CC84 PA0CC85
			     PA0CC86  PA0CC87  PA0CC96  PA0CC97  
				 PA0CC99  PA0CC100 PA0CC101 PA0CC103 PA0CC104   
				 PA0CC106 PA0CC107 PA0CC108 PA0CC109 PA0CC114 PA0CC115
				 PA0CC117 PA0CC134 PA0CC135 PA0CC140 PA0CC141 PA0CC142 PA0CC144
				 PA0CC157 PA0CC158 PA0CC159 PA0CC160 
				 PA0CC164 PA0CC166 PA0CC167 PA0CC168 PA0CC170
				 PA0CC171 PA0CC173 PA0CC175 PA0CC176 PA0CC177 
				 PA0CC186 PA0CC187 PA0CC188 PA0CC189 PA0CC190 PA0CC191;
				      
DO I=1 TO 56;
	IF COMP(I)=1 THEN COMP(I)=0;
END;


DO I=1 TO 201;
	IF PA0[I]=. THEN PA0[I]=0;
	IF PA2[I]=. THEN PA2[I]=0;
	IF PA1[I]=. THEN PA1[I]=0;
	CC[I]=PA0[I] OR PA2[I] OR PA1[I];
END;

Attrib Cohort length=$18.;

IF subcategory='Cardiorespiratory'  THEN Cohort='CARDIORESPIRATORY';
ELSE IF subcategory='CV' THEN Cohort='CV';
ELSE IF subcategory='Neurology' THEN Cohort='NEUROLOGY';
ELSE IF subcategory='Medicine' THEN Cohort='MEDICINE';
ELSE IF  subcategory='Surgical' then Cohort='SURGICAL';

%HWR_model_variables();
%HWR_model_Condition_Indicator();

If Cohort in ('SURGICAL', 'MEDICINE', 'CV', 'CARDIORESPIRATORY', 'NEUROLOGY');	
KEEP &MODEL_VAR HICNO CASE ADMIT DISCH PROVID COHORT CATEGORY condition radm30 
	 radm30p obs30_mort DVRSND;
RUN;

data &all;
merge readm1data (in=a) &analysis (keep=hicno case &model_var condition cohort DVRSND);
by hicno case;
if a;
run;

data &all._&YY.&YYE.(drop = _diag1 _disch _proc1 _proc2 _proc3 _proc4 _proc5 _proc6 _admit _provid);
set &all;
run;

data &analysis._&YY.&YYE.;
set &analysis;
run;


/***************************************************************************
* Goal: Generate final discharge level unplanned readmission file;
* Input dataset: &all._&YY.&YYE.
* Output dataset: an_files.&condition._readm_all_&YY.&YYE.
/***************************************************************************/

* Criteria 1: Drop discharges whose beneficiary was younger than 65; 
* Criteria 2: Keep discharges whose patient survives hospitalization;
* Criteria 3: Admissions for patients without at least 30 days of post-discharge
              enrollement in FFS Medicare should be dropped;
* Criteria 4: Admissions for patients not continuously enrolled in FFS Medicare
              for the 12 months prior to the inde admission are excluded;
* Criteria 5: Patients discharged against medical advice (AMA) are excluded;
* Criteria 6: Discharges to a PPS-exempt cancer hospital are excluded;
* Criteria 7: Admissions for medical treatment of cancer are excluded;
* Criteria 8: Admissions for primary psychiatric disease are excluded;
* Criteria 9: Admissions for rehabilitation care are excluded; 
* Criteria 10: Admissions for patients transferred among hospitals are excluded;

/* Rename readmission flages generated in the last steps */
DATA &all._&YY.&YYE.;
	SET &all._&YY.&YYE.;
	rename RADM30=RADM30_UNPLANNED	RADM30p=RADM30_PLANNED;
RUN;

data an_files.&condition._readm_all_&YY.&YYE._final (drop = obs30_mort);
set &all._&YY.&YYE.;
/* AGE65: equals to 1 if beneficiary at least 65 years old */
if age65 NE 1 then RADM30 = .;
/* DEAD: in hospital death*/
else if dead = 1 then RADM30 = .;
/* POSTMOD_A: continuous number of months enrolled in Medicare Part A and not 
enrolled in HMO within 1 month after discharge 
POST1: POST1=1 if POSTMOD_A=1 */
else if POST1 NE 1 then RADM30 = .;
/* PREMO_A: continuous number of months enrolled in Medicare Part A and not 
enrolled in HMO within 12 months before admission */
else if PRIOR12 NE 1 then RADM30 = .;
/* AMA: equals to 1 if patients are discharged against medical advice */
else if ama = 1 then RADM30 = .;
/* CANCER_HOSP: equals to 1 if it is admission to a PPS-exempt cance hospital */
else if cancer_hosp = 1 then RADM30 = .;
/* SUBCATEGORY: admission goals based on category code */
else if subcategory = 'Cancer' then RADM30 = .;
/* PSYCHEXCL: equals to 1 if it is admission for psychiatric disease */
else if psychexcl = 1 then RADM30 = .;
/* REHABEXCL: equals to 1 if it is admission for rehabilitation */
else if rehabexcl = 1 then RADM30 = .;
/* TRANS_COMBINE: equals to 1 if it is records within a transfer chain, no
matter if the provider is the same or not within this chain, except for the
last record of this chain*/
else if trans_combine = 1 then RADM30 = .;
else RADM30 = RADM30_unplanned;
run;

/* Variable Analysis */
proc means data=an_files.&condition._readm_all_&YY.&YYE._final;
var RADM30 age65 dead POST1 PRIOR12 ama cancer_hosp psychexcl rehabexcl trans_combine;
run;

proc freq data=an_files.&condition._readm_all_&YY.&YYE._final;
table RADM30 age65 dead POST1 PRIOR12 ama cancer_hosp psychexcl rehabexcl trans_combine subcategory;
run;

proc means data=an_files.&condition._readm_all_&YY.&YYE._final;
var radm30all RADM30_planned RADM30_unplanned RADM30 hxinfection otherinfectious metacancer 
severecancer othercancer diabetes malnutrition liverdisease hematological 
alcohol psychological motordisfunction seizure chf cadcvd arrhythmias copd 
lungdisorder ondialysis ulcers septicemia metabolicdisorder irondeficiency 
cardiorespiratory renalfailure pancreaticdisease arthritis respiratordependence
transplants coagulopathy hipfracture;
run;

data readm_all_final;
set an_files.&condition._readm_all_&YY.&YYE._final;
year1 = year(disch);
run;

/* Analyze the eligible readmission flag by year variable */
proc freq data=readm_all_final;
	table RADM30*year;
run;

proc freq data=readm_all_final;
	table RADM30*year1;
run;

proc means data=readm_all_final;
	var hxinfection otherinfectious metacancer 
		severecancer othercancer diabetes malnutrition liverdisease hematological 
		alcohol psychological motordisfunction seizure chf cadcvd arrhythmias copd 
		lungdisorder ondialysis ulcers septicemia metabolicdisorder irondeficiency 
		cardiorespiratory renalfailure pancreaticdisease arthritis respiratordependence
		transplants coagulopathy hipfracture;
	class year1;
run;

proc delete data = readm_all_final; run;

proc means data=an_files.&condition._readm_all_&YY.&YYE._final;
var PREMO_A POSTMOD_A;
run;

/* label all variables */
data an_files.&condition._readm_&YY.&YYE._final;
	set an_files.&condition._readm_all_&YY.&YYE._final;
	LABEL
		RADM30ALL = 'All Readmission (planned and unplanned)'
		RADM30_PLANNED = 'Planned Readmission'
		RADM30_UNPLANNED = 'Unplanned Readmission'
		RADM30 = 'Eligible Readmission'
		RADM_Rehab = 'Readmission for Rehabilitation'
		RADM_Psy = 'Readmission for Psychiatric Disease'
		Combine = 'If the discharge has overlap with others'
		INTERVAL = 'Diff between admission and last discharge'
		SAME = 'If the provider is the same as last discharge'
		condition = 'Hierarchical Condition Category'
		%MACRO labelcounts();
			%DO I=1 %TO &DXN;
				procccp_&I   = "Proc CCS Group #&I"
				%END;
			%MEND labelcounts;
			%labelcounts;;
run;

/* Create dataset used in Get Discharge Info.sas */
DATA final.hw_readm_&YY.&YYE._final;
	SET an_files.hw_readm_&YY.&YYE._final (drop=MDCL_REC TXFLAG trans case trans_first trans_last trans_mid case_p TRANSFER_OUT AGE_65 ADDXG DCGDIAG
			proccc1-proccc6 rehabexcl category subcategory all
			procccp_1 procccp_2 procccp_3 procccp_4 procccp_5 procccp_6 planned SAME Cohort ophtho vascular ortho gen ct uro neuro obgyn plastic ent psychexcl j surg_sum);
RUN;

/* Create preventable readmission indicator based on diagnosis codes */
data an_files.hw_readm_&YY.&YYE._final;
	set final.hw_readm_&YY.&YYE._final;
	array ICD(1:10) $ DIAG1-DIAG10;
	array PRCDR(1:6) $ PROC1-PROC6;
	PQI01=0; PQI02=0; PQI05=0; PQI07=0; PQI08=0; PQI10=0; PQI11=0; PQI12=0; PQI14=0;

	/* Inclusion Criteria */
	DO I=1;
		if DVRSND01='0' then do;
			if ICD(I) in ('E1010','E1011','E10641','E1065','E1100','E1101','E11641','E1165','E1300','E1301','E1310','E1311','E13641') then PQI01=1;
			if ICD(I) in ('K352','K353') then PQI02=1;
			if ICD(I) in ('J410','J411','J418','J42','J430','J431','J432','J438','J439','J440','J441','J449','J470','J471','J479','J4520','J4521','J4522','J4530','J4531','J4532','J4540','J4541',
	                      'J4542','J4550','J4551','J4552','J45901','J45902','J45909','J45990','J45991','J45998','J200','J201','J202','J203','J204','J205','J206','J207','J208','J209','J40') then PQI05=1;
			if ICD(I) in ('I10','I119','I129','I1310') then PQI07=1;
			if ICD(I) in ('I0981','I501','I5020','I5021','I5022','I5023','I5030','I5031','I5032','I5033','I5040','I5041','I5042','I5043','I509') then PQI08=1;
			if ICD(I) in ('E860','E861','E869','E870','A080','A0811','A0819','A082','A0831','A0832','A0839','A084','A088','A09','K5289','K529','N170','N171','N172','N178','N179','N19','N990') then PQI10=1;
			if ICD(I) in ('J13','J14','J15211','J15212','J153','J154','J157','J159','J160','J168','J180','J181','J188','J189') then PQI11=1;
			if ICD(I) in ('N10','N119','N12','N136','N151','N159','N16','N2884','N2885','N2886','N3000','N3001','N3090','N3091','N390') then PQI12=1;
			if ICD(I) in ('E1165','E1065','E10649','E11649') then PQI14=1;
		end;

		if DVRSND01='9' then do;
		   	if ICD(I) in ('25010','25011','25012','25013','25020','25021','25022','25023','25030','25031','25032','25033') then PQI01=1;
			if ICD(I) in ('5400','5401') then PQI02=1;
			if ICD(I) in ('4910','4911','49120','49121','49122','4918','4919','4920','4928','494','4940','4941','496','49300','49301','49302',
						  '49310','49311','49312','49320','49321','49322','49381','49382','49390','49391','49392') then PQI05=1;
			if ICD(I) in ('4010','4019','40200','40210','40290','40300','40310','40390','40400','40410','40490') then PQI07=1;
			if ICD(I) in ('39891','40201','40211','40291','40401','40403','40411','40413','40491','40493','4280','4281','42820','42821','42822',
						  '42823','42830','42831','42832','42833','42840','42841','42842','42843','4289') then PQI08=1;
			if ICD(I) in ('2765','27651','27650','27652','2760','00861','00862','00863','00864','00865','00866','00867','00869','0088','0090',
						  '0091','0092','0093','5589','5845','5846','5847','5848','5849','586','9975') then PQI10=1;
			if ICD(I) in ('481','4822','48230','48231','49232','48239','48240','48241','48242','48249','4829','4830','4831','4838','485','486') then PQI11=1;
			if ICD(I) in ('59010','59011','5902','5903','59080','59081','5909','5950','5959','5990') then PQI12=1;
			if ICD(I) in ('25002','25003') then PQI14=1;
		end;
	END;

	/* Exclusion Critera */
	DO I=1 to 10;
		if DVRSND01='0' then do;
			if ICD(I) in ('E840','E8411','E8419','E848','E849','J8483','J84841','J84842','J84843','J84848','P270','P271','P278','P279','Q254','Q311','Q312','Q313','Q315','Q318','Q319','Q320',
	                        'Q321','Q322','Q323','Q324','Q330','Q331','Q332','Q333','Q334','Q335','Q336','Q338','Q339','Q340','Q341','Q348','Q349','Q390','Q391','Q392','Q393','Q394','Q893') 
	                        then PQI05=0;
			if ICD(I) in ('I129','I1310') then PQI07=0;
			if ICD(I) in ('I120','I1311','I132','N185','N186') then PQI10=0;
			if ICD(I) in ('D5700','D5701','D5702','D571','D5720','D57211','D57212','D57219','D5740','D57411','D57412','D57419','D5780','D57811','D57812','D57819') then PQI11=0;
			if ICD(I) in ('N110','N111','N118','N1370','N1371','N13721','N13722','N13729','N13731','N13732','N13739','N139','Q600','Q601','Q602','Q603','Q604','Q605','Q606','Q6100','Q6101','Q6102','Q6111','Q6119','Q612',
	                      'Q613','Q614','Q615','Q618','Q619','Q620','Q6210','Q6211','Q6212','Q622','Q6231','Q6232','Q6239','Q624','Q625','Q6260','Q6261','Q6262','Q6263','Q6269','Q627','Q628','Q630','Q631','Q632','Q633',
	                      'Q638','Q639','Q6410','Q6411','Q6412','Q6419','Q642','Q6431','Q6432','Q6433','Q6439','Q645','Q646','Q6470','Q6471','Q6472','Q6473','Q6474','Q6475','Q6479','Q648','Q649') then PQI12=0;
			/* Exclude immunocompromised state diagnsis codes */
			if ICD(I) in ('B20','B59','C802','C888','C9440','C9441','C9442','C946','D4622','D471','D479','D47Z1','D47Z9','D6109','D61810','D61811','D61818','D700','D701','D702','D704','D708','D709','D71','D720','D72810',
	                      'D72818','D72819','D7381','D7581','D761','D762','D763','D800','D801','D802','D803','D804','D805','D806','D807','D808','D809','D810','D811','D812','D814','D816','D817','D8189','D819','D820','D821',
	                      'D822','D823','D824','D828','D829','D830','D831','D832','D838','D839','D840','D841','D848','D849','D893','D89810','D89811','D89812','D89813','D8982','D8989','D899','E40','E41','E42','E43','I120',
	                      'I1311','I132','K912','M359','N185','N186','T8600','T8601','T8602','T8603','T8609','T8610','T8611','T8612','T8613','T8619','T8620','T8621','T8622','T8623','T86290','T86298','T8630','T8631','T8632',
	                      'T8633','T8639','T8640','T8641','T8642','T8643','T8649','T865','T86810','T86811','T86812','T86818','T86819','T86830','T86831','T86832','T86838','T86839','T86850','T86851','T86852','T86858','T86859',
	                      'T86890','T86891','T86892','T86898','T86899','T8690','T8691','T8692','T8693','T8699','Z4821','Z4822','Z4823','Z4824','Z48280','Z48290','Z48298','Z4901','Z4902','Z4931','Z940','Z941','Z942','Z943',
	                      'Z944','Z9481','Z9482','Z9483','Z9484','Z9489','Z992') then DO; PQI11=0; PQI12=0; END;
		end;

		if DVRSND01='9' then do;
			 if ICD(I) in ('27700','27701','27702','27703','27709','51661','51662','51663','51664','51669','74721','7483','7484','7485','74860','74861',
							'74869','7488','7489','7503','7593','7707') then PQI05=0;
			 if ICD(I) in ('40300','40310','40390','40400','40410','40490') then PQI07=0;
			 if ICD(I) in ('40301','40311','40391','40402','40403','40412','40413','40492','40493','5855','5856') then PQI10=0;
			 if ICD(I) in ('28241','28242','28260','28261','28262','28263','28264','28268','28269') then PQI11=0;
			 if ICD(I) in ('59000','59001','59370','59371','59372','59373','7530','75310','75311','75312','75313','75314','75315','75316','75317','75319',
							'75320','75321','75322','75323','75329','7533','7534','7535','7536','7538','7539') then PQI12=0;
			/* Exclude immunocompromised state diagnsis codes */
			 if ICD(I) in ('042','1363','1992','23873','23876','23877','23879','260','261','262','27900','27901','27902','27903','27904','27905','27906',
							'27909','27910','27911','27912','27913','27919','2792','2793','2794','27941','27949','27950','27951','27952','27953','2798',
							'2799','28409','2841','28411','28412','28419','2880','28800','28801','28802','28803','28809','2882','2884','28850','28851',
							'28859','28953','28983','40301','40311','40391','40402','40403','40412','40413','40492','40493','5793','585','5855','5856',
							'9968','99680','99681','99682','99683','99684','99685','99686','99687','99688','99689','V420','V421','V426','V427','V428',
							'V4281','V4282','V4283','V4284','V4289','V451','V4511','V560','V561','V562') then DO; PQI11=0; PQI12=0; END;
		end;
	END;
	/* PQI07 procedures for exclusion */
	DO I=1 to 6;
		if DVRSND01='0' then do;
			if PRCDR(I) in ('031209D','031209F','03120AD','03120AF','03120JD','03120JF','03120KD','03120KF','03120ZD','03120ZF','031309D','031309F','03130AD','03130AF','03130JD','03130JF','03130KD','03130KF','03130ZD',
	                        '03130ZF','031409D','031409F','03140AD','03140AF','03140JD','03140JF','03140KD','03140KF','03140ZD','03140ZF','031509D','031509F','03150AD','03150AF','03150JD','03150JF','03150KD','03150KF',
	                        '03150ZD','03150ZF','031609D','031609F','03160AD','03160AF','03160JD','03160JF','03160KD','03160KF','03160ZD','03160ZF','031709D','031709F','03170AD','03170AF','03170JD','03170JF','03170KD',
	                        '03170KF','03170ZD','03170ZF','031809D','031809F','03180AD','03180AF','03180JD','03180JF','03180KD','03180KF','03180ZD','03180ZF','031909F','03190AF','03190JF','03190KF','03190ZF','031A09F',
	                        '031A0AF','031A0JF','031A0KF','031A0ZF','031B09F','031B0AF','031B0JF','031B0KF','031B0ZF','031C09F','031C0AF','031C0JF','031C0KF','031C0ZF','03PY07Z','03PY0JZ','03PY0KZ','03PY37Z','03PY3JZ',
	                        '03PY3KZ','03PY47Z','03PY4JZ','03PY4KZ','03WY0JZ','03WY3JZ','03WY4JZ','03WYXJZ','05HY33Z','06HY33Z') then PQI07=0;
		/* Exclude cardiac & immunocompromised state procedures */
			if PRCDR(I) in ('0210093','0210098','0210099','021009C','021009F','021009W','02100A3','02100A8','02100A9','02100AC','02100AF','02100AW','02100J3','02100J8','02100J9','02100JC','02100JF','02100JW','02100K3',
	                        '02100K8','02100K9','02100KC','02100KF','02100KW','02100Z3','02100Z8','02100Z9','02100ZC','02100ZF','0210344','02103D4','0210444','0210493','0210498','0210499','021049C','021049F','021049W',
	                        '02104A3','02104A8','02104A9','02104AC','02104AF','02104AW','02104D4','02104J3','02104J8','02104J9','02104JC','02104JF','02104JW','02104K3','02104K8','02104K9','02104KC','02104KF','02104KW',
	                        '02104Z3','02104Z8','02104Z9','02104ZC','02104ZF','0211093','0211098','0211099','021109C','021109F','021109W','02110A3','02110A8','02110A9','02110AC','02110AF','02110AW','02110J3','02110J8',
	                        '02110J9','02110JC','02110JF','02110JW','02110K3','02110K8','02110K9','02110KC','02110KF','02110KW','02110Z3','02110Z8','02110Z9','02110ZC','02110ZF','0211344','02113D4','0211444','0211493',
	                        '0211498','0211499','021149C','021149F','021149W','02114A3','02114A8','02114A9','02114AC','02114AF','02114AW','02114D4','02114J3','02114J8','02114J9','02114JC','02114JF','02114JW','02114K3',
	                        '02114K8','02114K9','02114KC','02114KF','02114KW','02114Z3','02114Z8','02114Z9','02114ZC','02114ZF','0212093','0212098','0212099','021209C','021209F','021209W','02120A3','02120A8','02120A9',
	                        '02120AC','02120AF','02120AW','02120J3','02120J8','02120J9','02120JC','02120JF','02120JW','02120K3','02120K8','02120K9','02120KC','02120KF','02120KW','02120Z3','02120Z8','02120Z9','02120ZC',
	                        '02120ZF','0212344','02123D4','0212444','0212493','0212498','0212499','021249C','021249F','021249W','02124A3','02124A8','02124A9','02124AC','02124AF','02124AW','02124D4','02124J3','02124J8',
	                        '02124J9','02124JC','02124JF','02124JW','02124K3','02124K8','02124K9','02124KC','02124KF','02124KW','02124Z3','02124Z8','02124Z9','02124ZC','02124ZF','0213093','0213098','0213099','021309C',
	                        '021309F','021309W','02130A3','02130A8','02130A9','02130AC','02130AF','02130AW','02130J3','02130J8','02130J9','02130JC','02130JF','02130JW','02130K3','02130K8','02130K9','02130KC','02130KF',
	                        '02130KW','02130Z3','02130Z8','02130Z9','02130ZC','02130ZF','0213344','02133D4','0213444','0213493','0213498','0213499','021349C','021349F','021349W','02134A3','02134A8','02134A9','02134AC',
	                        '02134AF','02134AW','02134D4','02134J3','02134J8','02134J9','02134JC','02134JF','02134JW','02134K3','02134K8','02134K9','02134KC','02134KF','02134KW','02134Z3','02134Z8','02134Z9','02134ZC',
	                        '02134ZF','021609P','021609Q','021609R','02160AP','02160AQ','02160AR','02160JP','02160JQ','02160JR','02160KP','02160KQ','02160KR','02160ZP','02160ZQ','02160ZR','021649P','021649Q','021649R',
	                        '02164AP','02164AQ','02164AR','02164JP','02164JQ','02164JR','02164KP','02164KQ','02164KR','02164ZP','02164ZQ','02164ZR','021709P','021709Q','021709R','02170AP','02170AQ','02170AR','02170JP',
	                        '02170JQ','02170JR','02170KP','02170KQ','02170KR','02170ZP','02170ZQ','02170ZR','021749P','021749Q','021749R','02174AP','02174AQ','02174AR','02174JP','02174JQ','02174JR','02174KP','02174KQ',	
	                        '02174KR','02174ZP','02174ZQ','02174ZR','021K09P','021K09Q','021K09R','021K0AP','021K0AQ','021K0AR','021K0JP','021K0JQ','021K0JR','021K0KP','021K0KQ','021K0KR','021K0Z5','021K0Z8','021K0Z9',
	                        '021K0ZC','021K0ZF','021K0ZP','021K0ZQ','021K0ZR','021K0ZW','021K49P','021K49Q','021K49R','021K4AP','021K4AQ','021K4AR','021K4JP','021K4JQ','021K4JR','021K4KP','021K4KQ','021K4KR','021K4Z5',
	                        '021K4Z8','021K4Z9','021K4ZC','021K4ZF','021K4ZP','021K4ZQ','021K4ZR','021K4ZW','021L09P','021L09Q','021L09R','021L0AP','021L0AQ','021L0AR','021L0JP','021L0JQ','021L0JR','021L0KP','021L0KQ',
	                        '021L0KR','021L0Z5','021L0Z8','021L0Z9','021L0ZC','021L0ZF','021L0ZP','021L0ZQ','021L0ZR','021L0ZW','021L49P','021L49Q','021L49R','021L4AP','021L4AQ','021L4AR','021L4JP','021L4JQ','021L4JR',
	                        '021L4KP','021L4KQ','021L4KR','021L4Z5','021L4Z8','021L4Z9','021L4ZC','021L4ZF','021L4ZP','021L4ZQ','021L4ZR','021L4ZW','02540ZZ','02543ZZ','02544ZZ','02550ZZ','02553ZZ','02554ZZ','02560ZZ',
	                        '02563ZZ','02564ZZ','02570ZK','02570ZZ','02573ZK','02573ZZ','02574ZK','02574ZZ','02580ZZ','02583ZZ','02584ZZ','02590ZZ','02593ZZ','02594ZZ','025D0ZZ','025D3ZZ','025D4ZZ','025F0ZZ','025F3ZZ',
	                        '025F4ZZ','025G0ZZ','025G3ZZ','025G4ZZ','025H0ZZ','025H3ZZ','025H4ZZ','025J0ZZ','025J3ZZ','025J4ZZ','025K0ZZ','025K3ZZ','025K4ZZ','025L0ZZ','025L3ZZ','025L4ZZ','025M0ZZ','025M3ZZ','025M4ZZ',
	                        '025N0ZZ','025N3ZZ','025N4ZZ','0270046','027004Z','02700D6','02700DZ','02700T6','02700TZ','02700Z6','02700ZZ','0270346','027034Z','02703D6','02703DZ','02703T6','02703TZ','02703Z6','02703ZZ',
	                        '0270446','027044Z','02704D6','02704DZ','02704T6','02704TZ','02704Z6','02704ZZ','0271046','027104Z','02710D6','02710DZ','02710T6','02710TZ','02710Z6','02710ZZ','0271346','027134Z','02713D6',
	                        '02713DZ','02713T6','02713TZ','02713Z6','02713ZZ','0271446','027144Z','02714D6','02714DZ','02714T6','02714TZ','02714Z6','02714ZZ','0272046','027204Z','02720D6','02720DZ','02720T6','02720TZ',
	                        '02720Z6','02720ZZ','0272346','027234Z','02723D6','02723DZ','02723T6','02723TZ','02723Z6','02723ZZ','0272446','027244Z','02724D6','02724DZ','02724T6','02724TZ','02724Z6','02724ZZ','0273046',
	                        '027304Z','02730D6','02730DZ','02730T6','02730TZ','02730Z6','02730ZZ','0273346','027334Z','02733D6','02733DZ','02733T6','02733TZ','02733Z6','02733ZZ','0273446','027344Z','02734D6','02734DZ',
	                        '02734T6','02734TZ','02734Z6','02734ZZ','027F04Z','027F0DZ','027F0ZZ','027F34Z','027F3DZ','027F3ZZ','027F44Z','027F4DZ','027F4ZZ','027G04Z','027G0DZ','027G0ZZ','027G34Z','027G3DZ','027G3ZZ',
	                        '027G44Z','027G4DZ','027G4ZZ','027H04Z','027H0DZ','027H0ZZ','027H34Z','027H3DZ','027H3ZZ','027H44Z','027H4DZ','027H4ZZ','027J04Z','027J0DZ','027J0ZZ','027J34Z','027J3DZ','027J3ZZ','027J44Z',
	                        '027J4DZ','027J4ZZ','02890ZZ','02893ZZ','02894ZZ','028D0ZZ','028D3ZZ','028D4ZZ','02B40ZZ','02B43ZZ','02B44ZZ','02B50ZZ','02B53ZZ','02B54ZZ','02B60ZZ','02B63ZZ','02B64ZZ','02B70ZK','02B70ZZ',
	                        '02B73ZK','02B73ZZ','02B74ZK','02B74ZZ','02B80ZZ','02B83ZZ','02B84ZZ','02B90ZZ','02B93ZZ','02B94ZZ','02BD0ZZ','02BD3ZZ','02BD4ZZ','02BF0ZZ','02BF3ZZ','02BF4ZZ','02BG0ZZ','02BG3ZZ','02BG4ZZ',
	                        '02BH0ZZ','02BH3ZZ','02BH4ZZ','02BJ0ZZ','02BJ3ZZ','02BJ4ZZ','02BK0ZZ','02BK3ZZ','02BK4ZZ','02BL0ZZ','02BL3ZZ','02BL4ZZ','02BM0ZZ','02BM3ZZ','02BM4ZZ','02BN0ZZ','02BN3ZZ','02BN4ZZ','02C00ZZ',
	                        '02C03ZZ','02C04ZZ','02C10ZZ','02C13ZZ','02C14ZZ','02C20ZZ','02C23ZZ','02C24ZZ','02C30ZZ','02C33ZZ','02C34ZZ','02C40ZZ','02C43ZZ','02C44ZZ','02C50ZZ','02C53ZZ','02C54ZZ','02CD0ZZ','02CD3ZZ',
	                        '02CD4ZZ','02CF0ZZ','02CF3ZZ','02CF4ZZ','02CG0ZZ','02CG3ZZ','02CG4ZZ','02CH0ZZ','02CH3ZZ','02CH4ZZ','02CJ0ZZ','02CJ3ZZ','02CJ4ZZ','02CM0ZZ','02CM3ZZ','02CM4ZZ','02H400Z','02H402Z','02H403Z',
	                        '02H40DZ','02H40JZ','02H40KZ','02H40MZ','02H430Z','02H432Z','02H433Z','02H43DZ','02H43JZ','02H43KZ','02H43MZ','02H440Z','02H442Z','02H443Z','02H44DZ','02H44JZ','02H44KZ','02H44MZ','02H600Z',
	                        '02H60JZ','02H60KZ','02H60MZ','02H630Z','02H63JZ','02H63KZ','02H63MZ','02H640Z','02H64JZ','02H64KZ','02H64MZ','02H700Z','02H70JZ','02H70KZ','02H70MZ','02H730Z','02H73JZ','02H73KZ','02H73MZ',
	                        '02H740Z','02H74JZ','02H74KZ','02H74MZ','02HA0QZ','02HA0RS','02HA0RZ','02HA3QZ','02HA3RS','02HA3RZ','02HA4QZ','02HA4RS','02HA4RZ','02HK00Z','02HK02Z','02HK0JZ','02HK0KZ','02HK0MZ','02HK30Z',
	                        '02HK32Z','02HK3JZ','02HK3KZ','02HK3MZ','02HK40Z','02HK42Z','02HK4JZ','02HK4KZ','02HK4MZ','02HL00Z','02HL0JZ','02HL0KZ','02HL0MZ','02HL30Z','02HL3JZ','02HL3KZ','02HL3MZ','02HL40Z','02HL4JZ',
	                        '02HL4KZ','02HL4MZ','02HN0JZ','02HN0KZ','02HN0MZ','02HN3JZ','02HN3KZ','02HN3MZ','02HN4JZ','02HN4KZ','02HN4MZ','02HS00Z','02HS30Z','02HS40Z','02HT00Z','02HT30Z','02HT40Z','02HV00Z','02HV30Z',
	                        '02HV40Z','02L70CK','02L70DK','02L70ZK','02L73CK','02L73DK','02L73ZK','02L74CK','02L74DK','02L74ZK','02LR0ZT','02LS0ZZ','02LT0ZZ','02N50ZZ','02N53ZZ','02N54ZZ','02N90ZZ','02N93ZZ','02N94ZZ',
	                        '02ND0ZZ','02ND3ZZ','02ND4ZZ','02NF0ZZ','02NF3ZZ','02NF4ZZ','02NG0ZZ','02NG3ZZ','02NG4ZZ','02NH0ZZ','02NH3ZZ','02NH4ZZ','02NJ0ZZ','02NJ3ZZ','02NJ4ZZ','02NK0ZZ','02NK3ZZ','02NK4ZZ','02NL0ZZ',
	                        '02NL3ZZ','02NL4ZZ','02NM0ZZ','02NM3ZZ','02NM4ZZ','02PA0MZ','02PA0QZ','02PA0RZ','02PA3MZ','02PA3QZ','02PA3RZ','02PA4MZ','02PA4QZ','02PA4RZ','02PAXMZ','02Q00ZZ','02Q03ZZ','02Q04ZZ','02Q10ZZ',
	                        '02Q13ZZ','02Q14ZZ','02Q20ZZ','02Q23ZZ','02Q24ZZ','02Q30ZZ','02Q33ZZ','02Q34ZZ','02Q40ZZ','02Q43ZZ','02Q44ZZ','02Q50ZZ','02Q53ZZ','02Q54ZZ','02Q70ZZ','02Q73ZZ','02Q74ZZ','02Q90ZZ','02Q93ZZ',
	                        '02Q94ZZ','02QA0ZZ','02QA3ZZ','02QA4ZZ','02QB0ZZ','02QB3ZZ','02QB4ZZ','02QC0ZZ','02QC3ZZ','02QC4ZZ','02QD0ZZ','02QD3ZZ','02QD4ZZ','02QF0ZZ','02QF3ZZ','02QF4ZZ','02QG0ZZ','02QG3ZZ','02QG4ZZ',
	                        '02QH0ZZ','02QH3ZZ','02QH4ZZ','02QJ0ZZ','02QJ3ZZ','02QJ4ZZ','02QM0ZZ','02QM3ZZ','02QM4ZZ','02R907Z','02R908Z','02R90JZ','02R90KZ','02R947Z','02R948Z','02R94JZ','02R94KZ','02RD07Z','02RD08Z',
	                        '02RD0JZ','02RD0KZ','02RD47Z','02RD48Z','02RD4JZ','02RD4KZ','02RF07Z','02RF08Z','02RF0JZ','02RF0KZ','02RF37H','02RF37Z','02RF38H','02RF38Z','02RF3JH','02RF3JZ','02RF3KH','02RF3KZ','02RF47Z',
	                        '02RF48Z','02RF4JZ','02RF4KZ','02RG07Z','02RG08Z','02RG0JZ','02RG0KZ','02RG37H','02RG37Z','02RG38H','02RG38Z','02RG3JH','02RG3JZ','02RG3KH','02RG3KZ','02RG47Z','02RG48Z','02RG4JZ','02RG4KZ',
	                        '02RH07Z','02RH08Z','02RH0JZ','02RH0KZ','02RH37H','02RH37Z','02RH38H','02RH38Z','02RH3JH','02RH3JZ','02RH3KH','02RH3KZ','02RH47Z','02RH48Z','02RH4JZ','02RH4KZ','02RJ07Z','02RJ08Z','02RJ0JZ',
	                        '02RJ0KZ','02RJ47Z','02RJ48Z','02RJ4JZ','02RJ4KZ','02RK07Z','02RK0JZ','02RK0KZ','02RK47Z','02RK4KZ','02RL07Z','02RL0JZ','02RL0KZ','02RL47Z','02RL4KZ','02RM07Z','02RM0JZ','02RM0KZ','02RM47Z',
	                        '02RM4JZ','02RM4KZ','02RP0JZ','02RQ07Z','02RQ0JZ','02RR07Z','02RR0JZ','02SP0ZZ','02SW0ZZ','02T50ZZ','02T53ZZ','02T54ZZ','02T80ZZ','02T83ZZ','02T84ZZ','02T90ZZ','02T93ZZ','02T94ZZ','02TD0ZZ',
	                        '02TD3ZZ','02TD4ZZ','02TH0ZZ','02TH3ZZ','02TH4ZZ','02TM0ZZ','02TM3ZZ','02TM4ZZ','02TN0ZZ','02TN3ZZ','02TN4ZZ','02U507Z','02U508Z','02U50JZ','02U50KZ','02U537Z','02U538Z','02U53JZ','02U53KZ',
							'02U547Z','02U548Z','02U54JZ','02U54KZ','02U607Z','02U608Z','02U60KZ','02U707Z','02U708Z','02U70JZ','02U70KZ','02U737Z','02U738Z','02U73KZ','02U747Z','02U748Z','02U74KZ','02U907Z','02U908Z',
	                        '02U90JZ','02U90KZ','02U937Z','02U938Z','02U93JZ','02U93KZ','02U947Z','02U948Z','02U94JZ','02U94KZ','02UA0JZ','02UA3JZ','02UA4JZ','02UD07Z','02UD08Z','02UD0JZ','02UD0KZ','02UD37Z','02UD38Z',
	                        '02UD3JZ','02UD3KZ','02UD47Z','02UD48Z','02UD4JZ','02UD4KZ','02UF07Z','02UF08Z','02UF0JZ','02UF0KZ','02UF37Z','02UF38Z','02UF3JZ','02UF3KZ','02UF47Z','02UF48Z','02UF4JZ','02UF4KZ','02UG07Z',
	                        '02UG08Z','02UG0JZ','02UG0KZ','02UG37Z','02UG38Z','02UG3JZ','02UG3KZ','02UG47Z','02UG48Z','02UG4JZ','02UG4KZ','02UH07Z','02UH08Z','02UH0JZ','02UH0KZ','02UH37Z','02UH38Z','02UH3JZ','02UH3KZ',
	                        '02UH47Z','02UH48Z','02UH4JZ','02UH4KZ','02UJ07Z','02UJ08Z','02UJ0JZ','02UJ0KZ','02UJ37Z','02UJ38Z','02UJ3JZ','02UJ3KZ','02UJ47Z','02UJ48Z','02UJ4JZ','02UJ4KZ','02UK0KZ','02UK3KZ','02UK4KZ',
	                        '02UL0KZ','02UL3KZ','02UL4KZ','02UM07Z','02UM0JZ','02UM0KZ','02UM37Z','02UM38Z','02UM3JZ','02UM3KZ','02UM47Z','02UM48Z','02UM4JZ','02UM4KZ','02VR0ZT','02W50JZ','02W54JZ','02WA0JZ','02WA0MZ',
	                        '02WA0QZ','02WA0RZ','02WA3MZ','02WA3QZ','02WA3RZ','02WA4MZ','02WA4QZ','02WA4RZ','02WF07Z','02WF08Z','02WF0JZ','02WF0KZ','02WF47Z','02WF48Z','02WF4JZ','02WF4KZ','02WG07Z','02WG08Z','02WG0JZ',
	                        '02WG0KZ','02WG47Z','02WG48Z','02WG4JZ','02WG4KZ','02WH07Z','02WH08Z','02WH0JZ','02WH0KZ','02WH47Z','02WH48Z','02WH4JZ','02WH4KZ','02WJ07Z','02WJ08Z','02WJ0JZ','02WJ0KZ','02WJ47Z','02WJ48Z',
	                        '02WJ4JZ','02WJ4KZ','02WM0JZ','02WM4JZ','02YA0Z0','02YA0Z1','02YA0Z2','0JH600Z','0JH604Z','0JH605Z','0JH606Z','0JH607Z','0JH608Z','0JH609Z','0JH60AZ','0JH60PZ','0JH630Z','0JH634Z','0JH635Z',
	                        '0JH636Z','0JH637Z','0JH638Z','0JH639Z','0JH63AZ','0JH63PZ','0JH800Z','0JH804Z','0JH805Z','0JH806Z','0JH807Z','0JH808Z','0JH809Z','0JH80AZ','0JH80PZ','0JH830Z','0JH834Z','0JH835Z','0JH836Z',
	                        '0JH837Z','0JH838Z','0JH839Z','0JH83AZ','0JH83PZ','0JPT0PZ','0JPT3PZ','0JWT0PZ','0JWT3PZ','3E07017','3E070PZ','3E07317','3E073PZ','5A02110','5A02116','5A02210','5A02216','5A1213Z','5A1223Z') 
	                        then DO; PQI07=0; PQI08=0; END;
			if PRCDR(I) in ('02YA0Z0','02YA0Z2','0BYC0Z0','0BYC0Z2','0BYD0Z0','0BYD0Z2','0BYF0Z0','0BYF0Z2','0BYG0Z0','0BYG0Z2','0BYH0Z0','0BYH0Z2','0BYJ0Z0','0BYJ0Z2','0BYK0Z0','0BYK0Z2','0BYL0Z0','0BYL0Z2','0BYM0Z0',
	                        '0BYM0Z2','0FSG0ZZ','0FSG4ZZ','0FY00Z0','0FY00Z2','0FYG0Z0','0FYG0Z2','0TY00Z0','0TY00Z2','0TY10Z0','0TY10Z2','30230AZ','30230G0','30230G1','30230X0','30230X1','30230Y0','30230Y1','30233AZ',
	                        '30233G0','30233G1','30233X0','30233X1','30233Y0','30233Y1','30240AZ','30240G0','30240G1','30240X0','30240X1','30240Y0','30240Y1','30243AZ','30243G0','30243G1','30243X0','30243X1','30243Y0',
	                        '30243Y1','30250G0','30250G1','30250X0','30250X1','30250Y0','30250Y1','30253G0','30253G1','30253X0','30253X1','30253Y0','30253Y1','30260G0','30260G1','30260X0','30260X1','30260Y0','30260Y1',
	                        '30263G0','30263G1','30263X0','30263X1','30263Y0','30263Y1','3E00X0M','3E0130M','3E0230M','3E03005','3E0300M','3E030U0','3E030U1','3E030WL','3E03305','3E0330M','3E033U0','3E033U1','3E033WL',
	                        '3E04005','3E0400M','3E040WL','3E04305','3E0430M','3E043WL','3E05005','3E0500M','3E050WL','3E05305','3E0530M','3E053WL','3E06005','3E0600M','3E060WL','3E06305','3E0630M','3E063WL','3E0930M',
	                        '3E0970M','3E09X0M','3E0A30M','3E0B30M','3E0B70M','3E0BX0M','3E0C30M','3E0C70M','3E0CX0M','3E0D30M','3E0D70M','3E0DX0M','3E0E30M','3E0E70M','3E0E80M','3E0F30M','3E0F70M','3E0F80M','3E0G30M',
	                        '3E0G70M','3E0G80M','3E0H30M','3E0H70M','3E0H80M','3E0J30M','3E0J3U0','3E0J3U1','3E0J70M','3E0J7U0','3E0J7U1','3E0J80M','3E0J8U0','3E0J8U1','3E0K30M','3E0K70M','3E0K80M','3E0L30M','3E0L70M',
	                        '3E0M30M','3E0M70M','3E0N30M','3E0N70M','3E0N80M','3E0P30M','3E0P70M','3E0P80M','3E0Q30M','3E0Q70M','3E0R30M','3E0S30M','3E0U30M','3E0V30M','3E0W30M','3E0Y30M','3E0Y70M') 
	                        then DO; PQI11=0; PQI12=0; END;
			end;

			if DVRSND01='9' then do;
				if PRCDR(I) in ('3895','3927','3929','3942','3943','3993','3994') then PQI07=0;
				if PRCDR(I) in ('0050','0051','0052','0053','0054','0056','0057','0066','1751','1752','1755','3500','3501','3502','3503','3504','3505','3506',
								'3507','3508','3509','3510','3511','3512','3513','3514','3520','3521','3522','3523','3524','3525','3526','3527','3528','3531',
								'3532','3533','3534','3535','3539','3541','3542','3550','3551','3552','3553','3554','3555','3560','3561','3562','3563','3570',
								'3571','3572','3573','3581','3582','3583','3584','3591','3592','3593','3594','3595','3596','3597','3598','3599','3601','3602',
								'3603','3604','3605','3606','3607','3609','3610','3611','3612','3613','3614','3615','3616','3617','3619','362','363','3631',
								'3632','3633','3634','3639','3691','3699','3731','3732','3733','3734','3735','3736','3737','3741','375','3751','3752','3753',
								'3754','3755','3760','3761','3762','3763','3764','3765','3766','3770','3771','3772','3773','3774','3775','3776','3777','3778',
								'3779','3780','3781','3782','3783','3785','3786','3787','3789','3794','3795','3796','3797','3798','3826') then DO;
								PQI07=0; PQI08=0; END;
				if PRCDR(I) in ('0018','335','3350','3351','3352','336','375','3751','410','4100','4101','4102','4103','4104','4105','4106','4107','4108','4109',
								'5051','5059','5280','5281','5282','5283','5285','5286','5569') then DO; PQI11=0; PQI12=0; END;
			end;
	END;
run;

proc sort data=an_files.hw_readm_&YY.&YYE._final; by hicno admit disch; run;

data an_files.hw_readm_&YY.&YYE._final_2;
	set an_files.hw_readm_&YY.&YYE._final;
	by hicno admit disch; 
	/* Create lead variables: a hospital stay record will have preventable readmission indicator equals to 1 if the readmission record has at least 1 PQI equals to 1 */
	if eof1=0 then set an_files.hw_readm_&YY.&YYE._final 
    (firstobs=2 keep=PQI01 PQI02 PQI05 PQI07 PQI08 PQI10 PQI11 PQI12 PQI14 hicno 
		        rename=(PQI01=PQI01_2 PQI02=PQI02_2 PQI05=PQI05_2 PQI07=PQI07_2 PQI08=PQI08_2 PQI10=PQI10_2 PQI11=PQI11_2 PQI12=PQI12_2 PQI14=PQI14_2 hicno=hicno2)) 
    end=eof1;
	if last.hicno then do PQI01_2=.; PQI02_2=.; PQI05_2=.; PQI07_2=.; PQI08_2=.; PQI10_2=.; PQI11_2=.; PQI12_2=.; PQI14_2=.; hicno2=.; end;

	drop PQI01 PQI02 PQI05 PQI07 PQI08 PQI10 PQI11 PQI12 PQI14 I;

	if RADM30=1 then do;
		if PQI01_2=1 or PQI02_2=1 or PQI05_2=1 or PQI07_2=1 or PQI08_2=1 or PQI10_2=1 or PQI11_2=1 or PQI12_2=1 or PQI14_2=1 then RADM30_Preventable=1;
		else RADM30_Preventable=0;
	end;

	if RADM30=0 then do; RADM30_Preventable=0; PQI01_2=.; PQI02_2=.; PQI05_2=.; PQI07_2=.; PQI08_2=.; PQI10_2=.; PQI11_2=.; PQI12_2=.; PQI14_2=.; end;
	if RADM30=. then do; RADM30_Preventable=.; PQI01_2=.; PQI02_2=.; PQI05_2=.; PQI07_2=.; PQI08_2=.; PQI10_2=.; PQI11_2=.; PQI12_2=.; PQI14_2=.; end;

	rename PQI01_2=PQI01 PQI02_2=PQI02 PQI05_2=PQI05 PQI07_2=PQI07 PQI08_2=PQI08 PQI10_2=PQI10 PQI11_2=PQI11 PQI12_2=PQI12 PQI14_2=PQI14;
	drop hicno2 year DVRSND01-DVRSND10 _DVRSND01 pvsncd01-pvsncd06 evrscd01-evrscd12 DVRSND bundle;
	
run;

* Generate final analytical data set for year 2010-2016;
data final.hw_readm_&YY.&YYE._final;
	set an_files.hw_readm_&YY.&YYE._final_2;
	year=year(disch); 
	label PQI01='Diabetes Short-term Complications'
		  PQI02='Perforated Appendix'
		  PQI05='COPD or Asthma'
		  PQI07='Hypertension'
		  PQI08='Heart Failure'
		  PQI10='Dehydration'
		  PQI11='Bacterial Pneumonia'
		  PQI12='Urinary Tract Infection'
		  PQI14='Uncontrolled Diabetes'
		  RADM30_Preventable = 'Preventable Readmission';
run;


