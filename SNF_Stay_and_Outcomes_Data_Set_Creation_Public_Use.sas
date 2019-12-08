/********************************************************************************************************************************************************************************
 *Goal: Create SNF stay level data set with SNF benefit day variable, SNF length of stay, Medicare payment variables, prior hospitalization covariates, SNF stay 
        covariates, patient socioeconomic covariates and patient outcomes.

 *Time period: Jan 1st 2012 - Dec 1st 2015

 *Required raw data: Medicare Part A Claim, Medicare Beneficiary Summary File, Provider of Services Current File, Minimum Data Set, LTCFocus Data, American Community Survey
                     2012-2016 5-Year Estimates Data

 *Last updated on: 12/08/2019
********************************************************************************************************************************************************************************/


***************************************************************************
***************************************************************************
PART I - Create a hospital stay and SNF stay combined data set
***************************************************************************
***************************************************************************;

/**********************************************************************
Step 1: Generate eligible cohort of hospitalizations and SNF stays
**********************************************************************/
***Create a subset of raw hospitalization data which discharged from hospital between 01/01/2012 - 12/01/2016;
data medpar1216_hosp;
set  medpar.mp100mod_2011(keep=BENE_ID AGE_CNT SEX RACE DSCHRGDT ADMSNDT PRVDR_NUM medpar_id LOSCNT UTIL_DAY COIN_DAY SPCLUNIT DSTNTNCD DSCHRGCD pmt_amt coin_amt ded_amt blddedam drg_cd)
     medpar.mp100mod_2012(keep=BENE_ID AGE_CNT SEX RACE DSCHRGDT ADMSNDT PRVDR_NUM medpar_id LOSCNT UTIL_DAY COIN_DAY SPCLUNIT DSTNTNCD DSCHRGCD pmt_amt coin_amt ded_amt blddedam drg_cd)
     medpar.mp100mod_2013(keep=BENE_ID AGE_CNT SEX RACE DSCHRGDT ADMSNDT PRVDR_NUM medpar_id LOSCNT UTIL_DAY COIN_DAY SPCLUNIT DSTNTNCD DSCHRGCD pmt_amt coin_amt ded_amt blddedam drg_cd)
     medpar.mp100mod_2014(keep=BENE_ID AGE_CNT SEX RACE DSCHRGDT ADMSNDT PRVDR_NUM medpar_id LOSCNT UTIL_DAY COIN_DAY SPCLUNIT DSTNTNCD DSCHRGCD pmt_amt coin_amt ded_amt blddedam drg_cd)
     medpar.mp100mod_2015(keep=BENE_ID AGE_CNT SEX RACE DSCHRGDT ADMSNDT PRVDR_NUM medpar_id LOSCNT UTIL_DAY COIN_DAY SPCLUNIT DSTNTNCD DSCHRGCD pmt_amt coin_amt ded_amt blddedam drg_cd)
	 medpar.mp100mod_2016(keep=BENE_ID AGE_CNT SEX RACE DSCHRGDT ADMSNDT PRVDR_NUM medpar_id LOSCNT UTIL_DAY COIN_DAY SPCLUNIT DSTNTNCD DSCHRGCD pmt_amt coin_amt ded_amt blddedam drg_cd);
where (substr(PRVDR_NUM,3,1) in ('0','M','R','S','T') or substr(PRVDR_NUM,3,2)="13") & SPCLUNIT not in ('M','R','S','T') & 18993<=DSCHRGDT<=20789;
run; *66,498,464;

proc sort data=medpar1216_hosp; by bene_id admsndt dschrgdt; run;

*Create a data set which only includes the first hospital stay for each beneficiary;
data medpar1216_hosp_first;
set medpar1216_hosp;
by bene_id admsndt dschrgdt;
if first.bene_id;
run; *26,711,998;

*Create a subset of 2012-2016 hospitalizations that discharged to hospice or died in hospital;
data medpar1216_hosp_hospice;
set medpar1216_hosp;
where DSTNTNCD in (41,42,50,51) or DSCHRGCD^='A';
run; *4,050,436;

****Create a subset of raw MedPAR data for SNF stays from 2012-2016. Keep only SNF utilization day count greater than 0;
data medpar1216_snf(keep=BENE_ID AGE_CNT SEX RACE ADMSNDT dschrgdt_pseudo PRVDR_NUM medpar_id LOSCNT UTIL_DAY COIN_DAY SPCLUNIT DSTNTNCD DSCHRGCD pmt_amt coin_amt ded_amt blddedam drg_cd);
retain BENE_ID AGE_CNT SEX RACE ADMSNDT dschrgdt_pseudo PRVDR_NUM medpar_id LOSCNT UTIL_DAY COIN_DAY SPCLUNIT DSTNTNCD DSCHRGCD pmt_amt;
set medpar.mp100mod_2012(keep=BENE_ID AGE_CNT SEX RACE DSCHRGDT ADMSNDT PRVDR_NUM medpar_id LOSCNT UTIL_DAY COIN_DAY SPCLUNIT DSTNTNCD DSCHRGCD pmt_amt coin_amt ded_amt blddedam drg_cd)
    medpar.mp100mod_2013(keep=BENE_ID AGE_CNT SEX RACE DSCHRGDT ADMSNDT PRVDR_NUM medpar_id LOSCNT UTIL_DAY COIN_DAY SPCLUNIT DSTNTNCD DSCHRGCD pmt_amt coin_amt ded_amt blddedam drg_cd)
    medpar.mp100mod_2014(keep=BENE_ID AGE_CNT SEX RACE DSCHRGDT ADMSNDT PRVDR_NUM medpar_id LOSCNT UTIL_DAY COIN_DAY SPCLUNIT DSTNTNCD DSCHRGCD pmt_amt coin_amt ded_amt blddedam drg_cd)
    medpar.mp100mod_2015(keep=BENE_ID AGE_CNT SEX RACE DSCHRGDT ADMSNDT PRVDR_NUM medpar_id LOSCNT UTIL_DAY COIN_DAY SPCLUNIT DSTNTNCD DSCHRGCD pmt_amt coin_amt ded_amt blddedam drg_cd)
	medpar.mp100mod_2016(keep=BENE_ID AGE_CNT SEX RACE DSCHRGDT ADMSNDT PRVDR_NUM medpar_id LOSCNT UTIL_DAY COIN_DAY SPCLUNIT DSTNTNCD DSCHRGCD pmt_amt coin_amt ded_amt blddedam drg_cd);
if DSCHRGDT=. or LOSCNT>UTIL_DAY then DSCHRGDT_pseudo=admsndt+UTIL_DAY;
else DSCHRGDT_pseudo=DSCHRGDT;
where  substr(PRVDR_NUM,3,1) in ('5','6','U','W','Y','Z') and 18993<=ADMSNDT<=20789 and util_day>0;
label DSCHRGDT_pseudo="Date beneficiary was discharged or died";
format DSCHRGDT_pseudo date9.;
run;  *12,475,855;
data medpar1216_snf; set medpar1216_snf; rename DSCHRGDT_pseudo=DSCHRGDT; run;	
	
proc sort data=medpar1216_snf; by bene_id admsndt dschrgdt; run;

*Create a data set which only includes the first SNF stay for each beneficiary;
data medpar1216_snf_first;
set medpar1216_snf;
by bene_id admsndt dschrgdt;
if first.bene_id;
run; *6,888,244;

*Set up denominator file;
data tempn.Denom1216_part_a;
set Denom.Dn100mod_2012 Denom.Dn100mod_2013 Denom.Dn100mod_2014 Denom.Dn100mod_2015 Denom.Dn100mod_2016;
rename  BUYIN01=BUYIN1 BUYIN02=BUYIN2 BUYIN03=BUYIN3 BUYIN04=BUYIN4 BUYIN05=BUYIN5 BUYIN06=BUYIN6 BUYIN07=BUYIN7 BUYIN08=BUYIN8 BUYIN09=BUYIN9;	         
keep BENE_ID BENE_DOB DEATH_DT RFRNC_YR SEX RACE BUYIN: Dual_stus_CD:;
run; /* Total: 283,854,904 */
proc sort data=tempn.Denom1216_part_a; by BENE_ID; run;

data Dn100mod_2012; set tempn.Denom1216_part_a; where RFRNC_YR=2012; 
rename DEATH_DT=DEATH_DT_12; rename Dual_stus_CD_01-Dual_stus_CD_12=Dual_stus_CD1-Dual_stus_CD12; 
run;*53,597,183;
data Dn100mod_2013; set tempn.Denom1216_part_a; where RFRNC_YR=2013; 
rename DEATH_DT=DEATH_DT_13; rename Dual_stus_CD_01-Dual_stus_CD_12=Dual_stus_CD13-Dual_stus_CD24; rename BUYIN1-BUYIN12=BUYIN13-BUYIN24; 
run;*55,277,442;
data Dn100mod_2014; set tempn.Denom1216_part_a; where RFRNC_YR=2014; 
rename DEATH_DT=DEATH_DT_14; rename Dual_stus_CD_01-Dual_stus_CD_12=Dual_stus_CD25-Dual_stus_CD36; rename BUYIN1-BUYIN12=BUYIN25-BUYIN36; 
run;*56,867,603;
data Dn100mod_2015; set tempn.Denom1216_part_a; where RFRNC_YR=2015; 
rename DEATH_DT=DEATH_DT_15; rename Dual_stus_CD_01-Dual_stus_CD_12=Dual_stus_CD37-Dual_stus_CD48; rename BUYIN1-BUYIN12=BUYIN37-BUYIN48; 
run;*58,294,195;
data Dn100mod_2016; set tempn.Denom1216_part_a; where RFRNC_YR=2016; 
rename DEATH_DT=DEATH_DT_16; rename Dual_stus_CD_01-Dual_stus_CD_12=Dual_stus_CD49-Dual_stus_CD60; rename BUYIN1-BUYIN12=BUYIN49-BUYIN60; 
run;*59,818,481;

proc sort data=Dn100mod_2012; by BENE_ID; run;
proc sort data=Dn100mod_2013; by BENE_ID; run;
proc sort data=Dn100mod_2014; by BENE_ID; run;
proc sort data=Dn100mod_2015; by BENE_ID; run;
proc sort data=Dn100mod_2016; by BENE_ID; run;

data tempn.Denom1216_part_a_2;
merge Dn100mod_2012 Dn100mod_2013 Dn100mod_2014 Dn100mod_2015 Dn100mod_2016;
by BENE_ID;
run; *Total: 68,740,878;

data tempn.Denom1216_part_a_2;
set tempn.Denom1216_part_a_2;
DEATH_DT=DEATH_DT_12;
if DEATH_DT_12 eq . & DEATH_DT_13 ne . then DEATH_DT=DEATH_DT_13;
else if DEATH_DT_12 eq . & DEATH_DT_13 eq . & DEATH_DT_14 ne . then DEATH_DT=DEATH_DT_14;
else if DEATH_DT_12 eq . & DEATH_DT_13 eq . & DEATH_DT_14 eq . & DEATH_DT_15 ne . then DEATH_DT=DEATH_DT_15;
else if DEATH_DT_12 eq . & DEATH_DT_13 eq . & DEATH_DT_14 eq . & DEATH_DT_15 eq . & DEATH_DT_16 ne . then DEATH_DT=DEATH_DT_16;
run;

*Create a subset of denominator data set which only includes the beneficiaries in SNF data set;
proc sql;
create table tempn.Denom1216_part_a_3 as 
select * from tempn.Denom1216_part_a_2 where bene_id in (select bene_id from medpar1216_snf);
quit; *6,888,243;

*Include only beneficiaries that continually enrolled in Part A (with or without part B) until discharge;
data tempn.Denom1216_part_a_4;
set tempn.Denom1216_part_a_3;
format death_dt date9.;
array buyin{60} BUYIN1-BUYIN60;
array elig_indicator{60} elig_indicator_1-elig_indicator_60;
array occur_indicator{60} occur_indicator_1-occur_indicator_60;  
if death_dt^=. then total_period=(year(death_dt)-2012)*12+month(death_dt);
else if death_dt=. then total_period=60;
do i=1 to total_period;
	* BUYIN: 1 means Part A only, 3 means Part A and B and C means Part A and B, state buy-in;
	if buyin(i) in ('1','3','C') then elig_indicator(i)=1;  else elig_indicator(i)=0; 
    if buyin(i)^=" " then occur_indicator(i)=i; else occur_indicator(i)=.;
end; 
drop i;
initial_occur=min(of occur_indicator_1-occur_indicator_60);
elig_sum=sum(of elig_indicator_1-elig_indicator_60);
if elig_sum=total_period-initial_occur+1 then elig_bene=1; 
keep bene_id death_dt initial_occur total_period elig_sum elig_bene BUYIN:;
run; *6,888,243;

proc sql; create table check_elig as select bene_id from tempn.Denom1216_part_a_4 where elig_bene=1; quit; *6,531,916 (94.8%);

*Exclude beneficiaries who have hospital or SNF stay in the prior 60-day period of the first hospitalization or SNF;
data medpar11_hospsnf;
set  medpar.mp100mod_2011(keep=BENE_ID DSCHRGDT ADMSNDT PRVDR_NUM medpar_id LOSCNT UTIL_DAY COIN_DAY SPCLUNIT);
where ((substr(PRVDR_NUM,3,1) in ('0','M','R','S','T') or substr(PRVDR_NUM,3,2)="13") & SPCLUNIT not in ('M','R','S','T')) or substr(PRVDR_NUM,3,1) in ('5','6','U','W','Y','Z');
if DSCHRGDT=. then DSCHRGDT=admsndt+UTIL_DAY;
run; *16,963,177;

proc sql;
create table hosp11_prior_60 as 
select distinct a.bene_id from medpar1216_hosp_first as a inner join medpar11_hospsnf as b on a.bene_id=b.bene_id and b.dschrgdt<=a.admsndt<=b.dschrgdt+60;
quit; *332,549;

proc sql;
create table snf11_prior_60 as 
select distinct a.bene_id from medpar1216_snf_first as a inner join medpar11_hospsnf as b on a.bene_id=b.bene_id and b.dschrgdt<=a.admsndt<=b.dschrgdt+60 
where b.medpar_id not in (select medpar_id from medpar1216_hosp);
quit; *108,351;

*Generate eligible SNF cohort;
proc sql;
create table medpar1216_snf_eligible as 
select * from medpar1216_snf where bene_id in (select bene_id from tempn.Denom1216_part_a_4 where elig_bene=1) 
                             and bene_id not in (select bene_id from medpar1216_hosp_hospice) 
                             and bene_id not in (select bene_id from hosp11_prior_60)
                             and bene_id not in (select bene_id from snf11_prior_60);
quit; *9,303,949;

proc sort data=medpar1216_snf_eligible; by bene_id admsndt dschrgdt; run;

*Create order number for each beneficiary's SNF stays;
data medpar1216_snf_eligible_2;
set medpar1216_snf_eligible;
by bene_id admsndt dschrgdt;
if first.bene_id then SNF_Num=1; else SNF_Num+1;
run; *9,303,949; 

*Used for identifying hospital records within the SNF break;
proc sql; create table SNF_break as 
select a.*, b.medpar_id as medpar_id_2, b.admsndt as admsndt_2, b.dschrgdt as dschrgdt_2 from 
medpar1216_snf_eligible_2 as a inner join medpar1216_snf_eligible_2 as b 
on a.bene_id=b.bene_id and 0<=b.admsndt-a.dschrgdt<30 and a.medpar_id^=b.medpar_id; 
quit; *2,178,558;

*Get hospitalization cohort;
proc sql;
create table medpar1216_hosp_eligible as 
select * from medpar1216_hosp where bene_id in (select bene_id from medpar1216_snf_eligible_2);
quit; *17,001,365;

*Merge with MDS data in order to find hospitalizations that have NH stay within prior 100 days;
proc sql;
create table medpar1216_hosp_eligible_2 as
select a.*, b.MDS_TRGT_DT1
from medpar1216_hosp_eligible as a
left join tempn.mds_prior_0916(rename=(MDS_TRGT_DT=MDS_TRGT_DT1)) as b  
on a.bene_id=b.bene_id and 0 lt a.ADMSNDT-b.MDS_TRGT_DT1 le 100;
quit; *29,951,587;

proc sort data=medpar1216_hosp_eligible_2 nodupkey; by medpar_id; run; *17,001,365;

*Identify NH stays in prior 100 days;
data medpar1216_hosp_eligible_2 (drop=MDS_TRGT_DT1);
set medpar1216_hosp_eligible_2;
if 	MDS_TRGT_DT1^=. then NH_Stay_Prior100=1; else NH_Stay_Prior100=0;
label NH_Stay_Prior100="Had Nursing Home Stay in Prior 100 Days";
run;

*Vertically combine hospital records and SNF records, replace negative payment amount with 0;
data tempn.medpar1216_hospsnf_elig(drop=pmt_amt);
set medpar1216_hosp_eligible_2 medpar1216_snf_eligible_2;
if pmt_amt<0 then pmt_amt_pseudo=0;
else pmt_amt_pseudo=pmt_amt;
label pmt_amt_pseudo="Medicare Payment Amount";
year_dschrg=year(dschrgdt);
month_dschrg=month(dschrgdt);
year_admsn=year(admsndt);
month_admsn=month(admsndt);
run; *26,305,314;

proc sort data=tempn.medpar1216_hospsnf_elig; by bene_id admsndt dschrgdt; run;

*Use discharge date to merge in dual eligible indicators;
proc sql;
create table dual_stus as
select medpar.*,denom.* 
from tempn.medpar1216_hospsnf_elig as medpar
left join tempn.dn100mod2010_2016_dual_status as denom
on medpar.BENE_ID=denom.bene_id and medpar.year_dschrg=denom.RFRNC_YR;
quit; *26,305,314;

data tempn.medpar1216_hospsnf_elig_2 (drop=RFRNC_YR dual_stus_cd:);
set dual_stus;
array dual_cd{12} dual_stus_cd_01-dual_stus_cd_12;
do i=1 to 12;
	if month(dschrgdt)=i then dual_stus_cd=dual_cd{i};
end;
if dual_stus_cd in ("02","04","08") then dual_stus=1; else dual_stus=0;
if dual_stus_cd="99" then dual_stus=.;
label dual_stus="Dual Eligible Status";
run;

data not_matched(keep=medpar_id bene_id year_admsn admsndt);
set tempn.medpar1216_hospsnf_elig_2;
where dual_stus=.;
run; *16,174 (0.06%) records that couldn't find a match in denominator file;

*Use admission date to search dual eligible information for records with missing dual eligible indicator; 
proc sql;
create table not_matched_2 as
select medpar.*,denom.* 
from not_matched as medpar
left join tempn.dn100mod2010_2016_dual_status as denom
on medpar.BENE_ID=denom.bene_id and medpar.year_admsn=denom.RFRNC_YR;
quit;

data not_matched_3 (drop=RFRNC_YR dual_stus_cd:);
set not_matched_2;
array dual_cd{12} dual_stus_cd_01-dual_stus_cd_12;
do i=1 to 12;
	if month(admsndt)=i then dual_stus_cd=dual_cd{i};
end;
if dual_stus_cd in ("02","04","08") then dual_stus=1; else dual_stus=0;
if dual_stus_cd="99" then dual_stus=.;
label dual_stus="Dual Eligible Status";
run;

data not_matched_4;set not_matched_3; where dual_stus=.; run; *9,566;

*Merge in dual eligible indicators for those records with missing dual eligible indicator in main data set;
proc sql;
create table tempn.medpar1216_hospsnf_elig_3 as
select medpar.*, nmatch.dual_stus as dual_stus_2
from tempn.medpar1216_hospsnf_elig_2 as medpar
left join not_matched_3 as nmatch
on medpar.medpar_ID=nmatch.medpar_ID;
quit;

*Replace the missing dual eligible indicators with the ones find by admission date;
data tempn.medpar1216_hospsnf_elig_4(drop=dual_stus_2);
set tempn.medpar1216_hospsnf_elig_3;
if dual_stus=. then dual_stus=dual_stus_2;
run; *26,305,314;

proc sort data=tempn.medpar1216_hospsnf_elig_4; by bene_id admsndt dschrgdt; run;


/**********************************************************************
 Step 2: Create SNF benefit period and benefit day indicators 
**********************************************************************/
***Create indicators for 1) qualified hospitalization (3 consecutive days), 2) last discharge date, 3)SNF utilization days and 4) previous SNF utilization days;
data tempn.medpar1216_hospsnf_elig_5(keep=BENE_ID AGE_CNT SEX RACE medpar_id PRVDR_NUM ADMSNDT DSCHRGDT dschrgdt_lag LOSCNT COIN_DAY UTIL_DAY pmt_amt_pseudo 
                                          coin_amt ded_amt blddedam DSCHRGCD DSTNTNCD drg_cd QFD_HOSP SNF_Num SNF_util_day SNF_util_day_lag Dual_Stus NH_Stay_Prior100);
retain BENE_ID AGE_CNT SEX RACE medpar_id PRVDR_NUM ADMSNDT DSCHRGDT dschrgdt_lag LOSCNT COIN_DAY UTIL_DAY pmt_amt_pseudo coin_amt ded_amt blddedam DSCHRGCD DSTNTNCD drg_cd 
       QFD_HOSP SNF_Num SNF_util_day SNF_util_day_lag Dual_Stus NH_Stay_Prior100; 
set tempn.medpar1216_hospsnf_elig_4;
by bene_id admsndt dschrgdt;
if SNF_NUM=. then do;
    SNF_Util_Day=.;
	if UTIL_DAY>=3 then QFD_HOSP=1; else QFD_HOSP=0; 
end;
if SNF_NUM^=. then do;
	QFD_HOSP=.;
	SNF_Util_Day=UTIL_DAY;
end;
dschrgdt_lag=lag(dschrgdt);
SNF_util_day_lag=lag(SNF_Util_Day);
if first.bene_id then do; dschrgdt_lag=.; SNF_util_day_lag=.;end;
format dschrgdt_lag date9.;
label QFD_HOSP="Qualified Hospitalization" dschrgdt_lag="Last Discharge_Date" SNF_util_day="SNF Utilization Day Count" SNF_util_day_lag="Last SNF Utilization Days";
run; *26,305,314;

proc sort data=tempn.medpar1216_hospsnf_elig_5; by bene_id admsndt dschrgdt; run;

***Check the percentage of patients who have used 1-30 SNF days that decided to go to SNF within 24 hours of hospital discharge;
*Identify unqualified hospitalizations which are within the 30-day SNF breaks;
proc sql;
create table tempn.hosp_within_snf_break_1216 as 
select a.* 
from tempn.medpar1216_hospsnf_elig_5 as a 
inner join SNF_break as b
on a.bene_id=b.bene_id and a.admsndt>=b.dschrgdt and a.dschrgdt<=b.admsndt_2
where a.SNF_NUM=. and a.QFD_HOSP^=1;
quit; *303,374;

proc sort data=tempn.hosp_within_snf_break_1216 nodupkey; by medpar_id; run; *224,687;

data tempn.hosp_within_snf_break_1216;
set tempn.hosp_within_snf_break_1216;
HOSP_IN_SNF_BREAK30=1;
label HOSP_IN_SNF_BREAK30="Unqualified Hospitalization within 30-Day SNF Break";
run; 

*Merge "HOSP_IN_SNF_BREAK30" indicator into main data set;
proc sql;
create table tempn.medpar1216_hospsnf_elig_6 as 
select a.*, b.HOSP_IN_SNF_BREAK30 
from tempn.medpar1216_hospsnf_elig_5 as a 
left join tempn.hosp_within_snf_break_1216 as b
on a.medpar_id=b.medpar_id;
quit; *26,305,314;

proc sort data=tempn.medpar1216_hospsnf_elig_6; by bene_id admsndt dschrgdt; run;

*Create benefit period variable;
data tempn.medpar1216_hospsnf_elig_7;
set tempn.medpar1216_hospsnf_elig_6;
by bene_id admsndt dschrgdt;
if first.bene_id then bene_period=1;
else if admsndt>=sum(dschrgdt_lag,60) then bene_period+1; 
else bene_period+0; 
if SNF_NUM=. & (HOSP_IN_SNF_BREAK30=1 or QFD_HOSP=1) then ELIG_HOSP=1;
label bene_period="Benefit Period" ELIG_HOSP="Eligible Hospitalization for SNF Stay";
run; *26,305,314;

*Create an indicator for patients who entered into SNF within 24h of hospital discharge;
data tempn.medpar1216_hosp24h tempn.medpar1216_snf24h;
set tempn.medpar1216_hospsnf_elig_7;
if SNF_NUM=.  then output tempn.medpar1216_hosp24h;
else if SNF_NUM^=.  then output tempn.medpar1216_snf24h;
run; *Hosp:17,001,365 / SNF:9,303,949;

data tempn.medpar1216_snf24h; 
set tempn.medpar1216_snf24h; 
Enter_SNF_24h=1; 
SNF_Prvdrnum=prvdr_num;
run;

*Merge entering SNF within 24h indicator into hospital data set;
proc sql;
create table tempn.medpar1216_hospsnf24h as
select a.*, b.Enter_SNF_24h
from tempn.medpar1216_hosp24h as a left join tempn.medpar1216_snf24h as b
on a.bene_id=b.bene_id and a.dschrgdt<=b.admsndt<=a.dschrgdt+1;
quit; *17,012,548; 
 
proc sort data=tempn.medpar1216_hospsnf24h nodupkey; by medpar_id; run; *17,001,365;

data tempn.medpar1216_hospsnf24h_2;
set tempn.medpar1216_hospsnf24h;
if Enter_SNF_24h=. then Enter_SNF_24h=0;
run;

data tempn.medpar1216_hospsnf_elig_8;
set tempn.medpar1216_hospsnf24h_2 tempn.medpar1216_snf24h(drop=Enter_SNF_24h SNF_Prvdrnum);
label Enter_SNF_24h="Entered SNF within 24 Hours";
run; *26,305,314;

proc sort data=tempn.medpar1216_hospsnf_elig_8; by bene_id admsndt dschrgdt; run;

*Create SNF day variable;
data tempn.medpar1216_hospsnf_elig_9;
set tempn.medpar1216_hospsnf_elig_8;
by bene_id admsndt dschrgdt;
if SNF_NUM=1 or admsndt>=sum(dschrgdt_lag,60) then SNF_Day=1;
else SNF_Day+SNF_util_day_lag;
if first.bene_id and SNF_Num=. then SNF_Day=1;
label SNF_Day="Number of Used SNF Days";
run; *26,305,314;

*Create a subset which only consist of hospital stay records;
data tempn.medpar1216_hosp_elig_9;
set tempn.medpar1216_hospsnf_elig_9;
where snf_num=.;
run; *17,001,365;

proc sort data=tempn.medpar1216_hosp_elig_9; by bene_id bene_period admsndt; run;

*Create an indicator for first hospitalization in benefit period;
data tempn.medpar1216_hosp_elig_9_2;
set tempn.medpar1216_hosp_elig_9; 
by bene_id bene_period admsndt;
first_hosp=0;
if first.bene_period then first_hosp=1;
label first_hosp="First Hospitalization in Benefit Period";
run; *17,001,365;

*Merge first hospital stay indicator into main data set;
proc sql; create table tempn.medpar1216_hospsnf_elig_10 as
select a.*, b.first_hosp from tempn.medpar1216_hospsnf_elig_9 as a left join tempn.medpar1216_hosp_elig_9_2 as b
on a.medpar_id=b.medpar_id;
quit; *26,305,314;



/************************************************************************************
 Step 3: Create indicators for all exclusion criteria and merge in payment variables
************************************************************************************/
*Get the maximum value of NH_Stay_Prior100 for each beneficiary's each benefit period;
proc sql; create table tempn.medpar1216_hospsnf_elig_10_nh as 
select bene_id, bene_period, max(NH_Stay_Prior100) as NH_Stay_Prior100_max
from tempn.medpar1216_hospsnf_elig_10
where first_hosp=1
group by bene_id, bene_period; 
quit; *11,484,300;

*Get the maximum value of dual_stus for each beneficiary's each benefit period;
proc sql; create table tempn.medpar1216_hospsnf_elig_10_du as 
select bene_id, bene_period, max(dual_stus) as dual_stus_max
from tempn.medpar1216_hospsnf_elig_10
group by bene_id, bene_period;
quit; *11,955,766;

*Merge two max values into data set;																																			  
proc sql; create table tempn.medpar1216_hospsnf_elig_11 as 
select a.*, b.NH_Stay_Prior100_max, c.dual_stus_max
from tempn.medpar1216_hospsnf_elig_10 as a 
left join tempn.medpar1216_hospsnf_elig_10_nh as b on a.bene_id =b.bene_id and a.bene_period=b.bene_period 
left join tempn.medpar1216_hospsnf_elig_10_du as c on a.bene_id =c.bene_id and a.bene_period=c.bene_period; 
quit; *26,305,314;

proc sort data=tempn.medpar1216_hospsnf_elig_11; by bene_id admsndt dschrgdt; run;

*Generate Pseudo Coinsurance day count for cross-checking;											  
data tempn.medpar1216_hospsnf_elig_12(drop=NH_Stay_Prior100_max dual_stus_max QFD_HOSP);
set tempn.medpar1216_hospsnf_elig_11;
if SNF_Num^=. then do;
	if SNF_Day>=21 then Coin_Day_Pseudo=SNF_util_day;
	else if 21-SNF_util_day<=SNF_Day<21 then Coin_Day_Pseudo=SNF_Day+SNF_util_day-20-1;
	else if SNF_Day+SNF_util_day<21 then Coin_Day_Pseudo=0;
end;
if Coin_Day_Pseudo>80 then Coin_Day_Pseudo=80;
Coin_Day_Diff=Coin_Day_Pseudo-Coin_Day;
NH_Stay_Prior100=NH_Stay_Prior100_max;
dual_stus=dual_stus_max;
benefit_day=SNF_day-1;
year_dschrg=year(dschrgdt);
rename age_cnt=age;
label Coin_Day_Pseudo="Pseudo Coinsurance Day Count" Coin_Day_Diff="Coinsurance Day Count Difference" SNF_Num="Ordinal Number of SNF for Each Beneficiary"
      SNF_util_day="SNF Utilization Day Count" dual_stus="Whether Dual Eligible" NH_Stay_Prior100="Had Nursing Home Stay in Prior 100 Days"
      Enter_SNF_24h="Entered SNF within 24 Hours" SNF_util_day_lag="Last SNF Utilization Day Count" HOSP_IN_SNF_BREAK30="Hospital Stay within 30-Day SNF Break"
      year_dschrg="Discharge Year";
run; *26,305,314;

*Get MDS discharge assessments for all beneficiaries that in our study cohort;
proc sql;
create table tempn.bene_in_nh as 
select * from pac_mds.mds3_beneid_2010_16 
where bene_id in (select distinct bene_id from tempn.medpar1216_hospsnf_elig_12) and A0310F_ENTRY_DSCHRG_CD ='10';
quit; *6,975,138;

proc sql; create table check_missing_dc_date as select bene_id from tempn.bene_in_nh where MDS_DSCHRG_DT=.; quit; *0 records --> no discharge assessments without discharge date;

***Create Long-term Care in Prior 100 Days Indicator;
*Merge analytical data set with MDS discharge data set to identify people who had MDS discharge assessments in prior 100 days;
proc sql;
create table tempn.medpar1216_hospsnf_elig_13 as
select a.*, b.MDS_DSCHRG_DT
from tempn.medpar1216_hospsnf_elig_12 as a
left join tempn.bene_in_nh as b  
on a.bene_id=b.bene_id and 0 lt a.ADMSNDT-b.MDS_DSCHRG_DT le 100;
quit; *26,606,411;

proc sql; create table check_missing as select bene_id from tempn.medpar1216_hospsnf_elig_13 where MDS_DSCHRG_DT^=.; quit;

*Get SNF claims from 2011-2016;
data medpar1116_snf;
set medpar.mp100mod_2011(keep=BENE_ID ADMSNDT DSCHRGDT PRVDR_NUM medpar_id util_day LOSCNT)
    medpar.mp100mod_2012(keep=BENE_ID ADMSNDT DSCHRGDT PRVDR_NUM medpar_id util_day LOSCNT)
	medpar.mp100mod_2013(keep=BENE_ID ADMSNDT DSCHRGDT PRVDR_NUM medpar_id util_day LOSCNT)
    medpar.mp100mod_2014(keep=BENE_ID ADMSNDT DSCHRGDT PRVDR_NUM medpar_id util_day LOSCNT)
    medpar.mp100mod_2015(keep=BENE_ID ADMSNDT DSCHRGDT PRVDR_NUM medpar_id util_day LOSCNT)
    medpar.mp100mod_2016(keep=BENE_ID ADMSNDT DSCHRGDT PRVDR_NUM medpar_id util_day LOSCNT);
if DSCHRGDT=. or LOSCNT>UTIL_DAY then DSCHRGDT_pseudo=admsndt+UTIL_DAY;
else DSCHRGDT_pseudo=DSCHRGDT;
where  substr(PRVDR_NUM,3,1) in ('5','6','U','W','Y','Z') and util_day>0 ;
label DSCHRGDT_pseudo="Date beneficiary was discharged or died";
format DSCHRGDT_pseudo date9.;
run; *15,289,807; 

*Merge analytical data set with SNF data set to identify people who had SNF claim in prior 100 days and within 24 hours of the discharge date from MDS;
proc sql;
create table tempn.medpar1216_hospsnf_elig_14 as
select a.*, b.DSCHRGDT_pseudo as SNF_Claim_DSCHRGDT
from tempn.medpar1216_hospsnf_elig_13 as a
left join medpar1116_snf as b  
on a.bene_id=b.bene_id and 0 lt a.ADMSNDT-b.DSCHRGDT_pseudo le 100 and -1<=a.MDS_DSCHRG_DT-b.DSCHRGDT_pseudo<=1;
quit; *26,610,560;

proc sort data=tempn.medpar1216_hospsnf_elig_14; by medpar_id MDS_DSCHRG_DT descending SNF_Claim_DSCHRGDT; run;

data tempn.medpar1216_hospsnf_elig_15;
set tempn.medpar1216_hospsnf_elig_14;
by medpar_id MDS_DSCHRG_DT descending SNF_Claim_DSCHRGDT; 
if first.MDS_DSCHRG_DT;
run;

*Create a long term care in prior 100 days indicator using the rule: beneficiaries had MDS assessments in prior 100 days but didn't have corresponding SNF claims or the difference of discharge dates
 between MDS and claims are greater than 3 days;
data tempn.medpar1216_hospsnf_elig_16;
set tempn.medpar1216_hospsnf_elig_15;
if MDS_DSCHRG_DT^=. & SNF_Claim_DSCHRGDT=. then LTC_Prior100=1;
else LTC_Prior100=0;
run;

proc sort data=tempn.medpar1216_hospsnf_elig_16; by medpar_id descending LTC_Prior100; run;

data tempn.medpar1216_hospsnf_elig_17;
set tempn.medpar1216_hospsnf_elig_16;
by medpar_id descending LTC_Prior100;
if first.medpar_id;
run; *26,305,314; 

proc freq data=tempn.medpar1216_hospsnf_elig_17; 
table LTC_Prior100;
run; 

*Get the maximum value of LTC_Prior100 for each beneficiary's each benefit period;
proc sql; create table tempn.medpar1216_hospsnf_elig_17_nh as 
select bene_id, bene_period, max(LTC_Prior100) as LTC_Prior100_max
from tempn.medpar1216_hospsnf_elig_17
group by bene_id, bene_period; 
quit; 

*Merge LTC_Prior100_max value into data set;																																			  
proc sql; create table tempn.medpar1216_hospsnf_elig_18 as 
select a.*, b.LTC_Prior100_max as LTC_Prior100 label="Long Term Care in Prior 100 Days"
from tempn.medpar1216_hospsnf_elig_17(drop=LTC_Prior100) as a 
left join tempn.medpar1216_hospsnf_elig_18_nh as b on a.bene_id =b.bene_id and a.bene_period=b.bene_period; 
quit; 

proc sort data=tempn.medpar1216_hospsnf_elig_18; by bene_id admsndt dschrgdt; run;

proc freq data=tempn.medpar1216_hospsnf_elig_18; 
tables (LTC_Prior100 NH_Stay_Prior100);
run; 

***Merge in indicators for patients received SNF hospice care or patients with end-stage disease, which are created in the PAC use code;
proc sql;
create table tempn.medpar1216_hospsnf_elig_19 as 
select a.*, b.O0100K2_HOSPC_POST_CD as snf_hospice label="Received Hospice Care at SNF"
from tempn.medpar1216_hospsnf_elig_18 as a
left join pac_mds.mds_hospice_2010_2016 as b
on a.bene_id=b.bene_id and -1 le a.admsndt-b.MDS_ENTRY_DT le 1 and SNF_NUM^=.;
quit;

proc sort data=tempn.medpar1216_hospsnf_elig_19 nodupkey; by medpar_id; run; 

proc sql;
create table tempn.medpar1216_hospsnf_elig_20 as 
select a.*, b.LIFE_PRGNS_6MONTH as LIFE_PRGNS_6MONTH label="Life Prognosis Less Than Six Months"
from tempn.medpar1216_hospsnf_elig_19 as a
left join pac_mds.mds_endstage_2010_16 as b
on a.bene_id=b.bene_id and -1 le a.admsndt-b.MDS_ENTRY_DT le 1 and SNF_NUM^=.;
quit;

proc sort data=tempn.medpar1216_hospsnf_elig_20 nodupkey; by medpar_id; run; 

*Get the maximum value of snf_hospice for each beneficiary's each benefit period;
proc sql; create table tempn.medpar1216_hospsnf_elig_sh as 
select bene_id, bene_period, max(snf_hospice) as snf_hospice_max
from tempn.medpar1216_hospsnf_elig_20
group by bene_id, bene_period; 
quit; 

*Merge snf_hospice_max value into data set;																																			  
proc sql; create table tempn.medpar1216_hospsnf_elig_21 as 
select a.*, b.snf_hospice_max
from tempn.medpar1216_hospsnf_elig_20 as a 
left join tempn.medpar1216_hospsnf_elig_sh as b 
on a.bene_id =b.bene_id and a.bene_period=b.bene_period; 
quit; 

*Get the maximum value of LIFE_PRGNS_6MONTH for each beneficiary's each benefit period;
proc sql; create table tempn.medpar1216_hospsnf_elig_lp6 as 
select bene_id, bene_period, max(LIFE_PRGNS_6MONTH) as LIFE_PRGNS_6MONTH_max
from tempn.medpar1216_hospsnf_elig_21
group by bene_id, bene_period; 
quit; 

*Merge LIFE_PRGNS_6MONTH_max value into data set;																																			  
proc sql; create table tempn.medpar1216_hospsnf_elig_22 as 
select a.*, b.LIFE_PRGNS_6MONTH_max
from tempn.medpar1216_hospsnf_elig_21 as a 
left join tempn.medpar1216_hospsnf_elig_lp6 as b 
on a.bene_id =b.bene_id and a.bene_period=b.bene_period; 
quit; 

data tempn.medpar1216_hospsnf_elig_23;
set tempn.medpar1216_hospsnf_elig_22(drop=snf_hospice LIFE_PRGNS_6MONTH);
rename LIFE_PRGNS_6MONTH_MAX=LIFE_PRGNS_6MONTH snf_hospice_max=snf_hospice;
label  LIFE_PRGNS_6MONTH_MAX="Life Prognosis Less Than Six Months"
       snf_hospice_max="Received Hospice Care at SNF";
run; *26,305,314; 

*Check the number of records that received hospice care in SNF;
proc sql; create table check_snf_hospice as select bene_id from tempn.medpar1216_hospsnf_elig_23 where snf_hospice="1"; quit; *649,926 (2.47%);

*Check the number of records with life prognosis less than 6 months;
proc sql; create table check_LIFE_PRGNS_6MONTH as select bene_id from tempn.medpar1216_hospsnf_elig_23 where LIFE_PRGNS_6MONTH="1"; quit; *243,568 (0.93%);


/****************************************************************************************************
 Step 4: Create final analytical data set which contains both hospitalization and SNF stay records
****************************************************************************************************/
***Exclude the last 60 days because we need to merge in the DRG code of readmission within 30 days of index hospital discharge. Since the readmission file doesn't have records for the last 30 days 
of 2016 and we also need to make sure we have a 30 day observation window for readmission, we have to exclude the last 60 days of 2016;
options validvarname=upcase;
data snf_bpa.Hosp_SNF_Bene_Period_1216;
set  tempn.medpar1216_hospsnf_elig_23;
where DSCHRGDT>=18993 & DSCHRGDT+60<=20819;

*Recode string variables to numeric;
if sex="1" then sex_num=1;
else if sex="2" then sex_num=2;
else if sex="0" then sex_num=3;

if race="0" then race_num=7;
else if race="1" then race_num=1;
else if race="2" then race_num=2; 
else if race="3" then race_num=3;
else if race="4" then race_num=4;
else if race="5" then race_num=5;
else if race="6" then race_num=6;

if snf_hospice=. then snf_hospice="0";
if LIFE_PRGNS_6MONTH=. then LIFE_PRGNS_6MONTH="0";
run; *25,684,686;

proc sort data=snf_bpa.Hosp_SNF_Bene_Period_1216; by bene_id admsndt dschrgdt; run;



***************************************************************************
***************************************************************************
PART II - Create hospital stay data set with post-discharge information 
          and Medicare payment variables
***************************************************************************
***************************************************************************;

/********************************************************************************
 Step 1: Create data set of hospital stays and merge in readmission variables
********************************************************************************/
* Seperate 2012-2016 hospital stay and SNF records;
data snf_bpt.SNF_Bene_Period_1216_h snf_bpt.SNF_Bene_Period_1216_s;
set snf_bpa.Hosp_SNF_Bene_Period_1216;
if SNF_num=. then output snf_bpt.SNF_Bene_Period_1216_h;
else output snf_bpt.SNF_Bene_Period_1216_s;
run; *16,714,746 records in hospital stay data set / 8,969,940 records in SNF data set;

* Merge SNF provider number into hospital stay data set;
proc sql;
create table snf_bpt.SNF_Bene_Period_1216_h_2 as 
select a.*, b.SNF_Prvdrnum, b.pmt_amt_pseudo as snf_pmt_amt_pseudo, b.admsndt as ADMSNDT_SNF, b.DSCHRGDT as DSCHRGDT_SNF
from snf_bpt.SNF_Bene_Period_1216_h as a left join tempn.medpar1216_snf24h as b
on a.bene_id=b.bene_id and a.dschrgdt<=b.admsndt<=a.dschrgdt+1;
quit; *16,725,741 records;  

proc sort data=snf_bpt.SNF_Bene_Period_1216_h_2 nodupkey; by medpar_id; run; *16,714,746;

proc sql; create table snf_missing as 
select bene_id from snf_bpt.SNF_Bene_Period_1216_h_2 
where enter_snf_24h=1 and (SNF_Prvdrnum="" or snf_pmt_amt_pseudo=.); 
quit; * 0 records; 

* Merge with readmission data set;
proc sql;
create table snf_bpt.SNF_Bene_Period_1216_h_3 as 
select a.*, b.radm30, b.dd30, b.drgcd, hxinfection, otherinfectious, metacancer, severecancer, othercancer, diabetes, malnutrition, liverdisease, 
hematological, alcohol, psychological, motordisfunction, seizure, chf, cadcvd, arrhythmias, copd, lungdisorder, ondialysis, ulcers, septicemia, metabolicdisorder, 
irondeficiency, cardiorespiratory, renalfailure, pancreaticdisease, arthritis, respiratordependence, transplants, coagulopathy, hipfracture
from snf_bpt.SNF_Bene_Period_1216_h_2 as a left join readm.hw_readm_0916_final as b
on a.BENE_ID=b.HICNO & a.ADMSNDT>=b.ADMIT & a.DSCHRGDT<=b.DISCH;
quit; *16,743,019 records; 
 
proc sort data=snf_bpt.SNF_Bene_Period_1216_h_3 nodupkey; by medpar_id; run; *16,714,746;

proc sql; create table master_only as select bene_id from snf_bpt.SNF_Bene_Period_1216_h_3 where drgcd=.; quit; 
*660,755 (3.95%) observations don't have matched records from readmission file;

* Merge with readmission data set (to get drg code for readmission;
proc sql;
create table snf_bpt.SNF_Bene_Period_1216_h_4 as 
select a.*, b.drgcd as drgcd_readm
from snf_bpt.SNF_Bene_Period_1216_h_3 as a left join readm.hw_readm_0916_final as b
on a.BENE_ID=b.HICNO & a.DSCHRGDT<=b.admit<=a.DSCHRGDT+30 & a.radm30=1;
quit;  *17,080,560;

proc sort data=snf_bpt.SNF_Bene_Period_1216_h_4 nodupkey; by medpar_id; run; *16,714,746 ; 

proc sql; create table radm as select bene_id from snf_bpt.SNF_Bene_Period_1216_h_4 where radm30=1; quit; *2,253,247 records have 30-day readmission;

proc sql; create table master_only as select bene_id from snf_bpt.SNF_Bene_Period_1216_h_4 where radm30=1 and drgcd_readm=.; quit;
*36,543 records among total 2,214,376 records (1.5%);

* Calculate total payment and count of comorbidities;
data snf_bpt.SNF_Bene_Period_1216_h_5;
set snf_bpt.SNF_Bene_Period_1216_h_4;
benefit_day=SNF_day-1;
tt_pmt_amt_pseudo=sum(pmt_amt_pseudo, snf_pmt_amt_pseudo);
combdty_sum=sum(hxinfection, otherinfectious, metacancer, severecancer, othercancer, diabetes, malnutrition, liverdisease, 
hematological, alcohol, psychological, motordisfunction, seizure, chf, cadcvd, arrhythmias, copd, lungdisorder, ondialysis, ulcers, septicemia, metabolicdisorder, 
irondeficiency, cardiorespiratory, renalfailure, pancreaticdisease, arthritis, respiratordependence, transplants, coagulopathy, hipfracture);
label SNF_Prvdrnum="SNF Provider Number"  drgcd_readm="DRG Code of Readmission" benefit_day="Benefit Period Day Count" combdty_sum="Count of Comorbidity"
      snf_pmt_amt_pseudo="SNF Medicare Payment Amount" tt_pmt_amt_pseudo="Total Medicare Payment Amount";
run; *16,714,746 ;

data snf_bpt.SNF_Bene_Period_1216_h_5_1 snf_bpt.SNF_Bene_Period_1216_h_5_2;
set snf_bpt.SNF_Bene_Period_1216_h_5;
if enter_snf_24h=1 then output snf_bpt.SNF_Bene_Period_1216_h_5_1;
else output snf_bpt.SNF_Bene_Period_1216_h_5_2;
run; * Enter SNF: 7,372,919 / Enter other PACs: 9,341,827;


/********************************************************************************
 Step 2: Merge with IRF, HHA and other destinations cohort to get the discharge 
         destination for the patients who were not discharged to SNF within 24 
         hours of hospital discharge
********************************************************************************/
*** Set up IRF cohort;
proc sql;
create table MedPAR2012_16_IRF as
select BENE_ID, ADMSNDT_IRF, DSCHRGDT_IRF, PMT_AMT_IRF, PRVDR_NUM, SPCLUNIT, UTIL_DAY, medpar_id from Medpar.Mp100mod_2012(rename=(ADMSNDT=ADMSNDT_IRF DSCHRGDT=DSCHRGDT_IRF PMT_AMT=PMT_AMT_IRF))
where SPCLUNIT in ('R','T') or ("3025"<=substr(prvdr_num,3,4)<="3099") union all
select BENE_ID, ADMSNDT_IRF, DSCHRGDT_IRF, PMT_AMT_IRF, PRVDR_NUM, SPCLUNIT, UTIL_DAY, medpar_id from Medpar.Mp100mod_2013(rename=(ADMSNDT=ADMSNDT_IRF DSCHRGDT=DSCHRGDT_IRF PMT_AMT=PMT_AMT_IRF))
where SPCLUNIT in ('R','T') or ("3025"<=substr(prvdr_num,3,4)<="3099") union all
select BENE_ID, ADMSNDT_IRF, DSCHRGDT_IRF, PMT_AMT_IRF, PRVDR_NUM, SPCLUNIT, UTIL_DAY, medpar_id from Medpar.Mp100mod_2014(rename=(ADMSNDT=ADMSNDT_IRF DSCHRGDT=DSCHRGDT_IRF PMT_AMT=PMT_AMT_IRF))
where SPCLUNIT in ('R','T') or ("3025"<=substr(prvdr_num,3,4)<="3099") union all
select BENE_ID, ADMSNDT_IRF, DSCHRGDT_IRF, PMT_AMT_IRF, PRVDR_NUM, SPCLUNIT, UTIL_DAY, medpar_id from Medpar.Mp100mod_2015(rename=(ADMSNDT=ADMSNDT_IRF DSCHRGDT=DSCHRGDT_IRF PMT_AMT=PMT_AMT_IRF))
where SPCLUNIT in ('R','T') or ("3025"<=substr(prvdr_num,3,4)<="3099") union all
select BENE_ID, ADMSNDT_IRF, DSCHRGDT_IRF, PMT_AMT_IRF, PRVDR_NUM, SPCLUNIT, UTIL_DAY, medpar_id from Medpar.Mp100mod_2016(rename=(ADMSNDT=ADMSNDT_IRF DSCHRGDT=DSCHRGDT_IRF PMT_AMT=PMT_AMT_IRF))
where SPCLUNIT in ('R','T') or ("3025"<=substr(prvdr_num,3,4)<="3099") 
order by BENE_ID, ADMSNDT_IRF;
quit; *2,189,771 records;
data snf_bpt.MedPAR2012_16_IRF_unique;
set MedPAR2012_16_IRF;
by BENE_ID ADMSNDT_IRF;
if first.ADMSNDT_IRF then output;
run; *2,189,712 records;

***Set up HHA cohort 2012-2016 - use mp.hha2010_16_unique which was created for PAC use project;

***Set up other PAC destinations cohort;
data medpar2012;
	set Medpar.Mp100mod_2012;
	if (substr(PRVDR_NUM,3,1) in ('0','M','R','S','T') or 1300<=substr(PRVDR_NUM,3,4)<=1399) & SPCLUNIT not in ('M','R','S','T') then type=0;
		else if substr(PRVDR_NUM,3,1) in ('5','6','U','W','Y','Z') then type=1;
		else if SPCLUNIT in ('R','T') or ("3025"<=substr(prvdr_num,3,4)<="3099") then type=2;
		else type=4;
	keep BENE_ID ADMSNDT DSCHRGDT PRVDR_NUM PMT_AMT type UTIL_DAY medpar_id;
run;
data medpar2013;
	set Medpar.Mp100mod_2013;
	if (substr(PRVDR_NUM,3,1) in ('0','M','R','S','T') or 1300<=substr(PRVDR_NUM,3,4)<=1399) & SPCLUNIT not in ('M','R','S','T') then type=0;
		else if substr(PRVDR_NUM,3,1) in ('5','6','U','W','Y','Z') then type=1;
		else if SPCLUNIT in ('R','T') or ("3025"<=substr(prvdr_num,3,4)<="3099") then type=2;
		else type=4;
	keep BENE_ID ADMSNDT DSCHRGDT PRVDR_NUM PMT_AMT type UTIL_DAY medpar_id;
run;
data medpar2014;
	set Medpar.Mp100mod_2014;
	if (substr(PRVDR_NUM,3,1) in ('0','M','R','S','T') or 1300<=substr(PRVDR_NUM,3,4)<=1399) & SPCLUNIT not in ('M','R','S','T') then type=0;
		else if substr(PRVDR_NUM,3,1) in ('5','6','U','W','Y','Z') then type=1;
		else if SPCLUNIT in ('R','T') or ("3025"<=substr(prvdr_num,3,4)<="3099") then type=2;
		else type=4;
	keep BENE_ID ADMSNDT DSCHRGDT PRVDR_NUM PMT_AMT type UTIL_DAY medpar_id;
run;
data medpar2015;
	set Medpar.Mp100mod_2015;
	if (substr(PRVDR_NUM,3,1) in ('0','M','R','S','T') or 1300<=substr(PRVDR_NUM,3,4)<=1399) & SPCLUNIT not in ('M','R','S','T') then type=0;
		else if substr(PRVDR_NUM,3,1) in ('5','6','U','W','Y','Z') then type=1;
		else if SPCLUNIT in ('R','T') or ("3025"<=substr(prvdr_num,3,4)<="3099") then type=2;
		else type=4;
	keep BENE_ID ADMSNDT DSCHRGDT PRVDR_NUM PMT_AMT type UTIL_DAY medpar_id;
run;
data medpar2016;
	set Medpar.Mp100mod_2016;
	if (substr(PRVDR_NUM,3,1) in ('0','M','R','S','T') or 1300<=substr(PRVDR_NUM,3,4)<=1399) & SPCLUNIT not in ('M','R','S','T') then type=0;
		else if substr(PRVDR_NUM,3,1) in ('5','6','U','W','Y','Z') then type=1;
		else if SPCLUNIT in ('R','T') or ("3025"<=substr(prvdr_num,3,4)<="3099") then type=2;
		else type=4;
	keep BENE_ID ADMSNDT DSCHRGDT PRVDR_NUM PMT_AMT type UTIL_DAY medpar_id;
run;

proc sql;
create table medpar2012_16_other as
select BENE_ID, ADMSNDT as ADMSNDT_other, DSCHRGDT as DSCHRGDT_other, PRVDR_NUM as other_PRVDRNUM, PMT_AMT as PMT_AMT_other, UTIL_DAY as UTIL_DAY_other, medpar_id as medpar_id_other 
from medpar2012 where type eq 4 union all
select BENE_ID, ADMSNDT as ADMSNDT_other, DSCHRGDT as DSCHRGDT_other, PRVDR_NUM as other_PRVDRNUM, PMT_AMT as PMT_AMT_other, UTIL_DAY as UTIL_DAY_other, medpar_id as medpar_id_other 
from medpar2013 where type eq 4 union all
select BENE_ID, ADMSNDT as ADMSNDT_other, DSCHRGDT as DSCHRGDT_other, PRVDR_NUM as other_PRVDRNUM, PMT_AMT as PMT_AMT_other, UTIL_DAY as UTIL_DAY_other, medpar_id as medpar_id_other 
from medpar2014 where type eq 4 union all
select BENE_ID, ADMSNDT as ADMSNDT_other, DSCHRGDT as DSCHRGDT_other, PRVDR_NUM as other_PRVDRNUM, PMT_AMT as PMT_AMT_other, UTIL_DAY as UTIL_DAY_other, medpar_id as medpar_id_other 
from medpar2015 where type eq 4 union all
select BENE_ID, ADMSNDT as ADMSNDT_other, DSCHRGDT as DSCHRGDT_other, PRVDR_NUM as other_PRVDRNUM, PMT_AMT as PMT_AMT_other, UTIL_DAY as UTIL_DAY_other, medpar_id as medpar_id_other 
from medpar2016 where type eq 4;
quit; *2,720,743 records;

proc sort data=medpar2012_16_other; by BENE_ID ADMSNDT_other; run; 

data snf_bpt.medpar2012_16_other_unique;
set medpar2012_16_other;
by BENE_ID ADMSNDT_other;
where 18993<=ADMSNDT_other<=20789;
if first.ADMSNDT_other then output;
run; *2,668,634 records;

***Use the cohort of hospitalization records to merge with IRF, HHA and other PACs;
proc sql;
create table snf_bpt.SNF_Bene_Period_1216_h_5_3 as
select distinct main.*, irf.PRVDR_NUM as IRF_PRVDRNUM, irf.ADMSNDT_IRF, irf.DSCHRGDT_IRF, 
                hha.PRVDR_NUM as HHA_PRVDRNUM, hha.CLM_FROM_DT as ADMSNDT_HHA, hha.CLM_THRU_DT as DSCHRGDT_HHA, hha.CLM_PMT_AMT as HHA_PMT_AMT,
                other.other_PRVDRNUM, other.ADMSNDT_other, other.DSCHRGDT_OTHER 
from snf_bpt.SNF_Bene_Period_1216_h_5_2 as main
left join snf_bpt.MedPAR2012_16_IRF_unique as irf on main.BENE_ID=irf.BENE_ID
left join mp.hha2010_16_unique as hha on main.BENE_ID=hha.BENE_ID
left join snf_bpt.medpar2012_16_other_unique as other on main.BENE_ID=other.BENE_ID;
quit; *30,154,286;

*Patients should be sent to post-acute care instutitions within 24 hours of hospital discharge date; 
data snf_bpt.SNF_Bene_Period_1216_h_5_4;
set snf_bpt.SNF_Bene_Period_1216_h_5_3;
if 0 le ADMSNDT_IRF-DSCHRGDT le 1 then disch_pac_n=2;
else if 0 le ADMSNDT_HHA-DSCHRGDT le 3 then disch_pac_n=3;
else if 0 le ADMSNDT_other-DSCHRGDT le 1 then disch_pac_n=4;
else disch_pac_n=99;
run; 
proc sort data=snf_bpt.SNF_Bene_Period_1216_h_5_4; by medpar_id disch_pac_n; run;

*Output the first matched PAC institution for each hospital stay;
data snf_bpt.SNF_Bene_Period_1216_h_5_5;
set snf_bpt.SNF_Bene_Period_1216_h_5_4;
by medpar_id disch_pac_n;
if first.medpar_id then output;
run; *9,341,827;

*Recode missing "disch_pac_n" to 0, which means patients were discharged to home;
data snf_bpt.SNF_Bene_Period_1216_h_5_5;
set snf_bpt.SNF_Bene_Period_1216_h_5_5;
if disch_pac_n=99 then disch_pac_n=0;
run;

*Combine SNF cohort with cohort of other PACs;
data snf_bpt.SNF_Bene_Period_1216_h_6;
set snf_bpt.SNF_Bene_Period_1216_h_5_1 snf_bpt.SNF_Bene_Period_1216_h_5_5;
if disch_pac_n=. then do; disch_pac_n=1; PAC_ADMSNDT=ADMSNDT_SNF; PAC_DSCHRGDT=DSCHRGDT_SNF; end;
else if disch_pac_n=2 then do; PAC_ADMSNDT=ADMSNDT_IRF; PAC_DSCHRGDT=DSCHRGDT_IRF; end;
else if disch_pac_n=3 then do; PAC_ADMSNDT=ADMSNDT_HHA;	PAC_DSCHRGDT=DSCHRGDT_HHA; end;
else if disch_pac_n=4 then do; PAC_ADMSNDT=ADMSNDT_OTHER; PAC_DSCHRGDT=DSCHRGDT_OTHER; end;
label disch_pac_n="PAC Destination within 24 Hours" PAC_ADMSNDT="PAC Admission Date";
run; *16,714,746 ;

*Subgroup discharges that went to HHA;
data snf_bpt.SNF_Bene_Period_1216_h_hha;
set snf_bpt.SNF_Bene_Period_1216_h_6;
where disch_pac_n=3 ;
run; *1,155,893;

*Generate pseudo HHA payment amount which recodes negative value to 0 and generate total payment amount;
data snf_bpt.SNF_Bene_Period_1216_h_hha_2(drop=hha_pmt_amt);
set snf_bpt.SNF_Bene_Period_1216_h_hha;
if hha_pmt_amt^=. and hha_pmt_amt<0 then hha_pmt_amt_pseudo=0; 
else hha_pmt_amt_pseudo=hha_pmt_amt;
tt_pmt_amt_pseudo_hha=pmt_amt_pseudo + hha_pmt_amt_pseudo;
run;

*Merge HHA payment variables into main hospital data set;
proc sql;
create table snf_bpt.SNF_Bene_Period_1216_h_7 as 
select a.*, b.hha_pmt_amt_pseudo, b.tt_pmt_amt_pseudo_hha from 
snf_bpt.SNF_Bene_Period_1216_h_6 as a left join snf_bpt.SNF_Bene_Period_1216_h_hha_4 as b
on a.medpar_id=b.medpar_id;
quit; *16,714,746;

data snf_bpt.SNF_Bene_Period_1216_h_8(drop=tt_pmt_amt_pseudo_hha);
set snf_bpt.SNF_Bene_Period_1216_h_7;
if disch_pac_n=3 then tt_pmt_amt_pseudo=tt_pmt_amt_pseudo_hha;
label hha_pmt_amt_pseudo="HHA Medicare Payment Amount";
run; *16,714,746;


/********************************************************************************
 Step 3: Merge in hospital characteristics and hospital-based SNF indicator
********************************************************************************/
data pos12; set snf_bpt.pos2012(keep=prvdr_num state_cd crtfd_bed_cnt CBSA_URBN_RRL_IND gnrl_cntl_type_cd mdcl_schl_afltn_cd); year=2012; run; *141,342;
data pos13; set snf_bpt.pos2013(keep=prvdr_num state_cd crtfd_bed_cnt CBSA_URBN_RRL_IND gnrl_cntl_type_cd mdcl_schl_afltn_cd); year=2013; run; *133,879;
data pos14; set snf_bpt.pos2014(keep=prvdr_num state_cd crtfd_bed_cnt CBSA_URBN_RRL_IND gnrl_cntl_type_cd mdcl_schl_afltn_cd); year=2014; run; *136,502;
data pos15; set snf_bpt.pos2015(keep=prvdr_num state_cd crtfd_bed_cnt CBSA_URBN_RRL_IND gnrl_cntl_type_cd mdcl_schl_afltn_cd); year=2015; run; *138,803;
data pos16; set snf_bpt.pos2016(keep=prvdr_num state_cd crtfd_bed_cnt CBSA_URBN_RRL_IND gnrl_cntl_type_cd mdcl_schl_afltn_cd); year=2016; run; *141,557;
data pos12_16; set pos12 pos13 pos14 pos15 pos16; run; *692,083;

proc sql;
create table snf_bpt.SNF_Bene_Period_1216_h_9 as
select main.*, pos.crtfd_bed_cnt, pos.gnrl_cntl_type_cd, pos.state_cd, pos.CBSA_URBN_RRL_IND, pos.mdcl_schl_afltn_cd
from snf_bpt.SNF_Bene_Period_1216_h_8 as main left join pos12_16 as pos
on main.prvdr_num=pos.prvdr_num and year_dschrg=year;
quit; *16,714,746;

*Check how many observations couldn't find a match;
proc sql; create table check_missing_1 as select bene_id from snf_bpt.SNF_Bene_Period_1216_h_9 where missing(crtfd_bed_cnt); quit; *2,945 (<0.1%);
proc sql; create table check_missing_2 as select bene_id from snf_bpt.SNF_Bene_Period_1216_h_9 where missing(gnrl_cntl_type_cd); quit; *3,296 (<0.1%);
proc sql; create table check_missing_3 as select bene_id from snf_bpt.SNF_Bene_Period_1216_h_9 where missing(state_cd); quit; *2,945 (<0.1%);
proc sql; create table check_missing_4 as select bene_id from snf_bpt.SNF_Bene_Period_1216_h_9 where missing(CBSA_URBN_RRL_IND); quit; *3,014 (<0.1%);
proc sql; create table check_missing_5 as select bene_id from snf_bpt.SNF_Bene_Period_1216_h_9 where missing(mdcl_schl_afltn_cd); quit; *2,945 (<0.1%);

*Create indicators for profit status and teching hospital;
data snf_bpt.SNF_Bene_Period_1216_h_10;
set snf_bpt.SNF_Bene_Period_1216_h_9;
if GNRL_CNTL_TYPE_CD in ("04","09") then for_profit=1;
else if GNRL_CNTL_TYPE_CD in ("01","02","03","05","06","07","08","10") then for_profit=0;
if mdcl_schl_afltn_cd in (1,2,3) then teaching_hosp=1 ;
else if  mdcl_schl_afltn_cd=4 then teaching_hosp=0;
if CBSA_URBN_RRL_IND="U" then urban=1;
else if CBSA_URBN_RRL_IND="R" then urban=0;
label for_profit="For-profit Hospital" teaching_hosp="Teaching Hospital" urban="Urban Area";
run; *16,714,746;

***Merge in Hospital-based SNF indicator;
proc sql;
create table snf_bpt.SNF_Bene_Period_1216_h_11 as 
select a.*, b.hosp_snf, b.snf_prvdrnum as hosp_based_snf_prvdrnum
from snf_bpt.SNF_Bene_Period_1216_h_10 as a left join pac.hosp_based_pac_1016_final as b
on a.prvdr_num=b.hosp_prvdrnum and a.year_dschrg=b.year;
quit; *16,714,746;


/************************************************************************************
 Step 4: Create an indicator for hospital stays that came right after a SNF stay 
         and another indicator for hospital stats that happened in 30-day SNF break
************************************************************************************/
proc sql;
create table snf_bpt.SNF_Bene_Period_1216_h_12 as 
select a.*, b.prvdr_num as prior_snf_prvdrnum label="Prior SNF Provider Number"
from snf_bpt.SNF_Bene_Period_1216_h_11 as a left join snf_bpt.SNF_Bene_Period_1216_s as b
on a.bene_id=b.bene_id and a.bene_period=b.bene_period and b.dschrgdt<=a.admsndt<=b.dschrgdt+1;
quit; *16,715,715;

proc sort data=snf_bpt.SNF_Bene_Period_1216_h_12 nodupkey; by medpar_id; run; *16,714,746;

proc sql;
create table snf_bpt.SNF_Bene_Period_1216_h_13 as 
select a.*, b.prvdr_num as prior_30_snf_prvdrnum label="Prior 30-Day SNF Provider Number"
from snf_bpt.SNF_Bene_Period_1216_h_12 as a left join snf_bpt.SNF_Bene_Period_1216_s as b
on a.bene_id=b.bene_id and a.bene_period=b.bene_period and b.dschrgdt<=a.admsndt<=b.dschrgdt+30;
quit; *17,037,627;

proc sort data=snf_bpt.SNF_Bene_Period_1216_h_13 nodupkey; by medpar_id; run; *16,714,746;

data snf_bpt.SNF_Bene_Period_1216_h_14(drop=prior_snf_prvdrnum prior_30_snf_prvdrnum);
set  snf_bpt.SNF_Bene_Period_1216_h_13;
if 	prior_snf_prvdrnum^="" then admit_from_snf=1;
else if prior_snf_prvdrnum="" then admit_from_snf=0;
if 	prior_30_snf_prvdrnum^="" then within_snf30break=1;
else if prior_30_snf_prvdrnum="" then within_snf30break=0;
if first_hosp=1 & (ded_amt=0 | ded_amt<0) then other_cvrg=1;
else other_cvrg=0;
label admit_from_snf="Admitted from SNF within 24 Hours" within_snf30break="Within the SNF 30-Day Break" other_cvrg="Other Coverage";
run; *16,714,746;

proc sql;
create table tempn.other_cvrg_1216 as 
select bene_id, bene_period, max(other_cvrg) as other_cvrg_max from snf_bpt.SNF_Bene_Period_1216_h_14
group by bene_id, bene_period;
quit; *11,303,392;

proc sql;
create table snf_bpt.SNF_Bene_Period_1216_h_15 as 
select a.*, b.other_cvrg_max
from snf_bpt.SNF_Bene_Period_1216_h_14 as a left join tempn.other_cvrg_1216 as b 
on a.bene_id=b.bene_id and a.bene_period=b.bene_period;
quit;


/************************************************************************************
 Step 5: Create 2 versions of discretionary hospital stay indicator
************************************************************************************/
data snf_bpt.SNF_Bene_Period_1216_h_16;
set snf_bpt.SNF_Bene_Period_1216_h_15;
*1;
if 280<=drg_cd<=284 then ami=1;else ami=0;
*2;
if 70<=drg_cd<=72 then cereb_do=1; else cereb_do=0;
*3;
if 480<=drg_cd<=482 then hip_rep=1; else hip_rep=0;
*4;
if 350<=drg_cd<=352 then ifhr=1; else ifhr=0;
*5;
if 329<=drg_cd<=331 then mbo=1; else mbo=0;
*6;
if 411<=drg_cd<=419 then ccy=1; else ccy=0;
*7;
if 377<=drg_cd<=379 then gast_bld=1; else gast_bld=0;
*8;
if 338<=drg_cd<=343 then appy=1; else appy=0;
*9;
if drg_cd=189 then resp_flr=1; else resp_flr=0;
*10;
if 870<=drg_cd<=872 or 94<=drg_cd<=96 or 288<=drg_cd<=290 then seve_inf=1; else seve_inf=0;
*11;
if ami=1 or cereb_do=1 or hip_rep=1 or ifhr=1 or mbo=1 or
   ccy=1 or gast_bld=1 or appy=1 or resp_flr=1 or seve_inf=1 then non_dsct_hosp=1; else non_dsct_hosp=0;

if non_dsct_hosp=0 then dsct_hosp=1; else dsct_hosp=0;

label non_dsct_hosp="Non-discretionary Hospitalization" dsct_hosp="Discretionary Hospitalization";
run;


/************************************************************************************
 Step 6: Create high and low percentage of dual eligible patient indicators and 
         emergent hospitalzation indicator
************************************************************************************/
***Summarize % of dual eligible patients at hospital level;
proc sql; 
create table tempn.pac_all_1016_dual as
select hosp_prvdrnum, avg(dual_stus) as dual_pat_pct label="Dual Eligible Patient Percentage"
from pac.pac_all_analytical_1016 
group by hosp_prvdrnum
order by hosp_prvdrnum;
quit;

proc means data=tempn.pac_all_1016_dual n mean median std min max maxdec=2;
var dual_pat_pct;
run;

*Create high percentage of dual eligible patient indicator;
data tempn.pac_all_1016_dual_2;
set tempn.pac_all_1016_dual;
if 	dual_pat_pct>=0.16 then high_pct_dual=1;
else high_pct_dual=0;
label high_pct_dual="High Percentage of Dual Eligible Patient";
run;

proc freq data=tempn.pac_all_1016_dual_2;
table high_pct_dual;
run;

*Merge high percentage of dual eligible patient indicator into hospital data set; 
proc sql;
create table snf_bpt.SNF_Bene_Period_1216_h_17 as 
select a.*, b.high_pct_dual 
from snf_bpt.SNF_Bene_Period_1216_h_16 as a
left join tempn.pac_all_1016_dual_2 as b
on a.PRVDR_NUM=b.hosp_prvdrnum;
quit;

proc sql; create table check_merge as select bene_id from snf_bpt.SNF_Bene_Period_1216_h_17 where high_pct_dual=.; quit;*6,941 (0.04%);

proc freq data=snf_bpt.SNF_Bene_Period_1216_h_17;
table high_pct_dual;
run;

data medpar1216_hosp_type_adm;
set  medpar.mp100mod_2011(keep=BENE_ID medpar_id TYPE_ADM PRVDR_NUM SPCLUNIT DSCHRGDT)
     medpar.mp100mod_2012(keep=BENE_ID medpar_id TYPE_ADM PRVDR_NUM SPCLUNIT DSCHRGDT)
     medpar.mp100mod_2013(keep=BENE_ID medpar_id TYPE_ADM PRVDR_NUM SPCLUNIT DSCHRGDT)
     medpar.mp100mod_2014(keep=BENE_ID medpar_id TYPE_ADM PRVDR_NUM SPCLUNIT DSCHRGDT)
     medpar.mp100mod_2015(keep=BENE_ID medpar_id TYPE_ADM PRVDR_NUM SPCLUNIT DSCHRGDT)
	 medpar.mp100mod_2016(keep=BENE_ID medpar_id TYPE_ADM PRVDR_NUM SPCLUNIT DSCHRGDT);
where (substr(PRVDR_NUM,3,1) in ('0','M','R','S','T') or substr(PRVDR_NUM,3,2)="13") & SPCLUNIT not in ('M','R','S','T') & 18993<=DSCHRGDT<=20789;
run; *66,498,464;

proc sql;
create table snf_bpt.SNF_Bene_Period_1216_h_18 as 
select a.*, b.type_Adm 
from snf_bpt.SNF_Bene_Period_1216_h_17 as a
left join medpar1216_hosp_type_adm as b
on a.medpar_id=b.medpar_id;
quit; *16,714,746;

data snf_bpt.SNF_Bene_Period_1216_h_19;
set snf_bpt.SNF_Bene_Period_1216_h_18(drop=other_cvrg);
rename other_cvrg_max=other_cvrg;
if type_adm in ("1", "2") then emergent=1; else emergent=0;
label emergent="Emergency or Urgent Admission";
if type_adm ="1" then emergency=1; else emergency=0;
label emergency="Emergency Admission";
if type_adm="2" then urgent=1; else urgent=0;
label urgent="Urgent Admission" other_cvrg_max="Other Coverage";
format disch_pac_n pacf_n.;
run; *16,714,746;


/****************************************************************************************
 Step 7: Create total payment to all types of PAC facilities variables and total payment
         within 60 days of hospital admission 
****************************************************************************************/
data snf_bpt.SNF_Bene_Period_1216_h_20;
set snf_bpt.SNF_Bene_Period_1216_h_19;
if DISCH_PAC_N ne 0 then do;
	if ADMSNDT+59 le DSCHRGDT then Hosp_Pmt_60=PMT_AMT_PSEUDO/(DSCHRGDT-ADMSNDT+1)*60; 
	else if DSCHRGDT lt ADMSNDT+59 lt PAC_ADMSNDT then Hosp_Pmt_60=PMT_AMT_PSEUDO; 
	else if PAC_ADMSNDT le ADMSNDT+59 lt PAC_DSCHRGDT then Hosp_Pmt_60=PMT_AMT_PSEUDO;
	else if ADMSNDT+59 ge PAC_DSCHRGDT then Hosp_Pmt_60=PMT_AMT_PSEUDO;
end; 
else do;
	if ADMSNDT+59 le DSCHRGDT then Hosp_Pmt_60=PMT_AMT_PSEUDO/(DSCHRGDT-ADMSNDT+1)*60; 
	else if ADMSNDT+59 gt DSCHRGDT then Hosp_Pmt_60=PMT_AMT_PSEUDO; 
end;

label Hosp_Pmt_60 = 'Payment amount to hospital within 60 days of hospital admission';
run; *16,714,746;

data merge_all_2010_16_all_trans;
	set temp.merge_all_2010_16_all_trans(keep = BENE_ID ADMSNDT_SNF DSCHRGDT_SNF_pseudo SNF_UTIL_DAY PMT_AMT_SNF SNF_PRVDRNUM ADMSNDT_IRF
	DSCHRGDT_IRF PMT_AMT_IRF IRF_PRVDRNUM ADMSNDT_HHA DSCHRGDT_HHA PMT_AMT_HHA HHA_PRVDRNUM ADMSNDT_other DSCHRGDT_other
	other_PRVDRNUM PMT_AMT_other ADMSNDT_ACUTE DSCHRGDT_ACUTE PMT_AMT_ACUTE PMT_AMT_ACUTE);
	rename DSCHRGDT_SNF_pseudo=DSCHRGDT_SNF;
run; *109,292,388;

proc sql;
create table snf_bpt.merge_all_2010_16_trans as
	select main.BENE_ID, main.ADMSNDT, main.DSCHRGDT, main.medpar_id, main.PRVDR_NUM as HOSP_PRVDRNUM, post.*
	from snf_bpt.SNF_Bene_Period_1216_h_20 as main
	left join merge_all_2010_16_all_trans as post on main.BENE_ID=post.BENE_ID;
quit; *170,515,505; 

*Keep acute-pac bundle only if pac admission is within 60 days after hospital admission;
data snf_bpt.merge_all_2010_16_trans_2;
set snf_bpt.merge_all_2010_16_trans;
if 0 le ADMSNDT_SNF-ADMSNDT le 59 then gap_1=ADMSNDT_SNF-ADMSNDT; else gap_1=.;
if 0 le ADMSNDT_IRF-ADMSNDT le 59 then gap_2=ADMSNDT_IRF-ADMSNDT; else gap_2=.;
if 0 le ADMSNDT_HHA-ADMSNDT le 59 then gap_3=ADMSNDT_HHA-ADMSNDT; else gap_3=.;
if 0 le ADMSNDT_OTHER-ADMSNDT le 59 then gap_4=ADMSNDT_OTHER-ADMSNDT; else gap_4=.;
*We require lt instead of le to avoid bundles with two same discharges;
if 0 lt ADMSNDT_ACUTE-ADMSNDT le 59 then gap_5=ADMSNDT_ACUTE-ADMSNDT; else gap_5=.;
if gap_1=. & gap_2=. & gap_3=. & gap_4=. & gap_5=. then delete;
*There should be only 1 nonmissing variable the five variables in each line;
PMT_AMT_60_AFTER=min(PMT_AMT_SNF, PMT_AMT_IRF, PMT_AMT_HHA, PMT_AMT_OTHER, PMT_AMT_ACUTE);
DSCHRGDT_60_AFTER=min(DSCHRGDT_SNF, DSCHRGDT_IRF, DSCHRGDT_HHA, DSCHRGDT_OTHER, DSCHRGDT_ACUTE);
ADMSNDT_60_AFTER=min(ADMSNDT_SNF, ADMSNDT_IRF, ADMSNDT_HHA, ADMSNDT_OTHER, ADMSNDT_ACUTE);
run; *19,680,425; 

proc sort data=snf_bpt.merge_all_2010_16_trans_2;
	by BENE_ID ADMSNDT DSCHRGDT;
run;

*Calculate each of pac and hospital payments within 60 days after hospital admission;
data snf_bpt.merge_all_2010_16_trans_3;
set snf_bpt.merge_all_2010_16_trans_2;
if DSCHRGDT_60_AFTER >= ADMSNDT+59 then do; 
	if gap_1^=. then do;
    	if SNF_UTIL_DAY^=0 then Pmt_After_Hosp_60=PMT_AMT_60_AFTER/SNF_UTIL_DAY*(ADMSNDT+59-ADMSNDT_60_AFTER+1);
		else Pmt_After_Hosp_60=PMT_AMT_60_AFTER/(DSCHRGDT_60_AFTER-ADMSNDT_60_AFTER+1)*(ADMSNDT+59-ADMSNDT_60_AFTER+1);
	end;
	else Pmt_After_Hosp_60=PMT_AMT_60_AFTER/(DSCHRGDT_60_AFTER-ADMSNDT_60_AFTER+1)*(ADMSNDT+59-ADMSNDT_60_AFTER+1);
end;
if DSCHRGDT_60_AFTER < ADMSNDT+59 then Pmt_After_Hosp_60=PMT_AMT_60_AFTER;
if Pmt_After_Hosp_60 < 0 then Pmt_After_Hosp_60 = 0;
*DSCHRG_ID=BENE_ID||'_'||ADMSNDT||'_'||DSCHRGDT;
if gap_1^=. then Pmt_After_Hosp_60_SNF=Pmt_After_Hosp_60;
else if gap_2^=. then Pmt_After_Hosp_60_IRF=Pmt_After_Hosp_60;
else if gap_3^=. then Pmt_After_Hosp_60_HHA=Pmt_After_Hosp_60;
else if gap_4^=. then Pmt_After_Hosp_60_Other=Pmt_After_Hosp_60;
else if gap_5^=. then Pmt_After_Hosp_60_ACUTE=Pmt_After_Hosp_60;
run; *19,680,425;

*Calculate the sum of pac and hospital payments for each hospital admission;
proc sql;
create table snf_bpt.merge_all_2010_16_trans_4 as
	select distinct BENE_ID, ADMSNDT, DSCHRGDT, medpar_id, SUM(Pmt_After_Hosp_60) as Pmt_After_Hosp_60_sum, HOSP_PRVDRNUM, DSCHRGDT_60_AFTER, ADMSNDT_60_AFTER,
	       SUM(Pmt_After_Hosp_60_SNF) as Pmt_After_Hosp_60_Sum_SNF, SUM(Pmt_After_Hosp_60_IRF) as Pmt_After_Hosp_60_Sum_IRF, SUM(Pmt_After_Hosp_60_HHA) as Pmt_After_Hosp_60_Sum_HHA,
		   SUM(Pmt_After_Hosp_60_Other) as Pmt_After_Hosp_60_Sum_Other, SUM(Pmt_After_Hosp_60_ACUTE) as Pmt_After_Hosp_60_Sum_ACUTE
	from snf_bpt.merge_all_2010_16_trans_3
	group by MEDPAR_ID;
quit; 

proc sort data=snf_bpt.merge_all_2010_16_trans_4 NODUP;
	by BENE_ID ADMSNDT DSCHRGDT;
run; 

*Merge into the original dataset;
proc sql;
create table snf_bpt.SNF_Bene_Period_1216_h_21 as
	select distinct main.*, payment.Pmt_After_Hosp_60_sum,
	                payment.Pmt_After_Hosp_60_sum_SNF, payment.Pmt_After_Hosp_60_sum_IRF, payment.Pmt_After_Hosp_60_sum_HHA, payment.Pmt_After_Hosp_60_sum_Other, payment.Pmt_After_Hosp_60_sum_Acute
	from snf_bpt.SNF_Bene_Period_1216_h_20  as main
	left join snf_bpt.merge_all_2010_16_trans_4 as payment
	on main.MEDPAR_ID=payment.MEDPAR_ID;
quit; *16,714,746; 


/***************************************************************************************
 Step 8: Create total payment to all types of PAC facilities variables and total payment
         within 90 days after hospital discharge
***************************************************************************************/
data merge_all_2010_16_all_trans;
	set temp.merge_all_2010_16_all_trans(keep = BENE_ID ADMSNDT_SNF DSCHRGDT_SNF_pseudo SNF_UTIL_DAY PMT_AMT_SNF SNF_PRVDRNUM ADMSNDT_IRF
	DSCHRGDT_IRF PMT_AMT_IRF IRF_PRVDRNUM ADMSNDT_HHA DSCHRGDT_HHA PMT_AMT_HHA HHA_PRVDRNUM ADMSNDT_other DSCHRGDT_other
	other_PRVDRNUM PMT_AMT_other ADMSNDT_ACUTE DSCHRGDT_ACUTE PMT_AMT_ACUTE PMT_AMT_ACUTE);
	rename DSCHRGDT_SNF_pseudo=DSCHRGDT_SNF;
run; *109,292,388;

proc sql;
create table snf_bpt.merge_all_2010_16_trans_90 as
	select main.BENE_ID, main.ADMSNDT, main.DSCHRGDT, main.medpar_id, main.PRVDR_NUM as HOSP_PRVDRNUM, post.*
	from snf_bpt.SNF_Bene_Period_1216_h_21 as main
	left join merge_all_2010_16_all_trans as post on main.BENE_ID=post.BENE_ID;
quit; *170,515,505; 

*Keep acute-pac bundle only if pac admission is within 90 days after hospital discharge;
data snf_bpt.merge_all_2010_16_trans_90_2;
set snf_bpt.merge_all_2010_16_trans_90;
if 0 le ADMSNDT_SNF-DSCHRGDT le 89 then gap_1=ADMSNDT_SNF-DSCHRGDT; else gap_1=.;
if 0 le ADMSNDT_IRF-DSCHRGDT le 89 then gap_2=ADMSNDT_IRF-DSCHRGDT; else gap_2=.;
if 0 le ADMSNDT_HHA-DSCHRGDT le 89 then gap_3=ADMSNDT_HHA-DSCHRGDT; else gap_3=.;
if 0 le ADMSNDT_OTHER-DSCHRGDT le 89 then gap_4=ADMSNDT_OTHER-DSCHRGDT; else gap_4=.;
if 0 le ADMSNDT_ACUTE-DSCHRGDT le 89 then gap_5=ADMSNDT_ACUTE-DSCHRGDT; else gap_5=.;
if gap_1=. & gap_2=. & gap_3=. & gap_4=. & gap_5=. then delete;
*There should be only 1 nonmissing variable the five variables in each line;
PMT_AMT_90_AFTER=min(PMT_AMT_SNF, PMT_AMT_IRF, PMT_AMT_HHA, PMT_AMT_OTHER, PMT_AMT_ACUTE);
DSCHRGDT_90_AFTER=min(DSCHRGDT_SNF, DSCHRGDT_IRF, DSCHRGDT_HHA, DSCHRGDT_OTHER, DSCHRGDT_ACUTE);
ADMSNDT_90_AFTER=min(ADMSNDT_SNF, ADMSNDT_IRF, ADMSNDT_HHA, ADMSNDT_OTHER, ADMSNDT_ACUTE);
run; *24,340,820; 

proc sort data=snf_bpt.merge_all_2010_16_trans_90_2;
	by BENE_ID ADMSNDT DSCHRGDT;
run;

*Calculate each of pac and hospital payments within 90 days after hospital discharge;
data snf_bpt.merge_all_2010_16_trans_90_3;
set snf_bpt.merge_all_2010_16_trans_90_2;

if DSCHRGDT_90_AFTER >= DSCHRGDT+89 then do; 
	if gap_1^=. then do;
    	if SNF_UTIL_DAY^=0 then Pmt_After_Hosp_90=PMT_AMT_90_AFTER/SNF_UTIL_DAY*(DSCHRGDT+89-ADMSNDT_90_AFTER+1);
		else Pmt_After_Hosp_90=PMT_AMT_90_AFTER/(DSCHRGDT_90_AFTER-ADMSNDT_90_AFTER+1)*(DSCHRGDT+89-ADMSNDT_90_AFTER+1);
	end;
	else Pmt_After_Hosp_90=PMT_AMT_90_AFTER/(DSCHRGDT_90_AFTER-ADMSNDT_90_AFTER+1)*(DSCHRGDT+89-ADMSNDT_90_AFTER+1);
end;
if DSCHRGDT_90_AFTER < DSCHRGDT+89 then Pmt_After_Hosp_90=PMT_AMT_90_AFTER;
if Pmt_After_Hosp_90 < 0 then Pmt_After_Hosp_90 = 0;

if gap_1^=. then Pmt_After_Hosp_90_SNF=Pmt_After_Hosp_90;
else if gap_2^=. then Pmt_After_Hosp_90_IRF=Pmt_After_Hosp_90;
else if gap_3^=. then Pmt_After_Hosp_90_HHA=Pmt_After_Hosp_90;
else if gap_4^=. then Pmt_After_Hosp_90_OTHER=Pmt_After_Hosp_90;
else if gap_5^=. then Pmt_After_Hosp_90_ACUTE=Pmt_After_Hosp_90;
run; *24,340,472;

*Calculate the sum of pac and hospital payments for each hospital admission;
proc sql;
create table snf_bpt.merge_all_2010_16_trans_90_4 as
	select distinct BENE_ID, ADMSNDT, DSCHRGDT, medpar_id, SUM(Pmt_After_Hosp_90) as Pmt_After_Hosp_90_sum, HOSP_PRVDRNUM, DSCHRGDT_90_AFTER, ADMSNDT_90_AFTER,
	       SUM(Pmt_After_Hosp_90_SNF) as Pmt_After_Hosp_90_Sum_SNF, SUM(Pmt_After_Hosp_90_IRF) as Pmt_After_Hosp_90_Sum_IRF, SUM(Pmt_After_Hosp_90_HHA) as Pmt_After_Hosp_90_Sum_HHA,
		   SUM(Pmt_After_Hosp_90_OTHER) as Pmt_After_Hosp_90_Sum_OTHER, SUM(Pmt_After_Hosp_90_ACUTE) as Pmt_After_Hosp_90_Sum_ACUTE
	from snf_bpt.merge_all_2010_16_trans_90_3
	group by MEDPAR_ID;
quit; 

proc sort data=snf_bpt.merge_all_2010_16_trans_90_4 NODUP;
	by BENE_ID ADMSNDT DSCHRGDT;
run; 

*Merge into the original dataset;
proc sql;
create table snf_bpt.SNF_Bene_Period_1216_h_22 as
	select distinct main.*, payment.Pmt_After_Hosp_90_sum,
	payment.Pmt_After_Hosp_90_sum_SNF, payment.Pmt_After_Hosp_90_sum_IRF, payment.Pmt_After_Hosp_90_sum_HHA, payment.Pmt_After_Hosp_90_sum_Other, payment.Pmt_After_Hosp_90_sum_Acute
	from snf_bpt.SNF_Bene_Period_1216_h_21  as main
	left join snf_bpt.merge_all_2010_16_trans_90_4  as payment
	on main.MEDPAR_ID=payment.MEDPAR_ID;
quit;*16,714,746;

data SNF_Bene_Period_1216_h_23;
set snf_bpt.SNF_Bene_Period_1216_h_22;

if Pmt_After_Hosp_60_sum =. then Total_Pmt_60 = Hosp_Pmt_60; else Total_Pmt_60=Hosp_Pmt_60+Pmt_After_Hosp_60_sum;
if disch_pac_n=3 and hha_pmt_amt_pseudo=. then Total_Pmt_60=.;
if disch_pac_n=1 then pac_pmt_amt_pseudo=snf_pmt_amt_pseudo;
else if disch_pac_n=3 then pac_pmt_amt_pseudo=hha_pmt_amt_pseudo;
label Total_Pmt_60 = 'Payment amount to all providers within 60 days of hospital admission' 
      pac_pmt_amt_pseudo="PAC Payment Amount"
      Pmt_After_Hosp_60_sum_SNF = 'Payment amount to SNF after discharge within 60 days of hospital admission' 
      Pmt_After_Hosp_60_sum_IRF = 'Payment amount to IRF after discharge within 60 days of hospital admission'  
      Pmt_After_Hosp_60_sum_HHA = 'Payment amount to HHA after discharge within 60 days of hospital admission' 
      Pmt_After_Hosp_60_sum_Other = 'Payment amount to other providers after discharge within 60 days of hospital admission'
      Pmt_After_Hosp_60_sum_Acute = 'Payment amount to acute care hospital after discharge within 60 days of hospital admission';

if Pmt_After_Hosp_90_sum =. then Total_Pmt_90 = PMT_AMT_PSEUDO; else Total_Pmt_90=PMT_AMT_PSEUDO+Pmt_After_Hosp_90_sum;
if disch_pac_n=3 and hha_pmt_amt_pseudo=. then Total_Pmt_90=.;
Hosp_Pmt_90=PMT_AMT_PSEUDO;
label Total_Pmt_90 = 'Payment amount to hospital stay and all providers within 90 days of hospital discharge' 
      Hosp_Pmt_90 = 'Payment amount to hospital'
      Pmt_After_Hosp_90_sum_SNF = 'Payment amount to SNF within 90 days of hospital discharge' 
      Pmt_After_Hosp_90_sum_IRF = 'Payment amount to IRF within 90 days of hospital discharge'  
      Pmt_After_Hosp_90_sum_HHA = 'Payment amount to HHA within 90 days of hospital discharge' 
      Pmt_After_Hosp_90_sum_OTHER = 'Payment amount to other providers within 90 days of hospital discharge'
      Pmt_After_Hosp_90_sum_Acute = 'Payment amount to acute care hospital within 90 days of hospital discharge';
run;

proc means data=SNF_Bene_Period_1216_h_23 nonobs n nmiss min max mean;
var  Total_Pmt_90 Hosp_Pmt_90 Pmt_After_Hosp_90_sum_SNF Pmt_After_Hosp_90_sum_IRF Pmt_After_Hosp_90_sum_HHA	Pmt_After_Hosp_90_sum_Other	Pmt_After_Hosp_90_sum_Acute;
run;

proc sql;
create table check_missing_60 as 
select bene_id, admsndt, dschrgdt, Total_Pmt_60, Hosp_Pmt_60, Pmt_After_Hosp_60_sum, 
     Pmt_After_Hosp_60_sum_IRF, Pmt_After_Hosp_60_sum_HHA, Pmt_After_Hosp_60_sum_Other, Pmt_After_Hosp_60_sum_Acute
from  snf_bpa.Hosp_Bene_Period_1216
where radm30^=. and Pmt_After_Hosp_60_sum=.;
quit; *3,555,531;

proc sql;
create table check_missing_90 as 
select bene_id, admsndt, dschrgdt, Total_Pmt_90, Hosp_Pmt_90, Pmt_After_Hosp_90_sum, 
     Pmt_After_Hosp_90_sum_IRF, Pmt_After_Hosp_90_sum_HHA, Pmt_After_Hosp_90_sum_Other, Pmt_After_Hosp_90_sum_Acute
from  SNF_Bene_Period_1216_h_23
where radm30^=. and Pmt_After_Hosp_90_sum=.;
quit; *3,104,671;


/************************************************************************************
 Step 9: Create final hospitalization analytical data set
************************************************************************************/
options validvarname=upcase;
data snf_bpa.Hosp_Bene_Period_1216;
set SNF_Bene_Period_1216_h_23;
run;

* Export to Stata file;
proc export data=snf_bpa.Hosp_Bene_Period_1216 
            outfile="/secure/project/PACUse_RWerner/SNF_bene_days/Analytical_Files/Hosp_Bene_Period_1216.dta" dbms=dta replace; 
run;



***************************************************************************
***************************************************************************
PART III - Create SNF stay data set with post-discharge information of 
           prior hospitalization and Medicare payment variables
***************************************************************************
***************************************************************************;

/***************************************************************************
 Step 1: Create a data set of SNF stays and merge in information of 
         preceding hospitalization and readmission
***************************************************************************/
data snf_bpt.SNF_Bene_Period_1216_h snf_bpt.SNF_Bene_Period_1216_s;
set snf_bpa.Hosp_SNF_Bene_Period_1216;
if SNF_num=. then output snf_bpt.SNF_Bene_Period_1216_h;
else output snf_bpt.SNF_Bene_Period_1216_s;
run; *16,714,746 records in hospital stay data set / 8,969,940 records in SNF data set;

*Merge with the hospital file to get useful information about the preceding hospitalization;
proc sql;
create table snf_bpt.SNF_Bene_Period_1216_s_2 as 
select a.*, b.admsndt as hosp_admsndt, b.dschrgdt as hosp_dschrgdt, b.pmt_amt_pseudo as hosp_pmt_amt_pseudo, b.prvdr_num as hosp_prvdrnum, 
            b.drg_cd as hosp_drgcd label="Diagnosis Related Group (Preceding Hospital Stay)", b.util_day as hosp_util_day label="Preceding Hospital Utilization Day Count",
            b.non_dsct_hosp, b.dsct_hosp 
from snf_bpt.SNF_Bene_Period_1216_s as a left join snf_bpa.Hosp_Bene_Period_1216 as b
on a.bene_id=b.bene_id and b.dschrgdt<=a.admsndt<=b.dschrgdt+1;
quit; *8,973,528;

* Keep the nearest hospitalization for records have more than 1 matched hospital stays;
proc sort data=snf_bpt.SNF_Bene_Period_1216_s_2; by medpar_id hosp_dschrgdt; run;

data snf_bpt.SNF_Bene_Period_1216_s_3;
set snf_bpt.SNF_Bene_Period_1216_s_2;
by medpar_id hosp_dschrgdt; 
if first.medpar_id;
run; *8,969,940;

proc sql; create table check_matched_hosp as select bene_id from snf_bpt.SNF_Bene_Period_1216_s_3 where hosp_admsndt^=.; quit; 
*7,253,507 (80.86%) SNF stays have prior hospital stays within 24 hours;
*7,320,170 (81.6%) SNF stays have prior hospital stays within 72 hours; 

*Merge with readmission data set (updated version);
proc sql;
create table snf_bpt.SNF_Bene_Period_1216_s_4 as 
select a.*, b.radm30, b.dd30, hxinfection, otherinfectious, metacancer, severecancer, othercancer, diabetes, malnutrition, liverdisease, 
hematological, alcohol, psychological, motordisfunction, seizure, chf, cadcvd, arrhythmias, copd, lungdisorder, ondialysis, ulcers, septicemia, metabolicdisorder, 
irondeficiency, cardiorespiratory, renalfailure, pancreaticdisease, arthritis, respiratordependence, transplants, coagulopathy, hipfracture, admit, disch
from snf_bpt.SNF_Bene_Period_1216_s_3 as a left join readm.hw_readm_0916_final as b	
on a.BENE_ID=b.HICNO & a.hosp_admsndt>=b.ADMIT & a.hosp_dschrgdt<=b.DISCH;
quit; *8,970,039;

proc sort data=snf_bpt.SNF_Bene_Period_1216_s_4; by medpar_id descending admit disch; run;

data snf_bpt.SNF_Bene_Period_1216_s_5(drop=admit disch);
set snf_bpt.SNF_Bene_Period_1216_s_4;
by medpar_id descending admit disch;
if first.medpar_id;
run; *8,969,940;

proc sql; create table check_merge as select bene_id from snf_bpt.SNF_Bene_Period_1216_s_5 where dd30^=.;quit; *6,876,372 (94.8% of total SNF stays with preceding hospitalization);

*Merge with readmission data set (X10 version) to get drg code for readmission;
proc sql;
create table snf_bpt.SNF_Bene_Period_1216_s_6 as 
select a.*, b.admit, b.drgcd as hosp_radm_drgcd label="Diagnosis Related Group (Hospital Readmission Stay)" 
from snf_bpt.SNF_Bene_Period_1216_s_5 as a left join readm.hw_readm_0916_final as b
on a.BENE_ID=b.HICNO & a.hosp_dschrgdt<=b.admit<=a.hosp_dschrgdt+30 & a.radm30=1;
quit; *9,077,734;

proc sort data=snf_bpt.SNF_Bene_Period_1216_s_6; by medpar_id descending admit; run;

data snf_bpt.SNF_Bene_Period_1216_s_7(drop=admit);
set snf_bpt.SNF_Bene_Period_1216_s_6;
by medpar_id descending admit;
if first.medpar_id;
run;  *8,969,940;

proc sql; create table radm30 as select bene_id from snf_bpt.SNF_Bene_Period_1216_s_7 where radm30=1;quit; *879,337;
proc sql; create table master_only as select bene_id from snf_bpt.SNF_Bene_Period_1216_s_7 where radm30=1 and hosp_radm_drgcd=.; quit; *13,631 records ~ 1.44%;

* Calculate "benefit_day" variable, total payment variable and count of comorbidities;
data snf_bpt.SNF_Bene_Period_1216_s_8;
set snf_bpt.SNF_Bene_Period_1216_s_7;
benefit_day=SNF_day-1;
tt_pmt_amt_pseudo=sum(pmt_amt_pseudo, hosp_pmt_amt_pseudo);
combdty_sum=sum(hxinfection, otherinfectious, metacancer, severecancer, othercancer, diabetes, malnutrition, liverdisease, 
hematological, alcohol, psychological, motordisfunction, seizure, chf, cadcvd, arrhythmias, copd, lungdisorder, ondialysis, ulcers, septicemia, metabolicdisorder, 
irondeficiency, cardiorespiratory, renalfailure, pancreaticdisease, arthritis, respiratordependence, transplants, coagulopathy, hipfracture);
label Hosp_Prvdrnum="Hospital Provider Number"  benefit_day="Benefit Period Day Count" combdty_sum="Count of Comorbidity"
      pmt_amt_pseudo="SNF Medicare Payment Amount" tt_pmt_amt_pseudo="Total Medicare Payment Amount";
run; *8,969,940;


/***************************************************************************
 Step 2: Merge in SNF characteristics from POS file and LTCFocus.org data 
***************************************************************************/
* Merge in SNF characteristics from 2012-2014 POS file;
%macro pos(yr=);
data pos&yr.; 
set snf_bpt.pos20&yr(keep=prvdr_num state_cd crtfd_bed_cnt CBSA_URBN_RRL_IND gnrl_cntl_type_cd prvdr_bsd_fac_sw mlt_ownd_fac_org_sw); 
year=20&yr.; 
run;
%mend pos;

%pos(yr=12);*141,342;
%pos(yr=13);*133,879;
%pos(yr=14);*136,502;
%pos(yr=15);*138,803;
%pos(yr=16);*141,557;
data pos12_16; set pos12 pos13 pos14 pos15 pos16; run; *692,083;

proc sql;
create table snf_bpt.SNF_Bene_Period_1216_s_9 as
select main.*, pos.crtfd_bed_cnt, pos.gnrl_cntl_type_cd, pos.state_cd, pos.CBSA_URBN_RRL_IND, pos.mlt_ownd_fac_org_sw
from snf_bpt.SNF_Bene_Period_1216_s_8 as main left join pos12_16 as pos
on main.prvdr_num=pos.prvdr_num and year_dschrg=year;
quit; *8,969,940; 

* Check how many observations couldn't find a match;
proc sql; create table check_missing_1 as select bene_id from snf_bpt.SNF_Bene_Period_1216_s_9 where missing(crtfd_bed_cnt); quit; *2,545 (<0.1%);
proc sql; create table check_missing_2 as select bene_id from snf_bpt.SNF_Bene_Period_1216_s_9 where missing(gnrl_cntl_type_cd); quit; *2,545 (<0.1%);
proc sql; create table check_missing_3 as select bene_id from snf_bpt.SNF_Bene_Period_1216_s_9 where missing(state_cd); quit; *2,545 (<0.1%);
proc sql; create table check_missing_4 as select bene_id from snf_bpt.SNF_Bene_Period_1216_s_9 where missing(CBSA_URBN_RRL_IND); quit; *6,311 (<0.1%);
proc sql; create table check_missing_5 as select bene_id from snf_bpt.SNF_Bene_Period_1216_s_9 where missing(mlt_ownd_fac_org_sw); quit; *2,545 (<0.1%);

* Create indicators for profit status and teching hospital;
data snf_bpt.SNF_Bene_Period_1216_s_10;
set snf_bpt.SNF_Bene_Period_1216_s_9;
if GNRL_CNTL_TYPE_CD in ("01","02","03","13") then for_profit=1;
else if GNRL_CNTL_TYPE_CD in ("04","05","06","07","08","09","10","11","12") then for_profit=0;
if CBSA_URBN_RRL_IND="U" then urban=1;
else if CBSA_URBN_RRL_IND="R" then urban=0;
if mlt_ownd_fac_org_sw="Y" then part_of_chain=1; 
else if mlt_ownd_fac_org_sw="N" then part_of_chain=0; 
benefitday_snfdc=benefit_day+snf_util_day;
label for_profit="For-profit Hospital" urban="Urban Area" part_of_chain="Facility is Part of a Chain that Owns Multiple Facilities"
      benefitday_snfdc="Used Benefit Day Count on Day of Discharge";
run; *8,969,940;

*Merge in SNF characteristics from LTCFocus.org data set;
proc sql;
create table snf_bpt.SNF_Bene_Period_1216_s_11 as 
select a.*, b.paymcaid label="% of patients whose primary support is Medicaid", 
            b.paymcare label="% of patients whose primary support is Medicare",
			b.hospbase label="Wether facility is hospital-based",
			b.rnhrppd  label="RN hours per resident day",
			b.lpnhrppd label="Licensed Practical Nurse hours per resident day",
			b.cnahrppd label="Certified Nursing Assistant hours per resident day",
			b.occpct   label="# of occupied beds in facility / total # of beds",
			b.avg_dailycensus label="Average Daily Census"
from snf_bpt.SNF_Bene_Period_1216_s_10 as a left join tempn.ltcf_fac_1216 as b
on a.prvdr_num=b.prov1680 and a.year_dschrg=b.year;
quit; *8,969,940;

*Convert character value into numeric;
data snf_bpt.SNF_Bene_Period_1216_s_12(rename=(paymcaid_2=paymcaid paymcare_2=paymcare rnhrppd_2=rnhrppd lpnhrppd_2=lpnhrppd
										       cnahrppd_2=cnahrppd occpct_2=occpct avg_dailycensus_2=avg_dailycensus));
set snf_bpt.SNF_Bene_Period_1216_s_11;
paymcaid_2=input(paymcaid, best12.);
paymcare_2=input(paymcare, best12.);
rnhrppd_2=input(rnhrppd, best12.);
lpnhrppd_2=input(lpnhrppd, best12.);
cnahrppd_2=input(cnahrppd, best12.); 
occpct_2=input(occpct, best12.);
avg_dailycensus_2=input(avg_dailycensus, best12.);
drop paymcaid paymcare rnhrppd lpnhrppd cnahrppd occpct avg_dailycensus;
label paymcaid_2="% of patients whose primary support is Medicaid" 
      paymcare_2="% of patients whose primary support is Medicare" 
	  rnhrppd_2="RN hours per resident day"
      lpnhrppd_2="Licensed Practical Nurse hours per resident day"
      cnahrppd_2="Certified Nursing Assistant hours per resident day"
      occpct_2="# of occupied beds in facility / total # of beds"
      avg_dailycensus_2="Average Daily Census"; 
run;


/*****************************************************************************
 Step 3: Create indicators for additional coverage and discharge from SNF 
         to hospital within 24 hours 
*****************************************************************************/
*Merge in other coverage indicator;
proc sql;
create table snf_bpt.SNF_Bene_Period_1216_s_13 as 
select a.*, b.other_cvrg_max
from snf_bpt.SNF_Bene_Period_1216_s_12 as a left join tempn.other_cvrg_1216 as b 
on a.bene_id=b.bene_id and a.bene_period=b.bene_period;
quit;

*Merge with the hospital file again to find who were discharged from SNF to hospital within one day;
proc sql;
create table snf_bpt.SNF_Bene_Period_1216_s_14 as 
select a.*, b.prvdr_num as prvdr_num_dc_hosp label="Provider Number of Hospital which Patient was discharged to"  
from snf_bpt.SNF_Bene_Period_1216_s_13 as a left join snf_bpa.Hosp_Bene_Period_1216 as b
on a.bene_id=b.bene_id and a.dschrgdt<=b.admsndt<=a.dschrgdt+1;
quit; *8,976,280;

proc sort data=snf_bpt.SNF_Bene_Period_1216_s_14 nodupkey; by medpar_id; run; *8,969,940;

data snf_bpt.SNF_Bene_Period_1216_s_15;
set snf_bpt.SNF_Bene_Period_1216_s_14;
format age best12.;
if prvdr_num_dc_hosp^="" then dschrg_to_hosp=1;
else dschrg_to_hosp=0;
rename other_cvrg_max=other_cvrg;
label dschrg_to_hosp="Readmitted into Hospital within 24 hours of SNF discharge" other_cvrg_max="Other Coverage";
run;

proc sql; create table check_hosp as select bene_id from snf_bpt.SNF_Bene_Period_1216_s_15 where dschrg_to_hosp=1; quit; *1,264,493 (80.9%);


/*****************************************************************************
 Step 4: Merge in death information, discharge destination and total Medicare
         spending of prior year from MedPAR files
*****************************************************************************/
***Merge in death date and death code from MedPAR data;
data snf_death_1216(keep=BENE_ID medpar_id DEATHDT DEATHCD);
set medpar.mp100mod_2012(keep=BENE_ID DEATHDT DEATHCD PRVDR_NUM medpar_id UTIL_DAY)
	medpar.mp100mod_2013(keep=BENE_ID DEATHDT DEATHCD PRVDR_NUM medpar_id UTIL_DAY)
    medpar.mp100mod_2014(keep=BENE_ID DEATHDT DEATHCD PRVDR_NUM medpar_id UTIL_DAY)
    medpar.mp100mod_2015(keep=BENE_ID DEATHDT DEATHCD PRVDR_NUM medpar_id UTIL_DAY)
    medpar.mp100mod_2016(keep=BENE_ID DEATHDT DEATHCD PRVDR_NUM medpar_id UTIL_DAY);
where  substr(PRVDR_NUM,3,1) in ('5','6','U','W','Y','Z') & util_day>0 & DEATHDT^=.;
format DEATHDT date9.;
run; *4,243,416;

proc sort data=snf_death_1216; by bene_id descending DEATHCD; run;

data snf_death_1216_2;
set snf_death_1216;
by bene_id descending DEATHCD;
if first.bene_id;
run; *2,632,420;

proc sql;
create table snf_bpt.SNF_Bene_Period_1216_s_16 as 
select a.*, b.DEATHDT, b.DEATHCD  
from snf_bpt.SNF_Bene_Period_1216_s_15 as a left join snf_death_1216_2 as b
on a.bene_id=b.bene_id;
quit; *8,969,940;

proc sql; create table check_DEATHDT as select bene_id from snf_bpt.SNF_Bene_Period_1216_s_16 where DEATHDT^=.; quit; *3,148,766;
proc sql; create table check_DEATH_SNF as select bene_id from snf_bpt.SNF_Bene_Period_1216_s_16 where DEATHDT^=. & DEATHDT=dschrgdt; quit;*344,321;
proc sql; create table check_DEATHDT as select distinct bene_id from snf_bpt.SNF_Bene_Period_1216_s_16 where DEATHDT^=.; quit; *1,547,062;

***Create a new variable for total Medicare spending in the prior year;
data MedPAR2011_16_Pmt;
set Medpar.Mp100mod_2011(keep=BENE_ID DSCHRGDT ADMSNDT PMT_AMT UTIL_DAY) 
    Medpar.Mp100mod_2012(keep=BENE_ID DSCHRGDT ADMSNDT PMT_AMT UTIL_DAY) 
    Medpar.Mp100mod_2013(keep=BENE_ID DSCHRGDT ADMSNDT PMT_AMT UTIL_DAY) 
    Medpar.Mp100mod_2014(keep=BENE_ID DSCHRGDT ADMSNDT PMT_AMT UTIL_DAY) 
    Medpar.Mp100mod_2015(keep=BENE_ID DSCHRGDT ADMSNDT PMT_AMT UTIL_DAY) 
    Medpar.Mp100mod_2016(keep=BENE_ID DSCHRGDT ADMSNDT PMT_AMT UTIL_DAY);

if DSCHRGDT=. then DSCHRGDT_pseudo=ADMSNDT+UTIL_DAY;
else DSCHRGDT_pseudo=dschrgdt;
label DSCHRGDT_pseudo="Pseudo Discharge Date";
run; *103,722,163;

data tempn.MedPAR2011_16_Pmt;
set MedPAR2011_16_Pmt(keep=BENE_ID ADMSNDT DSCHRGDT_pseudo PMT_AMT);
dschrg_year=year(DSCHRGDT_pseudo);
if PMT_AMT<0 then PMT_AMT_pseudo=0;
else PMT_AMT_pseudo=PMT_AMT;
where PMT_AMT^=.;
label dschrg_year="Discharge Year" PMT_AMT_pseudo="Pseudo Medicare Payment Amount";
run; *103,722,163;

proc sql;
create table SNF_Bene_Period_1216_PriorPmt as 
select a.medpar_id, b.PMT_AMT_pseudo
from snf_bpt.SNF_Bene_Period_1216_s_16 as a 
left join tempn.MedPAR2011_16_Pmt as b
on a.bene_id=b.bene_id and b.dschrgdt_pseudo<=a.admsndt<b.admsndt+365;
quit; *58,188,542;

proc sql;
create table SNF_Bene_Period_1216_PriorPmt_2 as 
select distinct medpar_id, sum(PMT_AMT_pseudo) as Prior_Pmt_Sum label="Total Medicare Spending in the Prior Year"
from SNF_Bene_Period_1216_PriorPmt
group by medpar_id;
quit; *8,969,940;

proc sql;
create table snf_bpt.SNF_Bene_Period_1216_s_17 as 
select a.*, b.Prior_Pmt_Sum
from snf_bpt.SNF_Bene_Period_1216_s_16 as a  
left join SNF_Bene_Period_1216_PriorPmt_2 as b
on a.medpar_id=b.medpar_id;
quit; *8,969,940;

data snf_bpt.SNF_Bene_Period_1216_s_18;
set snf_bpt.SNF_Bene_Period_1216_s_17;

if DEATHDT^=. & DEATHDT=dschrgdt then DEAD_in_SNF=1; else DEAD_in_SNF=0;
if occpct>=90 then occpc_status="High Occupancy"; else if occpct<90 & occpct^=. then occpc_status="Low Occupancy";
if GNRL_CNTL_TYPE_CD in ("01","02","03","13") then for_profit=1;
else if GNRL_CNTL_TYPE_CD in ("04","05","06","07","08","09","10","11","12") then for_profit=0;
label DEAD_in_SNF="Dead in SNF" occpc_status="Occupancy Status";

if Prior_Pmt_Sum=. then Prior_Pmt_Amt_Sum=0;
else Prior_Pmt_Amt_Sum=Prior_Pmt_Sum;
label  Prior_Pmt_Amt_Sum="Total Medicare Spending Amount in the Prior Year";

if Prior_Pmt_Sum=. or Prior_Pmt_Sum=0 then Any_Prior_Pmt=0; else Any_Prior_Pmt=1;
label Any_Prior_Pmt="Any Medicare Spending in the Prior Year";

run;


/*****************************************************************************
 Step 5: Merge in RUG code from MDS files and SES variables from American
         Community Survey; impute the missing values
*****************************************************************************/
***Merge in RUG code from MDS;
data tempn.mds3_2010_2016_RUG4;
set pac_mds.mds3_beneid_2010_16(keep=bene_id c_mdcr_rug4_hirchcl_grp_txt MDS_ENTRY_DT MDS_DSCHRG_DT MDS_ASMT_DT 
                                     MDS_TRGT_DT A0310F_ENTRY_DSCHRG_CD A0310B_PPS_CD A0310A_FED_OBRA_CD);
where c_mdcr_rug4_hirchcl_grp_txt^="" & MDS_TRGT_DT>=18993;
run; *59,829,482;

proc sql;
create table MDS_2012_16_Missing_bene_id as 
select RSDNT_INTRNL_ID from pac_mds.mds3_beneid_2010_16 where missing(bene_id) & c_mdcr_rug4_hirchcl_grp_txt^="" & MDS_TRGT_DT>=18993;
quit; *1,152,946 (1.93% records with missing bene_id);

proc sort data=tempn.mds3_2010_2016_RUG4; by bene_id A0310A_FED_OBRA_CD; run;

proc sql;
create table snf_bpt.SNF_Bene_Period_1216_s_19  as 
select a.*, b.c_mdcr_rug4_hirchcl_grp_txt as rug4, b.MDS_TRGT_DT   
from snf_bpt.SNF_Bene_Period_1216_s_18 as a 
left join tempn.mds3_2010_2016_RUG4 as b
on a.bene_id=b.bene_id and a.admsndt<=b.MDS_TRGT_DT<=a.dschrgdt;
quit; *20,735,072;

proc sort data=snf_bpt.SNF_Bene_Period_1216_s_19; by medpar_id MDS_TRGT_DT; run;

data snf_bpt.SNF_Bene_Period_1216_s_20 (drop=MDS_TRGT_DT);
set snf_bpt.SNF_Bene_Period_1216_s_19;
by medpar_id MDS_TRGT_DT;
if first.medpar_id;
run; *8,969,940;

proc sql; create table check_merge as select bene_id from snf_bpt.SNF_Bene_Period_1216_s_20 where rug4=""; quit; *376,244 (4.19%);

*Replace missing RUG4 value with the most common RUG4 code by DRG;
proc sql;
create table im_rug as
select drg_cd, rug4, count(rug4) as rug4_count  
from snf_bpt.SNF_Bene_Period_1216_s_20
group by drg_cd, rug4;
quit;

proc sort data=im_rug; by drg_cd descending rug4_count; run;

data im_rug_2;
set im_rug;
by drg_cd descending rug4_count;
if first.drg_cd;
where rug4^="";
run;

proc sql;
create table snf_bpt.SNF_Bene_Period_1216_s_21 as 
select a.*, b.rug4 as rug4_im 
from snf_bpt.SNF_Bene_Period_1216_s_20 as a 
left join im_rug_2 as b
on a.drg_cd=b.drg_cd and a.rug4="";
quit; 

*Create major RUG group based on RUG code;
data snf_bpt.SNF_Bene_Period_1216_s_22;
set snf_bpt.SNF_Bene_Period_1216_s_21;

if rug4^="" then rug4_im=rug4;

if rug4 in ("RUX", "RUL", "RVX", "RVL", "RHX", "RHL", "RMX", "RML", "RLX") then mjr_rug_grp="Rehab Plus Extensive";
else if rug4 in ("RUC", "RUB", "RUA", "RVC", "RVB", "RVA", "RHC", "RHB", "RHA", "RMC", "RMB", "RMA", "RLB", "RLA") then mjr_rug_grp="Rehab";  
else if rug4 in ("ES3", "ES2", "ES1") then mjr_rug_grp="Extensive Services";
else if rug4 in ("HE2", "HD2", "HC2", "HB2", "HE1", "HD1", "HC1", "HB1") then mjr_rug_grp="Special Care High";
else if rug4 in ("LE2", "LD2", "LC2", "LB2", "LE1", "LD1", "LC1", "LB1") then mjr_rug_grp="Special Care Low";
else if rug4 in ("CE2", "CD2", "CC2", "CB2", "CA2", "CE1", "CD1", "CC1", "CB1", "CA1") then mjr_rug_grp="Clinically Complex";
else if rug4 in ("BB2", "BA2", "BB1", "BA1") then mjr_rug_grp="Behavioral Symptoms and Cognitive Performance";
else if rug4 in ("PE2", "PD2", "PC2", "PB2", "PA2", "PE1", "PD1", "PC1", "PB1", "PA1") then mjr_rug_grp="Reduced Physical Functioning";

if rug4_im in ("RUX", "RUL", "RVX", "RVL", "RHX", "RHL", "RMX", "RML", "RLX") then mjr_rug_grp_im="Rehab Plus Extensive";
else if rug4_im in ("RUC", "RUB", "RUA", "RVC", "RVB", "RVA", "RHC", "RHB", "RHA", "RMC", "RMB", "RMA", "RLB", "RLA") then mjr_rug_grp_im="Rehab";  
else if rug4_im in ("ES3", "ES2", "ES1") then mjr_rug_grp_im="Extensive Services";
else if rug4_im in ("HE2", "HD2", "HC2", "HB2", "HE1", "HD1", "HC1", "HB1") then mjr_rug_grp_im="Special Care High";
else if rug4_im in ("LE2", "LD2", "LC2", "LB2", "LE1", "LD1", "LC1", "LB1") then mjr_rug_grp_im="Special Care Low";
else if rug4_im in ("CE2", "CD2", "CC2", "CB2", "CA2", "CE1", "CD1", "CC1", "CB1", "CA1") then mjr_rug_grp_im="Clinically Complex";
else if rug4_im in ("BB2", "BA2", "BB1", "BA1") then mjr_rug_grp_im="Behavioral Symptoms and Cognitive Performance";
else if rug4_im in ("PE2", "PD2", "PC2", "PB2", "PA2", "PE1", "PD1", "PC1", "PB1", "PA1") then mjr_rug_grp_im="Reduced Physical Functioning";

label rug4_im="Imputed RUG" mjr_rug_grp="Major RUG Group" mjr_rug_grp_im="Imputed Major RUG Group"; 
run; 

proc sql; create table check_merge_1 as select bene_id from snf_bpt.SNF_Bene_Period_1216_s_22 where rug4=""; quit; *376,244;
proc sql; create table check_merge_2 as select bene_id from snf_bpt.SNF_Bene_Period_1216_s_22 where mjr_rug_grp=""; quit; *376,244;
proc sql; create table check_merge_3 as select bene_id from snf_bpt.SNF_Bene_Period_1216_s_22 where rug4_im=""; quit; *5;
proc sql; create table check_merge_4 as select bene_id from snf_bpt.SNF_Bene_Period_1216_s_22 where mjr_rug_grp_im=""; quit; *5;

***Merge in median household income, unemployment rate and poverty rate from American Community Survey data (2012-2016 estimate);
*Pull beneficiary's zipcode from MedPAR;
data benezip;
set medpar.mp100mod_2012 (keep=medpar_id BENE_ZIP) medpar.mp100mod_2013 (keep=medpar_id BENE_ZIP) medpar.mp100mod_2014 (keep=medpar_id BENE_ZIP) 
    medpar.mp100mod_2015 (keep=medpar_id BENE_ZIP) medpar.mp100mod_2016 (keep=medpar_id BENE_ZIP);
run;

proc sql;
create table snf_bpt.SNF_Bene_Period_1216_s_23 as 
select a.*, b.bene_zip
from snf_bpt.SNF_Bene_Period_1216_s_22 as a 
left join benezip as b 
on a.medpar_id=b.medpar_id;
quit; *8,969,940;

proc sql; create table check_merge as select bene_id from snf_bpt.SNF_Bene_Period_1216_s_23 where bene_zip=""; quit; *1,416 (0.016%) missing;

*Use a crosswalk between zip code and ZCTA code to map beneficiary zip code to ZCTA code. 
*The crosswalk is developed by John Snow, Inc. (JSI), a public health management consulting and research organization contracted with Bureau of Primary Health Care,
 for use with UDS (Uniform Data System) Service Area data.  
*Link: https://www.udsmapper.org/zcta-crosswalk.cfm;
proc sql;
create table snf_bpt.SNF_Bene_Period_1216_s_24 as 
select a.*, b.zcta
from snf_bpt.SNF_Bene_Period_1216_s_23 as a 
left join snf_bpt.zip_zcta_cw as b 
on a.bene_zip=b.ZIP_CODE;
quit; *8,969,940;

proc sql; create table check_merge as select bene_id from snf_bpt.SNF_Bene_Period_1216_s_24 where zcta=""; quit; *2,883 (0.016%) missing;

*Merge in variables from ACS data based on zip code;
proc sql;
create table snf_bpt.SNF_Bene_Period_1216_s_25 as 
select a.*, b.med_hoshd_inc, b.unemplmt_rate, b.povty_rate
from snf_bpt.SNF_Bene_Period_1216_s_24 as a 
left join snf_bpt.acs_1216 as b 
on a.ZCTA=b.ZCTA5;
quit; *8,969,940;

proc sql; create table check_merge_hoshd as select bene_id from snf_bpt.SNF_Bene_Period_1216_s_25 where med_hoshd_inc=.; quit; *24,768 (0.28%) missing;
proc sql; create table check_merge_unemplmt as select bene_id from snf_bpt.SNF_Bene_Period_1216_s_25 where unemplmt_rate=.; quit; *10,312 (0.11%) missing;
proc sql; create table check_merge_povty as select bene_id from snf_bpt.SNF_Bene_Period_1216_s_25 where povty_rate=.; quit; *8,910 (0.1%) missing;

***Merge in marital status code from MDS data;
proc sql;
create table SNF_Bene_Period_1216_s_26  as 
select a.*, b.A1200_MRTL_STUS_CD   
from snf_bpt.SNF_Bene_Period_1216_s_25 as a 
left join pac_mds.mds3_beneid_2010_16 as b
on a.bene_id=b.bene_id and a.admsndt<=b.MDS_TRGT_DT<=a.dschrgdt;
quit; 

proc sort data=SNF_Bene_Period_1216_s_26; by medpar_id descending A1200_MRTL_STUS_CD; run; 
data snf_bpt.SNF_Bene_Period_1216_s_26; set SNF_Bene_Period_1216_s_26; by medpar_id descending A1200_MRTL_STUS_CD; if first.medpar_id; run; *8,969,940;
proc sql; create table check_merge as select bene_id from snf_bpt.SNF_Bene_Period_1216_s_26 where A1200_MRTL_STUS_CD in("" , "-"); quit; 
*128,134 (1.4%) missing because failing to merge / 193,542 (2.2%) missing because not assessed or no information;

*Impute missing marital status code by same race and sex group;
proc sql;
create table im_marital as 
select race_num, sex_num, A1200_MRTL_STUS_CD, count(A1200_MRTL_STUS_CD) as A1200_MRTL_STUS_CD_CT 
from snf_bpt.SNF_Bene_Period_1216_s_26
where A1200_MRTL_STUS_CD not in ("", "-")
group by race_num, sex_num, A1200_MRTL_STUS_CD;
quit;

proc sort data=im_marital; by race_num sex_num descending A1200_MRTL_STUS_CD_CT; run;

data im_marital_2;
set im_marital;
by race_num sex_num descending A1200_MRTL_STUS_CD_CT;;
if first.sex_num;
run;

proc sql;
create table snf_bpt.SNF_Bene_Period_1216_s_27 as 
select a.*, b.A1200_MRTL_STUS_CD as A1200_MRTL_STUS_CD_im label="Imputed Marital Status Code"
from snf_bpt.SNF_Bene_Period_1216_s_26 as a 
left join im_marital_2 as b
on a.race_num=b.race_num and a.sex_num=b.sex_num and a.A1200_MRTL_STUS_CD in ("", "-");
quit; 

data snf_bpt.SNF_Bene_Period_1216_s_28;
set snf_bpt.SNF_Bene_Period_1216_s_27;

if A1200_MRTL_STUS_CD not in ("", "-") then A1200_MRTL_STUS_CD_IM=A1200_MRTL_STUS_CD;

*Combine multiple marital status into married or not;
if A1200_MRTL_STUS_CD="2" then married=1; 
else if A1200_MRTL_STUS_CD in ("1", "3", "4", "5") then married=0;

if A1200_MRTL_STUS_CD_IM="2" then married_im=1; 
else if A1200_MRTL_STUS_CD_IM in ("1", "3", "4", "5") then married_im=0;

label married="Married" married="Married (Imputed)";
run;

proc sql; create table check_merge_1 as select bene_id from snf_bpt.SNF_Bene_Period_1216_s_28 where A1200_MRTL_STUS_CD in ("", "-"); quit; *321,676;
proc sql; create table check_merge_2 as select bene_id from snf_bpt.SNF_Bene_Period_1216_s_28 where A1200_MRTL_STUS_CD_IM in ("", "-"); quit; *0;
proc sql; create table check_merge_3 as select bene_id from snf_bpt.SNF_Bene_Period_1216_s_28 where married=.; quit; *321,676;
proc sql; create table check_merge_4 as select bene_id from snf_bpt.SNF_Bene_Period_1216_s_28 where married_im=.; quit; *0;


/******************************************************************************************************
 Step 6: Merge in 60-day and 90-day payment variables for SNF stays that have preceding hospital stays
******************************************************************************************************/
proc sql;
create table snf_bpt.SNF_Bene_Period_1216_s_29 as 
select a.*, b.Total_Pmt_60, b.Hosp_Pmt_60, b.Pmt_After_Hosp_60_sum, b.Pmt_After_Hosp_60_sum_SNF, b.Pmt_After_Hosp_60_sum_IRF, b.Pmt_After_Hosp_60_sum_HHA, 
            b.Pmt_After_Hosp_60_sum_Other, b.Pmt_After_Hosp_60_sum_Acute,
			b.Total_Pmt_90, b.Hosp_Pmt_90, b.Pmt_After_Hosp_90_sum, b.Pmt_After_Hosp_90_sum_SNF, b.Pmt_After_Hosp_90_sum_IRF, b.Pmt_After_Hosp_90_sum_HHA, 
            b.Pmt_After_Hosp_90_sum_Other, b.Pmt_After_Hosp_90_sum_Acute
from snf_bpt.SNF_Bene_Period_1216_s_28 as a 
left join snf_bpa.Hosp_Bene_Period_1216 as b
on a.bene_id=b.bene_id and b.dschrgdt<=a.admsndt<=b.dschrgdt+1;
quit; 

proc sort data=snf_bpt.SNF_Bene_Period_1216_s_29 nodupkey; by medpar_id; run; *8,969,940; 

data snf_bpt.SNF_Bene_Period_1216_s_30;
set snf_bpt.SNF_Bene_Period_1216_s_29;

*Set missing payment to 0;
if Pmt_After_Hosp_60_sum^=. and Pmt_After_Hosp_60_sum_SNF=. then Pmt_After_Hosp_60_sum_SNF=0;
if Pmt_After_Hosp_60_sum^=. and Pmt_After_Hosp_60_sum_IRF=. then Pmt_After_Hosp_60_sum_IRF=0;
if Pmt_After_Hosp_60_sum^=. and Pmt_After_Hosp_60_sum_HHA=. then Pmt_After_Hosp_60_sum_HHA=0;
if Pmt_After_Hosp_60_sum^=. and Pmt_After_Hosp_60_sum_Other=. then Pmt_After_Hosp_60_sum_Other=0;
if Pmt_After_Hosp_60_sum^=. and Pmt_After_Hosp_60_sum_Acute=. then Pmt_After_Hosp_60_sum_Acute=0;

if Pmt_After_Hosp_90_sum^=. and Pmt_After_Hosp_90_sum_SNF=. then Pmt_After_Hosp_90_sum_SNF=0;
if Pmt_After_Hosp_90_sum^=. and Pmt_After_Hosp_90_sum_IRF=. then Pmt_After_Hosp_90_sum_IRF=0;
if Pmt_After_Hosp_90_sum^=. and Pmt_After_Hosp_90_sum_HHA=. then Pmt_After_Hosp_90_sum_HHA=0;
if Pmt_After_Hosp_90_sum^=. and Pmt_After_Hosp_90_sum_Other=. then Pmt_After_Hosp_90_sum_Other=0;
if Pmt_After_Hosp_90_sum^=. and Pmt_After_Hosp_90_sum_Acute=. then Pmt_After_Hosp_90_sum_Acute=0;


*Create a variable for payment to later SNF within 60 days of hospital admission;
if Pmt_After_Hosp_60_sum^=. then do;
	if dschrgdt-hosp_admsndt+1>=60 then Pmt_After_Hosp_60_sum_SNF2=0;
	else Pmt_After_Hosp_60_sum_SNF2=Pmt_After_Hosp_60_sum_SNF-pmt_amt_pseudo;
end;

if Pmt_After_Hosp_60_sum_SNF2<0 & Pmt_After_Hosp_60_sum_SNF2^=. then Pmt_After_Hosp_60_sum_SNF2=0;

label Pmt_After_Hosp_60_sum_SNF2='Payment amount to later SNF after discharge within 60 days of hospital admission'; 

*Create a variable for payment to later SNF within 90 days of hospital discharge;
if Pmt_After_Hosp_90_sum^=. then do;
	if dschrgdt-hosp_dschrgdt+1>=90 then Pmt_After_Hosp_90_sum_SNF2=0;
	else Pmt_After_Hosp_90_sum_SNF2=Pmt_After_Hosp_90_sum_SNF-pmt_amt_pseudo;
end;

if Pmt_After_Hosp_90_sum_SNF2<0 & Pmt_After_Hosp_90_sum_SNF2^=. then Pmt_After_Hosp_90_sum_SNF2=0;

label Pmt_After_Hosp_90_sum_SNF2='Payment amount to later SNF after discharge within 90 days of hospital admission'; 

run; *8,969,940;

proc sql;
create table check_missing_60 as 
select bene_id, hosp_admsndt, hosp_dschrgdt, admsndt, dschrgdt, Total_Pmt_60, Hosp_Pmt_60, pmt_amt_pseudo, Pmt_After_Hosp_60_sum, 
     Pmt_After_Hosp_60_sum_SNF2, Pmt_After_Hosp_60_sum_IRF, Pmt_After_Hosp_60_sum_HHA, Pmt_After_Hosp_60_sum_Other, Pmt_After_Hosp_60_sum_Acute
from  snf_bpt.SNF_Bene_Period_1216_s_30
where radm30^=. and Pmt_After_Hosp_60_sum=.;
quit;*3,996;

data check_missing_60_2;
set check_missing_60;
where admsndt-hosp_admsndt<60;
run; *0;

proc sql;
create table check_missing_90 as 
select bene_id, hosp_admsndt, hosp_dschrgdt, admsndt, dschrgdt, Total_Pmt_90, Hosp_Pmt_90, pmt_amt_pseudo, Pmt_After_Hosp_90_sum, 
     Pmt_After_Hosp_90_sum_SNF2, Pmt_After_Hosp_90_sum_IRF, Pmt_After_Hosp_90_sum_HHA, Pmt_After_Hosp_90_sum_Other, Pmt_After_Hosp_90_sum_Acute
from  snf_bpt.SNF_Bene_Period_1216_s_30
where radm30^=. and Pmt_After_Hosp_90_sum=.;
quit; *0;


/*****************************************************************************
 Step 7: Create readmission in SNF indicator, 30-day readmission after SNF
         discharge indicator, and indicator for SNF admission after 30-day
         break in benefit period
*****************************************************************************/
proc sql;
create table SNF_BPT.SNF_Bene_Period_1216_s_31 as 
select a.*, b.RADM_SNF30, b.RADM_During_SNF, b.RADM_after_SNF30_1, b.RADM_after_SNF30_2, b.admit, b.disch, b.interval
from snf_bpt.SNF_Bene_Period_1216_s_30 as a 
left join SNF_BPT.HW_READM_ALL_0916_FINAL as b
on a.BENE_ID=b.HICNO & a.hosp_admsndt>=b.ADMIT & a.hosp_dschrgdt<=b.DISCH;
quit; *8,970,039;

proc sort data=SNF_BPT.SNF_Bene_Period_1216_s_31; by medpar_id descending admit disch; run;

data SNF_BPT.SNF_Bene_Period_1216_s_32;
set SNF_BPT.SNF_Bene_Period_1216_s_31;
by medpar_id descending admit disch;
if first.medpar_id;
run; *8,969,940;
proc sql; create table check_merge as select bene_id from SNF_BPT.SNF_Bene_Period_1216_s_32 where admit^=.; quit; *6,876,372;

data snf_bpt.SNF_Bene_Period_1216_s_33(drop=admit disch interval);
set snf_bpt.SNF_Bene_Period_1216_s_32;
if radm30^=. & RADM_SNF30=. then do;
	radm_SNF30=0;
	RADM_During_SNF=0; 
    RADM_after_SNF30_1=0; 
    RADM_after_SNF30_2=0;
end;

label radm_SNF30="Eligible Readmission during SNF or within 30 Days of SNF Discharge"
      RADM_During_SNF="Eligible Readmission during SNF"
	  RADM_after_SNF30_1="Eligible Readmission within 30 Days of SNF Discharge (V1)"
      RADM_after_SNF30_2="Eligible Readmission within 30 Days of SNF Discharge (V2)"; 
run; *8,969,940;

proc means data=snf_bpt.SNF_Bene_Period_1216_s_32 nolabels n nmiss mean min q1 median q3 max maxdec=3;
var radm30 radm_SNF30 RADM_During_SNF RADM_after_SNF30_1 RADM_after_SNF30_2;
run;

proc sort data=snf_bpt.SNF_Bene_Period_1216_s_33; by bene_id admsndt dschrgdt; run;

*Create an indicator for SNF after 30-day break;
data snf_bpt.SNF_Bene_Period_1216_s_34;
set snf_bpt.SNF_Bene_Period_1216_s_33;
by bene_id admsndt dschrgdt;
prior_snf_dschrgdt=lag(dschrgdt);
if first.bene_id then prior_snf_dschrgdt=.;
snf_intv=admsndt-prior_snf_dschrgdt;
if 30<=snf_intv<=60 then snf_after_30day_break=1; else snf_after_30day_break=0;
format prior_snf_dschrgdt date9.;
label prior_snf_dschrgdt="Prior SNF Discharge Date" snf_intv="Interval between Current SNF and Prior SNF" snf_after_30day_break="SNF after 30-Day Break"; 
run; 

*Merge in state code of beneficiary;
data medpar1216_STATE_CD;
set medpar.mp100mod_2012 (keep=medpar_id STATE_CD) medpar.mp100mod_2013 (keep=medpar_id STATE_CD) medpar.mp100mod_2014 (keep=medpar_id STATE_CD) 
    medpar.mp100mod_2015 (keep=medpar_id STATE_CD) medpar.mp100mod_2016 (keep=medpar_id STATE_CD);
run; *85,668,853;

proc sql; 
create table snf_bpt.SNF_Bene_Period_1216_s_35 as 
select a.*, b.state_cd as bene_state_cd 
from snf_bpt.SNF_Bene_Period_1216_s_34 as a 
left join medpar1216_STATE_CD as b
on a.medpar_id=b.medpar_id;
quit; *8,969,940 - all matched;

proc sql; create table state.ssa_state_crosswalk_2017 as select distinct state, ssastate from state.ssa_fips_state_county; quit;

proc sql; 
create table snf_bpt.SNF_Bene_Period_1216_s_36 as 
select a.*, b.state as bene_state 
from snf_bpt.SNF_Bene_Period_1216_s_35 as a 
left join state.ssa_state_crosswalk_2017 as b
on a.bene_state_cd=b.ssastate;
quit;



/*****************************************************************************
 Step 8: Create indicators for readmission in 90 days after hospital discharge 
         and readmission in 90 days and 30 days after SNF discharge
*****************************************************************************/
*Merge in indicators for readmission in 90 days and 30 days after SNF discharge;
proc sql;
create table readm90_after_snf as 
select a.*, b.RADM30SNF, b.RADM90SNF, b.admit, b.disch
from snf_bpt.SNF_Bene_Period_1216_s_37 as a 
left join snf_bpt.readm90_after_snf as b
on a.BENE_ID=b.HICNO & a.hosp_admsndt>=b.ADMIT & a.hosp_dschrgdt<=b.DISCH;
quit; *8,797,431;

proc sort data=readm90_after_snf; by medpar_id descending admit disch; run;

data readm90_after_snf_2;
set readm90_after_snf;
by medpar_id descending admit disch;
if first.medpar_id;
run; *8,797,332;

proc sql; create table check_merge as select bene_id from readm90_after_snf_2 where admit^=.; quit; *6,764,471;

data readm90_after_snf_3(drop=admit disch);
set readm90_after_snf_2;

if radm30=. then do;
   RADM30SNF=.;
   RADM90SNF=.;
end;

if radm30^=. & RADM90SNF=. then do;
	RADM30SNF=0;
	RADM90SNF=0; 
end;

label RADM30SNF="Eligible Readmission within 30 Days of SNF Discharge"
      RADM90SNF="Eligible Readmission within 90 Days of SNF Discharge";

run; *8,797,332;

proc means data=readm90_after_snf_3 nolabels n nmiss mean min q1 median q3 max maxdec=3;
var radm30 radm_SNF30 RADM30SNF RADM90SNF;
run;

*Merge in indicator for readmission in 90 days after hospital discharge;
proc sql;
create table readm90_after_hosp as 
select a.*, b.RADM90, b.admit, b.disch
from readm90_after_snf_3 as a 
left join snf_bpt.readm90_after_hosp as b
on a.BENE_ID=b.HICNO & a.hosp_admsndt>=b.ADMIT & a.hosp_dschrgdt<=b.DISCH;
quit; 

proc sort data=readm90_after_hosp; by medpar_id descending admit disch; run;

data readm90_after_hosp_2;
set readm90_after_hosp;
by medpar_id descending admit disch;
if first.medpar_id;
run; *8,797,332;

proc sql; create table check_merge as select bene_id from readm90_after_hosp_2 where admit^=.; quit; 

data snf_bpt.SNF_Bene_Period_1216_s_38(drop=admit disch);
set readm90_after_hosp_2;

if radm30=. then RADM90=.;

*if radm30^=. & RADM90=. then RADM90=0;

label RADM90="Eligible Readmission within 90 Days of Hospital Discharge";

run; *8,797,332;

proc means data=snf_bpt.SNF_Bene_Period_1216_s_38 nolabels n nmiss mean min q1 median q3 max maxdec=3;
var RADM90SNF radm90;
run;



/***************************************************************************************
 Step 9: Create variable of total Medicare payment within 90 days after SNF discharge
***************************************************************************************/
data merge_all_2010_16_all_trans;
	set temp.merge_all_2010_16_all_trans(keep = BENE_ID ADMSNDT_SNF DSCHRGDT_SNF_pseudo SNF_UTIL_DAY PMT_AMT_SNF SNF_PRVDRNUM ADMSNDT_IRF
	DSCHRGDT_IRF PMT_AMT_IRF IRF_PRVDRNUM ADMSNDT_HHA DSCHRGDT_HHA PMT_AMT_HHA HHA_PRVDRNUM ADMSNDT_other DSCHRGDT_other
	other_PRVDRNUM PMT_AMT_other ADMSNDT_ACUTE DSCHRGDT_ACUTE PMT_AMT_ACUTE PMT_AMT_ACUTE);
	rename DSCHRGDT_SNF_pseudo=DSCHRGDT_SNF;
run; *109,292,388;

proc sql;
create table snf_bpt.after_snf_90_trans_1016 as
	select main.BENE_ID, main.ADMSNDT, main.DSCHRGDT, main.medpar_id, post.*
	from snf_bpt.SNF_Bene_Period_1216_s_38 as main
	left join merge_all_2010_16_all_trans as post 
	on main.BENE_ID=post.BENE_ID;
quit; *79,476,502;

*Keep acute-pac bundle only if pac admission is within 90 days after SNF discharge;
data snf_bpt.after_snf_90_trans_1016_2;
set snf_bpt.after_snf_90_trans_1016;
if 0 le ADMSNDT_SNF-DSCHRGDT le 89 then gap_1=ADMSNDT_SNF-DSCHRGDT; else gap_1=.;
if 0 le ADMSNDT_IRF-DSCHRGDT le 89 then gap_2=ADMSNDT_IRF-DSCHRGDT; else gap_2=.;
if 0 le ADMSNDT_HHA-DSCHRGDT le 89 then gap_3=ADMSNDT_HHA-DSCHRGDT; else gap_3=.;
if 0 le ADMSNDT_OTHER-DSCHRGDT le 89 then gap_4=ADMSNDT_OTHER-DSCHRGDT; else gap_4=.;
if 0 le ADMSNDT_ACUTE-DSCHRGDT le 89 then gap_5=ADMSNDT_ACUTE-DSCHRGDT; else gap_5=.;
if gap_1=. & gap_2=. & gap_3=. & gap_4=. & gap_5=. then delete;
*There should be only 1 nonmissing variable the five variables in each line;
PMT_AMT_90_AFTER=min(PMT_AMT_SNF, PMT_AMT_IRF, PMT_AMT_HHA, PMT_AMT_OTHER, PMT_AMT_ACUTE);
DSCHRGDT_90_AFTER=min(DSCHRGDT_SNF, DSCHRGDT_IRF, DSCHRGDT_HHA, DSCHRGDT_OTHER, DSCHRGDT_ACUTE);
ADMSNDT_90_AFTER=min(ADMSNDT_SNF, ADMSNDT_IRF, ADMSNDT_HHA, ADMSNDT_OTHER, ADMSNDT_ACUTE);
run; *7,885,019; 

proc sort data=snf_bpt.after_snf_90_trans_1016_2;
	by BENE_ID ADMSNDT DSCHRGDT;
run;

*Calculate each of pac and hospital payments within 90 days after SNF discharge;
data snf_bpt.after_snf_90_trans_1016_3;
set snf_bpt.after_snf_90_trans_1016_2;

if DSCHRGDT_90_AFTER >= DSCHRGDT+89 then do; 
	if gap_1^=. then do;
    	if SNF_UTIL_DAY^=0 then Pmt_After_SNF_90=PMT_AMT_90_AFTER/SNF_UTIL_DAY*(DSCHRGDT+89-ADMSNDT_90_AFTER+1);
		else Pmt_After_SNF_90=PMT_AMT_90_AFTER/(DSCHRGDT_90_AFTER-ADMSNDT_90_AFTER+1)*(DSCHRGDT+89-ADMSNDT_90_AFTER+1);
	end;
	else Pmt_After_SNF_90=PMT_AMT_90_AFTER/(DSCHRGDT_90_AFTER-ADMSNDT_90_AFTER+1)*(DSCHRGDT+89-ADMSNDT_90_AFTER+1);
end;
if DSCHRGDT_90_AFTER < DSCHRGDT+89 then Pmt_After_SNF_90=PMT_AMT_90_AFTER;
if Pmt_After_SNF_90 < 0 then Pmt_After_SNF_90 = 0;

if gap_1^=. then Pmt_After_SNF_90_SNF=Pmt_After_SNF_90;
else if gap_2^=. then Pmt_After_SNF_90_IRF=Pmt_After_SNF_90;
else if gap_3^=. then Pmt_After_SNF_90_HHA=Pmt_After_SNF_90;
else if gap_4^=. then Pmt_After_SNF_90_OTHER=Pmt_After_SNF_90;
else if gap_5^=. then Pmt_After_SNF_90_ACUTE=Pmt_After_SNF_90;
run; *7,885,019; 

*Calculate the sum of pac and hospital payments for each hospital admission;
proc sql;
create table snf_bpt.after_snf_90_trans_1016_4 as
	select distinct BENE_ID, ADMSNDT, DSCHRGDT, medpar_id, SUM(Pmt_After_SNF_90) as Pmt_After_SNF_90_sum, 
	       			SUM(Pmt_After_SNF_90_SNF) as Pmt_After_SNF_90_Sum_SNF, 
                    SUM(Pmt_After_SNF_90_IRF) as Pmt_After_SNF_90_Sum_IRF, 
                    SUM(Pmt_After_SNF_90_HHA) as Pmt_After_SNF_90_Sum_HHA,
		            SUM(Pmt_After_SNF_90_OTHER) as Pmt_After_SNF_90_Sum_OTHER, 
                    SUM(Pmt_After_SNF_90_ACUTE) as Pmt_After_SNF_90_Sum_ACUTE
	from snf_bpt.after_snf_90_trans_1016_3
	group by MEDPAR_ID;
quit; *3,517,291;

proc sort data=snf_bpt.after_snf_90_trans_1016_4 NODUP;
	by BENE_ID ADMSNDT DSCHRGDT;
run; 

*Merge into the original dataset;
proc sql;
create table SNF_Bene_Period_1216_s_pmt as
	select distinct main.*, payment.Pmt_After_SNF_90_sum,
	                        payment.Pmt_After_SNF_90_sum_SNF, payment.Pmt_After_SNF_90_sum_IRF, payment.Pmt_After_SNF_90_sum_HHA, 
                            payment.Pmt_After_SNF_90_sum_Other, payment.Pmt_After_SNF_90_sum_Acute
	from snf_bpt.SNF_Bene_Period_1216_s_38  as main
	left join snf_bpt.after_snf_90_trans_1016_4 as payment
	on main.MEDPAR_ID=payment.MEDPAR_ID;
quit; *8,797,332;

data snf_bpt.SNF_Bene_Period_1216_s_39;
set SNF_Bene_Period_1216_s_pmt;

if Pmt_After_SNF_90_sum=. then Total_Pmt_90_after_SNF=0;
else Total_Pmt_90_after_SNF=Pmt_After_SNF_90_sum;

if Pmt_After_SNF_90_sum=. then Pmt_After_SNF_90_sum=0;
if Pmt_After_SNF_90_sum_SNF=. then Pmt_After_SNF_90_sum_SNF=0;
if Pmt_After_SNF_90_sum_IRF=. then Pmt_After_SNF_90_sum_IRF=0;
if Pmt_After_SNF_90_sum_HHA=. then Pmt_After_SNF_90_sum_HHA=0;
if Pmt_After_SNF_90_sum_Other=. then Pmt_After_SNF_90_sum_Other=0;
if Pmt_After_SNF_90_sum_Acute=. then Pmt_After_SNF_90_sum_Acute=0;

label Total_Pmt_90_after_SNF = 'Payment amount to all providers within 90 days of SNF discharge' 
      Pmt_After_SNF_90_sum_SNF = 'Payment amount to SNF within 90 days of SNF discharge'
      Pmt_After_SNF_90_sum_IRF = 'Payment amount to IRF within 90 days of SNF discharge' 
      Pmt_After_SNF_90_sum_HHA = 'Payment amount to HHA within 90 days of SNF discharge'
      Pmt_After_SNF_90_sum_OTHER = 'Payment amount to other providers within 90 days of SNF discharge'
      Pmt_After_SNF_90_sum_Acute = 'Payment amount to acute care hospital within 90 days of SNF discharge';
run; *8,797,332;



/***************************************************************************************
 Step 10: Create successful discharge from SNF indicator
***************************************************************************************/
*Step 1 - Flag observations that died within 30 days of SNF discharge; 
data snf_bpt.SNF_successful_dschrg;
set snf_bpt.SNF_Bene_Period_1216_s_39;
if deathdt ne . and 0 le deathdt-dschrgdt le 30 then death30_after_snf=1; 
else death30_after_snf=0;
run; *8,797,332;

*Step 2 - Flag observations with SNF claim or unplanned hospital claim within 30 days after SNF discharge;
data medpar1216_snf(keep=BENE_ID medpar_id ADMSNDT);
set medpar.mp100mod_2012(keep=BENE_ID ADMSNDT PRVDR_NUM medpar_id)
    medpar.mp100mod_2013(keep=BENE_ID ADMSNDT PRVDR_NUM medpar_id)
    medpar.mp100mod_2014(keep=BENE_ID ADMSNDT PRVDR_NUM medpar_id)
    medpar.mp100mod_2015(keep=BENE_ID ADMSNDT PRVDR_NUM medpar_id)
	medpar.mp100mod_2016(keep=BENE_ID ADMSNDT PRVDR_NUM medpar_id);
where  substr(PRVDR_NUM,3,1) in ('5','6','U','W','Y','Z');
run; *13,064,722;

proc sql;
create table snf_bpt.SNF_successful_dschrg_2 as
select all.*, 
       snf.medpar_id as snf_claim_id
from snf_bpt.SNF_successful_dschrg as all
left join medpar1216_snf as snf
on all.BENE_ID=snf.BENE_ID and 0 le snf.ADMSNDT-all.DSCHRGDT le 30;
quit; *9,069,337 records;

proc sort data=snf_bpt.SNF_successful_dschrg_2 nodupkey; by medpar_id; run;

*Step 3 - Define successful discharge as no SNF claim, no unplanned hospital claim and didn't died within first 30 days after SNF discharge;
data snf_bpt.SNF_successful_dschrg_3;
set snf_bpt.SNF_successful_dschrg_2;

if snf_claim_id^="" then claim_snf=1; else claim_snf=0;
if RADM30SNF=1 then claim_hosp=1; else claim_hosp=0;
if death30_after_snf=0 & claim_snf=0 & claim_hosp=0 then successful_discharge=1; else successful_discharge=0;

*Set readmission in 30/90 days after SNF discharge as missing if patients died in SNF (SNF discharge date equals death date of patient); 
if deathdt=dschrgdt then do;
	radm30snf=.;
	radm90snf=.;
	successful_discharge=.;
end;
run; *8,797,332;

proc means data=snf_bpt.SNF_successful_dschrg_3 n nmiss mean std min max maxdec=4;
var RADM30SNF successful_discharge;
run;

data snf_bpt.SNF_Bene_Period_1216_s_40;
set snf_bpt.SNF_successful_dschrg_3 (drop=death30_after_snf snf_claim_id claim_snf claim_hosp);
run;


/*****************************************************************************
 Final Step: Create the final SNF analytical data set
*****************************************************************************/
data snf_bpa.SNF_Bene_Period_1216;
set snf_bpt.SNF_Bene_Period_1216_s_40;
run;

ods pdf file="/PATH/SNF_Period_Var_List_1216.pdf";
title "List of Variables in the SNF Benefit Period Data Set";
proc contents data=snf_bpa.SNF_Bene_Period_1216 varnum;
run;
title;
ods pdf close;
