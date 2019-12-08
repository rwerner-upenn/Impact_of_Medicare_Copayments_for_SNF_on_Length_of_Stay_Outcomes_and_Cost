/********************************************************************************
 ** Goal: Define estimation sample, conducting statistical analysis and creating 
          tables and figures showed in paper and appendix
          
 ** Input data set: "/PATH/SNF_Bene_Period_1216.dta" (Stata data set created in
                    SNF_Stay_and_Outcomes_Data_Set_Creation_Public_Use.sas)

 ** Last updated on: 12/08/2019
********************************************************************************/


/********************************************************************************
 ** Define estimation sample
********************************************************************************/

use "/PATH/SNF_Bene_Period_1216.dta", replace 

*Limit the cohort to records of patients who admitted to a SNF within 24 hours after hospital discharge 
	keep if hosp_admsndt!=. 
	count
	
*Exclude discharges for patients who received hospice 
	keep if snf_hospice!="1"

*Exclude discharges for patients who had an MDS-reported life expectancy of less than six months
	keep if life_prgns_6month!="1"
	
*Exclude discharges for patients who had long-term care stay in a nursing home in the prior 100 days
	keep if ltc_prior100!=1

*Exclude discharges for patients who were ineligible for the main outcome of interest, planned readmission
	keep if radm30!=.

*Generate an indicator for first SNF  
	gen first_snf=1 if benefit_day==0
	replace first_snf=0 if benefit_day>0
	tab first_snf

*Create numeric id variable based on SNF provider number
	egen providernum=group(prvdr_num)

*Create numeric variable for RUG 
	egen rug4num=group(rug4_im)

*Create count of number of days observed
	gen ndays30_hosp=deathdt-hosp_dschrgdt if dd30==1
	replace ndays30_hosp=30 if dd30==0

	gen ndays90_hosp=deathdt-hosp_dschrgdt if deathdt!=. & deathdt<hosp_dschrgdt+90
	replace ndays90_hosp=90 if deathdt>=hosp_dschrgdt+90 | deathdt==.
	
	gen ndays30_snf=deathdt-dschrgdt if deathdt!=. & deathdt<dschrgdt+30
	replace ndays30_snf=30 if deathdt>=dschrgdt+30 | deathdt==.
	
	gen ndays90_snf=deathdt-dschrgdt if deathdt!=. & deathdt<dschrgdt+90
	replace ndays90_snf=90 if deathdt>=dschrgdt+90 | deathdt==.

*Add value labels to sex and race indicators
	label define sexla 1 "Male" 2 "Female" 3 "Unknown"
	label values sex_num sexla
	label define racela 1 "White" 2 "Black" 3 "Other" 4 "Asian" 5 "Hispanic" 6 "North American Native" 7 "Unknown"
	label values race_num racela
	label define mar 1 "Married" 0 "Not Married"
	label values married_im mar
	label define dstncd 1 "Home/Self Care" 2 "Short-term General Hospital" 2 "Short-term General Hospital" 3 "SNF" 4 "Intermediate Care Facility" 5 "Other Type of Inpatient Care" 6 "HHA" 20 "Expired" 30 "Still patient" 
	label values dstntncd dstncd

*Define patient covariates
	global pt_covar age i.race_num i.sex_num i.married_im med_hoshd_inc unemplmt_rate povty_rate prior_pmt_amt_sum_2 hxinfection otherinfectious metacancer severecancer othercancer diabetes malnutrition liverdisease hematological alcohol psychological motordisfunction seizure chf cadcvd arrhythmias copd lungdisorder ondialysis ulcers septicemia metabolicdisorder irondeficiency cardiorespiratory renalfailure pancreaticdisease arthritis respiratordependence transplants coagulopathy hipfracture

***Generate estimation sample
	count
	quietly xtreg radm30 util_day $pt_covar hosp_util_day i.hosp_drgcd ndays30_hosp i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
	keep if e(sample)
	count

*Limit to benefit day 1-100 at discharge 
	keep if benefitday_snfdc<=100

*Generate 5 most common DRG flags
	gen drg_joint=1 if hosp_drgcd==470
		replace drg_joint=0 if drg_joint==.
	gen drg_sepsis=1 if hosp_drgcd==871
		replace drg_sepsis=0 if drg_sepsis==.
	gen drg_uti=1 if hosp_drgcd==690
		replace drg_uti=0 if drg_uti==.
	gen drg_hipfx=1 if hosp_drgcd==481
		replace drg_hipfx=0 if drg_hipfx==.
	gen drg_chf=1 if hosp_drgcd==291
		replace drg_chf=0 if drg_chf==.

*Count the % of SNF admissions have been discharged by day 20
    gen dschrg_by_day_20=1 if benefitday_snfdc<=20
    replace dschrg_by_day_20=0 if dschrg_by_day_20==.
    label variable dschrg_by_day_20 "Discharged by day 20"
    tab dschrg_by_day_20, m
   
*Calculate the median of SNF length of stay 
	sum util_day, d

	
/********************************************************************************
 ** Table 1. Patient characteristics of the study cohort (those admitted to a SNF 
             in the middle of a benefit period and discharged from SNF on benefit 
             day 1-40 d) and of the larger cohort of all SNF admissions from which 
             the study cohort was drawn
********************************************************************************/

	sum age med_hoshd_inc unemplmt_rate povty_rate combdty_sum prior_pmt_amt_sum_2 hosp_util_day 
	foreach var of varlist sex_num race_num married_im drg_joint drg_sepsis drg_uti drg_hipfx drg_chf {
	tab `var'
	}
	preserve
	keep if benefitday_snfdc<=40
    keep if dual_stus!=1
	sum age med_hoshd_inc unemplmt_rate povty_rate combdty_sum prior_pmt_amt_sum_2 hosp_util_day 
	foreach var of varlist sex_num race_num married_im drg_joint drg_sepsis drg_uti drg_hipfx drg_chf {
	tab `var'
	}
	
	
/********************************************************************************
 ** Table 2. Relationship between benefit day on SNF admission and SNF length of 
             stay  in study cohort (1st stage of 2-stage least-squares analysis)
********************************************************************************/

*1st stage 
	quietly xtreg util_day benefit_day $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum) fe vce(robust)
		estimates store first_stage
		predict pr_snflos if e(sample)
		est table first_stage, keep(benefit_day) b se t p stats(N) title("1st Stage of Benefit Day 1-40 Sample") 
		eststo clear
	restore
		
/********************************************************************************
 ** Table 3. Unadjusted patient outcomes and Medicare payment of the study cohort 
            (those admitted to a SNF in the middle of a benefit period discharged 
            from SNF on benefit day 1 to 40 d) and of the larger cohort of all 
            SNF admissions from which the study cohort was drawn
********************************************************************************/

	sum radm30 dd30 radm90 radm30snf radm90snf successful_discharge total_pmt_90 hosp_pmt_90 pmt_amt_pseudo total_pmt_90_after_snf
    keep if benefitday_snfdc<=40
    keep if dual_stus!=1
    sum radm30 dd30 radm90 radm30snf radm90snf successful_discharge total_pmt_90 hosp_pmt_90 pmt_amt_pseudo total_pmt_90_after_snf

	
/********************************************************************************
 ** Table 4. Differences in outcome with one additional day in SNF using limited 
             sample of patients (excluding dual-eligible patients) discharged on
             benefit days 1-40, from multivariable and instrumental variable 
             regressions
********************************************************************************/

*Full sample
*1st stage 
	quietly xtreg util_day benefit_day $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum) fe vce(robust)
		estimates store first_stage
		predict pr_snflos if e(sample)
		est table first_stage, keep(benefit_day) b se t p stats(N) title("1st Stage of Benefit Day 1-40 Sample") 
		eststo clear

*2nd stage
	quietly xtreg radm30 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays30_hosp i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm30
	quietly xtreg radm90 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_hosp i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm90
	quietly xtreg radm30snf pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays30_snf i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm30snf
	quietly xtreg radm90snf pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_snf i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm90snf
	quietly xtreg successful_discharge pr_snflos $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store successful_discharge
	quietly xtreg total_pmt_90 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_hosp i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store totalpay90
	quietly xtreg hosp_pmt_90 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store hosp_pmt_90
	quietly xtreg pmt_amt_pseudo pr_snflos $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store snfpmt
	quietly xtreg total_pmt_90_after_snf pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_snf i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store total_pmt_90_after_snf
		est table radm30 radm90 radm30snf radm90snf successful_discharge totalpay90 hosp_pmt_90 snfpmt total_pmt_90_after_snf, keep(pr_snflos) b se t p stats(N) title("2nd Stage of Benefit Day 1-40 Sample")
		eststo clear

*Total Knee or Hip Replacement
	preserve 
	keep if drg_joint==1
	
*1st stage 
	quietly xtreg util_day benefit_day $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum) fe vce(robust)
		estimates store first_stage
		predict pr_snflos if e(sample)
		est table first_stage, keep(benefit_day) b t p stats(N) title("1st Stage on the sample of patients with total knee or hip replacement") 
		eststo clear

*2nd stage
	quietly xtreg radm30 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays30_hosp i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm30
	quietly xtreg radm90 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_hosp i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm90
	quietly xtreg radm30snf pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays30_snf i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm30snf
	quietly xtreg radm90snf pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_snf i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm90snf
	quietly xtreg successful_discharge pr_snflos $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store successful_discharge
	quietly xtreg total_pmt_90 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_hosp i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store totalpay90
	quietly xtreg hosp_pmt_90 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store hosp_pmt_90
	quietly xtreg pmt_amt_pseudo pr_snflos $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store snfpmt
	quietly xtreg total_pmt_90_after_snf pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_snf i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store total_pmt_90_after_snf
		est table radm30 radm90 radm30snf radm90snf successful_discharge totalpay90 hosp_pmt_90 snfpmt total_pmt_90_after_snf, keep(pr_snflos) b se t p stats(N) title("2nd Stage on the sample of patients with total knee or hip replacement")
		eststo clear
		
		restore

*Sepsis
	preserve 
	keep if drg_sepsis==1
	
*1st stage 
	quietly xtreg util_day benefit_day $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum) fe vce(robust)
		estimates store first_stage
		predict pr_snflos if e(sample)
		est table first_stage, keep(benefit_day) b t p stats(N) title("1st Stage on the sample of patients with Sepsis") 
		eststo clear

*2nd stage
	quietly xtreg radm30 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays30_hosp i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm30
	quietly xtreg radm90 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_hosp i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm90
	quietly xtreg radm30snf pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays30_snf i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm30snf
	quietly xtreg radm90snf pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_snf i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm90snf
	quietly xtreg successful_discharge pr_snflos $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store successful_discharge
	quietly xtreg total_pmt_90 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_hosp i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store totalpay90
	quietly xtreg hosp_pmt_90 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store hosp_pmt_90
	quietly xtreg pmt_amt_pseudo pr_snflos $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store snfpmt
	quietly xtreg total_pmt_90_after_snf pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_snf i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store total_pmt_90_after_snf
		est table radm30 radm90 radm30snf radm90snf successful_discharge totalpay90 hosp_pmt_90 snfpmt total_pmt_90_after_snf, keep(pr_snflos) b se t p stats(N) title("2nd Stage on the sample of patients with Sepsis")
		eststo clear

	restore

*Urinary Tract Infection
	preserve 
	keep if drg_uti==1
	
*1st stage 
	quietly xtreg util_day benefit_day $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum) fe vce(robust)
		estimates store first_stage
		predict pr_snflos if e(sample)
		est table first_stage, keep(benefit_day) b t p stats(N) title("1st Stage on the sample of patients with UTI") 
		eststo clear

*2nd stage
	quietly xtreg radm30 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays30_hosp i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm30
	quietly xtreg radm90 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_hosp i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm90
	quietly xtreg radm30snf pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays30_snf i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm30snf
	quietly xtreg radm90snf pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_snf i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm90snf
	quietly xtreg successful_discharge pr_snflos $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store successful_discharge
	quietly xtreg total_pmt_90 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_hosp i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store totalpay90
	quietly xtreg hosp_pmt_90 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store hosp_pmt_90
	quietly xtreg pmt_amt_pseudo pr_snflos $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store snfpmt
	quietly xtreg total_pmt_90_after_snf pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_snf i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store total_pmt_90_after_snf
		est table radm30 radm90 radm30snf radm90snf successful_discharge totalpay90 hosp_pmt_90 snfpmt total_pmt_90_after_snf, keep(pr_snflos) b se t p stats(N) title("2nd Stage on the sample of patients with UTI")
		eststo clear

	restore

*Hip Fracture
	preserve 
	keep if drg_hipfx==1
	
*1st stage 
	quietly xtreg util_day benefit_day $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum) fe vce(robust)
		estimates store first_stage
		predict pr_snflos if e(sample)
		est table first_stage, keep(benefit_day) b t p stats(N) title("1st Stage on the sample of patients with hip fracture") 
		eststo clear

*2nd stage
	quietly xtreg radm30 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays30_hosp i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm30
	quietly xtreg radm90 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_hosp i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm90
	quietly xtreg radm30snf pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays30_snf i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm30snf
	quietly xtreg radm90snf pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_snf i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm90snf
	quietly xtreg successful_discharge pr_snflos $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store successful_discharge
	quietly xtreg total_pmt_90 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_hosp i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store totalpay90
	quietly xtreg hosp_pmt_90 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store hosp_pmt_90
	quietly xtreg pmt_amt_pseudo pr_snflos $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store snfpmt
	quietly xtreg total_pmt_90_after_snf pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_snf i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store total_pmt_90_after_snf
		est table radm30 radm90 radm30snf radm90snf successful_discharge totalpay90 hosp_pmt_90 snfpmt total_pmt_90_after_snf, keep(pr_snflos) b se t p stats(N) title("2nd Stage on the sample of patients with hip fracture")
		eststo clear

	restore

*Congestive Heart Failure
	preserve 
	keep if drg_chf==1
	
*1st stage 
	quietly xtreg util_day benefit_day $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum) fe vce(robust)
		estimates store first_stage
		predict pr_snflos if e(sample)
		est table first_stage, keep(benefit_day) b t p stats(N) title("1st Stage on the sample of patients with congestive heart failure") 
		eststo clear

*2nd stage
	quietly xtreg radm30 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays30_hosp i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm30
	quietly xtreg radm90 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_hosp i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm90
	quietly xtreg radm30snf pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays30_snf i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm30snf
	quietly xtreg radm90snf pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_snf i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm90snf
	quietly xtreg successful_discharge pr_snflos $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store successful_discharge
	quietly xtreg total_pmt_90 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_hosp i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store totalpay90
	quietly xtreg hosp_pmt_90 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store hosp_pmt_90
	quietly xtreg pmt_amt_pseudo pr_snflos $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store snfpmt
	quietly xtreg total_pmt_90_after_snf pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_snf i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store total_pmt_90_after_snf
		est table radm30 radm90 radm30snf radm90snf successful_discharge totalpay90 hosp_pmt_90 snfpmt total_pmt_90_after_snf, keep(pr_snflos) b se t p stats(N) title("2nd Stage on the sample of patients with congestive heart failure")
		eststo clear

	restore
		
	
/*********************************************************************************************
 ** Supplemental Table 1. Characteristics of study cohort stratified by instrumental variable 
*********************************************************************************************/
	
*Days 1-6

	sum age med_hoshd_inc unemplmt_rate povty_rate combdty_sum prior_pmt_amt_sum_2 hosp_util_day if benefit_day>=1 & benefit_day<=6
	foreach var of varlist sex_num race_num married_im drg_joint drg_sepsis drg_uti drg_hipfx drg_chf {
	tab `var' if benefit_day>=1 & benefit_day<=6
	}

*Days 7-14
	
	sum age med_hoshd_inc unemplmt_rate povty_rate combdty_sum prior_pmt_amt_sum_2 hosp_util_day if benefit_day>=7 & benefit_day<=14 
	foreach var of varlist sex_num race_num married_im drg_joint drg_sepsis drg_uti drg_hipfx drg_chf {
	tab `var' if benefit_day>=7 & benefit_day<=14
	}

*Days 15-40
	
	sum age med_hoshd_inc unemplmt_rate povty_rate combdty_sum prior_pmt_amt_sum_2 hosp_util_day if benefit_day>=15 & benefit_day<=40 
	foreach var of varlist sex_num race_num married_im drg_joint drg_sepsis drg_uti drg_hipfx drg_chf {
	tab `var' if benefit_day>=15 & benefit_day<=40
	}
		
	
/*********************************************************************************************
 ** Supplemental Table 2. Characteristics of high-clinical-severity and low-clinical-severity 
                          study cohorts stratified by instrumental variable 
*********************************************************************************************/
	
***High Severity 
	preserve
	
	keep if combdty_sum>6 
	
*Days 1-7

	sum age med_hoshd_inc unemplmt_rate povty_rate combdty_sum prior_pmt_amt_sum_2 hosp_util_day if benefit_day>=1 & benefit_day<=6
	foreach var of varlist sex_num race_num married_im drg_joint drg_sepsis drg_uti drg_hipfx drg_chf {
	tab `var' if benefit_day>=1 & benefit_day<=7
	}

*Days 8-15
	
	sum age med_hoshd_inc unemplmt_rate povty_rate combdty_sum prior_pmt_amt_sum_2 hosp_util_day if benefit_day>=7 & benefit_day<=14 
	foreach var of varlist sex_num race_num married_im drg_joint drg_sepsis drg_uti drg_hipfx drg_chf {
	tab `var' if benefit_day>=8 & benefit_day<=15
	}

*Days 16-40
	
	sum age med_hoshd_inc unemplmt_rate povty_rate combdty_sum prior_pmt_amt_sum_2 hosp_util_day if benefit_day>=15 & benefit_day<=40 
	foreach var of varlist sex_num race_num married_im drg_joint drg_sepsis drg_uti drg_hipfx drg_chf {
	tab `var' if benefit_day>=16 & benefit_day<=40
	}
	
	restore  

***Low Severity 
	preserve
	
	keep if combdty_sum<=6 
	
*Days 1-6

	sum age med_hoshd_inc unemplmt_rate povty_rate combdty_sum prior_pmt_amt_sum_2 hosp_util_day if benefit_day>=1 & benefit_day<=6
	foreach var of varlist sex_num race_num married_im drg_joint drg_sepsis drg_uti drg_hipfx drg_chf {
	tab `var' if benefit_day>=1 & benefit_day<=6
	}

*Days 7-14
	
	sum age med_hoshd_inc unemplmt_rate povty_rate combdty_sum prior_pmt_amt_sum_2 hosp_util_day if benefit_day>=7 & benefit_day<=14 
	foreach var of varlist sex_num race_num married_im drg_joint drg_sepsis drg_uti drg_hipfx drg_chf {
	tab `var' if benefit_day>=7 & benefit_day<=14
	}

*Days 15-40
	
	sum age med_hoshd_inc unemplmt_rate povty_rate combdty_sum prior_pmt_amt_sum_2 hosp_util_day if benefit_day>=15 & benefit_day<=40 
	foreach var of varlist sex_num race_num married_im drg_joint drg_sepsis drg_uti drg_hipfx drg_chf {
	tab `var' if benefit_day>=15 & benefit_day<=40
	}
	
	restore 
		
	
/*********************************************************************************************
 ** Supplemental Table 3. Characteristics of study cohort stratified by instrumental variable, 
                          limiting the study cohort to those with 30 to 60 days between current 
                          SNF and prior SNF stay 
*********************************************************************************************/
	
	preserve
	
	keep if snf_after_30day_break==1

*Days 1-11

	sum age med_hoshd_inc unemplmt_rate povty_rate combdty_sum prior_pmt_amt_sum_2 hosp_util_day if benefit_day>=1 & benefit_day<=11
	foreach var of varlist sex_num race_num married_im drg_joint drg_sepsis drg_uti drg_hipfx drg_chf {
	tab `var' if benefit_day>=1 & benefit_day<=11
	}

*Days 12-18
	
	sum age med_hoshd_inc unemplmt_rate povty_rate combdty_sum prior_pmt_amt_sum_2 hosp_util_day if benefit_day>=12 & benefit_day<=18 
	foreach var of varlist sex_num race_num married_im drg_joint drg_sepsis drg_uti drg_hipfx drg_chf {
	tab `var' if benefit_day>=12 & benefit_day<=18
	}

*Days 19-40
	
	sum age med_hoshd_inc unemplmt_rate povty_rate combdty_sum prior_pmt_amt_sum_2 hosp_util_day if benefit_day>=19 & benefit_day<=40 
	foreach var of varlist sex_num race_num married_im drg_joint drg_sepsis drg_uti drg_hipfx drg_chf {
	tab `var' if benefit_day>=19 & benefit_day<=40
	}
	
	restore 	


/*********************************************************************************************
 ** Supplemental Table 4. Characteristics of study cohort stratified by instrumental variable, 
                          limiting the study cohort to those patients discharged on benefit 
                          day 16-25 
*********************************************************************************************/
	
    preserve
	
	keep if benefitday_snfdc>=16 & benefitday_snfdc<=25
	 
*Days 1-6

	sum age med_hoshd_inc unemplmt_rate povty_rate combdty_sum prior_pmt_amt_sum_2 hosp_util_day if benefit_day>=1 & benefit_day<=6
	foreach var of varlist sex_num race_num married_im drg_joint drg_sepsis drg_uti drg_hipfx drg_chf {
	tab `var' if benefit_day>=1 & benefit_day<=6
	}

*Days 7-12
	
	sum age med_hoshd_inc unemplmt_rate povty_rate combdty_sum prior_pmt_amt_sum_2 hosp_util_day if benefit_day>=7 & benefit_day<=12 
	foreach var of varlist sex_num race_num married_im drg_joint drg_sepsis drg_uti drg_hipfx drg_chf {
	tab `var' if benefit_day>=7 & benefit_day<=12
	}

*Days 13-25
	
	sum age med_hoshd_inc unemplmt_rate povty_rate combdty_sum prior_pmt_amt_sum_2 hosp_util_day if benefit_day>=13 & benefit_day<=25 
	foreach var of varlist sex_num race_num married_im drg_joint drg_sepsis drg_uti drg_hipfx drg_chf {
	tab `var' if benefit_day>=13 & benefit_day<=25
	}
	
	restore 


/*********************************************************************************************
 ** Supplemental Table 5. Differences in outcomes with one additional day in SNF from 
                          instrumental variable regressions, stratified by clinical severity 
*********************************************************************************************/

***High Severity 
	preserve
	
	keep if combdty_sum>6 
	
*1st stage 
	quietly xtreg util_day benefit_day $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum) fe vce(robust)
		estimates store first_stage
		predict pr_snflos if e(sample)
		est table first_stage, keep(benefit_day) b t p stats(N) title("1st Stage of Benefit Day 1-40 Sample (High Clinical Severity Cohort)") 
		eststo clear

*2nd stage
	quietly xtreg radm30 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays30_hosp i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm30
	quietly xtreg radm90 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_hosp i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm90
	quietly xtreg radm30snf pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays30_snf i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm30snf
	quietly xtreg radm90snf pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_snf i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm90snf
	quietly xtreg successful_discharge pr_snflos $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store successful_discharge
	quietly xtreg total_pmt_90 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_hosp i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store totalpay90
	quietly xtreg hosp_pmt_90 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store hosp_pmt_90
	quietly xtreg pmt_amt_pseudo pr_snflos $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store snfpmt
	quietly xtreg total_pmt_90_after_snf pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_snf i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store total_pmt_90_after_snf
		est table radm30 radm90 radm30snf radm90snf successful_discharge totalpay90 hosp_pmt_90 snfpmt total_pmt_90_after_snf, keep(pr_snflos) b se t p stats(N) title("2nd Stage of Benefit Day 1-40 Sample (High Clinical Severity Cohort)")
		eststo clear
	
	restore  

***Low Severity 
	preserve
	
	keep if combdty_sum<=6 
	
*1st stage 
	quietly xtreg util_day benefit_day $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum) fe vce(robust)
		estimates store first_stage
		predict pr_snflos if e(sample)
		est table first_stage, keep(benefit_day) b t p stats(N) title("1st Stage of Benefit Day 1-40 Sample (Low Clinical Severity Cohort)") 
		eststo clear

*2nd stage
	quietly xtreg radm30 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays30_hosp i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm30
	quietly xtreg radm90 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_hosp i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm90
	quietly xtreg radm30snf pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays30_snf i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm30snf
	quietly xtreg radm90snf pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_snf i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm90snf
	quietly xtreg successful_discharge pr_snflos $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store successful_discharge
	quietly xtreg total_pmt_90 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_hosp i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store totalpay90
	quietly xtreg hosp_pmt_90 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store hosp_pmt_90
	quietly xtreg pmt_amt_pseudo pr_snflos $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store snfpmt
	quietly xtreg total_pmt_90_after_snf pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_snf i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store total_pmt_90_after_snf
		est table radm30 radm90 radm30snf radm90snf successful_discharge totalpay90 hosp_pmt_90 snfpmt total_pmt_90_after_snf, keep(pr_snflos) b se t p stats(N) title("2nd Stage of Benefit Day 1-40 Sample (Low Clinical Severity Cohort)")
		eststo clear
	
	
	restore 


/*********************************************************************************************
 ** Supplemental Table 6. Differences in outcomes with one additional day in SNF from 
                          instrumental variable regressions, using study cohort limited to 
                          those with 30 to 60 days between current SNF and prior SNF stay 
*********************************************************************************************/
	
	preserve
	keep if snf_after_30day_break==1
	
*1st stage 
	quietly xtreg util_day benefit_day $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum) fe vce(robust)
		estimates store first_stage
		predict pr_snflos if e(sample)
		est table first_stage, keep(benefit_day) b t p stats(N) title("1st Stage of SNF of 30-Day Break Sample")
		eststo clear

*2nd stage
	quietly xtreg radm30 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays30_hosp i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm30
	quietly xtreg radm90 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_hosp i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm90
	quietly xtreg radm30snf pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays30_snf i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm30snf
	quietly xtreg radm90snf pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_snf i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm90snf
	quietly xtreg successful_discharge pr_snflos $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store successful_discharge
	quietly xtreg total_pmt_90 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_hosp i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store totalpay90
	quietly xtreg hosp_pmt_90 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store hosp_pmt_90
	quietly xtreg pmt_amt_pseudo pr_snflos $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store snfpmt
	quietly xtreg total_pmt_90_after_snf pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_snf i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store total_pmt_90_after_snf
		est table radm30 radm90 radm30snf radm90snf successful_discharge totalpay90 hosp_pmt_90 snfpmt total_pmt_90_after_snf, keep(pr_snflos) b se t p stats(N) title("2nd Stage of SNF of 30-Day Break Sample")
		eststo clear
	
	restore


/*********************************************************************************************
 ** Supplemental Table 7. Differences in outcome with one additional day in SNF from 
                          instrumental variable regressions, using study cohort limited to 
                          those discharged on benefit day 16-25 
*********************************************************************************************/

	preserve
	keep if benefitday_snfdc>=16 & benefitday_snfdc<=25

*1st stage 
	quietly xtreg util_day benefit_day $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum) fe vce(robust)
		estimates store first_stage
		predict pr_snflos if e(sample)
		est table first_stage, keep(benefit_day) b t p stats(N) title("1st Stage of Benefit Day 1-40 Sample") 
		eststo clear

*2nd stage
	quietly xtreg radm30 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays30_hosp i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm30
	quietly xtreg radm90 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_hosp i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm90
	quietly xtreg radm30snf pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays30_snf i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm30snf
	quietly xtreg radm90snf pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_snf i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store radm90snf
	quietly xtreg successful_discharge pr_snflos $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store successful_discharge
	quietly xtreg total_pmt_90 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_hosp i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store totalpay90
	quietly xtreg hosp_pmt_90 pr_snflos $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store hosp_pmt_90
	quietly xtreg pmt_amt_pseudo pr_snflos $pt_covar hosp_util_day i.hosp_drgcd i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store snfpmt
	quietly xtreg total_pmt_90_after_snf pr_snflos $pt_covar hosp_util_day i.hosp_drgcd ndays90_snf i.rug4num i.year_dschrg, i(providernum)fe vce(robust)
		estimates store total_pmt_90_after_snf
		est table radm30 radm90 radm30snf radm90snf successful_discharge totalpay90 hosp_pmt_90 snfpmt total_pmt_90_after_snf, keep(pr_snflos) b se t p stats(N) title("2nd Stage of Benefit Day 1-40 Sample")
		eststo clear
	
	restore
