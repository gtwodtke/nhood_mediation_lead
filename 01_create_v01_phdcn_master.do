capture log close
capture clear all
set more off

global project_directory "D:\projects\nhood_mediation_lead" 
*global project_directory "C:\Users\Geoff.Wodtke\Desktop\projects\nhood_mediation_lead"
*global project_directory "C:\nhood_mediation_lead"

log using "${project_directory}\programs\_LOGS\01_create_v01_phdcn_master.log", replace

/*
DO-FILE NAME: 01_create_v01_phdcn_master.do
AUTHOR: Sagi Ramaj
DATE UPDATED: 08/04/2020 (GW)
PURPOSE: pulling data from waves 1-3 masterfiles
NOTES:
*/

//MASTER FILE WAVE 1
use "${project_directory}\data\phdcn\14802059\ICPSR_13580\DS0001\13580-0001-Data-REST.dta" , clear

drop if COHORT >12
keep SUBID COHORT CELL AGE1 YRW1PC FAM_ID LINK_NC AGE1_PC ETHN_PC GEND_PC1 ///
		MSTAT_PC DB18 PC_RELAT GENDER ETHN_SP FAMSIZE FAMSTRUC EDUC_PC ///
		EDUC_PR EDUC_MAX IEDUCMAX SALARY ISALARY EMPLOY EMPL_PR ///
		EIA1C0 SESCOMP WISCRAW WISCSCAL 

//Renaming 
rename SUBID nSUBID
rename COHORT nCohort
rename YRW1PC nPCYearInterview_1
rename FAM_ID nFAMID_1
rename LINK_NC nlinknc_1
rename AGE1_PC nPCAge_1
rename FAMSIZE nFamSize_1 
rename EDUC_PR nPCPartnerEdu_1 
rename EDUC_MAX nPCandPartnerEduMax_1
rename IEDUCMAX nPCandPartnerEduMaxImputed_1
rename SESCOMP nSES_1
rename WISCRAW nWISCRaw_1
rename WISCSCAL nWISCScaled_1

//Recoding
gen nSUBAge_1 = . 
replace nSUBAge_1 = 0 if AGE1 <.5 | AGE1 ==.
replace nSUBAge_1 = .5 if AGE1 >=.5 & AGE1 <1 
replace nSUBAge_1 = 1 if AGE1 >=1 & AGE1 <2 
replace nSUBAge_1 = 2 if AGE1 >=2 & AGE1 <3
replace nSUBAge_1 = 3 if AGE1 >=3 & AGE1 <4
replace nSUBAge_1 = 4 if AGE1 >=4 & AGE1 <5
replace nSUBAge_1 = 5 if AGE1 >=5 & AGE1 <6
replace nSUBAge_1 = 6 if AGE1 >=6 & AGE1 <7
replace nSUBAge_1 = 7 if AGE1 >=7 & AGE1 <8
replace nSUBAge_1 = 8 if AGE1 >=8 & AGE1 <9
replace nSUBAge_1 = 9 if AGE1 >=9 & AGE1 <10
replace nSUBAge_1 = 10 if AGE1 >=10 & AGE1 <11
replace nSUBAge_1 = 11 if AGE1 >=11 & AGE1 <12
replace nSUBAge_1 = 12 if AGE1 >=12 & AGE1 <13
replace nSUBAge_1 = 13 if AGE1 >=13 & AGE1 <14

recode GEND_PC1 (1 = 0) (2 = 1) (.=.), gen(nPCFemale_1)
recode GENDER (1 = 0) (2 = 1) (.=.), gen(nSUBFemale_1)

recode ETHN_PC(0 = 1 Hispanic) (1 2 5 6 = 2 AsianPINativeOther) (3 = 3 Black) (4 = 4 White) (.=.), gen(nPCEthnicity_1)

recode MSTAT_PC (0 = 1 Married) (2 = 2 Partnered) (1 = 3 Single) (.=.), gen(nPCMarstat_1)

recode DB18 (1 11 = 1 English) (4 = 2 Spanish) (2 3 5 6 7 8 9 10 12 = 3 Other) (.=.), gen(nPCLanguage_1)

recode PC_RELAT (1 = 1 biomom) (11 = 2 biodad) (2 3 4 5 6 7 8 9 10 12 13 14 15 18 19 = 3 other) (.=.), gen(nPCRelaSubject_1)

recode EDUC_PC (1 2 = 1 LessThanHS) (3 = 2 HS) (4 = 3 MoreThanHS) (5 = 4 BA+) (.=.), gen(nPCEdu_1)

recode ETHN_SP (0 = 1 Hispanic) (1 2 5 6 = 2 AsianPINativeOther) (3 = 3 Black) (4 = 4 White) (.=.), gen(nSUBEthnicity_1) 

recode EMPLOY (2 3 = 0 Unemployed) (1 = 1 Employed) (.=.), gen(nPCEmployed_1)
recode EMPL_PR (2 3 = 0 Unemployed) (1 = 1 Employed) (.=.), gen(nPCPartnerEmployed_1)

rename EIA1C0 nPCPublicAssist_1
replace nPCPublicAssist_1 = . if nPCPublicAssist_1 == -96

gen nSalary_1 = 0  
replace nSalary_1 = 2500 if SALARY == 1
replace nSalary_1 = 7500 if SALARY == 2
replace nSalary_1 = 15000 if SALARY == 3
replace nSalary_1 = 25000 if SALARY == 4
replace nSalary_1 = 35000 if SALARY == 5
replace nSalary_1 = 45000 if SALARY == 6
replace nSalary_1 = 50000 * 1.4 if SALARY == 7
replace nSalary_1 = . if SALARY == . 
replace nSalary_1 = nSalary_1 * (220.0/225.3) if nPCYearInterview_1 == 1995  
replace nSalary_1 = nSalary_1 * (220.0/231.3) if nPCYearInterview_1 == 1996
replace nSalary_1 = nSalary_1 * (220.0/236.3) if nPCYearInterview_1 == 1997
gen nSalaryImputed_1 = 0
replace nSalaryImputed_1 = 2500 if ISALARY == 1
replace nSalaryImputed_1 = 7500 if ISALARY == 2
replace nSalaryImputed_1 = 15000 if ISALARY == 3
replace nSalaryImputed_1 = 25000 if ISALARY == 4
replace nSalaryImputed_1 = 35000 if ISALARY == 5
replace nSalaryImputed_1 = 45000 if ISALARY == 6
replace nSalaryImputed_1 = 50000 * 1.4 if ISALARY == 7
replace nSalaryImputed_1 = . if ISALARY == . 
replace nSalaryImputed_1 = nSalaryImputed_1 * (220.0/225.3) if nPCYearInterview_1 == 1995 
replace nSalaryImputed_1 = nSalaryImputed_1 * (220.0/231.3) if nPCYearInterview_1 == 1996
replace nSalaryImputed_1 = nSalaryImputed_1 * (220.0/236.3) if nPCYearInterview_1 == 1997

recode CELL ///
	(1 = 1) ///
	(2 = 2) ///
	(3 = 3) ///
	(5 = 4) ///
	(6 = 5) ///
	(7 = 6) ///
	(8 = 7) ///
	(10 = 8) ///
	(11 = 9) ///
	(12 = 10) ///
	(13 = 11) ///
	(14 = 12) ///
	(16 = 13) ///
	(17 = 13) ///
	(18 = 14) ///
	(19 = 15) ///
	(20 = 16) ///
	(21 = 16) ///
	(. = 8), gen(nstrata)

keep n*

save "${project_directory}\data\_TEMP\MasterFileWave1.dta", replace


//MASTER FILE WAVE 2
use "${project_directory}\data\phdcn\14802059\ICPSR_13608\DS0001\13608-0001-Data-REST.dta", clear

drop if COHORT >12
keep SUBID COHORT AGE2 YRW2PC FAM_ID2 LINK_NC FA3 FA13 GEO_ID1 GEO_ID2 GEOSAM2 ///
		ETHN_PC2 GEND_PC2 DD57 PC_LANG2 FA5 GENDER EDUC_PC2 EDUCMAX2 IEDUMAX2 ///
		SALARY2 ISALARY2 DD6A DD62A SESCOMP2 ///
		WISCRAW2 WISCSCL2 

//Renaming
rename SUBID nSUBID
rename COHORT nCohort
rename YRW2PC nPCYearInterview_2
rename FAM_ID2 nFAMID_2
rename LINK_NC nlinknc_2
rename GEOSAM21 nSameGeocodeW1_2
rename GEO_ID1 nGeoID_1
rename GEO_ID2 nGeoID_2
rename EDUCMAX2 nPCandPartnerEduMax_2
rename IEDUMAX2 nPCandPartnerEduMaxImputed_2
rename SESCOMP2 nSES_2
rename WISCRAW2 nWISCRaw_2
rename WISCSCL2 nWISCScaled_2

//Recoding
gen nSUBAge_2 = . 
replace nSUBAge_2 = 0 if AGE2 <1 
replace nSUBAge_2 = 1 if AGE2 >=1 & AGE2 <2 
replace nSUBAge_2 = 2 if AGE2 >=2 & AGE2 <3
replace nSUBAge_2 = 3 if AGE2 >=3 & AGE2 <4
replace nSUBAge_2 = 4 if AGE2 >=4 & AGE2 <5
replace nSUBAge_2 = 5 if AGE2 >=5 & AGE2 <6
replace nSUBAge_2 = 6 if AGE2 >=6 & AGE2 <7
replace nSUBAge_2 = 7 if AGE2 >=7 & AGE2 <8
replace nSUBAge_2 = 8 if AGE2 >=8 & AGE2 <9
replace nSUBAge_2 = 9 if AGE2 >=9 & AGE2 <10
replace nSUBAge_2 = 10 if AGE2 >=10 & AGE2 <11
replace nSUBAge_2 = 11 if AGE2 >=11 & AGE2 <12
replace nSUBAge_2 = 12 if AGE2 >=12 & AGE2 <13
replace nSUBAge_2 = 13 if AGE2 >=13 & AGE2 <14
replace nSUBAge_2 = 14 if AGE2 >=14 & AGE2 <15
replace nSUBAge_2 = 15 if AGE2 >=15 & AGE2 <16
replace nSUBAge_2 = 16 if AGE2 >=16 & AGE2 <17
replace nSUBAge_2 = 17 if AGE2 >=17 & AGE2 <18

rename FA3 nPCSame_2
replace nPCSame_2 = . if nPCSame_2 == -99
rename FA13 nSameAddressW1_2
replace nSameAddressW1_2 = . if nSameAddressW1_2 == -99

recode GEND_PC2 (1 = 0) (2 = 1) (.=.), gen(nPCFemale_2)
recode GENDER (1 = 0) (2 = 1) (.=.), gen(nSUBFemale_2)

recode ETHN_PC2 (0 = 1 Hispanic) (1 2 5 6 = 2 AsianPINativeOther) (3 = 3 Black) (4 = 4 White) (.=.), gen(nPCEthnicity_2)

recode DD57 (5 = 1 Married) (6 = 2 Cohab) (1 2 3 4 = 3 Single) (-99 -96 . = .), gen(nPCMarstat_2)

recode PC_LANG2 (1 5 = 1 English) (2 = 2 Spanish) (3 4 = 3 Other) (.=.), gen(nPCLanguage_2)

recode FA5 (1 = 1 biomom) (9 = 2 biodad) (2 3 4 5 6 7 8 10 11 12 13 14 15 16 17 = 3 other) (-99 . = .), gen(nPCRelaSubject_2)

recode EDUC_PC2 (1 2 = 1 LessThanHS) (3 = 2 HS) (4 = 3 MoreThanHS) (5 = 4 BA+) (.=.), gen(nPCEdu_2)

recode DD6A (2 3 4 5 6 7 = 0 Unemployed) (1=1 Employed) (-99 -96 .=.), gen(nPCEmployed_2)
recode DD62A (2 3 4 5 6 7 = 0 Unemployed) (1=1 Employed) (-99 -96 -98 .=.), gen(nPCPartnerEmployed_2)

gen nSalary_2 = 0  
replace nSalary_2 = 2500 if SALARY2 == 1
replace nSalary_2 = 7500 if SALARY2 == 2
replace nSalary_2 = 15000 if SALARY2 == 3
replace nSalary_2 = 25000 if SALARY2 == 4
replace nSalary_2 = 35000 if SALARY2 == 5
replace nSalary_2 = 45000 if SALARY2 == 6
replace nSalary_2 = 55000 if SALARY2 == 7
replace nSalary_2 = 65000 if SALARY2 == 8
replace nSalary_2 = 75000 if SALARY2 == 9
replace nSalary_2 = 85000 if SALARY2 == 10
replace nSalary_2 = 90000 * 1.4 if SALARY2 == 11  
replace nSalary_2 = . if SALARY2 == -99 | SALARY2 == -98 | SALARY2 == -96 | SALARY2 == .
replace nSalary_2 = nSalary_2 * (220.0/236.3) if nPCYearInterview_2 == 1997 
replace nSalary_2 = nSalary_2 * (220.0/239.5) if nPCYearInterview_2 == 1998
replace nSalary_2 = nSalary_2 * (220.0/244.6) if nPCYearInterview_2 == 1999
replace nSalary_2 = nSalary_2 * (220.0/252.9) if nPCYearInterview_2 == 2000
gen nSalaryImputed_2 = 0  
replace nSalaryImputed_2 = 2500 if ISALARY2 == 1
replace nSalaryImputed_2 = 7500 if ISALARY2 == 2
replace nSalaryImputed_2 = 15000 if ISALARY2 == 3
replace nSalaryImputed_2 = 25000 if ISALARY2 == 4
replace nSalaryImputed_2 = 35000 if ISALARY2 == 5
replace nSalaryImputed_2 = 45000 if ISALARY2 == 6
replace nSalaryImputed_2 = 50000 * 1.4 if ISALARY2 == 7 
replace nSalaryImputed_2 = . if ISALARY2 >=8 | ISALARY2 == .
replace nSalaryImputed_2 = nSalaryImputed_2 * (220.0/236.3) if nPCYearInterview_2 == 1997  
replace nSalaryImputed_2 = nSalaryImputed_2 * (220.0/239.5) if nPCYearInterview_2 == 1998
replace nSalaryImputed_2 = nSalaryImputed_2 * (220.0/244.6) if nPCYearInterview_2 == 1999
replace nSalaryImputed_2 = nSalaryImputed_2 * (220.0/252.9) if nPCYearInterview_2 == 2000

keep n*

save "${project_directory}\data\_TEMP\MasterFileWave2.dta", replace


//MASTER FILE WAVE 3
use "${project_directory}\data\phdcn\14802059\ICPSR_13668\DS0001\13668-0001-Data-REST.dta" , clear

drop if COHORT >12
keep SUBID COHORT AGE3 YRW3PC FAM_ID3 LINK_NC GEO_ID3 FSP3 GEOSAM31 GEOSAM32 ///
		FSP5 GENDER WISCRAW3 WISCSCL3 

//Renaming
rename SUBID nSUBID
rename COHORT nCohort
rename YRW3PC nPCYearInterview_3
rename FAM_ID3 nFAMID_3
rename LINK_NC nlinknc_3
rename GEO_ID3 nGeoID_3
rename GEOSAM31 nSameGeocodeW1_3
rename GEOSAM32 nSameGeocodeW2_3
rename WISCRAW3 nWISCRaw_3
rename WISCSCL3 nWISCScaled_3

//Recoding
rename FSP3 nPCSame_3
replace nPCSame_3 = . if nPCSame_3 == -99
recode FSP5 (1 = 1 biomom) (2 = 2 biodad) (3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 = 3 other) (99 . = .), gen(nPCRelaSubject_3)  
recode GENDER (1 = 0) (2 = 1) (.=.), gen(nSUBFemale_3)

gen nSUBAge_3 = . 
replace nSUBAge_3 = 0 if AGE3 <1 
replace nSUBAge_3 = 1 if AGE3 >=1 & AGE3 <2 
replace nSUBAge_3 = 2 if AGE3 >=2 & AGE3 <3
replace nSUBAge_3 = 3 if AGE3 >=3 & AGE3 <4
replace nSUBAge_3 = 4 if AGE3 >=4 & AGE3 <5
replace nSUBAge_3 = 5 if AGE3 >=5 & AGE3 <6
replace nSUBAge_3 = 6 if AGE3 >=6 & AGE3 <7
replace nSUBAge_3 = 7 if AGE3 >=7 & AGE3 <8
replace nSUBAge_3 = 8 if AGE3 >=8 & AGE3 <9
replace nSUBAge_3 = 9 if AGE3 >=9 & AGE3 <10
replace nSUBAge_3 = 10 if AGE3 >=10 & AGE3 <11
replace nSUBAge_3 = 11 if AGE3 >=11 & AGE3 <12
replace nSUBAge_3 = 12 if AGE3 >=12 & AGE3 <13
replace nSUBAge_3 = 13 if AGE3 >=13 & AGE3 <14
replace nSUBAge_3 = 14 if AGE3 >=14 & AGE3 <15
replace nSUBAge_3 = 15 if AGE3 >=15 & AGE3 <16
replace nSUBAge_3 = 16 if AGE3 >=16 & AGE3 <17
replace nSUBAge_3 = 17 if AGE3 >=17 & AGE3 <18
replace nSUBAge_3 = 18 if AGE3 >=18 & AGE3 <19

keep n*

save "${project_directory}\data\_TEMP\MasterFileWave3.dta", replace

//MERGING WAVES 1 TO 3
use "${project_directory}\data\_TEMP\MasterFileWave1.dta", clear
merge 1:1 nSUBID using "${project_directory}\data\_TEMP\MasterFileWave2.dta"
drop _merge
save "${project_directory}\data\_TEMP\MasterFileWave1And2.dta", replace
use "${project_directory}\data\_TEMP\MasterFileWave1And2.dta", clear
merge 1:1 nSUBID using "${project_directory}\data\_TEMP\MasterFileWave3.dta"
drop _merge

replace nSUBFemale_1 = nSUBFemale_2 if nSUBFemale_1 == . 

save "${project_directory}\data\phdcn\v01_phdcn_master.dta", replace

erase "${project_directory}\data\_TEMP\MasterFileWave1.dta"
erase "${project_directory}\data\_TEMP\MasterFileWave2.dta"
erase "${project_directory}\data\_TEMP\MasterFileWave3.dta"
erase "${project_directory}\data\_TEMP\MasterFileWave1And2.dta"

log close
