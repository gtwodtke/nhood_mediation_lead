capture log close
capture clear all
set more off

global project_directory "D:\projects\nhood_mediation_lead" 
*global project_directory "C:\Users\Geoff.Wodtke\Desktop\projects\nhood_mediation_lead"
*global project_directory "C:\nhood_mediation_lead"

log using "${project_directory}\programs\_LOGS\02_create_v01_phdcn_demo.log", replace

/*
DO-FILE NAME: 02_create_v01_phdcn_demo.do
AUTHOR: Sagi Ramaj 
DATE UPDATED: 08/04/2020 (GW)
PURPOSE: pulling data from waves 1-3 demographic files
NOTES:
*/

//Demographic File Wave 1 - Cohort 0
use "${project_directory}\data\phdcn\14802059\ICPSR_13581\DS0001\13581-0001-Data-REST.dta", clear

keep SUBID COHORT DB1

rename SUBID nSUBID
rename COHORT nCohort

gen nPCHomeowner_1 = 0
replace nPCHomeowner_1 = 1 if DB1 == 2
replace nPCHomeowner_1 = . if DB1 == . | DB1 == -99

keep n*

save "${project_directory}\data\_TEMP\DemoFileWave1Cohort0.dta", replace

//Demographic File Wave 1 - Cohort 3
use "${project_directory}\data\phdcn\14802059\ICPSR_13581\DS0002\13581-0002-Data-REST.dta", clear

keep SUBID COHORT DB1

rename SUBID nSUBID
rename COHORT nCohort

gen nPCHomeowner_1 = 0
replace nPCHomeowner_1 = 1 if DB1 == 2
replace nPCHomeowner_1 = . if DB1 == . | DB1 == -99

keep n*

save "${project_directory}\data\_TEMP\DemoFileWave1Cohort3.dta", replace

//Demographic File Wave 1 - Cohort 6
use "${project_directory}\data\phdcn\14802059\ICPSR_13581\DS0003\13581-0003-Data-REST.dta", clear

keep SUBID COHORT DB1

rename SUBID nSUBID
rename COHORT nCohort

gen nPCHomeowner_1 = 0
replace nPCHomeowner_1 = 1 if DB1 == 2
replace nPCHomeowner_1 = . if DB1 == . | DB1 == -99

keep n*

save "${project_directory}\data\_TEMP\DemoFileWave1Cohort6.dta", replace 

//Demographic File Wave 1 - Cohort 9
use "${project_directory}\data\phdcn\14802059\ICPSR_13581\DS0004\13581-0004-Data-REST.dta", clear

keep SUBID COHORT DB1

rename SUBID nSUBID
rename COHORT nCohort

gen nPCHomeowner_1 = 0
replace nPCHomeowner_1 = 1 if DB1 == 2
replace nPCHomeowner_1 = . if DB1 == . | DB1 == -99

keep n*

save "${project_directory}\data\_TEMP\DemoFileWave1Cohort9.dta", replace 

//Demographic File Wave 1 - Cohort 12
use "${project_directory}\data\phdcn\14802059\ICPSR_13581\DS0005\13581-0005-Data-REST.dta", clear

keep SUBID COHORT DB1

rename SUBID nSUBID
rename COHORT nCohort

gen nPCHomeowner_1 = 0
replace nPCHomeowner_1 = 1 if DB1 == 2
replace nPCHomeowner_1 = . if DB1 == . | DB1 == -99

keep n*

save "${project_directory}\data\_TEMP\DemoFileWave1Cohort12.dta", replace 

//Demographic File Wave 2 - Cohort 0
use "${project_directory}\data\phdcn\14802059\ICPSR_13609\DS0001\13609-0001-Data-REST.dta", clear
 
keep SUBID COHORT DD19 

rename SUBID nSUBID
rename COHORT nCohort

rename DD19 nPCPublicAssist_2

keep n*

save "${project_directory}\data\_TEMP\DemoFileWave2Cohort0.dta", replace 

//Demographic File Wave 2 - Cohort 3
use "${project_directory}\data\phdcn\14802059\ICPSR_13609\DS0002\13609-0002-Data-REST.dta", clear
 
keep SUBID COHORT DD19 

rename SUBID nSUBID
rename COHORT nCohort

rename DD19 nPCPublicAssist_2
replace nPCPublicAssist_2 = . if nPCPublicAssist_2 == -99

keep n*

save "${project_directory}\data\_TEMP\DemoFileWave2Cohort3.dta", replace 

//Demographic File Wave 2 - Cohort 6
use "${project_directory}\data\phdcn\14802059\ICPSR_13609\DS0003\13609-0003-Data-REST.dta", clear
 
keep SUBID COHORT DD19

rename SUBID nSUBID
rename COHORT nCohort

rename DD19 nPCPublicAssist_2
replace nPCPublicAssist_2 = . if nPCPublicAssist_2 == -99 | nPCPublicAssist_2 == -96

keep n*

save "${project_directory}\data\_TEMP\DemoFileWave2Cohort6.dta", replace 

//Demographic File Wave 2 - Cohort 9
use "${project_directory}\data\phdcn\14802059\ICPSR_13609\DS0004\13609-0004-Data-REST.dta", clear
 
keep SUBID COHORT DD19 

rename SUBID nSUBID
rename COHORT nCohort

rename DD19 nPCPublicAssist_2
replace nPCPublicAssist_2 = . if nPCPublicAssist_2 == -99 | nPCPublicAssist_2 == -96

keep n*

save "${project_directory}\data\_TEMP\DemoFileWave2Cohort9.dta", replace 

//Demographic File Wave 2 - Cohort 12
use "${project_directory}\data\phdcn\14802059\ICPSR_13609\DS0005\13609-0005-Data-REST.dta", clear
 
keep SUBID COHORT DD19

rename SUBID nSUBID
rename COHORT nCohort

rename DD19 nPCPublicAssist_2
replace nPCPublicAssist_2 = . if nPCPublicAssist_2 == -99 | nPCPublicAssist_2 == -96

keep n*

save "${project_directory}\data\_TEMP\DemoFileWave2Cohort12.dta", replace 

//Demographic File Wave 3 - Cohort 0
use "${project_directory}\data\phdcn\14802059\ICPSR_13669\DS0001\13669-0001-Data-REST.dta", clear

keep SUBID COHORT DF3 DF85 DF6A DF57A DF76 DF50

rename SUBID nSUBID
rename COHORT nCohort

recode DF3 (1 = 1 LessThanHS) (2 3 = 2 HS) (4 = 3 MoreThanHS) (5 6 7 = 4 BA+) (8 . = .), gen(nPCEdu_3) 

recode DF6A (2 3 4 5 6 7 = 0 Unemployed) (1 = 1 Employed) (.=.), gen(nPCEmployed_3)
recode DF57A (2 3 4 5 6 7 = 0 Unemployed) (1 = 1 Employed) (. -96 = .), gen(nPCPartnerEmployed_3)

rename DF76 nPCPublicAssist_3
replace nPCPublicAssist_3 = . if nPCPublicAssist_3 == -99

recode DF50 (5 = 1 Married) (6 = 2 Cohab) (1 2 3 4 = 3 Single) (-99 . = .), gen(nPCMarstat_3)

gen nSalary_3 = 0
replace nSalary_3 = 2500 if DF85 == 1
replace nSalary_3 = 7500 if DF85 == 2
replace nSalary_3 = 15000 if DF85 == 3
replace nSalary_3 = 25000 if DF85 == 4
replace nSalary_3 = 35000 if DF85 == 5
replace nSalary_3 = 45000 if DF85 == 6
replace nSalary_3 = 55000 if DF85 == 7
replace nSalary_3 = 65000 if DF85 == 8
replace nSalary_3 = 75000 if DF85 == 9
replace nSalary_3 = 85000 if DF85 == 10
replace nSalary_3 = 90000 * 1.4 if DF85 == 11  
replace nSalary_3 = . if DF85 == -99 | DF85 == -98 | DF85 == -96 | DF85 == .
merge 1:1 nSUBID using "${project_directory}\data\phdcn\v01_phdcn_master.dta", keepusing(nPCYearInterview_3) 
keep if _merge==3
replace nSalary_3 = nSalary_3 * (220.0/252.9) if nPCYearInterview_3 == 2000 
replace nSalary_3 = nSalary_3 * (220.0/260.1) if nPCYearInterview_3 == 2001
replace nSalary_3 = nSalary_3 * (220.0/264.2) if nPCYearInterview_3 == 2002
drop _merge nPCYearInterview_3

keep n*

save "${project_directory}\data\_TEMP\DemoFileWave3Cohort0.dta", replace 

//Demographic File Wave 2 - Cohort 3
use "${project_directory}\data\phdcn\14802059\ICPSR_13669\DS0002\13669-0002-Data-REST.dta", clear

keep SUBID COHORT DF3 DF85 DF6A DF57A DF76 DF50

rename SUBID nSUBID
rename COHORT nCohort

recode DF6A (2 3 4 5 6 7 = 0 Unemployed) (1 = 1 Employed) (.=.), gen(nPCEmployed_3)
recode DF57A (2 3 4 5 6 7 = 0 Unemployed) (1 = 1 Employed) (. -96 = .), gen(nPCPartnerEmployed_3)

recode DF3 (1 = 1 LessThanHS) (2 3 = 2 HS) (4 = 3 MoreThanHS) (5 6 7 = 4 BA+) (8 . = .), gen(nPCEdu_3) // 8 "other", not clear what that is. n = 10

rename DF76 nPCPublicAssist_3
replace nPCPublicAssist_3 = . if nPCPublicAssist_3 == -99

recode DF50 (5 = 1 Married) (6 = 2 Cohab) (1 2 3 4 = 3 Single) (-99 . = .), gen(nPCMarstat_3)

gen nSalary_3 = 0 
replace nSalary_3 = 2500 if DF85 == 1
replace nSalary_3 = 7500 if DF85 == 2
replace nSalary_3 = 15000 if DF85 == 3
replace nSalary_3 = 25000 if DF85 == 4
replace nSalary_3 = 35000 if DF85 == 5
replace nSalary_3 = 45000 if DF85 == 6
replace nSalary_3 = 55000 if DF85 == 7
replace nSalary_3 = 65000 if DF85 == 8
replace nSalary_3 = 75000 if DF85 == 9
replace nSalary_3 = 85000 if DF85 == 10
replace nSalary_3 = 90000 * 1.4 if DF85 == 11  
replace nSalary_3 = . if DF85 == -99 | DF85 == -98 | DF85 == -96 | DF85 == .
merge 1:1 nSUBID using "${project_directory}\data\phdcn\v01_phdcn_master.dta", keepusing(nPCYearInterview_3) 
keep if _merge==3
replace nSalary_3 = nSalary_3 * (220.0/252.9) if nPCYearInterview_3 == 2000 
replace nSalary_3 = nSalary_3 * (220.0/260.1) if nPCYearInterview_3 == 2001
replace nSalary_3 = nSalary_3 * (220.0/264.2) if nPCYearInterview_3 == 2002
drop _merge nPCYearInterview_3

keep n*

save "${project_directory}\data\_TEMP\DemoFileWave3Cohort3.dta", replace 

//Demographic File Wave 3 - Cohort 6
use "${project_directory}\data\phdcn\14802059\ICPSR_13669\DS0003\13669-0003-Data-REST.dta", clear

keep SUBID COHORT DF3 DF85 DF6A DF57A DF76 DF50

rename SUBID nSUBID
rename COHORT nCohort
rename DF76 nPCPublicAssist_3

recode DF6A (2 3 4 5 6 7 = 0 Unemployed) (1 = 1 Employed) (.=.), gen(nPCEmployed_3)
recode DF57A (2 3 4 5 6 7 = 0 Unemployed) (1 = 1 Employed) (. -96 = .), gen(nPCPartnerEmployed_3)

recode DF3 (1 = 1 LessThanHS) (2 3 = 2 HS) (4 = 3 MoreThanHS) (5 6 7 = 4 BA+) (8 . = .), gen(nPCEdu_3) 

recode DF50 (5 = 1 Married) (6 = 2 Cohab) (1 2 3 4 = 3 Single) (-99 . = .), gen(nPCMarstat_3)

gen nSalary_3 = 0  
replace nSalary_3 = 2500 if DF85 == 1
replace nSalary_3 = 7500 if DF85 == 2
replace nSalary_3 = 15000 if DF85 == 3
replace nSalary_3 = 25000 if DF85 == 4
replace nSalary_3 = 35000 if DF85 == 5
replace nSalary_3 = 45000 if DF85 == 6
replace nSalary_3 = 55000 if DF85 == 7
replace nSalary_3 = 65000 if DF85 == 8
replace nSalary_3 = 75000 if DF85 == 9
replace nSalary_3 = 85000 if DF85 == 10
replace nSalary_3 = 90000 * 1.4 if DF85 == 11  
replace nSalary_3 = . if DF85 == -99 | DF85 == -98 | DF85 == -96 | DF85 == .
merge 1:1 nSUBID using "${project_directory}\data\phdcn\v01_phdcn_master.dta", keepusing(nPCYearInterview_3) 
keep if _merge==3
replace nSalary_3 = nSalary_3 * (220.0/252.9) if nPCYearInterview_3 == 2000 
replace nSalary_3 = nSalary_3 * (220.0/260.1) if nPCYearInterview_3 == 2001
replace nSalary_3 = nSalary_3 * (220.0/264.2) if nPCYearInterview_3 == 2002
drop _merge nPCYearInterview_3

keep n*

save "${project_directory}\data\_TEMP\DemoFileWave3Cohort6.dta", replace 

//Demographic File Wave 3 - Cohort 9
use "${project_directory}\data\phdcn\14802059\ICPSR_13669\DS0004\13669-0004-Data-REST.dta", clear

keep SUBID COHORT DF3 DF85 DF6A DF57A DF76 DF50

rename SUBID nSUBID
rename COHORT nCohort
rename DF76 nPCPublicAssist_3

recode DF6A (2 3 4 5 6 7 = 0 Unemployed) (1 = 1 Employed) (.=.), gen(nPCEmployed_3)
recode DF57A (2 3 4 5 6 7 = 0 Unemployed) (1 = 1 Employed) (. -96 = .), gen(nPCPartnerEmployed_3)
recode nPCPublicAssist_3 (-96 = .)

recode DF3 (1 = 1 LessThanHS) (2 3 = 2 HS) (4 = 3 MoreThanHS) (5 6 7 = 4 BA+) (8 . = .), gen(nPCEdu_3) 

recode DF50 (5 = 1 Married) (6 = 2 Cohab) (1 2 3 4 = 3 Single) (-99 . = .), gen(nPCMarstat_3)

gen nSalary_3 = 0  
replace nSalary_3 = 2500 if DF85 == 1
replace nSalary_3 = 7500 if DF85 == 2
replace nSalary_3 = 15000 if DF85 == 3
replace nSalary_3 = 25000 if DF85 == 4
replace nSalary_3 = 35000 if DF85 == 5
replace nSalary_3 = 45000 if DF85 == 6
replace nSalary_3 = 55000 if DF85 == 7
replace nSalary_3 = 65000 if DF85 == 8
replace nSalary_3 = 75000 if DF85 == 9
replace nSalary_3 = 85000 if DF85 == 10
replace nSalary_3 = 90000 * 1.4 if DF85 == 11  
replace nSalary_3 = . if DF85 == -99 | DF85 == -98 | DF85 == -96 | DF85 == .
merge 1:1 nSUBID using "${project_directory}\data\phdcn\v01_phdcn_master.dta", keepusing(nPCYearInterview_3) 
keep if _merge==3
replace nSalary_3 = nSalary_3 * (220.0/252.9) if nPCYearInterview_3 == 2000 
replace nSalary_3 = nSalary_3 * (220.0/260.1) if nPCYearInterview_3 == 2001
replace nSalary_3 = nSalary_3 * (220.0/264.2) if nPCYearInterview_3 == 2002
drop _merge nPCYearInterview_3

keep n*

save "${project_directory}\data\_TEMP\DemoFileWave3Cohort9.dta", replace 

//Demographic File Wave 3 - Cohort 12
use "${project_directory}\data\phdcn\14802059\ICPSR_13669\DS0005\13669-0005-Data-REST.dta", clear

keep SUBID COHORT DF3 DF85 DF6A DF57A DF76 DF50

rename SUBID nSUBID
rename COHORT nCohort
rename DF76 nPCPublicAssist_3

recode DF6A (2 3 4 5 6 7 = 0 Unemployed) (1 = 1 Employed) (.=.), gen(nPCEmployed_3)
recode DF57A (2 3 4 5 6 7 = 0 Unemployed) (1 = 1 Employed) (. -96 = .), gen(nPCPartnerEmployed_3)
recode nPCPublicAssist_3 (-99 -96 = .)

recode DF3 (1 = 1 LessThanHS) (2 3 = 2 HS) (4 = 3 MoreThanHS) (5 6 7 = 4 BA+) (-99 8 . = .), gen(nPCEdu_3) 

recode DF50 (5 = 1 Married) (6 = 2 Cohab) (1 2 3 4 = 3 Single) (-99 . = .), gen(nPCMarstat_3)

gen nSalary_3 = 0  
replace nSalary_3 = 2500 if DF85 == 1
replace nSalary_3 = 7500 if DF85 == 2
replace nSalary_3 = 15000 if DF85 == 3
replace nSalary_3 = 25000 if DF85 == 4
replace nSalary_3 = 35000 if DF85 == 5
replace nSalary_3 = 45000 if DF85 == 6
replace nSalary_3 = 55000 if DF85 == 7
replace nSalary_3 = 65000 if DF85 == 8
replace nSalary_3 = 75000 if DF85 == 9
replace nSalary_3 = 85000 if DF85 == 10
replace nSalary_3 = 90000 * 1.4 if DF85 == 11  
replace nSalary_3 = . if DF85 == -99 | DF85 == -98 | DF85 == -96 | DF85 == .
merge 1:1 nSUBID using "${project_directory}\data\phdcn\v01_phdcn_master.dta", keepusing(nPCYearInterview_3) 
keep if _merge==3
replace nSalary_3 = nSalary_3 * (220.0/252.9) if nPCYearInterview_3 == 2000 
replace nSalary_3 = nSalary_3 * (220.0/260.1) if nPCYearInterview_3 == 2001
replace nSalary_3 = nSalary_3 * (220.0/264.2) if nPCYearInterview_3 == 2002
drop _merge nPCYearInterview_3

keep n*

save "${project_directory}\data\_TEMP\DemoFileWave3Cohort12.dta", replace 

//APPENDING AND MERGING
use "${project_directory}\data\_TEMP\DemoFileWave1Cohort0.dta", clear
append using  "${project_directory}\data\_TEMP\DemoFileWave1Cohort3.dta" ///
"${project_directory}\data\_TEMP\DemoFileWave1Cohort6.dta" ///
"${project_directory}\data\_TEMP\DemoFileWave1Cohort9.dta" ///
"${project_directory}\data\_TEMP\DemoFileWave1Cohort12.dta"
save "${project_directory}\data\_TEMP\DemoFileWave1.dta", replace

use "${project_directory}\data\_TEMP\DemoFileWave2Cohort0.dta", clear
append using  "${project_directory}\data\_TEMP\DemoFileWave2Cohort3.dta" ///
"${project_directory}\data\_TEMP\DemoFileWave2Cohort6.dta" ///
"${project_directory}\data\_TEMP\DemoFileWave2Cohort9.dta" ///
"${project_directory}\data\_TEMP\DemoFileWave2Cohort12.dta"
save "${project_directory}\data\_TEMP\DemoFileWave2.dta", replace

use "${project_directory}\data\_TEMP\DemoFileWave3Cohort0.dta", clear
append using "${project_directory}\data\_TEMP\DemoFileWave3Cohort3.dta" ///
"${project_directory}\data\_TEMP\DemoFileWave3Cohort6.dta" ///
"${project_directory}\data\_TEMP\DemoFileWave3Cohort9.dta" ///
"${project_directory}\data\_TEMP\DemoFileWave3Cohort12.dta"
save "${project_directory}\data\_TEMP\DemoFileWave3.dta", replace

use "${project_directory}\data\_TEMP\DemoFileWave1.dta", clear
merge 1:1 nSUBID using "${project_directory}\data\_TEMP\DemoFileWave2.dta"
drop _merge
save "${project_directory}\data\_TEMP\DemoFileWave1And2.dta", replace

use "${project_directory}\data\_TEMP\DemoFileWave1And2.dta", clear
merge 1:1 nSUBID using "${project_directory}\data\_TEMP\DemoFileWave3.dta"
drop _merge

save "${project_directory}\data\phdcn\v01_phdcn_demo.dta", replace 

erase "${project_directory}\data\_TEMP\DemoFileWave1Cohort0.dta"
erase "${project_directory}\data\_TEMP\DemoFileWave1Cohort3.dta"
erase "${project_directory}\data\_TEMP\DemoFileWave1Cohort6.dta"
erase "${project_directory}\data\_TEMP\DemoFileWave1Cohort9.dta"
erase "${project_directory}\data\_TEMP\DemoFileWave1Cohort12.dta"
erase "${project_directory}\data\_TEMP\DemoFileWave1.dta"
erase "${project_directory}\data\_TEMP\DemoFileWave2Cohort0.dta"
erase "${project_directory}\data\_TEMP\DemoFileWave2Cohort3.dta"
erase "${project_directory}\data\_TEMP\DemoFileWave2Cohort6.dta"
erase "${project_directory}\data\_TEMP\DemoFileWave2Cohort9.dta"
erase "${project_directory}\data\_TEMP\DemoFileWave2Cohort12.dta"
erase "${project_directory}\data\_TEMP\DemoFileWave2.dta"
erase "${project_directory}\data\_TEMP\DemoFileWave3Cohort0.dta"
erase "${project_directory}\data\_TEMP\DemoFileWave3Cohort3.dta"
erase "${project_directory}\data\_TEMP\DemoFileWave3Cohort6.dta"
erase "${project_directory}\data\_TEMP\DemoFileWave3Cohort9.dta"
erase "${project_directory}\data\_TEMP\DemoFileWave3Cohort12.dta"
erase "${project_directory}\data\_TEMP\DemoFileWave3.dta"
erase "${project_directory}\data\_TEMP\DemoFileWave1And2.dta"

log close
