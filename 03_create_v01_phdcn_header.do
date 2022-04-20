capture log close
capture clear all
set more off

global project_directory "D:\projects\nhood_mediation_lead" 
*global project_directory "C:\Users\Geoff.Wodtke\Desktop\projects\nhood_mediation_lead"
*global project_directory "C:\nhood_mediation_lead"

log using "${project_directory}\programs\_LOGS\03_create_v01_phdcn_header.log", replace

/*
DO-FILE NAME: 03_create_v01_phdcn_header.do
AUTHOR: Sagi Ramaj 
DATE UPDATED: 08/04/2020 (GW)
PURPOSE: wave 3 header data 
NOTES:
*/

//Header File Wave 3 - Cohort 0
use "${project_directory}\data\phdcn\14802059\ICPSR_13712\DS0001\13712-0001-Data-REST.dta", clear

rename SUBID nSUBID
rename COHORT nCohort
rename LANGPC3 nPCLanguage_3

keep n*

save "${project_directory}\data\_TEMP\HeaderFileWave3Cohort0.dta", replace

//Header File Wave 3 - Cohort 3
use "${project_directory}\data\phdcn\14802059\ICPSR_13712\DS0002\13712-0002-Data-REST.dta", clear

rename SUBID nSUBID
rename COHORT nCohort
rename LANGPC3 nPCLanguage_3

keep n*

save "${project_directory}\data\_TEMP\HeaderFileWave3Cohort3.dta", replace

//Header File Wave 3 - Cohort 6
use "${project_directory}\data\phdcn\14802059\ICPSR_13712\DS0003\13712-0003-Data-REST.dta", clear

rename SUBID nSUBID
rename COHORT nCohort
rename LANGPC3 nPCLanguage_3

keep n*

save "${project_directory}\data\_TEMP\HeaderFileWave3Cohort6.dta", replace

//Header File Wave 3 - Cohort 9
use "${project_directory}\data\phdcn\14802059\ICPSR_13712\DS0004\13712-0004-Data-REST.dta", clear

rename SUBID nSUBID
rename COHORT nCohort
rename LANGPC3 nPCLanguage_3

keep n*

save "${project_directory}\data\_TEMP\HeaderFileWave3Cohort9.dta", replace

//Header File Wave 3 - Cohort 12
use "${project_directory}\data\phdcn\14802059\ICPSR_13712\DS0005\13712-0005-Data-REST.dta", clear

rename SUBID nSUBID
rename COHORT nCohort
rename LANGPC3 nPCLanguage_3

keep n*

save "${project_directory}\data\_TEMP\HeaderFileWave3Cohort12.dta", replace

//APPENDING
use "${project_directory}\data\_TEMP\HeaderFileWave3Cohort0.dta", clear
append using "${project_directory}\data\_TEMP\HeaderFileWave3Cohort3.dta" ///
"${project_directory}\data\_TEMP\HeaderFileWave3Cohort6.dta" ///
"${project_directory}\data\_TEMP\HeaderFileWave3Cohort9.dta" ///
"${project_directory}\data\_TEMP\HeaderFileWave3Cohort12.dta"

save "${project_directory}\data\phdcn\v01_phdcn_header.dta", replace

erase "${project_directory}\data\_TEMP\HeaderFileWave3Cohort0.dta"
erase "${project_directory}\data\_TEMP\HeaderFileWave3Cohort3.dta"
erase "${project_directory}\data\_TEMP\HeaderFileWave3Cohort6.dta"
erase "${project_directory}\data\_TEMP\HeaderFileWave3Cohort9.dta"
erase "${project_directory}\data\_TEMP\HeaderFileWave3Cohort12.dta"

log close
