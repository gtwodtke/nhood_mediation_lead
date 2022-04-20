capture log close
capture clear all
set more off

global project_directory "D:\projects\nhood_mediation_lead" 

log using "${project_directory}\programs\appendix_c\_LOGS\01_create_v03_phdcn_6to12_merged_mi.log", replace

/***************************************************
DO-FILE NAME: 01_create_v03_phdcn_6to12_merged_mi.do
AUTHOR: GW
DATE UPDATED: 05/25/2021 (GW)
PURPOSE: multiple imputation of missing data
NOTES:
****************************************************/

//Cohorts 6 to 12
use "${project_directory}\data\v01_phdcn_merged.dta", clear
keep if inrange(nCohort,6,12)
tab nCohort, missing 

/**********************
TESTING TESTING TESTING
***********************/
keep ///
	nSUBID nCohort nstrata ///
	nFamSize_1 nPCHomeowner_1 nPCAge_1 nPCEdu_1 nSUBEthnicity_1 nSUBFemale_1 ///
	nlinknc_1 nPCPublicAssist_1 nPCEmployed_1 nPCEnglish_1 nPCMarried_1 nlnSalary_1 ///
	nlinknc_2 nPCPublicAssist_2 nPCEmployed_2 nPCEnglish_2 nPCMarried_2 nlnSalary_2 ///
	nlinknc_3 nPCPublicAssist_3 nPCEmployed_3 nPCEnglish_3 nPCMarried_3 nlnSalary_3 ///
	ncondadvg_1 ncondadvg_2 ncondadvg_3 nBL5_1 nBL5_2 nBL5_3 ///
	nSUBAge_3 nWISCScaled_1 nWISCScaled_2 nWISCScaled_3 
		
mi set mlong 
    
mi register imputed ///
	nFamSize_1 nPCHomeowner_1 nPCAge_1 nPCEdu_1 nSUBEthnicity_1  ///
	nPCPublicAssist_1 nPCEnglish_1 nPCMarried_1 nPCEmployed_1 nlnSalary_1 ///
	nPCPublicAssist_2 nPCEnglish_2 nPCMarried_2 nPCEmployed_2 nlnSalary_2 ///
	nPCPublicAssist_3 nPCEnglish_3 nPCMarried_3 nPCEmployed_3 nlnSalary_3 ///
	ncondadvg_2 ncondadvg_3 nBL5_2 nBL5_3 ///
	nSUBAge_3 nWISCScaled_1 nWISCScaled_2 nWISCScaled_3 
	
mi register regular nSUBFemale_1 ncondadvg_1 nBL5_1

mi describe

mi impute chain ///
	(mlogit, include(ncondadvg_1 nBL5_1) ///
			 omit(i.nPCEnglish_2 i.nPCPublicAssist_2 i.nPCMarried_2 i.nPCEmployed_2 nlnSalary_2 nBL5_2 ///
				  i.nPCEnglish_3 i.nPCPublicAssist_3 i.nPCMarried_3 i.nPCEmployed_3 nlnSalary_3 nBL5_3 ///
				  ncondadvg_2 ncondadvg_3 /// 
				  nSUBAge_3 nWISCScaled_1 nWISCScaled_2 nWISCScaled_3)) ///
		nSUBEthnicity_1 ///
	(ologit, include(nSUBFemale_1 ncondadvg_1 nBL5_1)) ///
		nPCEdu_1	///
	(logit, include(nSUBFemale_1 ncondadvg_1 nBL5_1)) ///
		nPCHomeowner_1 ///
		nPCEnglish_1 nPCMarried_1 nPCPublicAssist_1 nPCEmployed_1  ///
		nPCEnglish_2 nPCMarried_2 nPCPublicAssist_2 nPCEmployed_2 ///
		nPCEnglish_3 nPCMarried_3 nPCPublicAssist_3 nPCEmployed_3  ///
	(pmm, include(nSUBFemale_1 ncondadvg_1 nBL5_1) knn(10)) ///
		nPCAge_1 nFamSize_1 ///
		nlnSalary_1 nlnSalary_2 nlnSalary_3 ///
		ncondadvg_2 ncondadvg_3 ///
		nBL5_2 nBL5_3 ///
		nSUBAge_3 nWISCScaled_1 nWISCScaled_2 nWISCScaled_3 /// 
	, add(50) rseed(54645) augment dots

mi export ice, clear
save "${project_directory}\data\_TEMP\MICohort6to12.dta", replace

//Renaming, recoding, and labeling
use "${project_directory}\data\_TEMP\MICohort6to12.dta", clear

rename nSUBID nsubid
rename nCohort ncohort
rename nPCHomeowner_1 nownhome_b
rename nFamSize_1 nfamsize_b
rename nPCAge_1 npcage_b
rename nSUBFemale_1 nfemale_b
rename nPCEdu_1 npceduc_b
rename nSUBEthnicity_1 nrace_b
rename nSUBAge_3 nsubage_3

forval x=1/3 {
	rename nPCPublicAssist_`x' npcwelf_`x'
	rename nPCEmployed_`x' npcemply_`x'
	rename nBL5_`x' nbl5ug_`x'
	rename nlnSalary_`x' nlninc_`x'
	rename nPCMarried_`x' npcmarr_`x'
	rename nPCEnglish_`x' npcengl_`x'
	rename nWISCScaled_`x' nwisc_`x'
	}
	
tab npceduc_b, gen(tempeduc)
rename tempeduc1 npclesshs_b
rename tempeduc2 npchsgrad_b
rename tempeduc3 npcsomcol_b
rename tempeduc4 npccolgrd_b

tab nrace_b, gen(temprace)
rename temprace1 nhispan_b
rename temprace2 nothrace_b
rename temprace3 nblack_b
rename temprace4 nwhite_b

gen miwt = 1/50 if _mj!=0 

label var nlinknc_1 "Neighborhood cluster ID - wave 1"
label var nlinknc_2 "Neighborhood cluster ID - wave 2"
label var nlinknc_3 "Neighborhood cluster ID - wave 3"
label var npcwelf_1 "Family recieves public assistance - wave 1"
label var npcwelf_2 "Family recieves public assistance - wave 2"
label var npcwelf_3 "Family recieves public assistance - wave 3"
label var npcemply_1 "PC is employed - wave 1"
label var npcemply_2 "PC is employed - wave 2"
label var npcemply_3 "PC is employed - wave 3"
label var nbl5ug_1 "Elevated blood lead (>5ug) prevalence - wave 1"
label var nbl5ug_2 "Elevated blood lead (>5ug) prevalence - wave 2"
label var nbl5ug_3 "Elevated blood lead (>5ug) prevalence - wave 3"
label var ncondadvg_1 "Nhood disadvantage - wave 1"
label var ncondadvg_2 "Nhood disadvantage - wave 2"
label var ncondadvg_3 "Nhood disadvantage - wave 3"
label var npcmarr_1 "PC is married - wave 1"
label var npcmarr_2 "PC is married - wave 2"
label var npcmarr_3 "PC is married - wave 3"
label var npcengl_1 "English is PCs primary language - wave 1"
label var npcengl_2 "English is PCs primary language - wave 2"
label var npcengl_3 "English is PCs primary language - wave 3"
label var nownhome_b "Family owns home at baseline"
label var nfamsize_b "Family size at baseline"
label var npcage_b "PC age at baseline"
label var nfemale_b "Subject is female"
label var npceduc_b "PC education level at baseline"
label var nrace_b "Subject race/ethnicity"
label var nsubage_3 "Subject age - wave 3"
label var nwisc_1 "WISC scores - wave 1"
label var nwisc_2 "WISC scores - wave 2"
label var nwisc_3 "WISC scores - wave 3"

label values npcwelf_1 npcwelf_2 npcwelf_3 .
label values npcemply_1 npcemply_2 npcemply_3 .
label values npcmarr_1 npcmarr_2 npcmarr_3 .
label values npcengl_1 npcengl_2 npcengl_3 .

saveold "${project_directory}\data\v03_phdcn_merged_mi_6to12.dta", replace version(12)

erase "${project_directory}\data\_TEMP\MICohort6to12.dta"

capture clear all

log close

