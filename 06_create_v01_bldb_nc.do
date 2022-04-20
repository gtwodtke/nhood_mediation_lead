capture log close
capture clear all
set more off

global project_directory "D:\projects\nhood_mediation_lead" 
*global project_directory "C:\Users\Geoff.Wodtke\Desktop\projects\nhood_mediation_lead"
*global project_directory "C:\nhood_mediation_lead"
 
log using "${project_directory}\programs\_LOGS\06_create_v01_bldb_nc.log", replace

/********************************************
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DO-FILE NAME: 06_create_v01_bldb_nc.do
AUTHOR: Sagi Ramaj
DATE UPDATED: 08/04/2019 (GW)
PURPOSE: 
	inputting the bll data
	aggregating up to census tract
	merging with NC
	aggregating up to NC
	smoothing the prevalence estimates
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*********************************************/

use "${project_directory}\data\phdcn\v01_phdcn_tractlink.dta", clear
rename tract ntract
rename link_nc nlinknc
save "${project_directory}\data\_TEMP\tractlinknc.dta", replace 
clear 

import delimited using "D:\projects\nhood_mediation_lead\data\cdph_bldb\bldb\mh4.csv", delimiters(",") 

keep if inrange(yrtest,1995,2013) & status1=="VALID" 
count if id!=. 

drop if full_addr=="810 W MONTROSE AVE" 
drop if street_name=="DR MARTIN LUTHER KING JR" & house_low=="5110"
drop if inlist(lab,"C16","C18","016","018","16","18","026","058","116") 
drop if inlist(lab,"043","040","014")
keep if sample_type=="V" 

keep id namedob cb2000num yrtest yr bll 

rename id ntestid
rename namedob nchldid
rename cb2000num ncbgroupnum
rename yrtest nyear
rename yr byear
rename bll nbll

gen ntestage = nyear-byear
keep if inrange(ntestage,0,5)

tostring ncbgroupnum, replace
gen ntract = 0
replace ntract = real(substr((ncbgroupnum),1,3)) if real(ncbgroupnum) <=903001026 
replace ntract = real(substr((ncbgroupnum),1,4)) if real(ncbgroupnum) >903001026 

replace nbll=. if inrange(nbll,-99,-1)
replace nbll=100 if inrange(nbll,100,999) 
count if nbll!=.

bysort nchldid nyear ncbgroupnum: gen testCount=_n
bysort nchldid nyear ncbgroupnum: egen testTotal=max(testCount)
bysort nchldid nyear ncbgroupnum: egen meanBll=mean(nbll) if testTotal>1
keep if testCount==1
replace nbll=round(meanBll) if testTotal>1
drop testCount testTotal meanBll

gen nobs=0 
replace nobs=1 if inrange(nbll,0,100)

gen nbllgt5=0 if inrange(nbll,0,4)
replace nbllgt5=1 if inrange(nbll,5,100)

merge m:1 ntract using "${project_directory}\data\_TEMP\tractlinknc.dta"
keep if _merge == 3
drop _merge

sort nlinknc nyear
collapse (sum) nobs nbll nbllgt5, by(nlinknc nyear) 

gen navgbll=nbll/nobs
gen nprbllgt5=nbllgt5/nobs

gen navgbllsm = . 
forval x =  501/865 { 
	capture lpoly navgbll nyear [aw = nobs] if nlinknc == `x', degree(0) k(gaussian) bw(3) gen(estsm) at(nyear) nograph
	capture replace navgbllsm=estsm if nlinknc == `x' 
	capture drop estsm
	} 
sum navgbllsm, detail

gen nprbllgt5sm = . 
forval x =  501/865 { 
	capture lpoly nprbllgt5 nyear [aw = nobs] if nlinknc == `x', degree(0) k(gaussian) bw(3) gen(estsm) at(nyear) nograph
	capture replace nprbllgt5sm=estsm if nlinknc == `x' 
	capture drop estsm
	} 
sum nprbllgt5sm, detail

keep nyear nlinknc nobs navgbllsm nprbllgt5sm

saveold "${project_directory}\data\cdph_bldb\v01_bldb_nc.dta", replace v(12)

erase "${project_directory}\data\_TEMP\tractlinknc.dta"

//Descriptives
sum navgbllsm nprbllgt5sm if inrange(nyear,1995,2002), detail
centile navgbllsm nprbllgt5sm if inrange(nyear,1995,2002), c(10 25 50 75 90)
centile nobs if inrange(nyear,1995,2002), c(10 25 50 75 90)
corr navgbllsm nprbllgt5sm

log close
