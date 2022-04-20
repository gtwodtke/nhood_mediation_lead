capture log close
capture clear all
set more off

global project_directory "D:\projects\nhood_mediation_lead" 
*global project_directory "C:\Users\Geoff.Wodtke\Desktop\projects\nhood_mediation_lead"
*global project_directory "C:\nhood_mediation_lead"

log using "${project_directory}\programs\_LOGS\07_create_v01_ncdb_nc.log", replace

/********************************************
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DO-FILE NAME: 07_create_v01_ncdb_nc.do
AUTHOR: Sagi Ramaj
DATE UPDATED: 08/04/2020 (GW)
PURPOSE: 
	cleaning NCDB
	merge with NC
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*********************************************/

//NCDB 2000
insheet using "${project_directory}\data\ncdb2000\2009_03_20_NCDB_RAW_1.csv", clear  
sort geo2000 
save "${project_directory}\data\_TEMP\temp_ncdb_1.dta", replace 

insheet using "${project_directory}\data\ncdb2000\2009_03_20_NCDB_RAW_2.csv", clear  
sort geo2000 
save "${project_directory}\data\_TEMP\temp_ncdb_2.dta", replace 

use "${project_directory}\data\_TEMP\temp_ncdb_1.dta", clear 
merge 1:1 geo2000 using "${project_directory}\data\_TEMP\temp_ncdb_2.dta"
drop _merge 
sort geo2000 

save "${project_directory}\data\_TEMP\temp_ncdb.dta", replace 

//Chicago 
use "${project_directory}\data\_TEMP\temp_ncdb.dta", clear

drop if ucounty ~= 17031
format %13.0g geo2000
tostring geo2000, replace format(%13.0g)
gen ntract = 0
replace ntract = real(substr((geo2000),7,3)) if real(geo2000) <=17031090300 
replace ntract = real(substr((geo2000),6,4)) if real(geo2000) >=17031100100
label variable ntract " 2000 FIPS TRACT ID "

drop if ntract >7609

rename geo2000 ngeo2000
rename trctpop9 ntractpop1990
rename trctpop0 ntractpop2000
rename ffh9n nfemaleheadn1990
rename ffh9d nfemaleheadd1990
rename ffh0n nfemaleheadn2000
rename ffh0d nfemaleheadd2000
rename povrat9n npovertyn1990
rename povrat9d npovertyd1990
rename povrat0n npovertyn2000
rename povrat0d npovertyd2000
rename unempt9n nunemploymentn1990
rename unempt9d nunemploymentd1990 
rename unempt0n nunemploymentn2000
rename unempt0d nunemploymentd2000
rename welfar9n nwelfaren1990
rename welfar9d nwelfared1990
rename welfar0n nwelfaren2000
rename welfar0d nwelfared2000
rename educpp9 nedud1990
rename educpp0 nedud2000

gen nedulessthanHSn1990 = educ119 + educ89
gen nedulessthanHSn2000 = educ110 + educ80

rename educ129 neduHSn1990
rename educ120 neduHSn2000

gen nedusomecollegen1990 = educ159 + educa9
gen nedusomecollegen2000 = educ150 + educa0

rename educ169 neducollegen1990
rename educ160 neducollegen2000

rename shr9d nraced1990
rename shr0d nraced2000
rename shrblk9n nraceblackn1990
rename shrblk0n nraceblackn2000
rename shrwht9n nracewhiten1990
rename shrwht0n nracewhiten2000
rename shrhsp9n nracehispanicn1990
rename shrhsp0n nracehispanicn2000
rename forborn9 nforeignbornn1990  
rename forborn0 nforeignbornn2000

gen n16plus1990 = mnoprt9d + fnoprt9d
gen n16plus2000 = mnoprt0d + fnoprt0d 

rename occ19 noccprofn1990
rename occ10 noccprofn2000
rename occ29 noccmanagern1990
rename occ20 noccmanagern2000

keep n*
drop natborn7-natborn9 natborn0

save "${project_directory}\data\_TEMP\temp_ncdb_CHI.dta", replace 

//merge with NC ids and aggregate up to NC
use "${project_directory}\data\phdcn\v01_phdcn_tractlink.dta", clear
rename tract ntract
rename link_nc nlinknc
save "${project_directory}\data\_TEMP\tractlinknc.dta", replace 

//merge with NC
use "${project_directory}\data\_TEMP\temp_ncdb_CHI.dta", clear
merge m:1 ntract using "${project_directory}\data\_TEMP\tractlinknc.dta"
keep if _merge == 3
drop _merge 

//aggregate up to NC
sort nlinknc
rename nlinknc linknc
destring ngeo2000, replace
collapse (sum) n* , by (linknc)
rename linknc nlinknc
drop ngeo2000

// linear interpolate
capture program drop interpolate 
program define interpolate 
	local var `1' 
	local j=1990 + 1 
	local x=.9 
	local y=.1 
	while `j' <= 1990 + 9 { 
		gen `var'`j' = `x'*`var'1990  + `y'*`var'2000 
		local j = `j' + 1 
		local x = `x' - .1 
		local y = `y' + .1 
		} 
end	

foreach x in ///
	ntractpop nraced nracewhiten nraceblackn nracehispanicn nfemaleheadn nfemaleheadd ///
	nunemploymentn nunemploymentd npovertyn npovertyd nwelfaren nwelfared noccmanagern ///
	noccprofn nedud neducollegen neduHSn nforeignbornn nedulessthanHSn nedusomecollegen n16plus { 
		interpolate `x' 
		} 

//reshape
capture macro drop stubs
global stubs ///
	ntractpop nraced nracewhiten nraceblackn nracehispanicn nfemaleheadn nfemaleheadd ///
	nunemploymentn nunemploymentd npovertyn npovertyd nwelfaren nwelfared noccmanagern ///
	noccprofn nedud neducollegen neduHSn nforeignbornn nedulessthanHSn nedusomecollegen n16plus

reshape long $stubs, i(nlinknc) j(nyear)

gen nfemaleheadp = nfemaleheadn / nfemaleheadd
gen npovertyp = npovertyn / npovertyd
gen nunemploymentp = nunemploymentn / nunemploymentd
gen nwelfarep = nwelfaren / nwelfared

gen nedulessthanHSp = nedulessthanHSn / nedud
gen neduHSp = neduHSn / nedud
gen nedusomecollegep = nedusomecollegen / nedud
gen neducollegep = neducollegen / nedud

gen nraceblackp = nraceblackn / nraced
gen nracehispanicp = nracehispanicn / nraced
gen nracenonwhtp = 1 - (nracewhiten / nraced)
gen nforeignbornp = nforeignbornn / nraced

gen noccprofp = noccprofn / n16plus
gen noccmanagerp = noccmanagern / n16plus

//PCA 
pca npovertyp nedulessthanHSp nfemaleheadp nracenonwhtp
predict pc1
egen ncondadvg=std(pc1)
drop pc1

keep nlinknc nyear npovertyp nedulessthanHSp nfemaleheadp ///
		nracenonwhtp nraceblackp nracehispanicp ncondadvg

centile npovertyp ncondadvg, c(20 25 50 75 80)

save "${project_directory}\data\ncdb2000\v01_ncdb_nc", replace

erase "${project_directory}\data\_TEMP\temp_ncdb_1.dta"
erase "${project_directory}\data\_TEMP\temp_ncdb_2.dta"
erase "${project_directory}\data\_TEMP\temp_ncdb.dta"
erase "${project_directory}\data\_TEMP\temp_ncdb_CHI.dta"
erase "${project_directory}\data\_TEMP\tractlinknc.dta"

log close

