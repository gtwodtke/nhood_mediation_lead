capture log close
capture clear all
set more off

global project_directory "D:\projects\nhood_mediation_lead" 
*global project_directory "C:\Users\Geoff.Wodtke\Desktop\projects\nhood_mediation_lead"
*global project_directory "C:\nhood_mediation_lead"

log using "${project_directory}\programs\_LOGS\12_create_figure_2.log", replace

/****************************************
DO-FILE NAME: 12_create_figure_2.do
AUTHOR: GW
DATE UPDATED: 08/05/2020
PURPOSE: create maps of nh dadvg/lead
NOTES:
*****************************************/

///Figure 1: Spatial Distribution of Dadvg and Lead Exposure in Chicago, 1997
*sysdir set PLUS "C:\Program Files (x86)\Stata13\ado\plus"

capture erase "${project_directory}\data\_TEMP\ILcoord.dta"
capture erase "${project_directory}\data\_TEMP\ILdb.dta"

shp2dta using "${project_directory}\data\shpfiles\tr17_d00.shp", ///
	database("${project_directory}\data\_TEMP\ILdb.dta") ///
	coordinates("${project_directory}\data\_TEMP\ILcoord.dta") genid(id)
	
import excel using "${project_directory}\data\shpfiles\chicago2000.xlsx", firstrow
gen ntract = 0
replace ntract = real(substr((census_tra),1,4))
keep ntract

save "${project_directory}\data\_TEMP\chicago2000tracts.dta", replace

//Panel A: Nhood SES
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

drop if ucounty ~= 17031
format %13.0g geo2000

tostring geo2000, replace format(%13.0g)
gen ntract = 0
replace ntract = real(substr((geo2000),7,3)) if real(geo2000) <=17031090300 
replace ntract = real(substr((geo2000),6,4)) if real(geo2000) >=17031100100
label variable ntract " 2000 FIPS TRACT ID "

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
rename educ169 neducollegen1990
rename educ160 neducollegen2000
gen n16plus1990 = mnoprt9d + fnoprt9d 
gen n16plus2000 = mnoprt0d + fnoprt0d 
rename occ19 noccprofn1990
rename occ10 noccprofn2000
rename occ29 noccmanagern1990
rename occ20 noccmanagern2000
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

keep n*
drop natborn7-natborn9 natborn0

collapse (sum) ///
	nfemaleheadn* nfemaleheadd* nunemploymentn* nunemploymentd* npovertyn* ///
	npovertyd* nwelfaren* nwelfared* noccmanagern* noccprofn* nedud* ///
	neducollegen* nedulessthanHSn* n16plus* nrace* ///
	, by (ntract)

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
	nfemaleheadn nfemaleheadd nunemploymentn nunemploymentd npovertyn ///
	npovertyd nwelfaren nwelfared noccmanagern noccprofn nedud neducollegen ///
	nedulessthanHSn n16plus nraced nracewhiten nraceblackn nracehispanicn { 
		interpolate `x' 
		} 

reshape long ///
	nfemaleheadn nfemaleheadd nunemploymentn nunemploymentd npovertyn ///
	npovertyd nwelfaren nwelfared noccmanagern noccprofn nedud neducollegen ///
	nedulessthanHSn n16plus nraced nracewhiten nraceblackn nracehispanicn, i(ntract) j(nyear)

gen nfemaleheadp = nfemaleheadn / nfemaleheadd
gen npovertyp = npovertyn / npovertyd
gen nunemploymentp = nunemploymentn / nunemploymentd
gen nwelfarep = nwelfaren / nwelfared
gen nedulessthanHSp = nedulessthanHSn / nedud
gen neducollegep = neducollegen / nedud
gen noccprofp = noccprofn / n16plus
gen noccmanagerp = noccmanagern / n16plus
gen nraceblackp = nraceblackn / nraced
gen nracehispanicp = nracehispanicn / nraced
gen nracenonwhtp = 1 - (nracewhiten / nraced)

pca npovertyp nedulessthanHSp nfemaleheadp nracenonwhtp
predict pc1
egen ncondadvg=std(pc1)
drop pc1

drop if ntract == 0
keep if nyear == 1997 
keep ntract nyear npovertyp ncondadvg

save "${project_directory}\data\_TEMP\ncdb_temp.dta", replace

use "${project_directory}\data\_TEMP\ILdb.dta", clear

foreach x in STATE COUNTY TRACT NAME LSAD_TRANS {
	destring `x', replace 
	}
	
keep if CO == 31 //cook county 
sort TRACT

gen ntract = round(NAME, 1)	

merge m:1 ntract using "${project_directory}\data\_TEMP\chicago2000tracts.dta"
keep if _merge == 3
drop _merge

merge m:1 ntract using "${project_directory}\data\_TEMP\ncdb_temp.dta"
keep if _merge == 3
drop _merge

sort id
replace ncondadvg=(ncondadvg[_n-1]+ncondadvg[_n+1])/2 if ncondadvg==.
replace ncondadvg=ncondadvg[_n-1] if ncondadvg==.
replace ncondadvg=ncondadvg[_n+1] if ncondadvg==.
replace ncondadvg = round(ncondadvg, 0.01)

save "${project_directory}\data\_TEMP\ncdb_temp.dta", replace

spmap ncondadvg using "${project_directory}\data\_TEMP\ILcoord.dta", ///
	id(id) fcolor(Greys2) clm(q) cln(10) ndf(magenta) ///
	title("Concentrated Disadvantage", size(medium)) ///
	legtitle("Deciles")

graph save "${project_directory}\figures\_TEMP\fig2a.gph", replace

//Panel B: Elevated blood Pb prevalence
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

sort nyear ntract
collapse (sum) nobs nbll nbllgt5, by(nyear ntract) 

gen navgbll=nbll/nobs
gen nprbllgt5=nbllgt5/nobs

keep if inrange(ntract,101,8105)

sort ntract nyear
by ntract: gen nsmobs=nobs[_n-1]+nobs+nobs[_n+1]

gen nprbllgt5sm = . 
forval x = 101/8105 { 
	capture lpoly nprbllgt5 nyear [aw = nobs] if ntract == `x', degree(0) k(gaussian) bw(3) gen(pointest) at(nyear) nograph
	capture replace nprbllgt5sm = pointest if ntract == `x' 
	capture drop pointest
	} 

keep if nyear == 1997

save "${project_directory}\data\_TEMP\bldb_temp.dta", replace

use "${project_directory}\data\_TEMP\ILdb.dta", clear

foreach x in STATE COUNTY TRACT NAME LSAD_TRANS {
	destring `x', replace 
	}
	
keep if CO == 31 
sort TRACT

gen ntract = round(NAME, 1)	

merge m:1 ntract using "${project_directory}\data\_TEMP\chicago2000tracts.dta"
keep if _merge == 3
drop _merge

merge m:1 ntract using "${project_directory}\data\_TEMP\bldb_temp.dta"

replace nprbllgt5sm=. if nsmobs<10

save "${project_directory}\data\_TEMP\bldb_temp.dta", replace

sort id
replace nprbllgt5sm=(nprbllgt5sm[_n-1]+nprbllgt5sm[_n+1])/2 if nprbllgt5sm==.
replace nprbllgt5sm=nprbllgt5sm[_n-1] if nprbllgt5sm==.
replace nprbllgt5sm=nprbllgt5sm[_n+1] if nprbllgt5sm==.
replace nprbllgt5sm = round(nprbllgt5sm, 0.01)

spmap nprbllgt5sm using "${project_directory}\data\_TEMP\ILcoord.dta", ///
	id(id) fcolor(Greys2) clm(q) cln(10) ndf(magenta) /// 
	title("Elevated Blood Lead Prevalence", size(medium)) ///
	legtitle("Deciles")
	
graph save "${project_directory}\figures\_TEMP\fig2b.gph", replace

//Combine Panels A and B
graph combine ///
	"${project_directory}\figures\_TEMP\fig2a.gph" ///
	"${project_directory}\figures\_TEMP\fig2b.gph", ///
	col(2) row(1) ysize(5) xsize(9) scheme(s2mono)

graph export "${project_directory}\figures\figure_2.jpg", as(jpg) replace
*graph export "${project_directory}\figures\figure_2.emf", as(emf) replace
*graph export "${project_directory}\figures\_figure_2.pdf", as(pdf) replace

erase "${project_directory}\data\_TEMP\chicago2000tracts.dta"
erase "${project_directory}\data\_TEMP\ILcoord.dta"
erase "${project_directory}\data\_TEMP\ILdb.dta"
erase "${project_directory}\data\_TEMP\temp_ncdb_1.dta"
erase "${project_directory}\data\_TEMP\temp_ncdb_2.dta"
*erase "${project_directory}\data\_TEMP\ncdb_temp.dta"
*erase "${project_directory}\data\_TEMP\bldb_temp.dta"
erase "${project_directory}\figures\_TEMP\fig2a.gph"
erase "${project_directory}\figures\_TEMP\fig2b.gph"

log close
