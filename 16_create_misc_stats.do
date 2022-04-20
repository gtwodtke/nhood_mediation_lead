capture log close
capture clear all
set more off

global project_directory "D:\projects\nhood_mediation_lead" 

log using "${project_directory}\programs\_LOGS\16_create_misc_stats.log", replace

/******************************************
DO-FILE NAME: 16_create_misc_stats.do
AUTHOR: GW
DATE UPDATED: 11/08/2021
PURPOSE: create misc stats reported in text
NOTES:
*******************************************/

use "${project_directory}\data\v02_phdcn_merged_mi.dta", clear
keep if _mj!=0

///Intertemporal correlations
corr ncondadvg_1 ncondadvg_2 ncondadvg_3
corr nbl5ug_1 nbl5ug_2 nbl5ug_3

//Variance inflation factors
global bvars ///
	npcage_b npchsgrad_b npcsomcol_b npccolgrd_b nownhome_b ///
	nfemale_b nblack_b nhispan_b nothrace_b nfamsize_b

global t1vars nlninc_1 npcemply_1 npcwelf_1 npcmarr_1 npcengl_1 ncondadvg_1 nbl5ug_1
global t2vars nlninc_2 npcemply_2 npcwelf_2 npcmarr_2 npcengl_2 ncondadvg_2 nbl5ug_2
global t3vars nlninc_3 npcemply_3 npcwelf_3 npcmarr_3 npcengl_3 ncondadvg_3 nbl5ug_3

foreach x in $bvars nlninc_1 npcemply_1 npcwelf_1 npcmarr_1 npcengl_1 {
	quietly reg `x' [iw=miwt] 
	quietly predict `x'_resid, resid
	}
	
foreach x in nlninc_2 npcemply_2 npcwelf_2 npcmarr_2 npcengl_2 {
	quietly reg `x' $bvars $t1vars [iw=miwt] 
	quietly predict `x'_resid, resid
	}

foreach x in nlninc_3 npcemply_3 npcwelf_3 npcmarr_3 npcengl_3 {
	quietly reg `x' $bvars $t1vars $t2vars [iw=miwt] 
	quietly predict `x'_resid, resid
	}

quietly reg nsubage_3 $bvars $t1vars $t2vars $t3vars [iw=miwt] 
quietly predict nsubage_3_resid, resid
	
gen ncumdadvg=(ncondadvg_1+ncondadvg_2+ncondadvg_3)/3
gen ncumbll=(nbl5ug_1+nbl5ug_2+nbl5ug_3)/3
gen nlninc_resid=nlninc_1_resid+nlninc_2_resid+nlninc_3_resid
gen npcemply_resid=npcemply_1_resid+npcemply_2_resid+npcemply_3_resid
gen npcwelf_resid=npcwelf_1_resid+npcwelf_2_resid+npcwelf_3_resid
gen npcmarr_resid=npcmarr_1_resid+npcmarr_2_resid+npcmarr_3_resid
gen npcengl_resid=npcengl_1_resid+npcengl_2_resid+npcengl_3_resid

drop nlninc_1_resid nlninc_2_resid nlninc_3_resid
drop npcemply_1_resid npcemply_2_resid npcemply_3_resid
drop npcwelf_1_resid npcwelf_2_resid npcwelf_3_resid
drop npcmarr_1_resid npcmarr_2_resid npcmarr_3_resid
drop npcengl_1_resid npcengl_2_resid npcengl_3_resid

egen ystd=std(nppvt_3)
egen astd=std(ncumdadvg)
egen mstd=std(ncumbll)

quietly reg ystd astd mstd *_resid [iw=miwt], cluster(nlinknc_1)
vif  

//Regularized coefficients
reg ystd astd mstd *_resid [iw=miwt], cluster(nlinknc_1)
lasso linear ystd astd mstd *_resid [iw=miwt], selection(cv) folds(10) serule rseed(8675309) nolog
est store lasso1
elasticnet linear ystd astd mstd *_resid [iw=miwt], selection(cv) folds(10) serule rseed(8675309) nolog
est store elast1
lassocoef lasso1 elast1, display(coef, penalized)

//Cross-validated lambdas and alphas for appendices
quietly reg nbl5ug_1 $bvars $t1vars [iw=miwt] 
quietly predict nbl5ug_1_r, resid
quietly reg nbl5ug_2 $bvars $t1vars $t2vars [iw=miwt] 
quietly predict nbl5ug_2_r, resid
quietly reg nbl5ug_3 $bvars $t1vars $t2vars $t3vars [iw=miwt] 
quietly predict nbl5ug_3_r, resid
gen nbl5ug_r=nbl5ug_1_r+nbl5ug_2_r+nbl5ug_3_r

//appendix b
lasso linear ystd ///
	ncondadvg_1 ncondadvg_2 ncondadvg_3 ///
	nbl5ug_r *_resid ///
	[iw=miwt], selection(cv) folds(10) serule rseed(8675309) nolog

lasso linear ystd ///
	ncondadvg_1 ncondadvg_2 ncondadvg_3 ///
	nbl5ug_1 nbl5ug_2 nbl5ug_3 *_resid ///
	[iw=miwt], selection(cv) folds(10) serule rseed(8675309) nolog

elasticnet linear ystd ///
	ncondadvg_1 ncondadvg_2 ncondadvg_3 ///
	nbl5ug_r *_resid ///
	[iw=miwt], selection(cv) folds(10) serule rseed(8675309) nolog

elasticnet linear ystd ///
	ncondadvg_1 ncondadvg_2 ncondadvg_3 ///
	nbl5ug_1 nbl5ug_2 nbl5ug_3 *_resid ///
	[iw=miwt], selection(cv) folds(10) serule rseed(8675309) nolog

//appendix d
//wave 1
lasso linear ystd ///
	$bvars nlninc_1 npcemply_1 npcwelf_1 npcmarr_1 npcengl_1 nsubage_3_resid ///
	ncondadvg_1 ///
	[iw=miwt], selection(cv) folds(10) serule rseed(8675309) nolog

lasso linear ystd ///
	$bvars nlninc_1 npcemply_1 npcwelf_1 npcmarr_1 npcengl_1 nsubage_3_resid ///
	ncondadvg_1 nbl5ug_1 ///
	[iw=miwt], selection(cv) folds(10) serule rseed(8675309) nolog

elasticnet linear ystd ///
	$bvars nlninc_1 npcemply_1 npcwelf_1 npcmarr_1 npcengl_1 nsubage_3_resid ///
	ncondadvg_1 ///
	[iw=miwt], selection(cv) folds(10) serule rseed(8675309) nolog
	
elasticnet linear ystd ///
	$bvars nlninc_1 npcemply_1 npcwelf_1 npcmarr_1 npcengl_1 nsubage_3_resid ///
	ncondadvg_1 nbl5ug_1 ///
	[iw=miwt], selection(cv) folds(10) serule rseed(8675309) nolog

//wave 2
lasso linear ystd ///
	$bvars nlninc_1 npcemply_1 npcwelf_1 npcmarr_1 npcengl_1 ///
	nlninc_2 npcemply_2 npcwelf_2 npcmarr_2 npcengl_2 nsubage_3_resid ///
	ncondadvg_1 nbl5ug_1 ncondadvg_2 ///
	[iw=miwt], selection(cv) folds(10) serule rseed(8675309) nolog

lasso linear ystd ///
	$bvars nlninc_1 npcemply_1 npcwelf_1 npcmarr_1 npcengl_1 ///
	nlninc_2 npcemply_2 npcwelf_2 npcmarr_2 npcengl_2 nsubage_3_resid ///
	ncondadvg_1 nbl5ug_1 ncondadvg_2 nbl5ug_2 ///
	[iw=miwt], selection(cv) folds(10) serule rseed(8675309) nolog

elasticnet linear ystd ///
	$bvars nlninc_1 npcemply_1 npcwelf_1 npcmarr_1 npcengl_1 ///
	nlninc_2 npcemply_2 npcwelf_2 npcmarr_2 npcengl_2 nsubage_3_resid ///
	ncondadvg_1 nbl5ug_1 ncondadvg_2 ///
	[iw=miwt], selection(cv) folds(10) serule rseed(8675309) nolog

elasticnet linear ystd ///
	$bvars nlninc_1 npcemply_1 npcwelf_1 npcmarr_1 npcengl_1 ///
	nlninc_2 npcemply_2 npcwelf_2 npcmarr_2 npcengl_2 nsubage_3_resid ///
	ncondadvg_1 nbl5ug_1 ncondadvg_2 nbl5ug_2 ///
	[iw=miwt], selection(cv) folds(10) serule rseed(8675309) nolog

//wave 3
lasso linear ystd ///
	$bvars nlninc_1 npcemply_1 npcwelf_1 npcmarr_1 npcengl_1 ///
	nlninc_2 npcemply_2 npcwelf_2 npcmarr_2 npcengl_2 ///
	nlninc_3 npcemply_3 npcwelf_3 npcmarr_3 npcengl_3 nsubage_3_resid ///
	ncondadvg_1 nbl5ug_1 ncondadvg_2 nbl5ug_2 ncondadvg_3 ///
	[iw=miwt], selection(cv) folds(10) serule rseed(8675309) nolog

lasso linear ystd ///
	$bvars nlninc_1 npcemply_1 npcwelf_1 npcmarr_1 npcengl_1 ///
	nlninc_2 npcemply_2 npcwelf_2 npcmarr_2 npcengl_2 ///
	nlninc_3 npcemply_3 npcwelf_3 npcmarr_3 npcengl_3 nsubage_3_resid ///
	ncondadvg_1 nbl5ug_1 ncondadvg_2 nbl5ug_2 ncondadvg_3 nbl5ug_3 ///
	[iw=miwt], selection(cv) folds(10) serule rseed(8675309) nolog

elasticnet linear ystd ///
	$bvars nlninc_1 npcemply_1 npcwelf_1 npcmarr_1 npcengl_1 ///
	nlninc_2 npcemply_2 npcwelf_2 npcmarr_2 npcengl_2 ///
	nlninc_3 npcemply_3 npcwelf_3 npcmarr_3 npcengl_3 nsubage_3_resid ///
	ncondadvg_1 nbl5ug_1 ncondadvg_2 nbl5ug_2 ncondadvg_3 ///
	[iw=miwt], selection(cv) folds(10) serule rseed(8675309) nolog

elasticnet linear ystd ///
	$bvars nlninc_1 npcemply_1 npcwelf_1 npcmarr_1 npcengl_1 ///
	nlninc_2 npcemply_2 npcwelf_2 npcmarr_2 npcengl_2 ///
	nlninc_3 npcemply_3 npcwelf_3 npcmarr_3 npcengl_3 nsubage_3_resid ///
	ncondadvg_1 nbl5ug_1 ncondadvg_2 nbl5ug_2 ncondadvg_3 nbl5ug_3 ///
	[iw=miwt], selection(cv) folds(10) serule rseed(8675309) nolog
	
log close

