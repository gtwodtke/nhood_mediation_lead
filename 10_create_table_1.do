capture log close
capture clear all
set more off

global project_directory "D:\projects\nhood_mediation_lead" 
*global project_directory "C:\Users\Geoff.Wodtke\Desktop\projects\nhood_mediation_lead"
*global project_directory "C:\nhood_mediation_lead"

log using "${project_directory}\programs\_LOGS\10_create_table_1.log", replace

/******************************************
DO-FILE NAME: 10_create_table_1.do
AUTHOR: GW
DATE UPDATED: 08/04/2020
PURPOSE: create time-invariant descriptives
NOTES:
*******************************************/

///Table 1: Time-invariant sample characteristics
use "${project_directory}\data\v02_phdcn_merged_mi.dta", clear

//Sample size
tab ncohort if _mj==0

//Child measures
sum ///
	nfemale_b ///
	nwhite_b nblack_b nhispan_b nothrace_b ///
	[iw=miwt] if _mj!=0
	
//Family measures
sum ///
	nfamsize_b ///
	nownhome_b  ///
	[iw=miwt] if _mj!=0

//PCG measures
sum ///
	npcage_b ///
	npclesshs_b npchsgrad_b npcsomcol_b npccolgrd_b ///
	[iw=miwt] if _mj!=0

log close
