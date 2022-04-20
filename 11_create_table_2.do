capture log close
capture clear all
set more off

global project_directory "D:\projects\nhood_mediation_lead" 
*global project_directory "C:\Users\Geoff.Wodtke\Desktop\projects\nhood_mediation_lead"
*global project_directory "C:\nhood_mediation_lead"

log using "${project_directory}\programs\_LOGS\11_create_table_2.log", replace

/****************************************
DO-FILE NAME: 11_create_table_2.do
AUTHOR: GW
DATE UPDATED: 08/04/2020
PURPOSE: create time-varying descriptives
NOTES:
*****************************************/

///Table 2: Time-varying sample characteristics
use "${project_directory}\data\v02_phdcn_merged_mi.dta", clear

//Sample size
tab ncohort if _mj==0

//Contextual measures
sum ///
	ncondadvg_1 ncondadvg_2 ncondadvg_3 ///
	nbl5ug_1 nbl5ug_2 nbl5ug_3 ///
	[iw=miwt] if _mj!=0

//Child outcomes
sum nppvt_3 [iw=miwt] if _mj!=0
	
//PCG measures

sum ///
	nlninc_1 nlninc_2 nlninc_3 ///
	npcemply_1 npcemply_2 npcemply_3 ///
	npcwelf_1 npcwelf_2 npcwelf_3 ///
	npcmarr_1 npcmarr_2 npcmarr_3 ///
	npcengl_1 npcengl_2 npcengl_3 [iw=miwt] if _mj!=0

log close

