capture log close
capture clear all
set more off

global project_directory "D:\projects\nhood_mediation_lead" 
*global project_directory "C:\Users\Geoff.Wodtke\Desktop\projects\nhood_mediation_lead"
*global project_directory "C:\nhood_mediation_lead"

log using "${project_directory}\programs\_LOGS\05_create_v01_phdcn_tractlink.log", replace

/*
DO-FILE NAME: 05_create_v01_phdcn_tractlink.do
AUTHOR: Wodtke 
DATE UPDATED: 08/04/2020 (GW)
PURPOSE: read in tract to NC xwalk
NOTES:
*/

import spss using "${project_directory}\data\phdcn\tract_linknc\tract_linknc.sav", clear

save "${project_directory}\data\phdcn\v01_phdcn_tractlink.dta", replace

log close
