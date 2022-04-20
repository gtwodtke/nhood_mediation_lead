capture log close
capture clear all
set more off

global project_directory "D:\projects\nhood_mediation_lead" 
*global project_directory "C:\Users\Geoff.Wodtke\Desktop\projects\nhood_mediation_lead"
*global project_directory "C:\nhood_mediation_lead"

log using "${project_directory}\programs\_LOGS\04_create_v01_phdcn_ppvt.log", replace

/*
DO-FILE NAME: 04_create_v01_phdcn_PPVT.do
AUTHOR: Sagi Ramaj 
DATE UPDATED: 08/04/2020 (GW)
PURPOSE: constructing PPVT scores 
NOTES:
*/

use "${project_directory}\data\phdcn\14802059\ICPSR_13732\DS0001\13732-0001-Data-REST.dta", clear
sort SUBID
rename SUBID nSUBID
merge 1:1 nSUBID using "${project_directory}\data\phdcn\v01_phdcn_master.dta", keepusing(nSUBID nSUBAge_3)
keep if _merge == 3
drop _merge
sort nSUBID
save "${project_directory}\data\_TEMP\PPVTFileAgeWave3.dta", replace

use "${project_directory}\data\_TEMP\PPVTFileAgeWave3.dta", clear 
sort nSUBID
drop WAVE COHORT PPVTNOTE PPVT181-PPVT204

///.Z as missing 
recode PPVT1 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT1_3)
recode PPVT2 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT2_3)
recode PPVT3 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT3_3)
recode PPVT4 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT4_3)
recode PPVT5 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT5_3)
recode PPVT6 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT6_3)
recode PPVT7 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT7_3)
recode PPVT8 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT8_3)
recode PPVT9 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT9_3)
recode PPVT10 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT10_3)
recode PPVT11 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT11_3)
recode PPVT12 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT12_3)
recode PPVT13 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT13_3)
recode PPVT14 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT14_3)
recode PPVT15 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT15_3)
recode PPVT16 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT16_3)
recode PPVT17 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT17_3)
recode PPVT18 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT18_3)
recode PPVT19 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT19_3)
recode PPVT20 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT20_3)
recode PPVT21 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT21_3)
recode PPVT22 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT22_3)
recode PPVT23 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT23_3)
recode PPVT24 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT24_3)
recode PPVT25 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT25_3)
recode PPVT26 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT26_3)
recode PPVT27 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT27_3)
recode PPVT28 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT28_3)
recode PPVT29 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT29_3)
recode PPVT30 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT30_3)
recode PPVT31 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT31_3)
recode PPVT32 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT32_3)
recode PPVT33 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT33_3)
recode PPVT34 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT34_3)
recode PPVT35 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT35_3)
recode PPVT36 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT36_3)
recode PPVT37 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT37_3)
recode PPVT38 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT38_3)
recode PPVT39 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT39_3)
recode PPVT40 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT40_3)
recode PPVT41 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT41_3)
recode PPVT42 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT42_3)
recode PPVT43 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT43_3)
recode PPVT44 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT44_3)
recode PPVT45 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT45_3)
recode PPVT46 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT46_3)
recode PPVT47 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT47_3)
recode PPVT48 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT48_3)
recode PPVT49 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT49_3)
recode PPVT50 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT50_3)
recode PPVT51 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT51_3)
recode PPVT52 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT52_3)
recode PPVT53 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT53_3)
recode PPVT54 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT54_3)
recode PPVT55 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT55_3)
recode PPVT56 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT56_3)
recode PPVT57 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT57_3)
recode PPVT58 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT58_3)
recode PPVT59 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT59_3)
recode PPVT60 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT60_3)
recode PPVT61 (4 = 0) (1 2 3 = 1) (.=.) (0 5 6 8 = .z), gen(nPPVT61_3)
recode PPVT62 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT62_3)
recode PPVT63 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT63_3)
recode PPVT64 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT64_3)
recode PPVT65 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT65_3)
recode PPVT66 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT66_3)
recode PPVT67 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT67_3)
recode PPVT68 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT68_3)
recode PPVT69 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT69_3)
recode PPVT70 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT70_3)
recode PPVT71 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT71_3)
recode PPVT72 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT72_3)
recode PPVT73 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT73_3)
recode PPVT74 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT74_3)
recode PPVT75 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT75_3)
recode PPVT76 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT76_3)
recode PPVT77 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT77_3)
recode PPVT78 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT78_3)
recode PPVT79 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT79_3)
recode PPVT80 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT80_3)
recode PPVT81 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT81_3)
recode PPVT82 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT82_3)
recode PPVT83 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT83_3)
recode PPVT84 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT84_3)
recode PPVT85 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT85_3)
recode PPVT86 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT86_3)
recode PPVT87 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT87_3)
recode PPVT88 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT88_3)
recode PPVT89 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT89_3)
recode PPVT90 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT90_3)
recode PPVT91 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT91_3)
recode PPVT92 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT92_3)
recode PPVT93 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT93_3)
recode PPVT94 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT94_3)
recode PPVT95 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT95_3)
recode PPVT96 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT96_3)
recode PPVT97 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT97_3)
recode PPVT98 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT98_3)
recode PPVT99 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT99_3)
recode PPVT100 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT100_3)
recode PPVT101 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT101_3)
recode PPVT102 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT102_3)
recode PPVT103 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT103_3)
recode PPVT104 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT104_3)
recode PPVT105 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT105_3)
recode PPVT106 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT106_3)
recode PPVT107 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT107_3)
recode PPVT108 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT108_3)
recode PPVT109 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT109_3)
recode PPVT110 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT110_3)
recode PPVT111 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT111_3)
recode PPVT112 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT112_3)
recode PPVT113 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT113_3)
recode PPVT114 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT114_3)
recode PPVT115 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT115_3)
recode PPVT116 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT116_3)
recode PPVT117 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT117_3)
recode PPVT118 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT118_3)
recode PPVT119 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT119_3)
recode PPVT120 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT120_3)
recode PPVT121 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT121_3)
recode PPVT122 (4 = 0) (1 2 3 = 1) (.=.) (0 6 7 8 = .z), gen(nPPVT122_3)
recode PPVT123 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT123_3)
recode PPVT124 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT124_3)
recode PPVT125 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT125_3)
recode PPVT126 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT126_3)
recode PPVT127 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT127_3)
recode PPVT128 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT128_3)
recode PPVT129 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT129_3)
recode PPVT130 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT130_3)
recode PPVT131 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT131_3)
recode PPVT132 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT132_3)
recode PPVT133 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT133_3)
recode PPVT134 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT134_3)
recode PPVT135 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT135_3)
recode PPVT136 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT136_3)
recode PPVT137 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT137_3)
recode PPVT138 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT138_3)
recode PPVT139 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT139_3)
recode PPVT140 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT140_3)
recode PPVT141 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT141_3)
recode PPVT142 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT142_3)
recode PPVT143 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT143_3)
recode PPVT144 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT144_3)
recode PPVT145 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT145_3)
recode PPVT146 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT146_3)
recode PPVT147 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT147_3)
recode PPVT148 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT148_3)
recode PPVT149 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT149_3)
recode PPVT150 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT150_3)
recode PPVT151 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT151_3)
recode PPVT152 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT152_3)
recode PPVT153 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT153_3)
recode PPVT154 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT154_3)
recode PPVT155 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT155_3)
recode PPVT156 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT156_3)
recode PPVT157 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT157_3)
recode PPVT158 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT158_3)
recode PPVT159 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT159_3)
recode PPVT160 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT160_3)
recode PPVT161 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT161_3)
recode PPVT162 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT162_3)
recode PPVT163 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT163_3)
recode PPVT164 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT164_3)
recode PPVT165 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT165_3)
recode PPVT166 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT166_3)
recode PPVT167 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT167_3)
recode PPVT168 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT168_3)
recode PPVT169 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT169_3)
recode PPVT170 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT170_3)
recode PPVT171 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT171_3)
recode PPVT172 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT172_3)
recode PPVT173 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT173_3)
recode PPVT174 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT174_3)
recode PPVT175 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT175_3)
recode PPVT176 (2 = 0) (1 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT176_3)
recode PPVT177 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT177_3)
recode PPVT178 (3 = 0) (1 2 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT178_3)
recode PPVT179 (4 = 0) (1 2 3 = 1) (.=.) (0 6 8 = .z), gen(nPPVT179_3)
recode PPVT180 (1 = 0) (2 3 4 = 1) (.=.) (0 6 8 = .z), gen(nPPVT180_3)

egen missingset1 = rowmiss(nPPVT1_3 nPPVT2_3 nPPVT3_3 nPPVT4_3 nPPVT5_3 nPPVT6_3 nPPVT7_3 nPPVT8_3 nPPVT9_3 nPPVT10_3 nPPVT11_3 nPPVT12_3)
egen nPPVTSetErrors1_3 = rowtotal(nPPVT1_3 nPPVT2_3 nPPVT3_3 nPPVT4_3 nPPVT5_3 nPPVT6_3 nPPVT7_3 nPPVT8_3 nPPVT9_3 nPPVT10_3 nPPVT11_3 nPPVT12_3)
replace nPPVTSetErrors1_3 = . if missingset1 >3  //only counting those who have <=3 missing scores

egen missingset2 = rowmiss(nPPVT13_3 nPPVT14_3 nPPVT15_3 nPPVT16_3 nPPVT17_3 nPPVT18_3 nPPVT19_3 nPPVT20_3 nPPVT21_3 nPPVT22_3 nPPVT23_3 nPPVT24_3)
egen nPPVTSetErrors2_3 = rowtotal(nPPVT13_3 nPPVT14_3 nPPVT15_3 nPPVT16_3 nPPVT17_3 nPPVT18_3 nPPVT19_3 nPPVT20_3 nPPVT21_3 nPPVT22_3 nPPVT23_3 nPPVT24_3)
replace nPPVTSetErrors2_3 = . if missingset2 >3

egen missingset3 = rowmiss(nPPVT25_3 nPPVT26_3 nPPVT27_3 nPPVT28_3 nPPVT29_3 nPPVT30_3 nPPVT31_3 nPPVT32_3 nPPVT33_3 nPPVT34_3 nPPVT35_3 nPPVT36_3)
egen nPPVTSetErrors3_3 = rowtotal(nPPVT25_3 nPPVT26_3 nPPVT27_3 nPPVT28_3 nPPVT29_3 nPPVT30_3 nPPVT31_3 nPPVT32_3 nPPVT33_3 nPPVT34_3 nPPVT35_3 nPPVT36_3)
replace nPPVTSetErrors3_3 = . if missingset3 >3

egen missingset4 = rowmiss(nPPVT37_3 nPPVT38_3 nPPVT39_3 nPPVT40_3 nPPVT41_3 nPPVT42_3 nPPVT43_3 nPPVT44_3 nPPVT45_3 nPPVT46_3 nPPVT47_3 nPPVT48_3)
egen nPPVTSetErrors4_3 = rowtotal(nPPVT37_3 nPPVT38_3 nPPVT39_3 nPPVT40_3 nPPVT41_3 nPPVT42_3 nPPVT43_3 nPPVT44_3 nPPVT45_3 nPPVT46_3 nPPVT47_3 nPPVT48_3)
replace nPPVTSetErrors4_3 = . if missingset4 >3

egen missingset5 = rowmiss(nPPVT49_3 nPPVT50_3 nPPVT51_3 nPPVT52_3 nPPVT53_3 nPPVT54_3 nPPVT55_3 nPPVT56_3 nPPVT57_3 nPPVT58_3 nPPVT59_3 nPPVT60_3)
egen nPPVTSetErrors5_3 = rowtotal(nPPVT49_3 nPPVT50_3 nPPVT51_3 nPPVT52_3 nPPVT53_3 nPPVT54_3 nPPVT55_3 nPPVT56_3 nPPVT57_3 nPPVT58_3 nPPVT59_3 nPPVT60_3)
replace nPPVTSetErrors5_3 = . if missingset5 >3

egen missingset6 = rowmiss(nPPVT61_3 nPPVT62_3 nPPVT63_3 nPPVT64_3 nPPVT65_3 nPPVT66_3 nPPVT67_3 nPPVT68_3 nPPVT69_3 nPPVT70_3 nPPVT71_3 nPPVT72_3)
egen nPPVTSetErrors6_3 = rowtotal(nPPVT61_3 nPPVT62_3 nPPVT63_3 nPPVT64_3 nPPVT65_3 nPPVT66_3 nPPVT67_3 nPPVT68_3 nPPVT69_3 nPPVT70_3 nPPVT71_3 nPPVT72_3)
replace nPPVTSetErrors6_3 = . if missingset6 >3

egen missingset7 = rowmiss(nPPVT73_3 nPPVT74_3 nPPVT75_3 nPPVT76_3 nPPVT77_3 nPPVT78_3 nPPVT79_3 nPPVT80_3 nPPVT81_3 nPPVT82_3 nPPVT83_3 nPPVT84_3)
egen nPPVTSetErrors7_3 = rowtotal(nPPVT73_3 nPPVT74_3 nPPVT75_3 nPPVT76_3 nPPVT77_3 nPPVT78_3 nPPVT79_3 nPPVT80_3 nPPVT81_3 nPPVT82_3 nPPVT83_3 nPPVT84_3)
replace nPPVTSetErrors7_3 = . if missingset7 >3

egen missingset8 = rowmiss(nPPVT85_3 nPPVT86_3 nPPVT87_3 nPPVT88_3 nPPVT89_3 nPPVT90_3 nPPVT91_3 nPPVT92_3 nPPVT93_3 nPPVT94_3 nPPVT95_3 nPPVT96_3)
egen nPPVTSetErrors8_3 = rowtotal(nPPVT85_3 nPPVT86_3 nPPVT87_3 nPPVT88_3 nPPVT89_3 nPPVT90_3 nPPVT91_3 nPPVT92_3 nPPVT93_3 nPPVT94_3 nPPVT95_3 nPPVT96_3)
replace nPPVTSetErrors8_3 = . if missingset8 >3

egen missingset9 = rowmiss(nPPVT97_3 nPPVT98_3 nPPVT99_3 nPPVT100_3 nPPVT101_3 nPPVT102_3 nPPVT103_3 nPPVT104_3 nPPVT105_3 nPPVT106_3 nPPVT107_3 nPPVT108_3)
egen nPPVTSetErrors9_3 = rowtotal(nPPVT97_3 nPPVT98_3 nPPVT99_3 nPPVT100_3 nPPVT101_3 nPPVT102_3 nPPVT103_3 nPPVT104_3 nPPVT105_3 nPPVT106_3 nPPVT107_3 nPPVT108_3)
replace nPPVTSetErrors9_3 = . if missingset9 >3

egen missingset10 = rowmiss(nPPVT109_3 nPPVT110_3 nPPVT111_3 nPPVT112_3 nPPVT113_3 nPPVT114_3 nPPVT115_3 nPPVT116_3 nPPVT117_3 nPPVT118_3 nPPVT119_3 nPPVT120_3)
egen nPPVTSetErrors10_3 = rowtotal(nPPVT109_3 nPPVT110_3 nPPVT111_3 nPPVT112_3 nPPVT113_3 nPPVT114_3 nPPVT115_3 nPPVT116_3 nPPVT117_3 nPPVT118_3 nPPVT119_3 nPPVT120_3)
replace nPPVTSetErrors10_3 = . if missingset10 >3

egen missingset11 = rowmiss(nPPVT121_3 nPPVT122_3 nPPVT123_3 nPPVT124_3 nPPVT125_3 nPPVT126_3 nPPVT127_3 nPPVT128_3 nPPVT129_3 nPPVT130_3 nPPVT131_3 nPPVT132_3)
egen nPPVTSetErrors11_3 = rowtotal(nPPVT121_3 nPPVT122_3 nPPVT123_3 nPPVT124_3 nPPVT125_3 nPPVT126_3 nPPVT127_3 nPPVT128_3 nPPVT129_3 nPPVT130_3 nPPVT131_3 nPPVT132_3)
replace nPPVTSetErrors11_3 = . if missingset11 >3

egen missingset12 = rowmiss(nPPVT133_3 nPPVT134_3 nPPVT135_3 nPPVT136_3 nPPVT137_3 nPPVT138_3 nPPVT139_3 nPPVT140_3 nPPVT141_3 nPPVT142_3 nPPVT143_3 nPPVT144_3)
egen nPPVTSetErrors12_3 = rowtotal(nPPVT133_3 nPPVT134_3 nPPVT135_3 nPPVT136_3 nPPVT137_3 nPPVT138_3 nPPVT139_3 nPPVT140_3 nPPVT141_3 nPPVT142_3 nPPVT143_3 nPPVT144_3)
replace nPPVTSetErrors12_3 = . if missingset12 >3

egen missingset13 = rowmiss(nPPVT145_3 nPPVT146_3 nPPVT147_3 nPPVT148_3 nPPVT149_3 nPPVT150_3 nPPVT151_3 nPPVT152_3 nPPVT153_3 nPPVT154_3 nPPVT155_3 nPPVT156_3)
egen nPPVTSetErrors13_3 = rowtotal(nPPVT145_3 nPPVT146_3 nPPVT147_3 nPPVT148_3 nPPVT149_3 nPPVT150_3 nPPVT151_3 nPPVT152_3 nPPVT153_3 nPPVT154_3 nPPVT155_3 nPPVT156_3)
replace nPPVTSetErrors13_3 = . if missingset13 >3

egen missingset14 = rowmiss(nPPVT157_3 nPPVT158_3 nPPVT159_3 nPPVT160_3 nPPVT161_3 nPPVT162_3 nPPVT163_3 nPPVT164_3 nPPVT165_3 nPPVT166_3 nPPVT167_3 nPPVT168_3)
egen nPPVTSetErrors14_3 = rowtotal(nPPVT157_3 nPPVT158_3 nPPVT159_3 nPPVT160_3 nPPVT161_3 nPPVT162_3 nPPVT163_3 nPPVT164_3 nPPVT165_3 nPPVT166_3 nPPVT167_3 nPPVT168_3)
replace nPPVTSetErrors14_3 = . if missingset14 >3

egen missingset15 = rowmiss(nPPVT169_3 nPPVT170_3 nPPVT171_3 nPPVT172_3 nPPVT173_3 nPPVT174_3 nPPVT175_3 nPPVT176_3 nPPVT177_3 nPPVT178_3 nPPVT179_3 nPPVT180_3)
egen nPPVTSetErrors15_3 = rowtotal(nPPVT169_3 nPPVT170_3 nPPVT171_3 nPPVT172_3 nPPVT173_3 nPPVT174_3 nPPVT175_3 nPPVT176_3 nPPVT177_3 nPPVT178_3 nPPVT179_3 nPPVT180_3)
replace nPPVTSetErrors15_3 = . if missingset15 >3

gen nBasalset_3 = .
replace ///
	nBasalset_3 = 1 if nPPVTSetErrors1_3 <=1 | (nPPVTSetErrors8_3 == . & nPPVTSetErrors1_3 >1 & nPPVTSetErrors1_3 <=12 & nPPVTSetErrors2_3 >1 & nPPVTSetErrors2_3 <=12 & nPPVTSetErrors3_3 >1 & nPPVTSetErrors3_3 <=12 ///
		& nPPVTSetErrors4_3 >1 & nPPVTSetErrors4_3 <=12 & nPPVTSetErrors5_3 >1 & nPPVTSetErrors5_3 <=12 & nPPVTSetErrors6_3 >1 & nPPVTSetErrors6_3 <=12 & nPPVTSetErrors7_3 >1 & nPPVTSetErrors7_3 <=12) | ///
	(nPPVTSetErrors7_3 == . & nPPVTSetErrors1_3 >1 & nPPVTSetErrors1_3 <=12 & nPPVTSetErrors2_3 >1 & nPPVTSetErrors2_3 <=12 & nPPVTSetErrors3_3 >1 & nPPVTSetErrors3_3 <=12 ///
		& nPPVTSetErrors4_3 >1 & nPPVTSetErrors4_3 <=12 & nPPVTSetErrors5_3 >1 & nPPVTSetErrors5_3 <=12 & nPPVTSetErrors6_3 >1 & nPPVTSetErrors6_3 <=12) | ///
	(nPPVTSetErrors6_3 == . & nPPVTSetErrors1_3 >1 & nPPVTSetErrors1_3 <=12 & nPPVTSetErrors2_3 >1 & nPPVTSetErrors2_3 <=12 & nPPVTSetErrors3_3 >1 & nPPVTSetErrors3_3 <=12 & nPPVTSetErrors4_3 >1 & nPPVTSetErrors4_3 <=12 ///
		& nPPVTSetErrors5_3 >1 & nPPVTSetErrors5_3 <=12) | ///
	(nPPVTSetErrors5_3 == . & nPPVTSetErrors1_3 >1 & nPPVTSetErrors1_3 <=12 & nPPVTSetErrors2_3 >1 & nPPVTSetErrors2_3 <=12 & nPPVTSetErrors3_3 >1 & nPPVTSetErrors3_3 <=12 & nPPVTSetErrors4_3 >1 & nPPVTSetErrors4_3 <=12) | ///
	(nPPVTSetErrors4_3 == . & nPPVTSetErrors1_3 >1 & nPPVTSetErrors1_3 <=12 & nPPVTSetErrors2_3 >1 & nPPVTSetErrors2_3 <=12 & nPPVTSetErrors3_3 >1 & nPPVTSetErrors3_3 <=12) | ///
	(nPPVTSetErrors3_3 == . & nPPVTSetErrors1_3 >1 & nPPVTSetErrors1_3 <=12 & nPPVTSetErrors2_3 >1 & nPPVTSetErrors2_3 <=12) | ///
	(nPPVTSetErrors2_3 == . & nPPVTSetErrors1_3 >1 & nPPVTSetErrors1_3 <=12) 
replace nBasalset_3 = 2 if nBasalset_3 >1 & nPPVTSetErrors2_3 <=1
replace nBasalset_3 = 3 if nBasalset_3 >2 & nPPVTSetErrors3_3 <=1
replace nBasalset_3 = 4 if nBasalset_3 >3 & nPPVTSetErrors4_3 <=1
replace nBasalset_3 = 5 if nBasalset_3 >4 & nPPVTSetErrors5_3 <=1
replace nBasalset_3 = 6 if nBasalset_3 >5 & nPPVTSetErrors6_3 <=1
replace nBasalset_3 = 7 if nBasalset_3 >6 & nPPVTSetErrors7_3 <=1
replace nBasalset_3 = 8 if nBasalset_3 >7 & nPPVTSetErrors8_3 <=1
replace nBasalset_3 = 9 if nBasalset_3 >8 & nPPVTSetErrors9_3 <=1
replace nBasalset_3 = 10 if nBasalset_3 >9 & nPPVTSetErrors10_3 <=1
replace nBasalset_3 = 11 if nBasalset_3 >10 & nPPVTSetErrors11_3 <=1
replace nBasalset_3 = 12 if nBasalset_3 >11 & nPPVTSetErrors12_3 <=1
replace nBasalset_3 = 13 if nBasalset_3 >12 & nPPVTSetErrors13_3 <=1
replace nBasalset_3 = 14 if nBasalset_3 >13 & nPPVTSetErrors14_3 <=1
replace nBasalset_3 = 15 if nBasalset_3 >14 & nPPVTSetErrors15_3 <=1
	
gen nCeilingset_3 = 0
replace nCeilingset_3 = 15 if nPPVTSetErrors15_3 >=8 & nPPVTSetErrors15_3 <=12
replace nCeilingset_3 = 14 if nCeilingset_3 <15 & nPPVTSetErrors14_3 >=8 & nPPVTSetErrors14_3 <=12
replace nCeilingset_3 = 13 if nCeilingset_3 <14 & nPPVTSetErrors13_3 >=8 & nPPVTSetErrors13_3 <=12
replace nCeilingset_3 = 12 if nCeilingset_3 <13 & nPPVTSetErrors12_3 >=8 & nPPVTSetErrors12_3 <=12
replace nCeilingset_3 = 11 if nCeilingset_3 <12 & nPPVTSetErrors11_3 >=8 & nPPVTSetErrors11_3 <=12
replace nCeilingset_3 = 10 if nCeilingset_3 <11 & nPPVTSetErrors10_3 >=8 & nPPVTSetErrors10_3 <=12
replace nCeilingset_3 = 9 if nCeilingset_3 <10 & nPPVTSetErrors9_3 >=8 & nPPVTSetErrors9_3 <=12
replace nCeilingset_3 = 8 if nCeilingset_3 <9 & nPPVTSetErrors8_3 >=8 & nPPVTSetErrors8_3 <=12
replace nCeilingset_3 = 7 if nCeilingset_3 <8 & nPPVTSetErrors7_3 >=8 & nPPVTSetErrors7_3 <=12
replace nCeilingset_3 = 6 if nCeilingset_3 <7 & nPPVTSetErrors6_3 >=8 & nPPVTSetErrors6_3 <=12
replace nCeilingset_3 = 5 if nCeilingset_3 <6 & nPPVTSetErrors5_3 >=8 & nPPVTSetErrors5_3 <=12
replace nCeilingset_3 = 4 if nCeilingset_3 <5 & nPPVTSetErrors4_3 >=8 & nPPVTSetErrors4_3 <=12
replace nCeilingset_3 = 3 if nCeilingset_3 <4 & nPPVTSetErrors3_3 >=8 & nPPVTSetErrors3_3 <=12
replace nCeilingset_3 = 2 if nCeilingset_3 <3 & nPPVTSetErrors2_3 >=8 & nPPVTSetErrors2_3 <=12
replace ///
	nCeilingset_3 = 1 if (nCeilingset_3 <2 & nPPVTSetErrors1_3 >=8 & nPPVTSetErrors1_3 <=12) | ///
	(nPPVTSetErrors9_3 == . & nPPVTSetErrors1_3 <8 & nPPVTSetErrors2_3 <8 & nPPVTSetErrors3_3 <8 & nPPVTSetErrors4_3 <8 & nPPVTSetErrors5_3 <8 & nPPVTSetErrors6_3 <8 & nPPVTSetErrors7_3 <8 & nPPVTSetErrors8_3 <8) | ///
	(nPPVTSetErrors8_3 == . & nPPVTSetErrors1_3 <8 & nPPVTSetErrors2_3 <8 & nPPVTSetErrors3_3 <8 & nPPVTSetErrors4_3 <8 & nPPVTSetErrors5_3 <8 & nPPVTSetErrors6_3 <8 & nPPVTSetErrors7_3 <8) | ///
	(nPPVTSetErrors7_3 == . & nPPVTSetErrors1_3 <8 & nPPVTSetErrors2_3 <8 & nPPVTSetErrors3_3 <8 & nPPVTSetErrors4_3 <8 & nPPVTSetErrors5_3 <8 & nPPVTSetErrors6_3 <8) | ///
	(nPPVTSetErrors6_3 == . & nPPVTSetErrors1_3 <8 & nPPVTSetErrors2_3 <8 & nPPVTSetErrors3_3 <8 & nPPVTSetErrors4_3 <8 & nPPVTSetErrors5_3 <8) | ///
	(nPPVTSetErrors5_3 == . & nPPVTSetErrors1_3 <8 & nPPVTSetErrors2_3 <8 & nPPVTSetErrors3_3 <8 & nPPVTSetErrors4_3 <8) | ///
	(nPPVTSetErrors4_3 == . & nPPVTSetErrors1_3 <8 & nPPVTSetErrors2_3 <8 & nPPVTSetErrors3_3 <8) | ///
	(nPPVTSetErrors3_3 == . & nPPVTSetErrors1_3 <8 & nPPVTSetErrors2_3 <8) | ///
	(nPPVTSetErrors2_3 == . & nPPVTSetErrors1_3 <8)
replace nCeilingset_3 = . if nCeilingset_3 == 0

gen nCeilingitem_3 = .
replace nCeilingitem_3 = 12 if nCeilingset_3 == 1
replace nCeilingitem_3 = 24 if nCeilingset_3 == 2
replace nCeilingitem_3 = 36 if nCeilingset_3 == 3
replace nCeilingitem_3 = 48 if nCeilingset_3 == 4
replace nCeilingitem_3 = 60 if nCeilingset_3 == 5
replace nCeilingitem_3 = 72 if nCeilingset_3 == 6
replace nCeilingitem_3 = 84 if nCeilingset_3 == 7
replace nCeilingitem_3 = 96 if nCeilingset_3 == 8
replace nCeilingitem_3 = 108 if nCeilingset_3 == 9
replace nCeilingitem_3 = 120 if nCeilingset_3 == 10
replace nCeilingitem_3 = 132 if nCeilingset_3 == 11
replace nCeilingitem_3 = 144 if nCeilingset_3 == 12
replace nCeilingitem_3 = 156 if nCeilingset_3 == 13
replace nCeilingitem_3 = 168 if nCeilingset_3 == 14
replace nCeilingitem_3 = 180 if nCeilingset_3 == 15

gen nPPVTScore_3=. 
forval i=1/7 { 
	forval j=`i'/15 { 
		egen toterrors=rowtotal(nPPVTSetErrors`i'_3-nPPVTSetErrors`j'_3) if nCeilingset_3==`j' & nBasalset_3==`i' 
		replace nPPVTScore_3=nCeilingitem_3-toterrors if nCeilingset_3==`j' & nBasalset_3==`i' 
		drop toterrors 
		} 
	}

//.Z as error
recode nPPVT1_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT1e_3)
recode nPPVT2_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT2e_3)
recode nPPVT3_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT3e_3)
recode nPPVT4_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT4e_3)
recode nPPVT5_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT5e_3)
recode nPPVT6_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT6e_3)
recode nPPVT7_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT7e_3)
recode nPPVT8_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT8e_3)
recode nPPVT9_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT9e_3)
recode nPPVT10_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT10e_3)
recode nPPVT11_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT11e_3)
recode nPPVT12_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT12e_3)
recode nPPVT13_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT13e_3)
recode nPPVT14_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT14e_3)
recode nPPVT15_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT15e_3)
recode nPPVT16_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT16e_3)
recode nPPVT17_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT17e_3)
recode nPPVT18_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT18e_3)
recode nPPVT19_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT19e_3)
recode nPPVT20_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT20e_3)
recode nPPVT21_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT21e_3)
recode nPPVT22_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT22e_3)
recode nPPVT23_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT23e_3)
recode nPPVT24_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT24e_3)
recode nPPVT25_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT25e_3)
recode nPPVT26_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT26e_3)
recode nPPVT27_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT27e_3)
recode nPPVT28_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT28e_3)
recode nPPVT29_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT29e_3)
recode nPPVT30_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT30e_3)
recode nPPVT31_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT31e_3)
recode nPPVT32_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT32e_3)
recode nPPVT33_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT33e_3)
recode nPPVT34_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT34e_3)
recode nPPVT35_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT35e_3)
recode nPPVT36_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT36e_3)
recode nPPVT37_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT37e_3)
recode nPPVT38_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT38e_3)
recode nPPVT39_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT39e_3)
recode nPPVT40_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT40e_3)
recode nPPVT41_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT41e_3)
recode nPPVT42_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT42e_3)
recode nPPVT43_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT43e_3)
recode nPPVT44_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT44e_3)
recode nPPVT45_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT45e_3)
recode nPPVT46_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT46e_3)
recode nPPVT47_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT47e_3)
recode nPPVT48_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT48e_3)
recode nPPVT49_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT49e_3)
recode nPPVT50_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT50e_3)
recode nPPVT51_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT51e_3)
recode nPPVT52_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT52e_3)
recode nPPVT53_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT53e_3)
recode nPPVT54_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT54e_3)
recode nPPVT55_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT55e_3)
recode nPPVT56_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT56e_3)
recode nPPVT57_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT57e_3)
recode nPPVT58_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT58e_3)
recode nPPVT59_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT59e_3)
recode nPPVT60_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT60e_3)
recode nPPVT61_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT61e_3)
recode nPPVT62_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT62e_3)
recode nPPVT63_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT63e_3)
recode nPPVT64_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT64e_3)
recode nPPVT65_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT65e_3)
recode nPPVT66_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT66e_3)
recode nPPVT67_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT67e_3)
recode nPPVT68_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT68e_3)
recode nPPVT69_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT69e_3)
recode nPPVT70_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT70e_3)
recode nPPVT71_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT71e_3)
recode nPPVT72_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT72e_3)
recode nPPVT73_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT73e_3)
recode nPPVT74_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT74e_3)
recode nPPVT75_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT75e_3)
recode nPPVT76_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT76e_3)
recode nPPVT77_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT77e_3)
recode nPPVT78_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT78e_3)
recode nPPVT79_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT79e_3)
recode nPPVT80_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT80e_3)
recode nPPVT81_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT81e_3)
recode nPPVT82_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT82e_3)
recode nPPVT83_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT83e_3)
recode nPPVT84_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT84e_3)
recode nPPVT85_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT85e_3)
recode nPPVT86_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT86e_3)
recode nPPVT87_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT87e_3)
recode nPPVT88_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT88e_3)
recode nPPVT89_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT89e_3)
recode nPPVT90_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT90e_3)
recode nPPVT91_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT91e_3)
recode nPPVT92_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT92e_3)
recode nPPVT93_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT93e_3)
recode nPPVT94_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT94e_3)
recode nPPVT95_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT95e_3)
recode nPPVT96_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT96e_3)
recode nPPVT97_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT97e_3)
recode nPPVT98_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT98e_3)
recode nPPVT99_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT99e_3)
recode nPPVT100_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT100e_3)
recode nPPVT101_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT101e_3)
recode nPPVT102_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT102e_3)
recode nPPVT103_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT103e_3)
recode nPPVT104_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT104e_3)
recode nPPVT105_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT105e_3)
recode nPPVT106_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT106e_3)
recode nPPVT107_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT107e_3)
recode nPPVT108_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT108e_3)
recode nPPVT109_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT109e_3)
recode nPPVT110_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT110e_3)
recode nPPVT111_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT111e_3)
recode nPPVT112_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT112e_3)
recode nPPVT113_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT113e_3)
recode nPPVT114_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT114e_3)
recode nPPVT115_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT115e_3)
recode nPPVT116_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT116e_3)
recode nPPVT117_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT117e_3)
recode nPPVT118_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT118e_3)
recode nPPVT119_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT119e_3)
recode nPPVT120_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT120e_3)
recode nPPVT121_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT121e_3)
recode nPPVT122_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT122e_3)
recode nPPVT123_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT123e_3)
recode nPPVT124_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT124e_3)
recode nPPVT125_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT125e_3)
recode nPPVT126_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT126e_3)
recode nPPVT127_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT127e_3)
recode nPPVT128_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT128e_3)
recode nPPVT129_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT129e_3)
recode nPPVT130_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT130e_3)
recode nPPVT131_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT131e_3)
recode nPPVT132_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT132e_3)
recode nPPVT133_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT133e_3)
recode nPPVT134_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT134e_3)
recode nPPVT135_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT135e_3)
recode nPPVT136_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT136e_3)
recode nPPVT137_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT137e_3)
recode nPPVT138_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT138e_3)
recode nPPVT139_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT139e_3)
recode nPPVT140_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT140e_3)
recode nPPVT141_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT141e_3)
recode nPPVT142_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT142e_3)
recode nPPVT143_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT143e_3)
recode nPPVT144_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT144e_3)
recode nPPVT145_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT145e_3)
recode nPPVT146_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT146e_3)
recode nPPVT147_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT147e_3)
recode nPPVT148_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT148e_3)
recode nPPVT149_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT149e_3)
recode nPPVT150_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT150e_3)
recode nPPVT151_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT151e_3)
recode nPPVT152_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT152e_3)
recode nPPVT153_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT153e_3)
recode nPPVT154_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT154e_3)
recode nPPVT155_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT155e_3)
recode nPPVT156_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT156e_3)
recode nPPVT157_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT157e_3)
recode nPPVT158_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT158e_3)
recode nPPVT159_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT159e_3)
recode nPPVT160_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT160e_3)
recode nPPVT161_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT161e_3)
recode nPPVT162_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT162e_3)
recode nPPVT163_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT163e_3)
recode nPPVT164_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT164e_3)
recode nPPVT165_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT165e_3)
recode nPPVT166_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT166e_3)
recode nPPVT167_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT167e_3)
recode nPPVT168_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT168e_3)
recode nPPVT169_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT169e_3)
recode nPPVT170_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT170e_3)
recode nPPVT171_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT171e_3)
recode nPPVT172_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT172e_3)
recode nPPVT173_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT173e_3)
recode nPPVT174_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT174e_3)
recode nPPVT175_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT175e_3)
recode nPPVT176_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT176e_3)
recode nPPVT177_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT177e_3)
recode nPPVT178_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT178e_3)
recode nPPVT179_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT179e_3)
recode nPPVT180_3 (1 .z =1) (0 = 0) (.=.), gen(nPPVT180e_3)

egen missingset1e = rowmiss(nPPVT1e_3 nPPVT2e_3 nPPVT3e_3 nPPVT4e_3 nPPVT5e_3 nPPVT6e_3 nPPVT7e_3 nPPVT8e_3 nPPVT9e_3 nPPVT10e_3 nPPVT11e_3 nPPVT12e_3)
egen nPPVTSetErrors1e_3 = rowtotal(nPPVT1e_3 nPPVT2e_3 nPPVT3e_3 nPPVT4e_3 nPPVT5e_3 nPPVT6e_3 nPPVT7e_3 nPPVT8e_3 nPPVT9e_3 nPPVT10e_3 nPPVT11e_3 nPPVT12e_3)
replace nPPVTSetErrors1e_3 = . if missingset1e >3  

egen missingset2e = rowmiss(nPPVT13e_3 nPPVT14e_3 nPPVT15e_3 nPPVT16e_3 nPPVT17e_3 nPPVT18e_3 nPPVT19e_3 nPPVT20e_3 nPPVT21e_3 nPPVT22e_3 nPPVT23e_3 nPPVT24e_3)
egen nPPVTSetErrors2e_3 = rowtotal(nPPVT13e_3 nPPVT14e_3 nPPVT15e_3 nPPVT16e_3 nPPVT17e_3 nPPVT18e_3 nPPVT19e_3 nPPVT20e_3 nPPVT21e_3 nPPVT22e_3 nPPVT23e_3 nPPVT24e_3)
replace nPPVTSetErrors2e_3 = . if missingset2e >3 

egen missingset3e = rowmiss(nPPVT25e_3 nPPVT26e_3 nPPVT27e_3 nPPVT28e_3 nPPVT29e_3 nPPVT30e_3 nPPVT31e_3 nPPVT32e_3 nPPVT33e_3 nPPVT34e_3 nPPVT35e_3 nPPVT36e_3)
egen nPPVTSetErrors3e_3 = rowtotal(nPPVT25e_3 nPPVT26e_3 nPPVT27e_3 nPPVT28e_3 nPPVT29e_3 nPPVT30e_3 nPPVT31e_3 nPPVT32e_3 nPPVT33e_3 nPPVT34e_3 nPPVT35e_3 nPPVT36e_3)
replace nPPVTSetErrors3e_3 = . if missingset3e >3 

egen missingset4e = rowmiss(nPPVT37e_3 nPPVT38e_3 nPPVT39e_3 nPPVT40e_3 nPPVT41e_3 nPPVT42e_3 nPPVT43e_3 nPPVT44e_3 nPPVT45e_3 nPPVT46e_3 nPPVT47e_3 nPPVT48e_3)
egen nPPVTSetErrors4e_3 = rowtotal(nPPVT37e_3 nPPVT38e_3 nPPVT39e_3 nPPVT40e_3 nPPVT41e_3 nPPVT42e_3 nPPVT43e_3 nPPVT44e_3 nPPVT45e_3 nPPVT46e_3 nPPVT47e_3 nPPVT48e_3)
replace nPPVTSetErrors4e_3 = . if missingset4e >3 

egen missingset5e = rowmiss(nPPVT49e_3 nPPVT50e_3 nPPVT51e_3 nPPVT52e_3 nPPVT53e_3 nPPVT54e_3 nPPVT55e_3 nPPVT56e_3 nPPVT57e_3 nPPVT58e_3 nPPVT59e_3 nPPVT60e_3)
egen nPPVTSetErrors5e_3 = rowtotal(nPPVT49e_3 nPPVT50e_3 nPPVT51e_3 nPPVT52e_3 nPPVT53e_3 nPPVT54e_3 nPPVT55e_3 nPPVT56e_3 nPPVT57e_3 nPPVT58e_3 nPPVT59e_3 nPPVT60e_3)
replace nPPVTSetErrors5e_3 = . if missingset5e >3 

egen missingset6e = rowmiss(nPPVT61_3 nPPVT62e_3 nPPVT63e_3 nPPVT64e_3 nPPVT65e_3 nPPVT66e_3 nPPVT67e_3 nPPVT68e_3 nPPVT69e_3 nPPVT70e_3 nPPVT71e_3 nPPVT72e_3)
egen nPPVTSetErrors6e_3 = rowtotal(nPPVT61e_3 nPPVT62e_3 nPPVT63e_3 nPPVT64e_3 nPPVT65e_3 nPPVT66e_3 nPPVT67e_3 nPPVT68e_3 nPPVT69e_3 nPPVT70e_3 nPPVT71e_3 nPPVT72e_3)
replace nPPVTSetErrors6e_3 = . if missingset6e >3 

egen missingset7e = rowmiss(nPPVT73e_3 nPPVT74e_3 nPPVT75e_3 nPPVT76e_3 nPPVT77e_3 nPPVT78e_3 nPPVT79e_3 nPPVT80e_3 nPPVT81e_3 nPPVT82e_3 nPPVT83e_3 nPPVT84e_3)
egen nPPVTSetErrors7e_3 = rowtotal(nPPVT73e_3 nPPVT74e_3 nPPVT75e_3 nPPVT76e_3 nPPVT77e_3 nPPVT78e_3 nPPVT79e_3 nPPVT80e_3 nPPVT81e_3 nPPVT82e_3 nPPVT83e_3 nPPVT84e_3)
replace nPPVTSetErrors7e_3 = . if missingset7e >3 

egen missingset8e = rowmiss(nPPVT85e_3 nPPVT86e_3 nPPVT87e_3 nPPVT88e_3 nPPVT89e_3 nPPVT90e_3 nPPVT91e_3 nPPVT92e_3 nPPVT93e_3 nPPVT94e_3 nPPVT95e_3 nPPVT96e_3)
egen nPPVTSetErrors8e_3 = rowtotal(nPPVT85e_3 nPPVT86e_3 nPPVT87e_3 nPPVT88e_3 nPPVT89e_3 nPPVT90e_3 nPPVT91e_3 nPPVT92e_3 nPPVT93e_3 nPPVT94e_3 nPPVT95e_3 nPPVT96e_3)
replace nPPVTSetErrors8e_3 = . if missingset8e >3 

egen missingset9e = rowmiss(nPPVT97e_3 nPPVT98e_3 nPPVT99e_3 nPPVT100e_3 nPPVT101e_3 nPPVT102e_3 nPPVT103e_3 nPPVT104e_3 nPPVT105e_3 nPPVT106e_3 nPPVT107e_3 nPPVT108e_3)
egen nPPVTSetErrors9e_3 = rowtotal(nPPVT97e_3 nPPVT98e_3 nPPVT99e_3 nPPVT100e_3 nPPVT101e_3 nPPVT102e_3 nPPVT103e_3 nPPVT104e_3 nPPVT105e_3 nPPVT106e_3 nPPVT107e_3 nPPVT108e_3)
replace nPPVTSetErrors9e_3 = . if missingset9e >3 

egen missingset10e = rowmiss(nPPVT109e_3 nPPVT110e_3 nPPVT111e_3 nPPVT112e_3 nPPVT113e_3 nPPVT114e_3 nPPVT115e_3 nPPVT116e_3 nPPVT117e_3 nPPVT118e_3 nPPVT119e_3 nPPVT120e_3)
egen nPPVTSetErrors10e_3 = rowtotal(nPPVT109e_3 nPPVT110e_3 nPPVT111e_3 nPPVT112e_3 nPPVT113e_3 nPPVT114e_3 nPPVT115e_3 nPPVT116e_3 nPPVT117e_3 nPPVT118e_3 nPPVT119e_3 nPPVT120e_3)
replace nPPVTSetErrors10e_3 = . if missingset10e >3 

egen missingset11e = rowmiss(nPPVT121e_3 nPPVT122e_3 nPPVT123e_3 nPPVT124e_3 nPPVT125e_3 nPPVT126e_3 nPPVT127e_3 nPPVT128e_3 nPPVT129e_3 nPPVT130e_3 nPPVT131e_3 nPPVT132e_3)
egen nPPVTSetErrors11e_3 = rowtotal(nPPVT121e_3 nPPVT122e_3 nPPVT123e_3 nPPVT124e_3 nPPVT125e_3 nPPVT126e_3 nPPVT127e_3 nPPVT128e_3 nPPVT129e_3 nPPVT130e_3 nPPVT131e_3 nPPVT132e_3)
replace nPPVTSetErrors11e_3 = . if missingset11e >3 

egen missingset12e = rowmiss(nPPVT133e_3 nPPVT134e_3 nPPVT135e_3 nPPVT136e_3 nPPVT137e_3 nPPVT138e_3 nPPVT139e_3 nPPVT140e_3 nPPVT141e_3 nPPVT142e_3 nPPVT143e_3 nPPVT144e_3)
egen nPPVTSetErrors12e_3 = rowtotal(nPPVT133e_3 nPPVT134e_3 nPPVT135e_3 nPPVT136e_3 nPPVT137e_3 nPPVT138e_3 nPPVT139e_3 nPPVT140e_3 nPPVT141e_3 nPPVT142e_3 nPPVT143e_3 nPPVT144e_3)
replace nPPVTSetErrors12e_3 = . if missingset12e >3 

egen missingset13e = rowmiss(nPPVT145e_3 nPPVT146e_3 nPPVT147e_3 nPPVT148e_3 nPPVT149e_3 nPPVT150e_3 nPPVT151e_3 nPPVT152e_3 nPPVT153e_3 nPPVT154e_3 nPPVT155e_3 nPPVT156e_3)
egen nPPVTSetErrors13e_3 = rowtotal(nPPVT145e_3 nPPVT146e_3 nPPVT147e_3 nPPVT148e_3 nPPVT149e_3 nPPVT150e_3 nPPVT151e_3 nPPVT152e_3 nPPVT153e_3 nPPVT154e_3 nPPVT155e_3 nPPVT156e_3)
replace nPPVTSetErrors13e_3 = . if missingset13e >3 

egen missingset14e = rowmiss(nPPVT157e_3 nPPVT158e_3 nPPVT159e_3 nPPVT160e_3 nPPVT161e_3 nPPVT162e_3 nPPVT163e_3 nPPVT164e_3 nPPVT165e_3 nPPVT166e_3 nPPVT167e_3 nPPVT168e_3)
egen nPPVTSetErrors14e_3 = rowtotal(nPPVT157e_3 nPPVT158e_3 nPPVT159e_3 nPPVT160e_3 nPPVT161e_3 nPPVT162e_3 nPPVT163e_3 nPPVT164e_3 nPPVT165e_3 nPPVT166e_3 nPPVT167e_3 nPPVT168e_3)
replace nPPVTSetErrors14e_3 = . if missingset14e >3 

egen missingset15e = rowmiss(nPPVT169e_3 nPPVT170e_3 nPPVT171e_3 nPPVT172e_3 nPPVT173e_3 nPPVT174e_3 nPPVT175e_3 nPPVT176e_3 nPPVT177e_3 nPPVT178e_3 nPPVT179e_3 nPPVT180e_3)
egen nPPVTSetErrors15e_3 = rowtotal(nPPVT169e_3 nPPVT170e_3 nPPVT171e_3 nPPVT172e_3 nPPVT173e_3 nPPVT174e_3 nPPVT175e_3 nPPVT176e_3 nPPVT177e_3 nPPVT178e_3 nPPVT179e_3 nPPVT180e_3)
replace nPPVTSetErrors15e_3 = . if missingset15e >3 

gen nBasalsete_3 = .
replace ///
	nBasalsete_3 = 1 if nPPVTSetErrors1e_3 <=1 | (nPPVTSetErrors8e_3 == . & nPPVTSetErrors1e_3 >1 & nPPVTSetErrors1e_3 <=12 & nPPVTSetErrors2e_3 >1 & nPPVTSetErrors2e_3 <=12 & nPPVTSetErrors3e_3 >1 & nPPVTSetErrors3e_3 <=12 ///
		& nPPVTSetErrors4e_3 >1 & nPPVTSetErrors4e_3 <=12 & nPPVTSetErrors5e_3 >1 & nPPVTSetErrors5e_3 <=12 & nPPVTSetErrors6e_3 >1 & nPPVTSetErrors6e_3 <=12 & nPPVTSetErrors7e_3 >1 & nPPVTSetErrors7e_3 <=12) | ///
	(nPPVTSetErrors7e_3 == . & nPPVTSetErrors1e_3 >1 & nPPVTSetErrors1e_3 <=12 & nPPVTSetErrors2e_3 >1 & nPPVTSetErrors2e_3 <=12 & nPPVTSetErrors3e_3 >1 & nPPVTSetErrors3e_3 <=12 ///
		& nPPVTSetErrors4e_3 >1 & nPPVTSetErrors4e_3 <=12 & nPPVTSetErrors5e_3 >1 & nPPVTSetErrors5e_3 <=12 & nPPVTSetErrors6e_3 >1 & nPPVTSetErrors6e_3 <=12) | ///
	(nPPVTSetErrors6e_3 == . & nPPVTSetErrors1e_3 >1 & nPPVTSetErrors1e_3 <=12 & nPPVTSetErrors2e_3 >1 & nPPVTSetErrors2_3 <=12 & nPPVTSetErrors3e_3 >1 & nPPVTSetErrors3e_3 <=12 & nPPVTSetErrors4e_3 >1 & nPPVTSetErrors4e_3 <=12 ///
		& nPPVTSetErrors5e_3 >1 & nPPVTSetErrors5e_3 <=12) | ///
	(nPPVTSetErrors5e_3 == . & nPPVTSetErrors1e_3 >1 & nPPVTSetErrors1e_3 <=12 & nPPVTSetErrors2e_3 >1 & nPPVTSetErrors2e_3 <=12 & nPPVTSetErrors3e_3 >1 & nPPVTSetErrors3e_3 <=12 & nPPVTSetErrors4e_3 >1 ///
		& nPPVTSetErrors4e_3 <=12) | ///
	(nPPVTSetErrors4e_3 == . & nPPVTSetErrors1e_3 >1 & nPPVTSetErrors1e_3 <=12 & nPPVTSetErrors2e_3 >1 & nPPVTSetErrors2e_3 <=12 & nPPVTSetErrors3e_3 >1 & nPPVTSetErrors3e_3 <=12) | ///
	(nPPVTSetErrors3e_3 == . & nPPVTSetErrors1e_3 >1 & nPPVTSetErrors1e_3 <=12 & nPPVTSetErrors2e_3 >1 & nPPVTSetErrors2e_3 <=12) | ///
	(nPPVTSetErrors2e_3 == . & nPPVTSetErrors1e_3 >1 & nPPVTSetErrors1e_3 <=12) 
replace nBasalsete_3 = 2 if nBasalsete_3 >1 & nPPVTSetErrors2e_3 <=1
replace nBasalsete_3 = 3 if nBasalsete_3 >2 & nPPVTSetErrors3e_3 <=1
replace nBasalsete_3 = 4 if nBasalsete_3 >3 & nPPVTSetErrors4e_3 <=1
replace nBasalsete_3 = 5 if nBasalsete_3 >4 & nPPVTSetErrors5e_3 <=1
replace nBasalsete_3 = 6 if nBasalsete_3 >5 & nPPVTSetErrors6e_3 <=1
replace nBasalsete_3 = 7 if nBasalsete_3 >6 & nPPVTSetErrors7e_3 <=1
replace nBasalsete_3 = 8 if nBasalsete_3 >7 & nPPVTSetErrors8e_3 <=1
replace nBasalsete_3 = 9 if nBasalsete_3 >8 & nPPVTSetErrors9e_3 <=1
replace nBasalsete_3 = 10 if nBasalsete_3 >9 & nPPVTSetErrors10e_3 <=1
replace nBasalsete_3 = 11 if nBasalsete_3 >10 & nPPVTSetErrors11e_3 <=1
replace nBasalsete_3 = 12 if nBasalsete_3 >11 & nPPVTSetErrors12e_3 <=1
replace nBasalsete_3 = 13 if nBasalsete_3 >12 & nPPVTSetErrors13e_3 <=1
replace nBasalsete_3 = 14 if nBasalsete_3 >13 & nPPVTSetErrors14e_3 <=1
replace nBasalsete_3 = 15 if nBasalsete_3 >14 & nPPVTSetErrors15e_3 <=1
	
gen nCeilingsete_3 = 0
replace nCeilingsete_3 = 15 if nPPVTSetErrors15e_3 >=8 & nPPVTSetErrors15e_3 <=12
replace nCeilingsete_3 = 14 if nCeilingsete_3 <15 & nPPVTSetErrors14e_3 >=8 & nPPVTSetErrors14e_3 <=12
replace nCeilingsete_3 = 13 if nCeilingsete_3 <14 & nPPVTSetErrors13e_3 >=8 & nPPVTSetErrors13e_3 <=12
replace nCeilingsete_3 = 12 if nCeilingsete_3 <13 & nPPVTSetErrors12e_3 >=8 & nPPVTSetErrors12e_3 <=12
replace nCeilingsete_3 = 11 if nCeilingsete_3 <12 & nPPVTSetErrors11e_3 >=8 & nPPVTSetErrors11e_3 <=12
replace nCeilingsete_3 = 10 if nCeilingsete_3 <11 & nPPVTSetErrors10e_3 >=8 & nPPVTSetErrors10e_3 <=12
replace nCeilingsete_3 = 9 if nCeilingsete_3 <10 & nPPVTSetErrors9e_3 >=8 & nPPVTSetErrors9e_3 <=12
replace nCeilingsete_3 = 8 if nCeilingsete_3 <9 & nPPVTSetErrors8e_3 >=8 & nPPVTSetErrors8e_3 <=12
replace nCeilingsete_3 = 7 if nCeilingsete_3 <8 & nPPVTSetErrors7e_3 >=8 & nPPVTSetErrors7e_3 <=12
replace nCeilingsete_3 = 6 if nCeilingsete_3 <7 & nPPVTSetErrors6e_3 >=8 & nPPVTSetErrors6e_3 <=12
replace nCeilingsete_3 = 5 if nCeilingsete_3 <6 & nPPVTSetErrors5e_3 >=8 & nPPVTSetErrors5e_3 <=12
replace nCeilingsete_3 = 4 if nCeilingsete_3 <5 & nPPVTSetErrors4e_3 >=8 & nPPVTSetErrors4e_3 <=12
replace nCeilingsete_3 = 3 if nCeilingsete_3 <4 & nPPVTSetErrors3e_3 >=8 & nPPVTSetErrors3e_3 <=12
replace nCeilingsete_3 = 2 if nCeilingsete_3 <3 & nPPVTSetErrors2e_3 >=8 & nPPVTSetErrors2e_3 <=12
replace ///
	nCeilingsete_3 = 1 if (nCeilingsete_3 <2 & nPPVTSetErrors1e_3 >=8 & nPPVTSetErrors1e_3 <=12) | ///
	(nPPVTSetErrors9e_3 == . & nPPVTSetErrors1e_3 <8 & nPPVTSetErrors2e_3 <8 & nPPVTSetErrors3e_3 <8 & nPPVTSetErrors4e_3 <8 & nPPVTSetErrors5e_3 <8 & nPPVTSetErrors6e_3 <8 & nPPVTSetErrors7e_3 <8 & nPPVTSetErrors8e_3 <8) | ///
	(nPPVTSetErrors8e_3 == . & nPPVTSetErrors1e_3 <8 & nPPVTSetErrors2e_3 <8 & nPPVTSetErrors3e_3 <8 & nPPVTSetErrors4e_3 <8 & nPPVTSetErrors5e_3 <8 & nPPVTSetErrors6e_3 <8 & nPPVTSetErrors7e_3 <8) | ///
	(nPPVTSetErrors7e_3 == . & nPPVTSetErrors1e_3 <8 & nPPVTSetErrors2e_3 <8 & nPPVTSetErrors3e_3 <8 & nPPVTSetErrors4e_3 <8 & nPPVTSetErrors5e_3 <8 & nPPVTSetErrors6e_3 <8) | ///
	(nPPVTSetErrors6e_3 == . & nPPVTSetErrors1e_3 <8 & nPPVTSetErrors2e_3 <8 & nPPVTSetErrors3e_3 <8 & nPPVTSetErrors4e_3 <8 & nPPVTSetErrors5e_3 <8) | ///
	(nPPVTSetErrors5e_3 == . & nPPVTSetErrors1e_3 <8 & nPPVTSetErrors2e_3 <8 & nPPVTSetErrors3e_3 <8 & nPPVTSetErrors4e_3 <8) | ///
	(nPPVTSetErrors4e_3 == . & nPPVTSetErrors1e_3 <8 & nPPVTSetErrors2e_3 <8 & nPPVTSetErrors3e_3 <8) | ///
	(nPPVTSetErrors3e_3 == . & nPPVTSetErrors1e_3 <8 & nPPVTSetErrors2e_3 <8) | ///
	(nPPVTSetErrors2e_3 == . & nPPVTSetErrors1e_3 <8) 
replace nCeilingsete_3 = . if nCeilingsete_3 == 0

gen nCeilingiteme_3 = .
replace nCeilingiteme_3 = 12 if nCeilingsete_3 == 1
replace nCeilingiteme_3 = 24 if nCeilingsete_3 == 2
replace nCeilingiteme_3 = 36 if nCeilingsete_3 == 3
replace nCeilingiteme_3 = 48 if nCeilingsete_3 == 4
replace nCeilingiteme_3 = 60 if nCeilingsete_3 == 5
replace nCeilingiteme_3 = 72 if nCeilingsete_3 == 6
replace nCeilingiteme_3 = 84 if nCeilingsete_3 == 7
replace nCeilingiteme_3 = 96 if nCeilingsete_3 == 8
replace nCeilingiteme_3 = 108 if nCeilingsete_3 == 9
replace nCeilingiteme_3 = 120 if nCeilingsete_3 == 10
replace nCeilingiteme_3 = 132 if nCeilingsete_3 == 11
replace nCeilingiteme_3 = 144 if nCeilingsete_3 == 12
replace nCeilingiteme_3 = 156 if nCeilingsete_3 == 13
replace nCeilingiteme_3 = 168 if nCeilingsete_3 == 14
replace nCeilingiteme_3 = 180 if nCeilingsete_3 == 15

gen nPPVTScore2_3=. 
forval i=1/7 { 
	forval j=`i'/15 { 
		egen toterrors=rowtotal(nPPVTSetErrors`i'e_3-nPPVTSetErrors`j'e_3) if nCeilingsete_3==`j' & nBasalsete_3==`i' 
		replace nPPVTScore2_3=nCeilingiteme_3-toterrors if nCeilingsete_3==`j' & nBasalsete_3==`i' 
		drop toterrors 
		} 
	}

keep nPPVTScore_3 nPPVTScore2_3 nSUBID

save "${project_directory}\data\phdcn\v01_phdcn_ppvt.dta", replace

erase "${project_directory}\data\_TEMP\PPVTFileAgeWave3.dta"

log close
