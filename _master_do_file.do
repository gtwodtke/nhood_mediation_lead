capture clear all

global prgmpath "D:\projects\nhood_mediation_lead\programs\" 

/***DATA PROCESSING***/
do "${prgmpath}01_create_v01_phdcn_master.do" nostop

do "${prgmpath}02_create_v01_phdcn_demo.do" nostop

do "${prgmpath}03_create_v01_phdcn_header.do" nostop

do "${prgmpath}04_create_v01_phdcn_ppvt.do" nostop

do "${prgmpath}05_create_v01_phdcn_tractlink.do" nostop

do "${prgmpath}06_create_v01_bldb_nc.do" nostop

do "${prgmpath}07_create_v01_ncdb_nc.do" nostop

do "${prgmpath}08_create_v01_phdcn_merged.do" nostop

do "${prgmpath}09_create_v02_phdcn_merged_mi.do" nostop

/***TABLES AND FIGURES***/
do "${prgmpath}10_create_table_1.do" nostop

do "${prgmpath}11_create_table_2.do" nostop

do "${prgmpath}12_create_figure_2.do" nostops

shell "D:\Program Files\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}13_create_figure_3.R"
shell DEL "${prgmpath}13_create_figure_3.Rout"

shell "D:\Program Files\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}14_create_table_3.R"
shell DEL "${prgmpath}14_create_table_3.Rout"

shell "D:\Program Files\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}15_create_figure_4.R"
shell DEL "${prgmpath}15_create_figure_4.Rout"

do "${prgmpath}16_create_misc_stats.do" nostops

/***APPENDICES***/
/*PART A*/
shell "D:\Program Files\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}\appendix_a\01_create_figure_a1.R"
shell DEL "${prgmpath}\appendix_a\01_create_figure_a1.Rout"

/*PART B*/
shell "D:\Program Files\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}\appendix_b\01_create_table_b1.R"
shell DEL "${prgmpath}\appendix_b\01_create_table_b1.Rout"

/*PART C*/
do "${prgmpath}\appendix_c\01_create_v03_phdcn_merged_mi_6to12.do" nostop

shell "D:\Program Files\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}\appendix_c\02_create_table_c1.R"
shell DEL "${prgmpath}\appendix_c\02_create_table_c1.Rout"

/*PART D*/
shell "D:\Program Files\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}\appendix_d\01_create_table_d1.R"
shell DEL "${prgmpath}\appendix_d\01_create_table_d1.Rout"

shell "D:\Program Files\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}\appendix_d\02_create_table_d2.R"
shell DEL "${prgmpath}\appendix_d\02_create_table_d2.Rout"

shell "D:\Program Files\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}\appendix_d\03_create_table_d3.R"
shell DEL "${prgmpath}\appendix_d\03_create_table_d3.Rout"

/*PART E*/
shell "D:\Program Files\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}\appendix_e\01_create_figure_e1.R"
shell DEL "${prgmpath}\appendix_e\01_create_figure_e1.Rout"
