sink("D:\\projects\\nhood_mediation_lead\\programs\\_LOGS\\13_create_figure_3_log.txt")

################################################
################################################
##                                            ##
## PROGRAM NAME: 13_create_figure_3           ##
## AUTHOR: GW                                 ##
## DATE: 8/7/2020                             ##
## DESCRIPTION:                               ##
##                                            ##
##  creates scatterplot of blood lead         ##
##  prevalence and nhood dadvg level          ##
##                                            ##
################################################
################################################

rm(list=ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(haven)
library(mgcv)

### LOAD BLDB ###
bldb<-read_dta("D:\\projects\\nhood_mediation_lead\\data\\_TEMP\\bldb_temp.dta")
vars<-c("ntract","nyear","nprbllgt5sm","nsmobs")
bldb<-bldb[which((bldb$nyear==1997)),vars]

### LOAD NCDB ###
ncdb<-read_dta("D:\\projects\\nhood_mediation_lead\\data\\_TEMP\\ncdb_temp.dta")
vars<-c("ntract","nyear","ncondadvg")
ncdb<-ncdb[which((ncdb$nyear==1997)),vars]

### MERGE BLDB WITH NCDB ###
blncdb<-merge(bldb,ncdb,by="ntract")
blncdb<-blncdb[which((blncdb$nsmobs>10)),]

### SCATTERPLOT ###
scfit<-gam(nprbllgt5sm~s(ncondadvg),gamma=12,data=blncdb)
xvals<-seq(-1.6,3.0,by=0.05)
ndata<-data.frame(ncondadvg=xvals)
nfit<-predict(scfit,newdata=ndata,se.fit=T)

#tiff
tiff("D:\\projects\\nhood_mediation_lead\\figures\\figure_3.tiff",
	width=5,
	height=5,
	units='in',
	res=600)

plot(blncdb$ncondadvg,blncdb$nprbllgt5sm,
	ylim=c(0,1.0),
	xlim=c(-1.9,3.1),
	yaxt="n",
	ylab=("Elevated Blood-Lead Prevalence"),
	xlab=("Concentrated Disadvantage"),
	col="grey",
	font.lab=2)

axis(2, at=c(0.0,0.2,0.4,0.6,0.8,1.0), labels=c("0",".2",".4",".6",".8","1"), las=2)

lines(ndata$ncondadvg,nfit$fit,type="l",lty="solid")
lines(ndata$ncondadvg,(nfit$fit+1.96*nfit$se.fit),type="l",lty="dotted")
lines(ndata$ncondadvg,(nfit$fit-1.96*nfit$se.fit),type="l",lty="dotted")

dev.off()

print(Sys.time())

sink()

file.remove("D:\\projects\\nhood_mediation_lead\\data\\_TEMP\\ncdb_temp.dta")
file.remove("D:\\projects\\nhood_mediation_lead\\data\\_TEMP\\bldb_temp.dta")