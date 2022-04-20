sink("D:\\projects\\nhood_mediation_lead\\programs\\appendix_e\\_LOGS\\01_create_figure_e1_log.txt")

print(Sys.time())

################################################
################################################
##                                            ##
## PROGRAM NAME: 01_create_figure_e1          ##
## AUTHOR: GW                                 ##
## DATE: 5/24/2021                            ##
## DESCRIPTION:                               ##
##                                            ##  
##  creates density plots of blood lead       ##
##  prevalence over time                      ##
##                                            ##
################################################
################################################

rm(list=ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(haven)
library(foreign)
library(mgcv)

### LOAD BLDB ###
bldb<-read.dta("D:\\projects\\nhood_mediation_lead\\data\\cdph_bldb\\v01_bldb_nc.dta")
vars<-c("nlinknc","nyear","nprbllgt5sm")
bldb<-bldb[,vars]

### COMPUTE DENSITIES ###
d95<-density(bldb$nprbllgt5sm[bldb$nyear==1995],bw=0.05,from=0,to=1)
d00<-density(bldb$nprbllgt5sm[bldb$nyear==2000],bw=0.05,from=0,to=1)
d05<-density(bldb$nprbllgt5sm[bldb$nyear==2005],bw=0.05,from=0,to=1)
d10<-density(bldb$nprbllgt5sm[bldb$nyear==2010],bw=0.05,from=0,to=1)

d95$y<-d95$y/sum(d95$y)
d00$y<-d00$y/sum(d00$y)
d05$y<-d05$y/sum(d05$y)
d10$y<-d10$y/sum(d10$y)

### PLOT DENSITIES ###
tiff("D:\\projects\\nhood_mediation_lead\\figures\\figure_e1.tiff",
	width=5,
	height=5,
	units='in',
	res=600)

plot(d95,
	main="",
	xlab="Elevated Blood Lead Prevalence",
	ylab="Density",
	xlim=c(0.0,1.0),
	ylim=c(0.0,0.017),
	lwd=1.5)

par(new=T)

plot(d00,
	main="",
	xlab="",
	ylab="",
	xlim=c(0.0,1.0),
	ylim=c(0.0,0.017),
	lty="dashed",
	lwd=1.5,
	axes=FALSE)

par(new=T)

plot(d05,
	main="",
	xlab="",
	ylab="",
	xlim=c(0.0,1.0),
	ylim=c(0.0,0.017),
	lty="dotted",
	lwd=1.5,
	axes=FALSE)

par(new=T)

plot(d10,
	main="",
	xlab="",
	ylab="",
	xlim=c(0.0,1.0),
	ylim=c(0.0,0.017),
	lty="dotdash",
	lwd=1.5,
	axes=FALSE)

legend("topright",inset=0.075,c("1995","2000","2005","2010"),lty=c(1,2,3,4),seg.len=4)

dev.off()

print(Sys.time())

sink()
