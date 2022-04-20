sink("D:\\projects\\nhood_mediation_lead\\programs\\_LOGS\\15_create_figure_4_log.txt")

print(Sys.time())

################################################
################################################
##                                            ##
## PROGRAM NAME: 15_create_figure_4           ##
## AUTHOR: GW                                 ##
## DATE: 11/08/2021                           ##
## DESCRIPTION:                               ##
##                                            ##
##  sensitivity analysis                      ##
##                                            ##
################################################
################################################

rm(list=ls())
library(haven)
library(foreign)
library(ebal)
library(dplyr)
library(tidyr)
library(CBPS)
library(IDPmisc)
library(ggplot2)
library(survey)
library(mgcv)
library(geeM)

##### LOAD PHDCN #####
phdcnmi<-read.dta("D:\\projects\\nhood_mediation_lead\\data\\v02_phdcn_merged_mi.dta")
phdcnmi<-phdcnmi[which((phdcnmi$"_mj"!=0)),]

##### SET ESTIMATION PARAMETERS #####
set.seed(8675309)
nmi<-50
nboot<-500
astar<-0.70
a<-(-0.90)
mstar<-0.65
m<-0.30

##### SCALE SENSITIVITY PARAMETERS #####
miest.tau<-miest.phi<-miest.psi<-matrix(data=NA,nrow=nmi,ncol=1)

for (i in 1:nmi) {

	phdcn<-phdcnmi[which(phdcnmi$"_mj"==i),]

	resid_l1<-function(y) { residuals(lm(y~1,data=phdcn)) }
	resid_l2<-function(y) { residuals(lm(y~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
				   			   nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1+nbl5ug_1,data=phdcn)) }
	resid_l3<-function(y) { residuals(lm(y~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
				   			   nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1+nbl5ug_1+
							   nlninc_2+npcemply_2+npcwelf_2+npcmarr_2+npcengl_2+ncondadvg_2+nbl5ug_2,data=phdcn)) }
	phdcn<-phdcn %>% mutate_at(vars(npcage_b,npchsgrad_b,npcsomcol_b,npccolgrd_b,nownhome_b,nfemale_b,nblack_b,nhispan_b,nothrace_b,nfamsize_b), funs(r=resid_l1))
	phdcn<-phdcn %>% mutate_at(vars(nlninc_1,npcemply_1,npcwelf_1,npcmarr_1,npcengl_1), funs(r=resid_l1))
	phdcn<-phdcn %>% mutate_at(vars(nlninc_2,npcemply_2,npcwelf_2,npcmarr_2,npcengl_2), funs(r=resid_l2))
	phdcn<-phdcn %>% mutate_at(vars(nlninc_3,npcemply_3,npcwelf_3,npcmarr_3,npcengl_3), funs(r=resid_l3))
	phdcn$nbl5ug_1_r<-residuals(lm(nbl5ug_1~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
							    nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1,data=phdcn))
	phdcn$nbl5ug_2_r<-residuals(lm(nbl5ug_2~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
							    nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1+nbl5ug_1+
							    nlninc_2+npcemply_2+npcwelf_2+npcmarr_2+npcengl_2+ncondadvg_2,data=phdcn)) 
	phdcn$nsubage_3_r<-residuals(lm(nsubage_3~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
						    		nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1+nbl5ug_1+
								nlninc_2+npcemply_2+npcwelf_2+npcmarr_2+npcengl_2+ncondadvg_2+nbl5ug_2+
								nlninc_3+npcemply_3+npcwelf_3+npcmarr_3+npcengl_3+ncondadvg_3+nbl5ug_3,data=phdcn))

	phdcn$nlninc_s_r<-phdcn$nlninc_1_r+phdcn$nlninc_2_r+phdcn$nlninc_3_r
	phdcn$npcemply_s_r<-phdcn$npcemply_1_r+phdcn$npcemply_2_r+phdcn$npcemply_3_r
	phdcn$npcwelf_s_r<-phdcn$npcwelf_1_r+phdcn$npcwelf_2_r+phdcn$npcwelf_3_r
	phdcn$npcmarr_s_r<-phdcn$npcmarr_1_r+phdcn$npcmarr_2_r+phdcn$npcmarr_3_r
	phdcn$npcengl_s_r<-phdcn$npcengl_1_r+phdcn$npcengl_2_r+phdcn$npcengl_3_r
	phdcn$nbl5ug_s_r<-phdcn$nbl5ug_1_r+phdcn$nbl5ug_2_r
	phdcn$cumndadvg<-(phdcn$ncondadvg_1+phdcn$ncondadvg_2+phdcn$ncondadvg_3)/3
	phdcn$cumnbl5ug<-(phdcn$nbl5ug_1+phdcn$nbl5ug_2+phdcn$nbl5ug_3)/3
	phdcn$nppvt_3<-(phdcn$nppvt_3-mean(phdcn$nppvt_3))/sd(phdcn$nppvt_3)

	m1.adj<-lm(nppvt_3~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
				 nsubage_3_r+nbl5ug_s_r+nlninc_s_r+npcemply_s_r+npcwelf_s_r+npcmarr_s_r+npcengl_s_r+cumndadvg,data=phdcn)

	m1.unadj<-lm(nppvt_3~npcage_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
				 nsubage_3_r+nbl5ug_s_r+nlninc_s_r+npcemply_s_r+npcwelf_s_r+npcmarr_s_r+npcengl_s_r+cumndadvg,data=phdcn)	

	m2.adj<-lm(nppvt_3~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
			      nsubage_3_r+nlninc_s_r+npcemply_s_r+npcwelf_s_r+npcmarr_s_r+npcengl_s_r+cumndadvg+cumnbl5ug,data=phdcn)

	m2.unadj<-lm(nppvt_3~npcage_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
				 nsubage_3_r+nlninc_s_r+npcemply_s_r+npcwelf_s_r+npcmarr_s_r+npcengl_s_r+cumndadvg+cumnbl5ug,data=phdcn)

	miest.tau[i,1]<-(m1.unadj$coefficients["cumndadvg"]-m1.adj$coefficients["cumndadvg"])/3
	miest.phi[i,1]<-(m2.unadj$coefficients["cumndadvg"]-m2.adj$coefficients["cumndadvg"])/3
	miest.psi[i,1]<-(m2.unadj$coefficients["cumnbl5ug"]-m2.adj$coefficients["cumnbl5ug"])/3
	}

tau<-round(mean(miest.tau[,1]),digits=3)
phi<-round(mean(miest.phi[,1]),digits=3)
psi<-round(mean(miest.psi[,1]),digits=3)

xTau<-seq(-1,2,by=(0.25))
xPhi<-seq(-1,2,by=(0.25))
xPsi<-seq(-1,2,by=(0.25))

##### COMPUTE BIAS-ADJUSTED EFFECT ESTIMATES #####
miest.rwr.ate<-mivar.rwr.ate<-matrix(data=NA,nrow=nmi,ncol=length(xTau))
miest.rwr.cde<-mivar.rwr.cde<-matrix(data=NA,nrow=nmi,ncol=length(xPhi))
miest.rwr.cme<-mivar.rwr.cme<-matrix(data=NA,nrow=nmi,ncol=length(xPhi))
miest.rwr.aje<-mivar.rwr.aje<-matrix(data=NA,nrow=nmi,ncol=1)
rwrest.aje<-matrix(data=0,nrow=length(xPhi),ncol=length(xPsi))

### BIAS-ADJUSTED ATE ###
for (i in 1:nmi) {

	phdcn<-phdcnmi[which(phdcnmi$"_mj"==i),]

	resid_l1<-function(y) { residuals(lm(y~1,data=phdcn)) }
	resid_l2<-function(y) { residuals(lm(y~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
				   			   nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1+nbl5ug_1,data=phdcn)) }
	resid_l3<-function(y) { residuals(lm(y~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
				   			   nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1+nbl5ug_1+
							   nlninc_2+npcemply_2+npcwelf_2+npcmarr_2+npcengl_2+ncondadvg_2+nbl5ug_2,data=phdcn)) }
	phdcn<-phdcn %>% mutate_at(vars(npcage_b,npchsgrad_b,npcsomcol_b,npccolgrd_b,nownhome_b,nfemale_b,nblack_b,nhispan_b,nothrace_b,nfamsize_b), funs(r=resid_l1))
	phdcn<-phdcn %>% mutate_at(vars(nlninc_1,npcemply_1,npcwelf_1,npcmarr_1,npcengl_1), funs(r=resid_l1))
	phdcn<-phdcn %>% mutate_at(vars(nlninc_2,npcemply_2,npcwelf_2,npcmarr_2,npcengl_2), funs(r=resid_l2))
	phdcn<-phdcn %>% mutate_at(vars(nlninc_3,npcemply_3,npcwelf_3,npcmarr_3,npcengl_3), funs(r=resid_l3))
	phdcn$nbl5ug_1_r<-residuals(lm(nbl5ug_1~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
							    nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1,data=phdcn))
	phdcn$nbl5ug_2_r<-residuals(lm(nbl5ug_2~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
							    nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1+nbl5ug_1+
							    nlninc_2+npcemply_2+npcwelf_2+npcmarr_2+npcengl_2+ncondadvg_2,data=phdcn)) 
	phdcn$nbl5ug_3_r<-residuals(lm(nbl5ug_3~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
						    		nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1+nbl5ug_1+
								nlninc_2+npcemply_2+npcwelf_2+npcmarr_2+npcengl_2+ncondadvg_2+nbl5ug_2+
								nlninc_3+npcemply_3+npcwelf_3+npcmarr_3+npcengl_3+ncondadvg_3,data=phdcn))
	phdcn$nsubage_3_r<-residuals(lm(nsubage_3~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
						    		nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1+nbl5ug_1+
								nlninc_2+npcemply_2+npcwelf_2+npcmarr_2+npcengl_2+ncondadvg_2+nbl5ug_2+
								nlninc_3+npcemply_3+npcwelf_3+npcmarr_3+npcengl_3+ncondadvg_3+nbl5ug_3,data=phdcn))

	phdcn$nlninc_s_r<-phdcn$nlninc_1_r+phdcn$nlninc_2_r+phdcn$nlninc_3_r
	phdcn$npcemply_s_r<-phdcn$npcemply_1_r+phdcn$npcemply_2_r+phdcn$npcemply_3_r
	phdcn$npcwelf_s_r<-phdcn$npcwelf_1_r+phdcn$npcwelf_2_r+phdcn$npcwelf_3_r
	phdcn$npcmarr_s_r<-phdcn$npcmarr_1_r+phdcn$npcmarr_2_r+phdcn$npcmarr_3_r
	phdcn$npcengl_s_r<-phdcn$npcengl_1_r+phdcn$npcengl_2_r+phdcn$npcengl_3_r
	phdcn$nbl5ug_s_r<-phdcn$nbl5ug_1_r+phdcn$nbl5ug_2_r+phdcn$nbl5ug_3_r
	phdcn$cumndadvg<-(phdcn$ncondadvg_1+phdcn$ncondadvg_2+phdcn$ncondadvg_3)/3
	phdcn$cumnbl5ug<-(phdcn$nbl5ug_1+phdcn$nbl5ug_2+phdcn$nbl5ug_3)/3
	phdcn$nppvt_3<-(phdcn$nppvt_3-mean(phdcn$nppvt_3))/sd(phdcn$nppvt_3)

	aprime1<-sort(phdcn$ncondadvg_1)
	aprime2<-sort(phdcn$ncondadvg_2)
	aprime3<-sort(phdcn$ncondadvg_3)

	b1<-rep(0,length(phdcn$ncondadvg_1))
	b2<-rep(0,length(phdcn$ncondadvg_2))
	b3<-rep(0,length(phdcn$ncondadvg_3))

	for (q in aprime1) { b1<-b1+(phdcn$ncondadvg_1-q)*tau*(1/length(aprime1)) }
	for (q in aprime2) { b2<-b2+(phdcn$ncondadvg_2-q)*tau*(1/length(aprime2)) }
	for (q in aprime3) { b3<-b3+(phdcn$ncondadvg_3-q)*tau*(1/length(aprime3)) }

	for (j in 1:length(xTau)) {
		phdcn$y_bias<-phdcn$nppvt_3-(b1+b2+b3)*xTau[j]
		m1.bias<-lm(y_bias~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
			             nsubage_3_r+nbl5ug_s_r+nlninc_s_r+npcemply_s_r+npcwelf_s_r+npcmarr_s_r+npcengl_s_r+cumndadvg,data=phdcn)
		miest.rwr.ate[i,j]<-m1.bias$coefficients["cumndadvg"]*(astar-a)
		}

	boot.ate.rwr<-matrix(data=NA,nrow=nboot,ncol=1)
	
	for (j in 1:nboot) {
		boot.phdcn<-NULL
		for (s in 1:16) {
			phdcn.strata<-phdcn[which(phdcn$nstrata==s),]
			idboot.1<-sample(unique(phdcn.strata$nlinknc_1),length(unique(phdcn.strata$nlinknc_1))-1,replace=T)
			idboot.2<-table(idboot.1)
			boot.phdcn.strata<-NULL
			for (k in 1:max(idboot.2)) {
				boot.data<-phdcn.strata[phdcn.strata$nlinknc_1 %in% names(idboot.2[idboot.2 %in% k]),]
				for (l in 1:k) { boot.phdcn.strata<-rbind(boot.phdcn.strata,boot.data) }
				}
			boot.phdcn<-rbind(boot.phdcn,boot.phdcn.strata)
			}

		boot.resid_l1<-function(y) { residuals(lm(y~1,data=boot.phdcn)) }
		boot.resid_l2<-function(y) { residuals(lm(y~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
								   nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1+nbl5ug_1,data=boot.phdcn)) }
		boot.resid_l3<-function(y) { residuals(lm(y~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
								   nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1+nbl5ug_1+
								   nlninc_2+npcemply_2+npcwelf_2+npcmarr_2+npcengl_2+ncondadvg_2+nbl5ug_2,data=boot.phdcn)) }
		boot.phdcn<-boot.phdcn %>% mutate_at(vars(npcage_b,npchsgrad_b,npcsomcol_b,npccolgrd_b,nownhome_b,nfemale_b,nblack_b,nhispan_b,nothrace_b,nfamsize_b), funs(r=boot.resid_l1))
		boot.phdcn<-boot.phdcn %>% mutate_at(vars(nlninc_1,npcemply_1,npcwelf_1,npcmarr_1,npcengl_1), funs(r=boot.resid_l1))
		boot.phdcn<-boot.phdcn %>% mutate_at(vars(nlninc_2,npcemply_2,npcwelf_2,npcmarr_2,npcengl_2), funs(r=boot.resid_l2))
		boot.phdcn<-boot.phdcn %>% mutate_at(vars(nlninc_3,npcemply_3,npcwelf_3,npcmarr_3,npcengl_3), funs(r=boot.resid_l3))
		boot.phdcn$nbl5ug_1_r<-residuals(lm(nbl5ug_1~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
									   nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1,data=boot.phdcn))
		boot.phdcn$nbl5ug_2_r<-residuals(lm(nbl5ug_2~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
									   nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1+nbl5ug_1+
									   nlninc_2+npcemply_2+npcwelf_2+npcmarr_2+npcengl_2+ncondadvg_2,data=boot.phdcn)) 
		boot.phdcn$nbl5ug_3_r<-residuals(lm(nbl5ug_3~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
						    			   nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1+nbl5ug_1+
									   nlninc_2+npcemply_2+npcwelf_2+npcmarr_2+npcengl_2+ncondadvg_2+nbl5ug_2+
									   nlninc_3+npcemply_3+npcwelf_3+npcmarr_3+npcengl_3+ncondadvg_3,data=boot.phdcn))
		boot.phdcn$nsubage_3_r<-residuals(lm(nsubage_3~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
									     nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1+nbl5ug_1+
									     nlninc_2+npcemply_2+npcwelf_2+npcmarr_2+npcengl_2+ncondadvg_2+nbl5ug_2+
									     nlninc_3+npcemply_3+npcwelf_3+npcmarr_3+npcengl_3+ncondadvg_3+nbl5ug_3,data=boot.phdcn))

		boot.phdcn$nlninc_s_r<-boot.phdcn$nlninc_1_r+boot.phdcn$nlninc_2_r+boot.phdcn$nlninc_3_r
		boot.phdcn$npcemply_s_r<-boot.phdcn$npcemply_1_r+boot.phdcn$npcemply_2_r+boot.phdcn$npcemply_3_r
		boot.phdcn$npcwelf_s_r<-boot.phdcn$npcwelf_1_r+boot.phdcn$npcwelf_2_r+boot.phdcn$npcwelf_3_r
		boot.phdcn$npcmarr_s_r<-boot.phdcn$npcmarr_1_r+boot.phdcn$npcmarr_2_r+boot.phdcn$npcmarr_3_r
		boot.phdcn$npcengl_s_r<-boot.phdcn$npcengl_1_r+boot.phdcn$npcengl_2_r+boot.phdcn$npcengl_3_r
		boot.phdcn$nbl5ug_s_r<-boot.phdcn$nbl5ug_1_r+boot.phdcn$nbl5ug_2_r+boot.phdcn$nbl5ug_3_r

		boot.m1<-lm(nppvt_3~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
					  nsubage_3_r+nbl5ug_s_r+nlninc_s_r+npcemply_s_r+npcwelf_s_r+npcmarr_s_r+npcengl_s_r+cumndadvg,data=boot.phdcn)

		boot.ate.rwr[j,1]<-boot.m1$coefficients["cumndadvg"]*(astar-a)
		}
	
	for (k in 1:length(xTau)) { mivar.rwr.ate[i,k]<-var(boot.ate.rwr[,1]) }
	}

### BIAS-ADJUSTED AJE ###
for (i in 1:nmi) {

	phdcn<-phdcnmi[which(phdcnmi$"_mj"==i),]

	resid_l1<-function(y) { residuals(lm(y~1,data=phdcn)) }
	resid_l2<-function(y) { residuals(lm(y~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
				   			   nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1+nbl5ug_1,data=phdcn)) }
	resid_l3<-function(y) { residuals(lm(y~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
				   			   nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1+nbl5ug_1+
							   nlninc_2+npcemply_2+npcwelf_2+npcmarr_2+npcengl_2+ncondadvg_2+nbl5ug_2,data=phdcn)) }
	phdcn<-phdcn %>% mutate_at(vars(npcage_b,npchsgrad_b,npcsomcol_b,npccolgrd_b,nownhome_b,nfemale_b,nblack_b,nhispan_b,nothrace_b,nfamsize_b), funs(r=resid_l1))
	phdcn<-phdcn %>% mutate_at(vars(nlninc_1,npcemply_1,npcwelf_1,npcmarr_1,npcengl_1), funs(r=resid_l1))
	phdcn<-phdcn %>% mutate_at(vars(nlninc_2,npcemply_2,npcwelf_2,npcmarr_2,npcengl_2), funs(r=resid_l2))
	phdcn<-phdcn %>% mutate_at(vars(nlninc_3,npcemply_3,npcwelf_3,npcmarr_3,npcengl_3), funs(r=resid_l3))
	phdcn$nsubage_3_r<-residuals(lm(nsubage_3~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
						    		nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1+nbl5ug_1+
								nlninc_2+npcemply_2+npcwelf_2+npcmarr_2+npcengl_2+ncondadvg_2+nbl5ug_2+
								nlninc_3+npcemply_3+npcwelf_3+npcmarr_3+npcengl_3+ncondadvg_3+nbl5ug_3,data=phdcn))

	phdcn$nlninc_s_r<-phdcn$nlninc_1_r+phdcn$nlninc_2_r+phdcn$nlninc_3_r
	phdcn$npcemply_s_r<-phdcn$npcemply_1_r+phdcn$npcemply_2_r+phdcn$npcemply_3_r
	phdcn$npcwelf_s_r<-phdcn$npcwelf_1_r+phdcn$npcwelf_2_r+phdcn$npcwelf_3_r
	phdcn$npcmarr_s_r<-phdcn$npcmarr_1_r+phdcn$npcmarr_2_r+phdcn$npcmarr_3_r
	phdcn$npcengl_s_r<-phdcn$npcengl_1_r+phdcn$npcengl_2_r+phdcn$npcengl_3_r
	phdcn$cumndadvg<-(phdcn$ncondadvg_1+phdcn$ncondadvg_2+phdcn$ncondadvg_3)/3
	phdcn$cumnbl5ug<-(phdcn$nbl5ug_1+phdcn$nbl5ug_2+phdcn$nbl5ug_3)/3
	phdcn$nppvt_3<-(phdcn$nppvt_3-mean(phdcn$nppvt_3))/sd(phdcn$nppvt_3)

	aprime1<-sort(phdcn$ncondadvg_1)
	aprime2<-sort(phdcn$ncondadvg_2)
	aprime3<-sort(phdcn$ncondadvg_3)

	b1a<-rep(0,length(phdcn$ncondadvg_1))
	b2a<-rep(0,length(phdcn$ncondadvg_2))
	b3a<-rep(0,length(phdcn$ncondadvg_3))

	for (q in aprime1) { b1a<-b1a+(phdcn$ncondadvg_1-q)*phi*(1/length(aprime1)) }
	for (q in aprime2) { b2a<-b2a+(phdcn$ncondadvg_2-q)*phi*(1/length(aprime2)) }
	for (q in aprime3) { b3a<-b3a+(phdcn$ncondadvg_3-q)*phi*(1/length(aprime3)) }

	mprime1<-sort(phdcn$nbl5ug_1)
	mprime2<-sort(phdcn$nbl5ug_2)
	mprime3<-sort(phdcn$nbl5ug_3)

	b1m<-rep(0,length(phdcn$nbl5ug_1))
	b2m<-rep(0,length(phdcn$nbl5ug_2))
	b3m<-rep(0,length(phdcn$nbl5ug_3))

	for (q in mprime1) { b1m<-b1m+(phdcn$nbl5ug_1-q)*psi*(1/length(mprime1)) }
	for (q in mprime2) { b2m<-b2m+(phdcn$nbl5ug_2-q)*psi*(1/length(mprime2)) }
	for (q in mprime3) { b3m<-b3m+(phdcn$nbl5ug_3-q)*psi*(1/length(mprime3)) }

	for (j in 1:length(xPhi)) {
		for (k in 1:length(xPsi)) {
			phdcn$y_bias<-phdcn$nppvt_3-(((b1a+b2a+b3a)*xPhi[j])+((b1m+b2m+b3m)*xPsi[k]))
			m2.bias<-lm(y_bias~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
					nsubage_3_r+nlninc_s_r+npcemply_s_r+npcwelf_s_r+npcmarr_s_r+npcengl_s_r+cumndadvg+cumnbl5ug,data=phdcn)
			aje.bias<-m2.bias$coefficients["cumndadvg"]*(astar-a)+m2.bias$coefficients["cumnbl5ug"]*(mstar-m)
			rwrest.aje[j,k]<-rwrest.aje[j,k]+(aje.bias*(1/nmi))
			}
		}
	
	m2<-lm(phdcn$nppvt_3~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
				   nsubage_3_r+nlninc_s_r+npcemply_s_r+npcwelf_s_r+npcmarr_s_r+npcengl_s_r+cumndadvg+cumnbl5ug,data=phdcn)

	miest.rwr.aje[i,1]<-m2$coefficients["cumndadvg"]*(astar-a)+m2$coefficients["cumnbl5ug"]*(mstar-m)

	boot.aje.rwr<-matrix(data=NA,nrow=nboot,ncol=1)
	
	for (j in 1:nboot) {
		boot.phdcn<-NULL
		for (s in 1:16) {
			phdcn.strata<-phdcn[which(phdcn$nstrata==s),]
			idboot.1<-sample(unique(phdcn.strata$nlinknc_1),length(unique(phdcn.strata$nlinknc_1))-1,replace=T)
			idboot.2<-table(idboot.1)
			boot.phdcn.strata<-NULL
			for (k in 1:max(idboot.2)) {
				boot.data<-phdcn.strata[phdcn.strata$nlinknc_1 %in% names(idboot.2[idboot.2 %in% k]),]
				for (l in 1:k) { boot.phdcn.strata<-rbind(boot.phdcn.strata,boot.data) }
				}
			boot.phdcn<-rbind(boot.phdcn,boot.phdcn.strata)
			}

		boot.resid_l1<-function(y) { residuals(lm(y~1,data=boot.phdcn)) }
		boot.resid_l2<-function(y) { residuals(lm(y~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
								   nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1+nbl5ug_1,data=boot.phdcn)) }
		boot.resid_l3<-function(y) { residuals(lm(y~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
								   nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1+nbl5ug_1+
								   nlninc_2+npcemply_2+npcwelf_2+npcmarr_2+npcengl_2+ncondadvg_2+nbl5ug_2,data=boot.phdcn)) }
		boot.phdcn<-boot.phdcn %>% mutate_at(vars(npcage_b,npchsgrad_b,npcsomcol_b,npccolgrd_b,nownhome_b,nfemale_b,nblack_b,nhispan_b,nothrace_b,nfamsize_b), funs(r=boot.resid_l1))
		boot.phdcn<-boot.phdcn %>% mutate_at(vars(nlninc_1,npcemply_1,npcwelf_1,npcmarr_1,npcengl_1), funs(r=boot.resid_l1))
		boot.phdcn<-boot.phdcn %>% mutate_at(vars(nlninc_2,npcemply_2,npcwelf_2,npcmarr_2,npcengl_2), funs(r=boot.resid_l2))
		boot.phdcn<-boot.phdcn %>% mutate_at(vars(nlninc_3,npcemply_3,npcwelf_3,npcmarr_3,npcengl_3), funs(r=boot.resid_l3))
		boot.phdcn$nsubage_3_r<-residuals(lm(nsubage_3~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
									     nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1+nbl5ug_1+
									     nlninc_2+npcemply_2+npcwelf_2+npcmarr_2+npcengl_2+ncondadvg_2+nbl5ug_2+
									     nlninc_3+npcemply_3+npcwelf_3+npcmarr_3+npcengl_3+ncondadvg_3+nbl5ug_3,data=boot.phdcn))

		boot.phdcn$nlninc_s_r<-boot.phdcn$nlninc_1_r+boot.phdcn$nlninc_2_r+boot.phdcn$nlninc_3_r
		boot.phdcn$npcemply_s_r<-boot.phdcn$npcemply_1_r+boot.phdcn$npcemply_2_r+boot.phdcn$npcemply_3_r
		boot.phdcn$npcwelf_s_r<-boot.phdcn$npcwelf_1_r+boot.phdcn$npcwelf_2_r+boot.phdcn$npcwelf_3_r
		boot.phdcn$npcmarr_s_r<-boot.phdcn$npcmarr_1_r+boot.phdcn$npcmarr_2_r+boot.phdcn$npcmarr_3_r
		boot.phdcn$npcengl_s_r<-boot.phdcn$npcengl_1_r+boot.phdcn$npcengl_2_r+boot.phdcn$npcengl_3_r

		boot.m2<-lm(nppvt_3~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
					  nsubage_3_r+nlninc_s_r+npcemply_s_r+npcwelf_s_r+npcmarr_s_r+npcengl_s_r+cumndadvg+cumnbl5ug,data=boot.phdcn)

		boot.aje.rwr[j,1]<-boot.m2$coefficients["cumndadvg"]*(astar-a)+boot.m2$coefficients["cumnbl5ug"]*(mstar-m)
		}
	
	mivar.rwr.aje[i,1]<-var(boot.aje.rwr[,1]) 
	}

### BIAS-ADJUSTED CDE ###
for (i in 1:nmi) {

	phdcn<-phdcnmi[which(phdcnmi$"_mj"==i),]

	resid_l1<-function(y) { residuals(lm(y~1,data=phdcn)) }
	resid_l2<-function(y) { residuals(lm(y~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
				   			   nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1+nbl5ug_1,data=phdcn)) }
	resid_l3<-function(y) { residuals(lm(y~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
				   			   nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1+nbl5ug_1+
							   nlninc_2+npcemply_2+npcwelf_2+npcmarr_2+npcengl_2+ncondadvg_2+nbl5ug_2,data=phdcn)) }
	phdcn<-phdcn %>% mutate_at(vars(npcage_b,npchsgrad_b,npcsomcol_b,npccolgrd_b,nownhome_b,nfemale_b,nblack_b,nhispan_b,nothrace_b,nfamsize_b), funs(r=resid_l1))
	phdcn<-phdcn %>% mutate_at(vars(nlninc_1,npcemply_1,npcwelf_1,npcmarr_1,npcengl_1), funs(r=resid_l1))
	phdcn<-phdcn %>% mutate_at(vars(nlninc_2,npcemply_2,npcwelf_2,npcmarr_2,npcengl_2), funs(r=resid_l2))
	phdcn<-phdcn %>% mutate_at(vars(nlninc_3,npcemply_3,npcwelf_3,npcmarr_3,npcengl_3), funs(r=resid_l3))
	phdcn$nsubage_3_r<-residuals(lm(nsubage_3~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
						    		nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1+nbl5ug_1+
								nlninc_2+npcemply_2+npcwelf_2+npcmarr_2+npcengl_2+ncondadvg_2+nbl5ug_2+
								nlninc_3+npcemply_3+npcwelf_3+npcmarr_3+npcengl_3+ncondadvg_3+nbl5ug_3,data=phdcn))

	phdcn$nlninc_s_r<-phdcn$nlninc_1_r+phdcn$nlninc_2_r+phdcn$nlninc_3_r
	phdcn$npcemply_s_r<-phdcn$npcemply_1_r+phdcn$npcemply_2_r+phdcn$npcemply_3_r
	phdcn$npcwelf_s_r<-phdcn$npcwelf_1_r+phdcn$npcwelf_2_r+phdcn$npcwelf_3_r
	phdcn$npcmarr_s_r<-phdcn$npcmarr_1_r+phdcn$npcmarr_2_r+phdcn$npcmarr_3_r
	phdcn$npcengl_s_r<-phdcn$npcengl_1_r+phdcn$npcengl_2_r+phdcn$npcengl_3_r
	phdcn$cumndadvg<-(phdcn$ncondadvg_1+phdcn$ncondadvg_2+phdcn$ncondadvg_3)/3
	phdcn$cumnbl5ug<-(phdcn$nbl5ug_1+phdcn$nbl5ug_2+phdcn$nbl5ug_3)/3
	phdcn$nppvt_3<-(phdcn$nppvt_3-mean(phdcn$nppvt_3))/sd(phdcn$nppvt_3)

	aprime1<-sort(phdcn$ncondadvg_1)
	aprime2<-sort(phdcn$ncondadvg_2)
	aprime3<-sort(phdcn$ncondadvg_3)

	b1<-rep(0,length(phdcn$ncondadvg_1))
	b2<-rep(0,length(phdcn$ncondadvg_2))
	b3<-rep(0,length(phdcn$ncondadvg_3))

	for (q in aprime1) { b1<-b1+(phdcn$ncondadvg_1-q)*phi*(1/length(aprime1)) }
	for (q in aprime2) { b2<-b2+(phdcn$ncondadvg_2-q)*phi*(1/length(aprime2)) }
	for (q in aprime3) { b3<-b3+(phdcn$ncondadvg_3-q)*phi*(1/length(aprime3)) }

	for (j in 1:length(xPhi)) {
		phdcn$y_bias<-phdcn$nppvt_3-(b1+b2+b3)*xPhi[j]
		m2.bias<-lm(y_bias~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
					 nsubage_3_r+nlninc_s_r+npcemply_s_r+npcwelf_s_r+npcmarr_s_r+npcengl_s_r+cumndadvg+cumnbl5ug,data=phdcn)
		miest.rwr.cde[i,j]<-m2.bias$coefficients["cumndadvg"]*(astar-a)
		}

	boot.cde.rwr<-matrix(data=NA,nrow=nboot,ncol=1)
	
	for (j in 1:nboot) {
		boot.phdcn<-NULL
		for (s in 1:16) {
			phdcn.strata<-phdcn[which(phdcn$nstrata==s),]
			idboot.1<-sample(unique(phdcn.strata$nlinknc_1),length(unique(phdcn.strata$nlinknc_1))-1,replace=T)
			idboot.2<-table(idboot.1)
			boot.phdcn.strata<-NULL
			for (k in 1:max(idboot.2)) {
				boot.data<-phdcn.strata[phdcn.strata$nlinknc_1 %in% names(idboot.2[idboot.2 %in% k]),]
				for (l in 1:k) { boot.phdcn.strata<-rbind(boot.phdcn.strata,boot.data) }
				}
			boot.phdcn<-rbind(boot.phdcn,boot.phdcn.strata)
			}

		boot.resid_l1<-function(y) { residuals(lm(y~1,data=boot.phdcn)) }
		boot.resid_l2<-function(y) { residuals(lm(y~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
								   nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1+nbl5ug_1,data=boot.phdcn)) }
		boot.resid_l3<-function(y) { residuals(lm(y~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
								   nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1+nbl5ug_1+
								   nlninc_2+npcemply_2+npcwelf_2+npcmarr_2+npcengl_2+ncondadvg_2+nbl5ug_2,data=boot.phdcn)) }
		boot.phdcn<-boot.phdcn %>% mutate_at(vars(npcage_b,npchsgrad_b,npcsomcol_b,npccolgrd_b,nownhome_b,nfemale_b,nblack_b,nhispan_b,nothrace_b,nfamsize_b), funs(r=boot.resid_l1))
		boot.phdcn<-boot.phdcn %>% mutate_at(vars(nlninc_1,npcemply_1,npcwelf_1,npcmarr_1,npcengl_1), funs(r=boot.resid_l1))
		boot.phdcn<-boot.phdcn %>% mutate_at(vars(nlninc_2,npcemply_2,npcwelf_2,npcmarr_2,npcengl_2), funs(r=boot.resid_l2))
		boot.phdcn<-boot.phdcn %>% mutate_at(vars(nlninc_3,npcemply_3,npcwelf_3,npcmarr_3,npcengl_3), funs(r=boot.resid_l3))
		boot.phdcn$nsubage_3_r<-residuals(lm(nsubage_3~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
									     nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1+nbl5ug_1+
									     nlninc_2+npcemply_2+npcwelf_2+npcmarr_2+npcengl_2+ncondadvg_2+nbl5ug_2+
									     nlninc_3+npcemply_3+npcwelf_3+npcmarr_3+npcengl_3+ncondadvg_3+nbl5ug_3,data=boot.phdcn))

		boot.phdcn$nlninc_s_r<-boot.phdcn$nlninc_1_r+boot.phdcn$nlninc_2_r+boot.phdcn$nlninc_3_r
		boot.phdcn$npcemply_s_r<-boot.phdcn$npcemply_1_r+boot.phdcn$npcemply_2_r+boot.phdcn$npcemply_3_r
		boot.phdcn$npcwelf_s_r<-boot.phdcn$npcwelf_1_r+boot.phdcn$npcwelf_2_r+boot.phdcn$npcwelf_3_r
		boot.phdcn$npcmarr_s_r<-boot.phdcn$npcmarr_1_r+boot.phdcn$npcmarr_2_r+boot.phdcn$npcmarr_3_r
		boot.phdcn$npcengl_s_r<-boot.phdcn$npcengl_1_r+boot.phdcn$npcengl_2_r+boot.phdcn$npcengl_3_r

		boot.m2<-lm(nppvt_3~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
					  nsubage_3_r+nlninc_s_r+npcemply_s_r+npcwelf_s_r+npcmarr_s_r+npcengl_s_r+cumndadvg+cumnbl5ug,data=boot.phdcn)

		boot.cde.rwr[j,1]<-boot.m2$coefficients["cumndadvg"]*(astar-a)
		}
	
	for (k in 1:length(xPhi)) { mivar.rwr.cde[i,k]<-var(boot.cde.rwr[,1]) }
	}

### BIAS-ADJUSTED CME ###
for (i in 1:nmi) {

	phdcn<-phdcnmi[which(phdcnmi$"_mj"==i),]

	resid_l1<-function(y) { residuals(lm(y~1,data=phdcn)) }
	resid_l2<-function(y) { residuals(lm(y~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
				   			   nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1+nbl5ug_1,data=phdcn)) }
	resid_l3<-function(y) { residuals(lm(y~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
				   			   nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1+nbl5ug_1+
							   nlninc_2+npcemply_2+npcwelf_2+npcmarr_2+npcengl_2+ncondadvg_2+nbl5ug_2,data=phdcn)) }
	phdcn<-phdcn %>% mutate_at(vars(npcage_b,npchsgrad_b,npcsomcol_b,npccolgrd_b,nownhome_b,nfemale_b,nblack_b,nhispan_b,nothrace_b,nfamsize_b), funs(r=resid_l1))
	phdcn<-phdcn %>% mutate_at(vars(nlninc_1,npcemply_1,npcwelf_1,npcmarr_1,npcengl_1), funs(r=resid_l1))
	phdcn<-phdcn %>% mutate_at(vars(nlninc_2,npcemply_2,npcwelf_2,npcmarr_2,npcengl_2), funs(r=resid_l2))
	phdcn<-phdcn %>% mutate_at(vars(nlninc_3,npcemply_3,npcwelf_3,npcmarr_3,npcengl_3), funs(r=resid_l3))
	phdcn$nsubage_3_r<-residuals(lm(nsubage_3~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
						    		nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1+nbl5ug_1+
								nlninc_2+npcemply_2+npcwelf_2+npcmarr_2+npcengl_2+ncondadvg_2+nbl5ug_2+
								nlninc_3+npcemply_3+npcwelf_3+npcmarr_3+npcengl_3+ncondadvg_3+nbl5ug_3,data=phdcn))

	phdcn$nlninc_s_r<-phdcn$nlninc_1_r+phdcn$nlninc_2_r+phdcn$nlninc_3_r
	phdcn$npcemply_s_r<-phdcn$npcemply_1_r+phdcn$npcemply_2_r+phdcn$npcemply_3_r
	phdcn$npcwelf_s_r<-phdcn$npcwelf_1_r+phdcn$npcwelf_2_r+phdcn$npcwelf_3_r
	phdcn$npcmarr_s_r<-phdcn$npcmarr_1_r+phdcn$npcmarr_2_r+phdcn$npcmarr_3_r
	phdcn$npcengl_s_r<-phdcn$npcengl_1_r+phdcn$npcengl_2_r+phdcn$npcengl_3_r
	phdcn$cumndadvg<-(phdcn$ncondadvg_1+phdcn$ncondadvg_2+phdcn$ncondadvg_3)/3
	phdcn$cumnbl5ug<-(phdcn$nbl5ug_1+phdcn$nbl5ug_2+phdcn$nbl5ug_3)/3
	phdcn$nppvt_3<-(phdcn$nppvt_3-mean(phdcn$nppvt_3))/sd(phdcn$nppvt_3)

	mprime1<-sort(phdcn$nbl5ug_1)
	mprime2<-sort(phdcn$nbl5ug_2)
	mprime3<-sort(phdcn$nbl5ug_3)

	b1<-rep(0,length(phdcn$nbl5ug_1))
	b2<-rep(0,length(phdcn$nbl5ug_2))
	b3<-rep(0,length(phdcn$nbl5ug_3))

	for (q in mprime1) { b1<-b1+(phdcn$nbl5ug_1-q)*psi*(1/length(mprime1)) }
	for (q in mprime2) { b2<-b2+(phdcn$nbl5ug_2-q)*psi*(1/length(mprime2)) }
	for (q in mprime3) { b3<-b3+(phdcn$nbl5ug_3-q)*psi*(1/length(mprime3)) }

	for (j in 1:length(xPsi)) {
		phdcn$y_bias<-phdcn$nppvt_3-(b1+b2+b3)*xPsi[j]
		m2.bias<-lm(y_bias~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
					 nsubage_3_r+nlninc_s_r+npcemply_s_r+npcwelf_s_r+npcmarr_s_r+npcengl_s_r+cumndadvg+cumnbl5ug,data=phdcn)
		miest.rwr.cme[i,j]<-m2.bias$coefficients["cumnbl5ug"]*(mstar-m)
		}

	boot.cme.rwr<-matrix(data=NA,nrow=nboot,ncol=1)
	
	for (j in 1:nboot) {
		boot.phdcn<-NULL
		for (s in 1:16) {
			phdcn.strata<-phdcn[which(phdcn$nstrata==s),]
			idboot.1<-sample(unique(phdcn.strata$nlinknc_1),length(unique(phdcn.strata$nlinknc_1))-1,replace=T)
			idboot.2<-table(idboot.1)
			boot.phdcn.strata<-NULL
			for (k in 1:max(idboot.2)) {
				boot.data<-phdcn.strata[phdcn.strata$nlinknc_1 %in% names(idboot.2[idboot.2 %in% k]),]
				for (l in 1:k) { boot.phdcn.strata<-rbind(boot.phdcn.strata,boot.data) }
				}
			boot.phdcn<-rbind(boot.phdcn,boot.phdcn.strata)
			}

		boot.resid_l1<-function(y) { residuals(lm(y~1,data=boot.phdcn)) }
		boot.resid_l2<-function(y) { residuals(lm(y~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
								   nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1+nbl5ug_1,data=boot.phdcn)) }
		boot.resid_l3<-function(y) { residuals(lm(y~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
								   nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1+nbl5ug_1+
								   nlninc_2+npcemply_2+npcwelf_2+npcmarr_2+npcengl_2+ncondadvg_2+nbl5ug_2,data=boot.phdcn)) }
		boot.phdcn<-boot.phdcn %>% mutate_at(vars(npcage_b,npchsgrad_b,npcsomcol_b,npccolgrd_b,nownhome_b,nfemale_b,nblack_b,nhispan_b,nothrace_b,nfamsize_b), funs(r=boot.resid_l1))
		boot.phdcn<-boot.phdcn %>% mutate_at(vars(nlninc_1,npcemply_1,npcwelf_1,npcmarr_1,npcengl_1), funs(r=boot.resid_l1))
		boot.phdcn<-boot.phdcn %>% mutate_at(vars(nlninc_2,npcemply_2,npcwelf_2,npcmarr_2,npcengl_2), funs(r=boot.resid_l2))
		boot.phdcn<-boot.phdcn %>% mutate_at(vars(nlninc_3,npcemply_3,npcwelf_3,npcmarr_3,npcengl_3), funs(r=boot.resid_l3))
		boot.phdcn$nsubage_3_r<-residuals(lm(nsubage_3~npcage_b+npchsgrad_b+npcsomcol_b+npccolgrd_b+nownhome_b+nfemale_b+nblack_b+nhispan_b+nothrace_b+nfamsize_b+
									     nlninc_1+npcemply_1+npcwelf_1+npcmarr_1+npcengl_1+ncondadvg_1+nbl5ug_1+
									     nlninc_2+npcemply_2+npcwelf_2+npcmarr_2+npcengl_2+ncondadvg_2+nbl5ug_2+
									     nlninc_3+npcemply_3+npcwelf_3+npcmarr_3+npcengl_3+ncondadvg_3+nbl5ug_3,data=boot.phdcn))

		boot.phdcn$nlninc_s_r<-boot.phdcn$nlninc_1_r+boot.phdcn$nlninc_2_r+boot.phdcn$nlninc_3_r
		boot.phdcn$npcemply_s_r<-boot.phdcn$npcemply_1_r+boot.phdcn$npcemply_2_r+boot.phdcn$npcemply_3_r
		boot.phdcn$npcwelf_s_r<-boot.phdcn$npcwelf_1_r+boot.phdcn$npcwelf_2_r+boot.phdcn$npcwelf_3_r
		boot.phdcn$npcmarr_s_r<-boot.phdcn$npcmarr_1_r+boot.phdcn$npcmarr_2_r+boot.phdcn$npcmarr_3_r
		boot.phdcn$npcengl_s_r<-boot.phdcn$npcengl_1_r+boot.phdcn$npcengl_2_r+boot.phdcn$npcengl_3_r

		boot.m2<-lm(nppvt_3~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
					  nsubage_3_r+nlninc_s_r+npcemply_s_r+npcwelf_s_r+npcmarr_s_r+npcengl_s_r+cumndadvg+cumnbl5ug,data=boot.phdcn)

		boot.cme.rwr[j,1]<-boot.m2$coefficients["cumnbl5ug"]*(mstar-m)
		}
	
	for (k in 1:length(xPsi)) { mivar.rwr.cme[i,k]<-var(boot.cme.rwr[,1]) }
	}

##### COMBINE MI ESTIMATES #####
rwrest.ate<-rwrest.cde<-rwrest.cme<-matrix(data=NA,nrow=length(xTau),ncol=3)

for (i in 1:length(xTau)) { 
	rwrest.ate[i,1]<-round(mean(miest.rwr.ate[,i]),digits=4)
	rwrest.ate[i,2]<-rwrest.ate[i,1]-1.96*round(sqrt(mean(mivar.rwr.ate[,i])+(var(miest.rwr.ate[,i])*(1+(1/nmi)))),digits=4)
	rwrest.ate[i,3]<-rwrest.ate[i,1]+1.96*round(sqrt(mean(mivar.rwr.ate[,i])+(var(miest.rwr.ate[,i])*(1+(1/nmi)))),digits=4)

	rwrest.cde[i,1]<-round(mean(miest.rwr.cde[,i]),digits=4)
	rwrest.cde[i,2]<-rwrest.cde[i,1]-1.96*round(sqrt(mean(mivar.rwr.cde[,i])+(var(miest.rwr.cde[,i])*(1+(1/nmi)))),digits=4)
	rwrest.cde[i,3]<-rwrest.cde[i,1]+1.96*round(sqrt(mean(mivar.rwr.cde[,i])+(var(miest.rwr.cde[,i])*(1+(1/nmi)))),digits=4)

	rwrest.cme[i,1]<-round(mean(miest.rwr.cme[,i]),digits=4)
	rwrest.cme[i,2]<-rwrest.cme[i,1]-1.96*round(sqrt(mean(mivar.rwr.cme[,i])+(var(miest.rwr.cme[,i])*(1+(1/nmi)))),digits=4)
	rwrest.cme[i,3]<-rwrest.cme[i,1]+1.96*round(sqrt(mean(mivar.rwr.cme[,i])+(var(miest.rwr.cme[,i])*(1+(1/nmi)))),digits=4)
	}

bootse.aje<-round(sqrt(mean(mivar.rwr.aje[,1])+(var(miest.rwr.aje[,1])*(1+(1/nmi)))),digits=4)

rwrest.ate<-cbind(xTau,rwrest.ate)
output.ate<-data.frame(rwrest.ate)
colnames(output.ate)<-c('xTau','ATE','95LL','95UL')

rwrest.cde<-cbind(xPhi,rwrest.cde)
output.cde<-data.frame(rwrest.cde)
colnames(output.cde)<-c('xPhi','CDE','95LL','95UL')

rwrest.cme<-cbind(xPsi,rwrest.cme)
output.cme<-data.frame(rwrest.cme)
colnames(output.cme)<-c('xPsi','CME','95LL','95UL')

##### PRINT RESULTS #####
print(output.ate)
cat(" \n")
print(output.cde)
cat(" \n")
print(output.cme)
cat(" \n")
print(rwrest.aje)
cat(" \n")
print(c(tau,phi,psi))

##### PLOT RESULTS #####
#dev.new(width=9,height=9)
tiff("D:\\projects\\nhood_mediation_lead\\figures\\figure_4.tiff",
	width=9,
	height=9,
	units='in',
	res=600)

par(mfrow=c(2,2))

plot(1:4,
	panel.first = 
       	c(abline(h=0,col="grey"), 
		  lines(output.ate[,1],output.ate[,3],type="l",lty="dashed"),
		  lines(output.ate[,1],output.ate[,4],type="l",lty="dashed"),
		  lines(output.ate[,1],output.ate[,2],type="l",lty="solid")),
	main="Sens. of ATE to Unobserved Selection",
	xlab="Multiples of Tau",
	ylab="Bias-corrected ATE estimate",
	xlim=c(-1.0,2.0),
	ylim=c(-1.0,0.75))

xval<-yval<-seq(-1,2,by=0.25)
zeros<-rwrest.aje-rwrest.aje
ul95<-rwrest.aje+1.96*bootse.aje
ll95<-rwrest.aje-1.96*bootse.aje

persp(xval,yval,ll95,
	main="Sens. of AJE to Unobserved Selection",
	xlab="Multiples of Phi",
	ylab="Multiples of Psi",
	zlab="Bias-corrected AJE estimate",
	ylim=c(-1.1,2.1),
	xlim=c(-1.1,2.1),
	zlim=c(-1.0,0.75),
	theta=42,
	phi=17,
	col="grey75",
	ticktype="detailed",
	nticks=6,
	cex.axis=0.7,
	cex.lab=0.8,
	mar=c(1,1,4,1))

par(new=T)

persp(xval,yval,rwrest.aje,
	xlab="",
	ylab="",
	zlab="",
	ylim=c(-1.1,2.1),
	xlim=c(-1.1,2.1),
	zlim=c(-1.0,0.75),
	theta=42,
	phi=17,
	col="grey40",
	ticktype="detailed",
	nticks=6,
	cex.axis=0.7,
	cex.lab=0.8,
	mar=c(1,1,4,1))

par(new=T)

persp(xval,yval,ul95,
	xlab="",
	ylab="",
	zlab="",
	ylim=c(-1.1,2.1),
	xlim=c(-1.1,2.1),
	zlim=c(-1.0,0.75),
	theta=42,
	phi=17,
	col="grey75",
	ticktype="detailed",
	nticks=6,
	cex.axis=0.7,
	cex.lab=0.8,
	mar=c(1,1,4,1))

par(new=T)

persp(xval,yval,zeros,
	xlab="",
	ylab="",
	zlab="",
	ylim=c(-1.1,2.1),
	xlim=c(-1.1,2.1),
	zlim=c(-1.0,0.75),
	theta=42,
	phi=17,
	col="white",
	ticktype="detailed",
	nticks=6,
	cex.axis=0.7,
	cex.lab=0.8,
	mar=c(1,1,4,1))

plot(1:4,
	panel.first = 
       	c(abline(h=0,col="grey"), 
		  lines(output.cde[,1],output.cde[,3],type="l",lty="dashed"),
		  lines(output.cde[,1],output.cde[,4],type="l",lty="dashed"),
		  lines(output.cde[,1],output.cde[,2],type="l",lty="solid")),
	main="Sens. of CDE to Unobserved Selection",
	xlab="Multiples of Phi",
	ylab="Bias-corrected CDE estimate",
	xlim=c(-1.0,2.0),
	ylim=c(-1.0,0.75))

plot(1:4,
	panel.first = 
       	c(abline(h=0,col="grey"), 
		  lines(output.cme[,1],output.cme[,3],type="l",lty="dashed"),
		  lines(output.cme[,1],output.cme[,4],type="l",lty="dashed"),
		  lines(output.cme[,1],output.cme[,2],type="l",lty="solid")),
	main="Sens. of CME to Unobserved Selection",
	xlab="Multiples of Psi",
	ylab="Bias-corrected CME estimate",
	xlim=c(-1.0,2.0),
	ylim=c(-1.0,0.75))

dev.off()

print(Sys.time())

sink()


