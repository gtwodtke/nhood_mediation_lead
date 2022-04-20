sink("D:\\projects\\nhood_mediation_lead\\programs\\_LOGS\\14_create_table_3_log.txt")
print(Sys.time())

################################################
################################################
##                                            ##
## PROGRAM NAME: 14_create_table_3            ##
## AUTHOR: GW                                 ##
## DATE: 8/7/2020                             ##
## DESCRIPTION:                               ##
##                                            ##
##  creates table of nh effect estimates      ##
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

##### COMPUTE RWR EFFECT ESTIMATES #####
set.seed(8675309)
nmi<-50
nboot<-500
astar<-0.70
a<-(-0.90)
mstar<-0.65
m<-0.30

miest.rwr<-mivar.rwr<-matrix(data=NA,nrow=nmi,ncol=5)
miest.rwr.gen<-mivar.rwr.gen<-matrix(data=NA,nrow=nmi,ncol=5)
miest.rwr.race<-mivar.rwr.race<-matrix(data=NA,nrow=nmi,ncol=5)
miest.rwr.inc<-mivar.rwr.inc<-matrix(data=NA,nrow=nmi,ncol=5)

for (i in 1:nmi) {

	# LOAD MI DATA #
	phdcn<-phdcnmi[which(phdcnmi$"_mj"==i),]

	# RESIDUALIZE CONFOUNDERS #
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

	# COMPUTE NEW VARIABLES #
	phdcn$nlninc_s_r<-phdcn$nlninc_1_r+phdcn$nlninc_2_r+phdcn$nlninc_3_r
	phdcn$npcemply_s_r<-phdcn$npcemply_1_r+phdcn$npcemply_2_r+phdcn$npcemply_3_r
	phdcn$npcwelf_s_r<-phdcn$npcwelf_1_r+phdcn$npcwelf_2_r+phdcn$npcwelf_3_r
	phdcn$npcmarr_s_r<-phdcn$npcmarr_1_r+phdcn$npcmarr_2_r+phdcn$npcmarr_3_r
	phdcn$npcengl_s_r<-phdcn$npcengl_1_r+phdcn$npcengl_2_r+phdcn$npcengl_3_r
	phdcn$nbl5ug_s_r<-phdcn$nbl5ug_1_r+phdcn$nbl5ug_2_r+phdcn$nbl5ug_3_r
	phdcn$cumndadvg<-(phdcn$ncondadvg_1+phdcn$ncondadvg_2+phdcn$ncondadvg_3)/3
	phdcn$cumnbl5ug<-(phdcn$nbl5ug_1+phdcn$nbl5ug_2+phdcn$nbl5ug_3)/3
	phdcn$nppvt_3<-(phdcn$nppvt_3-mean(phdcn$nppvt_3))/sd(phdcn$nppvt_3)

	# FIT MODELS #
	# BASE MODEL #
	m1<-lm(nppvt_3~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
			   nsubage_3_r+nbl5ug_s_r+nlninc_s_r+npcemply_s_r+npcwelf_s_r+npcmarr_s_r+npcengl_s_r+cumndadvg,data=phdcn)
	
	m2<-lm(nppvt_3~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
			   nsubage_3_r+nlninc_s_r+npcemply_s_r+npcwelf_s_r+npcmarr_s_r+npcengl_s_r+cumndadvg+cumnbl5ug,data=phdcn)

	# BASE + GENDER MODERATION #
	m1.gen<-lm(nppvt_3~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
				 nsubage_3_r+nbl5ug_s_r+nlninc_s_r+npcemply_s_r+npcwelf_s_r+npcmarr_s_r+npcengl_s_r+cumndadvg+
				 (cumndadvg*nfemale_b_r),data=phdcn)
	
	m2.gen<-lm(nppvt_3~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
				 nsubage_3_r+nlninc_s_r+npcemply_s_r+npcwelf_s_r+npcmarr_s_r+npcengl_s_r+cumndadvg+cumnbl5ug+
				 (cumndadvg*nfemale_b_r)+(cumnbl5ug*nfemale_b_r),data=phdcn)

	# BASE + RACE MODERATION #
	m1.race<-lm(nppvt_3~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
				  nsubage_3_r+nbl5ug_s_r+nlninc_s_r+npcemply_s_r+npcwelf_s_r+npcmarr_s_r+npcengl_s_r+cumndadvg+
				  (cumndadvg*nblack_b_r)+(cumndadvg*nhispan_b_r),data=phdcn)
	
	m2.race<-lm(nppvt_3~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
				 nsubage_3_r+nlninc_s_r+npcemply_s_r+npcwelf_s_r+npcmarr_s_r+npcengl_s_r+cumndadvg+cumnbl5ug+
				 (cumndadvg*nblack_b_r)+(cumndadvg*nhispan_b_r)+(cumnbl5ug*nblack_b_r)+(cumnbl5ug*nhispan_b_r),data=phdcn)

	# BASE + INCOME MODERATION #
	m1.inc<-lm(nppvt_3~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
				 nsubage_3_r+nbl5ug_s_r+nlninc_s_r+npcemply_s_r+npcwelf_s_r+npcmarr_s_r+npcengl_s_r+cumndadvg+
				 (cumndadvg*nlninc_s_r),data=phdcn)
	
	m2.inc<-lm(nppvt_3~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
				 nsubage_3_r+nlninc_s_r+npcemply_s_r+npcwelf_s_r+npcmarr_s_r+npcengl_s_r+cumndadvg+cumnbl5ug+
				 (cumndadvg*nlninc_s_r)+(cumnbl5ug*nlninc_s_r),data=phdcn)

	# TOTAL EFFECT OF NHOOD POVERTY #
	miest.rwr[i,1]<-m1$coefficients["cumndadvg"]*(astar-a)
	miest.rwr.gen[i,1]<-m1.gen$coefficients["cumndadvg"]*(astar-a)
	miest.rwr.race[i,1]<-m1.race$coefficients["cumndadvg"]*(astar-a)
	miest.rwr.inc[i,1]<-m1.inc$coefficients["cumndadvg"]*(astar-a)

	# JOINT EFFECT OF NHOOD POVERTY AND ENVIRONMENTAL LEAD CONTAMINATION #
	miest.rwr[i,2]<-m2$coefficients["cumndadvg"]*(astar-a)+m2$coefficients["cumnbl5ug"]*(mstar-m)
	miest.rwr.gen[i,2]<-m2.gen$coefficients["cumndadvg"]*(astar-a)+m2.gen$coefficients["cumnbl5ug"]*(mstar-m)
	miest.rwr.race[i,2]<-m2.race$coefficients["cumndadvg"]*(astar-a)+m2.race$coefficients["cumnbl5ug"]*(mstar-m)
	miest.rwr.inc[i,2]<-m2.inc$coefficients["cumndadvg"]*(astar-a)+m2.inc$coefficients["cumnbl5ug"]*(mstar-m)

	# CONTROLLED DIRECT EFFECT OF NHOOD POVERTY #
	miest.rwr[i,3]<-m2$coefficients["cumndadvg"]*(astar-a)
	miest.rwr.gen[i,3]<-m2.gen$coefficients["cumndadvg"]*(astar-a)
	miest.rwr.race[i,3]<-m2.race$coefficients["cumndadvg"]*(astar-a)
	miest.rwr.inc[i,3]<-m2.inc$coefficients["cumndadvg"]*(astar-a)

	# CONTROLLED MEDIATOR EFFECT OF ENVIRONMENTAL LEAD CONTAMINATION #
	miest.rwr[i,4]<-m2$coefficients["cumnbl5ug"]*(mstar-m)
	miest.rwr.gen[i,4]<-m2.gen$coefficients["cumnbl5ug"]*(mstar-m)
	miest.rwr.race[i,4]<-m2.race$coefficients["cumnbl5ug"]*(mstar-m)
	miest.rwr.inc[i,4]<-m2.inc$coefficients["cumnbl5ug"]*(mstar-m)

	# DIFFERENCE BETWEEN ATE AND CDE #
	miest.rwr[i,5]<-miest.rwr[i,1]-miest.rwr[i,3]
	miest.rwr.gen[i,5]<-miest.rwr.gen[i,1]-miest.rwr.gen[i,3]
	miest.rwr.race[i,5]<-miest.rwr.race[i,1]-miest.rwr.race[i,3]
	miest.rwr.inc[i,5]<-miest.rwr.inc[i,1]-miest.rwr.inc[i,3]

	# COMPUTE BOOTSTRAP SEs #
	boot.ate.rwr<-boot.aje.rwr<-boot.cde.rwr<-boot.cme.rwr<-boot.dif1.rwr<-matrix(data=NA,nrow=nboot,ncol=1)
	boot.ate.rwr.gen<-boot.aje.rwr.gen<-boot.cde.rwr.gen<-boot.cme.rwr.gen<-boot.dif1.rwr.gen<-matrix(data=NA,nrow=nboot,ncol=1)
	boot.ate.rwr.race<-boot.aje.rwr.race<-boot.cde.rwr.race<-boot.cme.rwr.race<-boot.dif1.rwr.race<-matrix(data=NA,nrow=nboot,ncol=1)
	boot.ate.rwr.inc<-boot.aje.rwr.inc<-boot.cde.rwr.inc<-boot.cme.rwr.inc<-boot.dif1.rwr.inc<-matrix(data=NA,nrow=nboot,ncol=1)
	
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
		
		boot.m2<-lm(nppvt_3~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
					  nsubage_3_r+nlninc_s_r+npcemply_s_r+npcwelf_s_r+npcmarr_s_r+npcengl_s_r+cumndadvg+cumnbl5ug,data=boot.phdcn)

		boot.m1.gen<-lm(nppvt_3~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
						nsubage_3_r+nbl5ug_s_r+nlninc_s_r+npcemply_s_r+npcwelf_s_r+npcmarr_s_r+npcengl_s_r+cumndadvg+
						(cumndadvg*nfemale_b_r),data=boot.phdcn)
	
		boot.m2.gen<-lm(nppvt_3~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
						nsubage_3_r+nlninc_s_r+npcemply_s_r+npcwelf_s_r+npcmarr_s_r+npcengl_s_r+cumndadvg+cumnbl5ug+
						(cumndadvg*nfemale_b_r)+(cumnbl5ug*nfemale_b_r),data=boot.phdcn)

		boot.m1.race<-lm(nppvt_3~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
						 nsubage_3_r+nbl5ug_s_r+nlninc_s_r+npcemply_s_r+npcwelf_s_r+npcmarr_s_r+npcengl_s_r+cumndadvg+
						 (cumndadvg*nblack_b_r)+(cumndadvg*nhispan_b_r),data=boot.phdcn)
	
		boot.m2.race<-lm(nppvt_3~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
						nsubage_3_r+nlninc_s_r+npcemply_s_r+npcwelf_s_r+npcmarr_s_r+npcengl_s_r+cumndadvg+cumnbl5ug+
						(cumndadvg*nblack_b_r)+(cumndadvg*nhispan_b_r)+(cumnbl5ug*nblack_b_r)+(cumnbl5ug*nhispan_b_r),data=boot.phdcn)

		boot.m1.inc<-lm(nppvt_3~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
						nsubage_3_r+nbl5ug_s_r+nlninc_s_r+npcemply_s_r+npcwelf_s_r+npcmarr_s_r+npcengl_s_r+cumndadvg+
						(cumndadvg*nlninc_s_r),data=boot.phdcn)
	
		boot.m2.inc<-lm(nppvt_3~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
						nsubage_3_r+nlninc_s_r+npcemply_s_r+npcwelf_s_r+npcmarr_s_r+npcengl_s_r+cumndadvg+cumnbl5ug+
						(cumndadvg*nlninc_s_r)+(cumnbl5ug*nlninc_s_r),data=boot.phdcn)

		boot.ate.rwr[j,1]<-boot.m1$coefficients["cumndadvg"]*(astar-a)
		boot.ate.rwr.gen[j,1]<-boot.m1.gen$coefficients["cumndadvg"]*(astar-a)
		boot.ate.rwr.race[j,1]<-boot.m1.race$coefficients["cumndadvg"]*(astar-a)
		boot.ate.rwr.inc[j,1]<-boot.m1.inc$coefficients["cumndadvg"]*(astar-a)

		boot.aje.rwr[j,1]<-boot.m2$coefficients["cumndadvg"]*(astar-a)+boot.m2$coefficients["cumnbl5ug"]*(mstar-m)
		boot.aje.rwr.gen[j,1]<-boot.m2.gen$coefficients["cumndadvg"]*(astar-a)+boot.m2.gen$coefficients["cumnbl5ug"]*(mstar-m)
		boot.aje.rwr.race[j,1]<-boot.m2.race$coefficients["cumndadvg"]*(astar-a)+boot.m2.race$coefficients["cumnbl5ug"]*(mstar-m)
		boot.aje.rwr.inc[j,1]<-boot.m2.inc$coefficients["cumndadvg"]*(astar-a)+boot.m2.inc$coefficients["cumnbl5ug"]*(mstar-m)

		boot.cde.rwr[j,1]<-boot.m2$coefficients["cumndadvg"]*(astar-a)
		boot.cde.rwr.gen[j,1]<-boot.m2.gen$coefficients["cumndadvg"]*(astar-a)
		boot.cde.rwr.race[j,1]<-boot.m2.race$coefficients["cumndadvg"]*(astar-a)
		boot.cde.rwr.inc[j,1]<-boot.m2.inc$coefficients["cumndadvg"]*(astar-a)

		boot.cme.rwr[j,1]<-boot.m2$coefficients["cumnbl5ug"]*(mstar-m)
		boot.cme.rwr.gen[j,1]<-boot.m2.gen$coefficients["cumnbl5ug"]*(mstar-m)
		boot.cme.rwr.race[j,1]<-boot.m2.race$coefficients["cumnbl5ug"]*(mstar-m)
		boot.cme.rwr.inc[j,1]<-boot.m2.inc$coefficients["cumnbl5ug"]*(mstar-m)

		boot.dif1.rwr[j,1]<-boot.ate.rwr[j,1]-boot.cde.rwr[j,1]
		boot.dif1.rwr.gen[j,1]<-boot.ate.rwr.gen[j,1]-boot.cde.rwr.gen[j,1]
		boot.dif1.rwr.race[j,1]<-boot.ate.rwr.race[j,1]-boot.cde.rwr.race[j,1]
		boot.dif1.rwr.inc[j,1]<-boot.ate.rwr.inc[j,1]-boot.cde.rwr.inc[j,1]
		}

	mivar.rwr[i,1]<-var(boot.ate.rwr)
	mivar.rwr[i,2]<-var(boot.aje.rwr)
	mivar.rwr[i,3]<-var(boot.cde.rwr)
	mivar.rwr[i,4]<-var(boot.cme.rwr)
	mivar.rwr[i,5]<-var(boot.dif1.rwr)

	mivar.rwr.gen[i,1]<-var(boot.ate.rwr.gen)
	mivar.rwr.gen[i,2]<-var(boot.aje.rwr.gen)
	mivar.rwr.gen[i,3]<-var(boot.cde.rwr.gen)
	mivar.rwr.gen[i,4]<-var(boot.cme.rwr.gen)
	mivar.rwr.gen[i,5]<-var(boot.dif1.rwr.gen)

	mivar.rwr.race[i,1]<-var(boot.ate.rwr.race)
	mivar.rwr.race[i,2]<-var(boot.aje.rwr.race)
	mivar.rwr.race[i,3]<-var(boot.cde.rwr.race)
	mivar.rwr.race[i,4]<-var(boot.cme.rwr.race)
	mivar.rwr.race[i,5]<-var(boot.dif1.rwr.race)

	mivar.rwr.inc[i,1]<-var(boot.ate.rwr.inc)
	mivar.rwr.inc[i,2]<-var(boot.aje.rwr.inc)
	mivar.rwr.inc[i,3]<-var(boot.cde.rwr.inc)
	mivar.rwr.inc[i,4]<-var(boot.cme.rwr.inc)
	mivar.rwr.inc[i,5]<-var(boot.dif1.rwr.inc)
	}

##### COMPUTE RESIDUAL BALANCING ESTIMATES #####

### DEFINE RESIDUAL BALANCING FUNCTION ###
resbal<-function(M,C,Q,Z=rep(0,ncol(C)),max_iter=300,tol=1,print.level=0) {
  converged <- FALSE
  for (iter in 1:max_iter) {
    if (print.level >= 1) cat("iteration",iter,"\n")
    W<-c(Q * exp(C %*% Z))
    sumC<-t(C) %*% W
    gradient<-sumC-M
    if (max(abs(gradient)) < tol) {
      converged<-TRUE
      break
      }
    if (print.level >= 2) { cat("Iteration",iter,"maximum deviation is =",format(max(abs(gradient)),digits=4),"\n") }
    hessian=t(C) %*% (W*C)
    Coefs<-Z
    newton<-solve(hessian,gradient)
    Z<-Z-newton
    loss.new<-line.searcher(Base.weight=Q,Co.x=C,Tr.total=M,coefs=Z,Newton=newton,ss=1)
    loss.old<-line.searcher(Base.weight=Q,Co.x=C,Tr.total=M,coefs=Coefs,Newton=newton,ss=0)
    if (print.level>=3) { cat("new loss=",loss.new,"old loss=",loss.old,"\n") }
    if (loss.old <= loss.new) {
      ss.out<-optimize(line.searcher,lower=.00001,upper=1,maximum=FALSE,Base.weight=Q,Co.x=C,Tr.total=M,coefs=Coefs,Newton=newton)
      if (print.level >= 3) { cat("LS Step Length is ",ss.out$minimum,"\n") }
      if (print.level >= 3) { cat("Loss is",ss.out$objective,"\n") }
      Z <- Coefs-ss.out$minimum*solve(hessian, gradient)
      }
    }
  list(W=W,sumC=sumC,converged=converged,maxdiff=max(abs(gradient)))
  }

miest.rbal<-mivar.rbal<-matrix(data=NA,nrow=nmi,ncol=5)

### COMPUTE ESTIMATES ###
for (i in 1:nmi) {

	# LOAD DATA #
	phdcn<-phdcnmi[which(phdcnmi$"_mj"==i),]
	
	# RESIDUALIZE POST-TREATMENT CONFOUNDERS #
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

	# DEFINE CONSTRAINT MATRICES #
	Cm<-cbind(1,
	   phdcn$nbl5ug_1_r,
	   phdcn$nbl5ug_1_r*phdcn$ncondadvg_2,
	   phdcn$nbl5ug_1_r*phdcn$ncondadvg_3,
	   phdcn$nbl5ug_1_r*phdcn$npcage_b,
	   phdcn$nbl5ug_1_r*phdcn$npchsgrad_b,
	   phdcn$nbl5ug_1_r*phdcn$npcsomcol_b,
	   phdcn$nbl5ug_1_r*phdcn$npccolgrd_b,
	   phdcn$nbl5ug_1_r*phdcn$nownhome_b,
	   phdcn$nbl5ug_1_r*phdcn$nfemale_b,
	   phdcn$nbl5ug_1_r*phdcn$nblack_b,
	   phdcn$nbl5ug_1_r*phdcn$nhispan_b,
	   phdcn$nbl5ug_1_r*phdcn$nothrace_b,
	   phdcn$nbl5ug_1_r*phdcn$nfamsize_b,
	   phdcn$nbl5ug_1_r*phdcn$nlninc_1,
	   phdcn$nbl5ug_1_r*phdcn$npcemply_1,
	   phdcn$nbl5ug_1_r*phdcn$npcwelf_1,
	   phdcn$nbl5ug_1_r*phdcn$npcmarr_1,
	   phdcn$nbl5ug_1_r*phdcn$npcengl_1,
	   phdcn$nbl5ug_1_r*phdcn$ncondadvg_1,
	   phdcn$nlninc_2_r,
	   phdcn$nlninc_2_r*phdcn$ncondadvg_2,
	   phdcn$nlninc_2_r*phdcn$ncondadvg_3,
	   phdcn$nlninc_2_r*phdcn$npcage_b,
	   phdcn$nlninc_2_r*phdcn$npchsgrad_b,
	   phdcn$nlninc_2_r*phdcn$npcsomcol_b,
	   phdcn$nlninc_2_r*phdcn$npccolgrd_b,
	   phdcn$nlninc_2_r*phdcn$nownhome_b,
	   phdcn$nlninc_2_r*phdcn$nfemale_b,
	   phdcn$nlninc_2_r*phdcn$nblack_b,
	   phdcn$nlninc_2_r*phdcn$nhispan_b,
	   phdcn$nlninc_2_r*phdcn$nothrace_b,
	   phdcn$nlninc_2_r*phdcn$nfamsize_b,
	   phdcn$nlninc_2_r*phdcn$nlninc_1,
	   phdcn$nlninc_2_r*phdcn$npcemply_1,
	   phdcn$nlninc_2_r*phdcn$npcwelf_1,
	   phdcn$nlninc_2_r*phdcn$npcmarr_1,
	   phdcn$nlninc_2_r*phdcn$npcengl_1,
	   phdcn$nlninc_2_r*phdcn$ncondadvg_1,
	   phdcn$nlninc_2_r*phdcn$nbl5ug_1,
	   phdcn$npcemply_2_r,
	   phdcn$npcemply_2_r*phdcn$ncondadvg_2,
	   phdcn$npcemply_2_r*phdcn$ncondadvg_3,
	   phdcn$npcemply_2_r*phdcn$npcage_b,
	   phdcn$npcemply_2_r*phdcn$npchsgrad_b,
	   phdcn$npcemply_2_r*phdcn$npcsomcol_b,
	   phdcn$npcemply_2_r*phdcn$npccolgrd_b,
	   phdcn$npcemply_2_r*phdcn$nownhome_b,
	   phdcn$npcemply_2_r*phdcn$nfemale_b,
	   phdcn$npcemply_2_r*phdcn$nblack_b,
	   phdcn$npcemply_2_r*phdcn$nhispan_b,
	   phdcn$npcemply_2_r*phdcn$nothrace_b,
	   phdcn$npcemply_2_r*phdcn$nfamsize_b,
	   phdcn$npcemply_2_r*phdcn$nlninc_1,
	   phdcn$npcemply_2_r*phdcn$npcemply_1,
	   phdcn$npcemply_2_r*phdcn$npcwelf_1,
	   phdcn$npcemply_2_r*phdcn$npcmarr_1,
	   phdcn$npcemply_2_r*phdcn$npcengl_1,
	   phdcn$npcemply_2_r*phdcn$ncondadvg_1,
	   phdcn$npcemply_2_r*phdcn$nbl5ug_1,
	   phdcn$npcwelf_2_r,
	   phdcn$npcwelf_2_r*phdcn$ncondadvg_2,
	   phdcn$npcwelf_2_r*phdcn$ncondadvg_3,
	   phdcn$npcwelf_2_r*phdcn$npcage_b,
	   phdcn$npcwelf_2_r*phdcn$npchsgrad_b,
	   phdcn$npcwelf_2_r*phdcn$npcsomcol_b,
	   phdcn$npcwelf_2_r*phdcn$npccolgrd_b,
	   phdcn$npcwelf_2_r*phdcn$nownhome_b,
	   phdcn$npcwelf_2_r*phdcn$nfemale_b,
	   phdcn$npcwelf_2_r*phdcn$nblack_b,
	   phdcn$npcwelf_2_r*phdcn$nhispan_b,
	   phdcn$npcwelf_2_r*phdcn$nothrace_b,
	   phdcn$npcwelf_2_r*phdcn$nfamsize_b,
	   phdcn$npcwelf_2_r*phdcn$nlninc_1,
	   phdcn$npcwelf_2_r*phdcn$npcemply_1,
	   phdcn$npcwelf_2_r*phdcn$npcwelf_1,
	   phdcn$npcwelf_2_r*phdcn$npcmarr_1,
	   phdcn$npcwelf_2_r*phdcn$npcengl_1,
	   phdcn$npcwelf_2_r*phdcn$ncondadvg_1,
	   phdcn$npcwelf_2_r*phdcn$nbl5ug_1,
	   phdcn$npcmarr_2_r,
	   phdcn$npcmarr_2_r*phdcn$ncondadvg_2,
	   phdcn$npcmarr_2_r*phdcn$ncondadvg_3,
	   phdcn$npcmarr_2_r*phdcn$npcage_b,
	   phdcn$npcmarr_2_r*phdcn$npchsgrad_b,
	   phdcn$npcmarr_2_r*phdcn$npcsomcol_b,
	   phdcn$npcmarr_2_r*phdcn$npccolgrd_b,
	   phdcn$npcmarr_2_r*phdcn$nownhome_b,
	   phdcn$npcmarr_2_r*phdcn$nfemale_b,
	   phdcn$npcmarr_2_r*phdcn$nblack_b,
	   phdcn$npcmarr_2_r*phdcn$nhispan_b,
	   phdcn$npcmarr_2_r*phdcn$nothrace_b,
	   phdcn$npcmarr_2_r*phdcn$nfamsize_b,
	   phdcn$npcmarr_2_r*phdcn$nlninc_1,
	   phdcn$npcmarr_2_r*phdcn$npcemply_1,
	   phdcn$npcmarr_2_r*phdcn$npcwelf_1,
	   phdcn$npcmarr_2_r*phdcn$npcmarr_1,
	   phdcn$npcmarr_2_r*phdcn$npcengl_1,
	   phdcn$npcmarr_2_r*phdcn$ncondadvg_1,
	   phdcn$npcmarr_2_r*phdcn$nbl5ug_1,
	   phdcn$npcengl_2_r,
	   phdcn$npcengl_2_r*phdcn$ncondadvg_2,
	   phdcn$npcengl_2_r*phdcn$ncondadvg_3,
	   phdcn$npcengl_2_r*phdcn$npcage_b,
	   phdcn$npcengl_2_r*phdcn$npchsgrad_b,
	   phdcn$npcengl_2_r*phdcn$npcsomcol_b,
	   phdcn$npcengl_2_r*phdcn$npccolgrd_b,
	   phdcn$npcengl_2_r*phdcn$nownhome_b,
	   phdcn$npcengl_2_r*phdcn$nfemale_b,
	   phdcn$npcengl_2_r*phdcn$nblack_b,
	   phdcn$npcengl_2_r*phdcn$nhispan_b,
	   phdcn$npcengl_2_r*phdcn$nothrace_b,
	   phdcn$npcengl_2_r*phdcn$nfamsize_b,
	   phdcn$npcengl_2_r*phdcn$nlninc_1,
	   phdcn$npcengl_2_r*phdcn$npcemply_1,
	   phdcn$npcengl_2_r*phdcn$npcwelf_1,
	   phdcn$npcengl_2_r*phdcn$npcmarr_1,
	   phdcn$npcengl_2_r*phdcn$npcengl_1,
	   phdcn$npcengl_2_r*phdcn$ncondadvg_1,
	   phdcn$npcengl_2_r*phdcn$nbl5ug_1,
	   phdcn$nbl5ug_2_r,
	   phdcn$nbl5ug_2_r*phdcn$ncondadvg_3,
	   phdcn$nbl5ug_2_r*phdcn$npcage_b,
	   phdcn$nbl5ug_2_r*phdcn$npchsgrad_b,
	   phdcn$nbl5ug_2_r*phdcn$npcsomcol_b,
	   phdcn$nbl5ug_2_r*phdcn$npccolgrd_b,
	   phdcn$nbl5ug_2_r*phdcn$nownhome_b,
	   phdcn$nbl5ug_2_r*phdcn$nfemale_b,
	   phdcn$nbl5ug_2_r*phdcn$nblack_b,
	   phdcn$nbl5ug_2_r*phdcn$nhispan_b,
	   phdcn$nbl5ug_2_r*phdcn$nothrace_b,
	   phdcn$nbl5ug_2_r*phdcn$nfamsize_b,
	   phdcn$nbl5ug_2_r*phdcn$nlninc_1,
	   phdcn$nbl5ug_2_r*phdcn$npcemply_1,
	   phdcn$nbl5ug_2_r*phdcn$npcwelf_1,
	   phdcn$nbl5ug_2_r*phdcn$npcmarr_1,
	   phdcn$nbl5ug_2_r*phdcn$npcengl_1,
	   phdcn$nbl5ug_2_r*phdcn$ncondadvg_1,
	   phdcn$nbl5ug_2_r*phdcn$nbl5ug_1,
	   phdcn$nbl5ug_2_r*phdcn$nlninc_2,
	   phdcn$nbl5ug_2_r*phdcn$npcemply_2,
	   phdcn$nbl5ug_2_r*phdcn$npcwelf_2,
	   phdcn$nbl5ug_2_r*phdcn$npcmarr_2,
	   phdcn$nbl5ug_2_r*phdcn$npcengl_2,
	   phdcn$nbl5ug_2_r*phdcn$ncondadvg_2,
	   phdcn$nlninc_3_r,
	   phdcn$nlninc_3_r*phdcn$ncondadvg_3,
	   phdcn$nlninc_3_r*phdcn$npcage_b,
	   phdcn$nlninc_3_r*phdcn$npchsgrad_b,
	   phdcn$nlninc_3_r*phdcn$npcsomcol_b,
	   phdcn$nlninc_3_r*phdcn$npccolgrd_b,
	   phdcn$nlninc_3_r*phdcn$nownhome_b,
	   phdcn$nlninc_3_r*phdcn$nfemale_b,
	   phdcn$nlninc_3_r*phdcn$nblack_b,
	   phdcn$nlninc_3_r*phdcn$nhispan_b,
	   phdcn$nlninc_3_r*phdcn$nothrace_b,
	   phdcn$nlninc_3_r*phdcn$nfamsize_b,
	   phdcn$nlninc_3_r*phdcn$nlninc_1,
	   phdcn$nlninc_3_r*phdcn$npcemply_1,
	   phdcn$nlninc_3_r*phdcn$npcwelf_1,
	   phdcn$nlninc_3_r*phdcn$npcmarr_1,
	   phdcn$nlninc_3_r*phdcn$npcengl_1,
	   phdcn$nlninc_3_r*phdcn$ncondadvg_1,
	   phdcn$nlninc_3_r*phdcn$nbl5ug_1,
	   phdcn$nlninc_3_r*phdcn$nlninc_2,
	   phdcn$nlninc_3_r*phdcn$npcemply_2,
	   phdcn$nlninc_3_r*phdcn$npcwelf_2,
	   phdcn$nlninc_3_r*phdcn$npcmarr_2,
	   phdcn$nlninc_3_r*phdcn$npcengl_2,
	   phdcn$nlninc_3_r*phdcn$ncondadvg_2,
	   phdcn$nlninc_3_r*phdcn$nbl5ug_2,
	   phdcn$npcemply_3_r,
	   phdcn$npcemply_3_r*phdcn$ncondadvg_3,
	   phdcn$npcemply_3_r*phdcn$npcage_b,
	   phdcn$npcemply_3_r*phdcn$npchsgrad_b,
	   phdcn$npcemply_3_r*phdcn$npcsomcol_b,
	   phdcn$npcemply_3_r*phdcn$npccolgrd_b,
	   phdcn$npcemply_3_r*phdcn$nownhome_b,
	   phdcn$npcemply_3_r*phdcn$nfemale_b,
	   phdcn$npcemply_3_r*phdcn$nblack_b,
	   phdcn$npcemply_3_r*phdcn$nhispan_b,
	   phdcn$npcemply_3_r*phdcn$nothrace_b,
	   phdcn$npcemply_3_r*phdcn$nfamsize_b,
	   phdcn$npcemply_3_r*phdcn$nlninc_1,
	   phdcn$npcemply_3_r*phdcn$npcemply_1,
	   phdcn$npcemply_3_r*phdcn$npcwelf_1,
	   phdcn$npcemply_3_r*phdcn$npcmarr_1,
	   phdcn$npcemply_3_r*phdcn$npcengl_1,
	   phdcn$npcemply_3_r*phdcn$ncondadvg_1,
	   phdcn$npcemply_3_r*phdcn$nbl5ug_1,
	   phdcn$npcemply_3_r*phdcn$nlninc_2,
	   phdcn$npcemply_3_r*phdcn$npcemply_2,
	   phdcn$npcemply_3_r*phdcn$npcwelf_2,
	   phdcn$npcemply_3_r*phdcn$npcmarr_2,
	   phdcn$npcemply_3_r*phdcn$npcengl_2,
	   phdcn$npcemply_3_r*phdcn$ncondadvg_2,
	   phdcn$npcemply_3_r*phdcn$nbl5ug_2,
	   phdcn$npcwelf_3_r,
	   phdcn$npcwelf_3_r*phdcn$ncondadvg_3,
	   phdcn$npcwelf_3_r*phdcn$npcage_b,
	   phdcn$npcwelf_3_r*phdcn$npchsgrad_b,
	   phdcn$npcwelf_3_r*phdcn$npcsomcol_b,
	   phdcn$npcwelf_3_r*phdcn$npccolgrd_b,
	   phdcn$npcwelf_3_r*phdcn$nownhome_b,
	   phdcn$npcwelf_3_r*phdcn$nfemale_b,
	   phdcn$npcwelf_3_r*phdcn$nblack_b,
	   phdcn$npcwelf_3_r*phdcn$nhispan_b,
	   phdcn$npcwelf_3_r*phdcn$nothrace_b,
	   phdcn$npcwelf_3_r*phdcn$nfamsize_b,
	   phdcn$npcwelf_3_r*phdcn$nlninc_1,
	   phdcn$npcwelf_3_r*phdcn$npcemply_1,
	   phdcn$npcwelf_3_r*phdcn$npcwelf_1,
	   phdcn$npcwelf_3_r*phdcn$npcmarr_1,
	   phdcn$npcwelf_3_r*phdcn$npcengl_1,
	   phdcn$npcwelf_3_r*phdcn$ncondadvg_1,
	   phdcn$npcwelf_3_r*phdcn$nbl5ug_1,
	   phdcn$npcwelf_3_r*phdcn$nlninc_2,
	   phdcn$npcwelf_3_r*phdcn$npcemply_2,
	   phdcn$npcwelf_3_r*phdcn$npcwelf_2,
	   phdcn$npcwelf_3_r*phdcn$npcmarr_2,
	   phdcn$npcwelf_3_r*phdcn$npcengl_2,
	   phdcn$npcwelf_3_r*phdcn$ncondadvg_2,
	   phdcn$npcwelf_3_r*phdcn$nbl5ug_2,
	   phdcn$npcmarr_3_r,
	   phdcn$npcmarr_3_r*phdcn$ncondadvg_3,
	   phdcn$npcmarr_3_r*phdcn$npcage_b,
	   phdcn$npcmarr_3_r*phdcn$npchsgrad_b,
	   phdcn$npcmarr_3_r*phdcn$npcsomcol_b,
	   phdcn$npcmarr_3_r*phdcn$npccolgrd_b,
	   phdcn$npcmarr_3_r*phdcn$nownhome_b,
	   phdcn$npcmarr_3_r*phdcn$nfemale_b,
	   phdcn$npcmarr_3_r*phdcn$nblack_b,
	   phdcn$npcmarr_3_r*phdcn$nhispan_b,
	   phdcn$npcmarr_3_r*phdcn$nothrace_b,
	   phdcn$npcmarr_3_r*phdcn$nfamsize_b,
	   phdcn$npcmarr_3_r*phdcn$nlninc_1,
	   phdcn$npcmarr_3_r*phdcn$npcemply_1,
	   phdcn$npcmarr_3_r*phdcn$npcwelf_1,
	   phdcn$npcmarr_3_r*phdcn$npcmarr_1,
	   phdcn$npcmarr_3_r*phdcn$npcengl_1,
	   phdcn$npcmarr_3_r*phdcn$ncondadvg_1,
	   phdcn$npcmarr_3_r*phdcn$nbl5ug_1,
	   phdcn$npcmarr_3_r*phdcn$nlninc_2,
	   phdcn$npcmarr_3_r*phdcn$npcemply_2,
	   phdcn$npcmarr_3_r*phdcn$npcwelf_2,
	   phdcn$npcmarr_3_r*phdcn$npcmarr_2,
	   phdcn$npcmarr_3_r*phdcn$npcengl_2,
	   phdcn$npcmarr_3_r*phdcn$ncondadvg_2,
	   phdcn$npcmarr_3_r*phdcn$nbl5ug_2,
	   phdcn$npcengl_3_r,
	   phdcn$npcengl_3_r*phdcn$ncondadvg_3,
	   phdcn$npcengl_3_r*phdcn$npcage_b,
	   phdcn$npcengl_3_r*phdcn$npchsgrad_b,
	   phdcn$npcengl_3_r*phdcn$npcsomcol_b,
	   phdcn$npcengl_3_r*phdcn$npccolgrd_b,
	   phdcn$npcengl_3_r*phdcn$nownhome_b,
	   phdcn$npcengl_3_r*phdcn$nfemale_b,
	   phdcn$npcengl_3_r*phdcn$nblack_b,
	   phdcn$npcengl_3_r*phdcn$nhispan_b,
	   phdcn$npcengl_3_r*phdcn$nothrace_b,
	   phdcn$npcengl_3_r*phdcn$nfamsize_b,
	   phdcn$npcengl_3_r*phdcn$nlninc_1,
	   phdcn$npcengl_3_r*phdcn$npcemply_1,
	   phdcn$npcengl_3_r*phdcn$npcwelf_1,
	   phdcn$npcengl_3_r*phdcn$npcmarr_1,
	   phdcn$npcengl_3_r*phdcn$npcengl_1,
	   phdcn$npcengl_3_r*phdcn$ncondadvg_1,
	   phdcn$npcengl_3_r*phdcn$nbl5ug_1,
	   phdcn$npcengl_3_r*phdcn$nlninc_2,
	   phdcn$npcengl_3_r*phdcn$npcemply_2,
	   phdcn$npcengl_3_r*phdcn$npcwelf_2,
	   phdcn$npcengl_3_r*phdcn$npcmarr_2,
	   phdcn$npcengl_3_r*phdcn$npcengl_2,
	   phdcn$npcengl_3_r*phdcn$ncondadvg_2,
	   phdcn$npcengl_3_r*phdcn$nbl5ug_2,
	   phdcn$nbl5ug_3_r,
	   phdcn$nbl5ug_3_r*phdcn$ncondadvg_3,
	   phdcn$nbl5ug_3_r*phdcn$npcage_b,
	   phdcn$nbl5ug_3_r*phdcn$npchsgrad_b,
	   phdcn$nbl5ug_3_r*phdcn$npcsomcol_b,
	   phdcn$nbl5ug_3_r*phdcn$npccolgrd_b,
	   phdcn$nbl5ug_3_r*phdcn$nownhome_b,
	   phdcn$nbl5ug_3_r*phdcn$nfemale_b,
	   phdcn$nbl5ug_3_r*phdcn$nblack_b,
	   phdcn$nbl5ug_3_r*phdcn$nhispan_b,
	   phdcn$nbl5ug_3_r*phdcn$nothrace_b,
	   phdcn$nbl5ug_3_r*phdcn$nfamsize_b,
	   phdcn$nbl5ug_3_r*phdcn$nlninc_1,
	   phdcn$nbl5ug_3_r*phdcn$npcemply_1,
	   phdcn$nbl5ug_3_r*phdcn$npcwelf_1,
	   phdcn$nbl5ug_3_r*phdcn$npcmarr_1,
	   phdcn$nbl5ug_3_r*phdcn$npcengl_1,
	   phdcn$nbl5ug_3_r*phdcn$ncondadvg_1,
	   phdcn$nbl5ug_3_r*phdcn$nbl5ug_1,
	   phdcn$nbl5ug_3_r*phdcn$nlninc_2,
	   phdcn$nbl5ug_3_r*phdcn$npcemply_2,
	   phdcn$nbl5ug_3_r*phdcn$npcwelf_2,
	   phdcn$nbl5ug_3_r*phdcn$npcmarr_2,
	   phdcn$nbl5ug_3_r*phdcn$npcengl_2,
	   phdcn$nbl5ug_3_r*phdcn$ncondadvg_2,
	   phdcn$nbl5ug_3_r*phdcn$nbl5ug_2)

	Cj<-cbind(1,
	   phdcn$nlninc_2_r,
	   phdcn$nlninc_2_r*phdcn$ncondadvg_2,
	   phdcn$nlninc_2_r*phdcn$nbl5ug_2,
	   phdcn$nlninc_2_r*phdcn$ncondadvg_3,
	   phdcn$nlninc_2_r*phdcn$nbl5ug_3,
	   phdcn$nlninc_2_r*phdcn$npcage_b,
	   phdcn$nlninc_2_r*phdcn$npchsgrad_b,
	   phdcn$nlninc_2_r*phdcn$npcsomcol_b,
	   phdcn$nlninc_2_r*phdcn$npccolgrd_b,
	   phdcn$nlninc_2_r*phdcn$nownhome_b,
	   phdcn$nlninc_2_r*phdcn$nfemale_b,
	   phdcn$nlninc_2_r*phdcn$nblack_b,
	   phdcn$nlninc_2_r*phdcn$nhispan_b,
	   phdcn$nlninc_2_r*phdcn$nothrace_b,
	   phdcn$nlninc_2_r*phdcn$nfamsize_b,
	   phdcn$nlninc_2_r*phdcn$nlninc_1,
	   phdcn$nlninc_2_r*phdcn$npcemply_1,
	   phdcn$nlninc_2_r*phdcn$npcwelf_1,
	   phdcn$nlninc_2_r*phdcn$npcmarr_1,
	   phdcn$nlninc_2_r*phdcn$npcengl_1,
	   phdcn$nlninc_2_r*phdcn$ncondadvg_1,
	   phdcn$nlninc_2_r*phdcn$nbl5ug_1,
	   phdcn$npcemply_2_r,
	   phdcn$npcemply_2_r*phdcn$ncondadvg_2,
	   phdcn$npcemply_2_r*phdcn$nbl5ug_2,
	   phdcn$npcemply_2_r*phdcn$ncondadvg_3,
	   phdcn$npcemply_2_r*phdcn$nbl5ug_3,
	   phdcn$npcemply_2_r*phdcn$npcage_b,
	   phdcn$npcemply_2_r*phdcn$npchsgrad_b,
	   phdcn$npcemply_2_r*phdcn$npcsomcol_b,
	   phdcn$npcemply_2_r*phdcn$npccolgrd_b,
	   phdcn$npcemply_2_r*phdcn$nownhome_b,
	   phdcn$npcemply_2_r*phdcn$nfemale_b,
	   phdcn$npcemply_2_r*phdcn$nblack_b,
	   phdcn$npcemply_2_r*phdcn$nhispan_b,
	   phdcn$npcemply_2_r*phdcn$nothrace_b,
	   phdcn$npcemply_2_r*phdcn$nfamsize_b,
	   phdcn$npcemply_2_r*phdcn$nlninc_1,
	   phdcn$npcemply_2_r*phdcn$npcemply_1,
	   phdcn$npcemply_2_r*phdcn$npcwelf_1,
	   phdcn$npcemply_2_r*phdcn$npcmarr_1,
	   phdcn$npcemply_2_r*phdcn$npcengl_1,
	   phdcn$npcemply_2_r*phdcn$ncondadvg_1,
	   phdcn$npcemply_2_r*phdcn$nbl5ug_1,
	   phdcn$npcwelf_2_r,
	   phdcn$npcwelf_2_r*phdcn$ncondadvg_2,
	   phdcn$npcwelf_2_r*phdcn$nbl5ug_2,
	   phdcn$npcwelf_2_r*phdcn$ncondadvg_3,
	   phdcn$npcwelf_2_r*phdcn$nbl5ug_3,
	   phdcn$npcwelf_2_r*phdcn$npcage_b,
	   phdcn$npcwelf_2_r*phdcn$npchsgrad_b,
	   phdcn$npcwelf_2_r*phdcn$npcsomcol_b,
	   phdcn$npcwelf_2_r*phdcn$npccolgrd_b,
	   phdcn$npcwelf_2_r*phdcn$nownhome_b,
	   phdcn$npcwelf_2_r*phdcn$nfemale_b,
	   phdcn$npcwelf_2_r*phdcn$nblack_b,
	   phdcn$npcwelf_2_r*phdcn$nhispan_b,
	   phdcn$npcwelf_2_r*phdcn$nothrace_b,
	   phdcn$npcwelf_2_r*phdcn$nfamsize_b,
	   phdcn$npcwelf_2_r*phdcn$nlninc_1,
	   phdcn$npcwelf_2_r*phdcn$npcemply_1,
	   phdcn$npcwelf_2_r*phdcn$npcwelf_1,
	   phdcn$npcwelf_2_r*phdcn$npcmarr_1,
	   phdcn$npcwelf_2_r*phdcn$npcengl_1,
	   phdcn$npcwelf_2_r*phdcn$ncondadvg_1,
	   phdcn$npcwelf_2_r*phdcn$nbl5ug_1,
	   phdcn$npcmarr_2_r,
	   phdcn$npcmarr_2_r*phdcn$ncondadvg_2,
	   phdcn$npcmarr_2_r*phdcn$nbl5ug_2,
	   phdcn$npcmarr_2_r*phdcn$ncondadvg_3,
	   phdcn$npcmarr_2_r*phdcn$nbl5ug_3,
	   phdcn$npcmarr_2_r*phdcn$npcage_b,
	   phdcn$npcmarr_2_r*phdcn$npchsgrad_b,
	   phdcn$npcmarr_2_r*phdcn$npcsomcol_b,
	   phdcn$npcmarr_2_r*phdcn$npccolgrd_b,
	   phdcn$npcmarr_2_r*phdcn$nownhome_b,
	   phdcn$npcmarr_2_r*phdcn$nfemale_b,
	   phdcn$npcmarr_2_r*phdcn$nblack_b,
	   phdcn$npcmarr_2_r*phdcn$nhispan_b,
	   phdcn$npcmarr_2_r*phdcn$nothrace_b,
	   phdcn$npcmarr_2_r*phdcn$nfamsize_b,
	   phdcn$npcmarr_2_r*phdcn$nlninc_1,
	   phdcn$npcmarr_2_r*phdcn$npcemply_1,
	   phdcn$npcmarr_2_r*phdcn$npcwelf_1,
	   phdcn$npcmarr_2_r*phdcn$npcmarr_1,
	   phdcn$npcmarr_2_r*phdcn$npcengl_1,
	   phdcn$npcmarr_2_r*phdcn$ncondadvg_1,
	   phdcn$npcmarr_2_r*phdcn$nbl5ug_1,
	   phdcn$npcengl_2_r,
	   phdcn$npcengl_2_r*phdcn$ncondadvg_2,
	   phdcn$npcengl_2_r*phdcn$nbl5ug_2,
	   phdcn$npcengl_2_r*phdcn$ncondadvg_3,
	   phdcn$npcengl_2_r*phdcn$nbl5ug_3,
	   phdcn$npcengl_2_r*phdcn$npcage_b,
	   phdcn$npcengl_2_r*phdcn$npchsgrad_b,
	   phdcn$npcengl_2_r*phdcn$npcsomcol_b,
	   phdcn$npcengl_2_r*phdcn$npccolgrd_b,
	   phdcn$npcengl_2_r*phdcn$nownhome_b,
	   phdcn$npcengl_2_r*phdcn$nfemale_b,
	   phdcn$npcengl_2_r*phdcn$nblack_b,
	   phdcn$npcengl_2_r*phdcn$nhispan_b,
	   phdcn$npcengl_2_r*phdcn$nothrace_b,
	   phdcn$npcengl_2_r*phdcn$nfamsize_b,
	   phdcn$npcengl_2_r*phdcn$nlninc_1,
	   phdcn$npcengl_2_r*phdcn$npcemply_1,
	   phdcn$npcengl_2_r*phdcn$npcwelf_1,
	   phdcn$npcengl_2_r*phdcn$npcmarr_1,
	   phdcn$npcengl_2_r*phdcn$npcengl_1,
	   phdcn$npcengl_2_r*phdcn$ncondadvg_1,
	   phdcn$npcengl_2_r*phdcn$nbl5ug_1,
	   phdcn$nlninc_3_r,
	   phdcn$nlninc_3_r*phdcn$ncondadvg_3,
	   phdcn$nlninc_3_r*phdcn$nbl5ug_3,
	   phdcn$nlninc_3_r*phdcn$npcage_b,
	   phdcn$nlninc_3_r*phdcn$npchsgrad_b,
	   phdcn$nlninc_3_r*phdcn$npcsomcol_b,
	   phdcn$nlninc_3_r*phdcn$npccolgrd_b,
	   phdcn$nlninc_3_r*phdcn$nownhome_b,
	   phdcn$nlninc_3_r*phdcn$nfemale_b,
	   phdcn$nlninc_3_r*phdcn$nblack_b,
	   phdcn$nlninc_3_r*phdcn$nhispan_b,
	   phdcn$nlninc_3_r*phdcn$nothrace_b,
	   phdcn$nlninc_3_r*phdcn$nfamsize_b,
	   phdcn$nlninc_3_r*phdcn$nlninc_1,
	   phdcn$nlninc_3_r*phdcn$npcemply_1,
	   phdcn$nlninc_3_r*phdcn$npcwelf_1,
	   phdcn$nlninc_3_r*phdcn$npcmarr_1,
	   phdcn$nlninc_3_r*phdcn$npcengl_1,
	   phdcn$nlninc_3_r*phdcn$ncondadvg_1,
	   phdcn$nlninc_3_r*phdcn$nbl5ug_1,
	   phdcn$nlninc_3_r*phdcn$nlninc_2,
	   phdcn$nlninc_3_r*phdcn$npcemply_2,
	   phdcn$nlninc_3_r*phdcn$npcwelf_2,
	   phdcn$nlninc_3_r*phdcn$npcmarr_2,
	   phdcn$nlninc_3_r*phdcn$npcengl_2,
	   phdcn$nlninc_3_r*phdcn$ncondadvg_2,
	   phdcn$nlninc_3_r*phdcn$nbl5ug_2,
	   phdcn$npcemply_3_r,
	   phdcn$npcemply_3_r*phdcn$ncondadvg_3,
	   phdcn$npcemply_3_r*phdcn$nbl5ug_3,
	   phdcn$npcemply_3_r*phdcn$npcage_b,
	   phdcn$npcemply_3_r*phdcn$npchsgrad_b,
	   phdcn$npcemply_3_r*phdcn$npcsomcol_b,
	   phdcn$npcemply_3_r*phdcn$npccolgrd_b,
	   phdcn$npcemply_3_r*phdcn$nownhome_b,
	   phdcn$npcemply_3_r*phdcn$nfemale_b,
	   phdcn$npcemply_3_r*phdcn$nblack_b,
	   phdcn$npcemply_3_r*phdcn$nhispan_b,
	   phdcn$npcemply_3_r*phdcn$nothrace_b,
	   phdcn$npcemply_3_r*phdcn$nfamsize_b,
	   phdcn$npcemply_3_r*phdcn$nlninc_1,
	   phdcn$npcemply_3_r*phdcn$npcemply_1,
	   phdcn$npcemply_3_r*phdcn$npcwelf_1,
	   phdcn$npcemply_3_r*phdcn$npcmarr_1,
	   phdcn$npcemply_3_r*phdcn$npcengl_1,
	   phdcn$npcemply_3_r*phdcn$ncondadvg_1,
	   phdcn$npcemply_3_r*phdcn$nbl5ug_1,
	   phdcn$npcemply_3_r*phdcn$nlninc_2,
	   phdcn$npcemply_3_r*phdcn$npcemply_2,
	   phdcn$npcemply_3_r*phdcn$npcwelf_2,
	   phdcn$npcemply_3_r*phdcn$npcmarr_2,
	   phdcn$npcemply_3_r*phdcn$npcengl_2,
	   phdcn$npcemply_3_r*phdcn$ncondadvg_2,
	   phdcn$npcemply_3_r*phdcn$nbl5ug_2,
	   phdcn$npcwelf_3_r,
	   phdcn$npcwelf_3_r*phdcn$ncondadvg_3,
	   phdcn$npcwelf_3_r*phdcn$nbl5ug_3,
	   phdcn$npcwelf_3_r*phdcn$npcage_b,
	   phdcn$npcwelf_3_r*phdcn$npchsgrad_b,
	   phdcn$npcwelf_3_r*phdcn$npcsomcol_b,
	   phdcn$npcwelf_3_r*phdcn$npccolgrd_b,
	   phdcn$npcwelf_3_r*phdcn$nownhome_b,
	   phdcn$npcwelf_3_r*phdcn$nfemale_b,
	   phdcn$npcwelf_3_r*phdcn$nblack_b,
	   phdcn$npcwelf_3_r*phdcn$nhispan_b,
	   phdcn$npcwelf_3_r*phdcn$nothrace_b,
	   phdcn$npcwelf_3_r*phdcn$nfamsize_b,
	   phdcn$npcwelf_3_r*phdcn$nlninc_1,
	   phdcn$npcwelf_3_r*phdcn$npcemply_1,
	   phdcn$npcwelf_3_r*phdcn$npcwelf_1,
	   phdcn$npcwelf_3_r*phdcn$npcmarr_1,
	   phdcn$npcwelf_3_r*phdcn$npcengl_1,
	   phdcn$npcwelf_3_r*phdcn$ncondadvg_1,
	   phdcn$npcwelf_3_r*phdcn$nbl5ug_1,
	   phdcn$npcwelf_3_r*phdcn$nlninc_2,
	   phdcn$npcwelf_3_r*phdcn$npcemply_2,
	   phdcn$npcwelf_3_r*phdcn$npcwelf_2,
	   phdcn$npcwelf_3_r*phdcn$npcmarr_2,
	   phdcn$npcwelf_3_r*phdcn$npcengl_2,
	   phdcn$npcwelf_3_r*phdcn$ncondadvg_2,
	   phdcn$npcwelf_3_r*phdcn$nbl5ug_2,
	   phdcn$npcmarr_3_r,
	   phdcn$npcmarr_3_r*phdcn$ncondadvg_3,
	   phdcn$npcmarr_3_r*phdcn$nbl5ug_3,
	   phdcn$npcmarr_3_r*phdcn$npcage_b,
	   phdcn$npcmarr_3_r*phdcn$npchsgrad_b,
	   phdcn$npcmarr_3_r*phdcn$npcsomcol_b,
	   phdcn$npcmarr_3_r*phdcn$npccolgrd_b,
	   phdcn$npcmarr_3_r*phdcn$nownhome_b,
	   phdcn$npcmarr_3_r*phdcn$nfemale_b,
	   phdcn$npcmarr_3_r*phdcn$nblack_b,
	   phdcn$npcmarr_3_r*phdcn$nhispan_b,
	   phdcn$npcmarr_3_r*phdcn$nothrace_b,
	   phdcn$npcmarr_3_r*phdcn$nfamsize_b,
	   phdcn$npcmarr_3_r*phdcn$nlninc_1,
	   phdcn$npcmarr_3_r*phdcn$npcemply_1,
	   phdcn$npcmarr_3_r*phdcn$npcwelf_1,
	   phdcn$npcmarr_3_r*phdcn$npcmarr_1,
	   phdcn$npcmarr_3_r*phdcn$npcengl_1,
	   phdcn$npcmarr_3_r*phdcn$ncondadvg_1,
	   phdcn$npcmarr_3_r*phdcn$nbl5ug_1,
	   phdcn$npcmarr_3_r*phdcn$nlninc_2,
	   phdcn$npcmarr_3_r*phdcn$npcemply_2,
	   phdcn$npcmarr_3_r*phdcn$npcwelf_2,
	   phdcn$npcmarr_3_r*phdcn$npcmarr_2,
	   phdcn$npcmarr_3_r*phdcn$npcengl_2,
	   phdcn$npcmarr_3_r*phdcn$ncondadvg_2,
	   phdcn$npcmarr_3_r*phdcn$nbl5ug_2,
	   phdcn$npcengl_3_r,
	   phdcn$npcengl_3_r*phdcn$ncondadvg_3,
	   phdcn$npcengl_3_r*phdcn$nbl5ug_3,
	   phdcn$npcengl_3_r*phdcn$npcage_b,
	   phdcn$npcengl_3_r*phdcn$npchsgrad_b,
	   phdcn$npcengl_3_r*phdcn$npcsomcol_b,
	   phdcn$npcengl_3_r*phdcn$npccolgrd_b,
	   phdcn$npcengl_3_r*phdcn$nownhome_b,
	   phdcn$npcengl_3_r*phdcn$nfemale_b,
	   phdcn$npcengl_3_r*phdcn$nblack_b,
	   phdcn$npcengl_3_r*phdcn$nhispan_b,
	   phdcn$npcengl_3_r*phdcn$nothrace_b,
	   phdcn$npcengl_3_r*phdcn$nfamsize_b,
	   phdcn$npcengl_3_r*phdcn$nlninc_1,
	   phdcn$npcengl_3_r*phdcn$npcemply_1,
	   phdcn$npcengl_3_r*phdcn$npcwelf_1,
	   phdcn$npcengl_3_r*phdcn$npcmarr_1,
	   phdcn$npcengl_3_r*phdcn$npcengl_1,
	   phdcn$npcengl_3_r*phdcn$ncondadvg_1,
	   phdcn$npcengl_3_r*phdcn$nbl5ug_1,
	   phdcn$npcengl_3_r*phdcn$nlninc_2,
	   phdcn$npcengl_3_r*phdcn$npcemply_2,
	   phdcn$npcengl_3_r*phdcn$npcwelf_2,
	   phdcn$npcengl_3_r*phdcn$npcmarr_2,
	   phdcn$npcengl_3_r*phdcn$npcengl_2,
	   phdcn$npcengl_3_r*phdcn$ncondadvg_2,
	   phdcn$npcengl_3_r*phdcn$nbl5ug_2)

	# DEFINE BASE WEIGHTS #
	Q<-rep(1,length(phdcn$nsubid))

	# DEFINE COLUMN MEANS OF CONSTRAINT MATRIX #
	Mm<-c(sum(Q),rep(0,ncol(Cm)-1))
	Mj<-c(sum(Q),rep(0,ncol(Cj)-1))

	# COMPUTE BALANCING WEIGHTS #
	rbalwtm<-resbal(Mm,Cm,Q,tol=0.01)$W
	rbalwtj<-resbal(Mj,Cj,Q,tol=0.01)$W

	# COMPUTE NEW VARIABLES #
	phdcn$cumndadvg<-(phdcn$ncondadvg_1+phdcn$ncondadvg_2+phdcn$ncondadvg_3)/3
	phdcn$cumnbl5ug<-(phdcn$nbl5ug_1+phdcn$nbl5ug_2+phdcn$nbl5ug_3)/3
	phdcn$nppvt_3<-(phdcn$nppvt_3-mean(phdcn$nppvt_3))/sd(phdcn$nppvt_3)

	# FIT MODELS #
	m1<-lm(nppvt_3~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
			   nlninc_1_r+npcemply_1_r+npcwelf_1_r+npcmarr_1_r+npcengl_1_r+nsubage_3_r+cumndadvg,data=phdcn,weights=rbalwtm)
	
	m2<-lm(nppvt_3~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
			   nlninc_1_r+npcemply_1_r+npcwelf_1_r+npcmarr_1_r+npcengl_1_r+nsubage_3_r+cumndadvg+cumnbl5ug,data=phdcn,weights=rbalwtj)

	# TOTAL EFFECT OF NHOOD POVERTY #
	miest.rbal[i,1]<-m1$coefficients["cumndadvg"]*(astar-a)

	# JOINT EFFECT OF NHOOD POVERTY AND ENVIRONMENTAL LEAD CONTAMINATION #
	miest.rbal[i,2]<-m2$coefficients["cumndadvg"]*(astar-a)+m2$coefficients["cumnbl5ug"]*(mstar-m)

	# CONTROLLED DIRECT EFFECT OF NHOOD POVERTY #
	miest.rbal[i,3]<-m2$coefficients["cumndadvg"]*(astar-a)

	# CONTROLLED MEDIATOR EFFECT OF ENVIRONMENTAL LEAD CONTAMINATION #
	miest.rbal[i,4]<-m2$coefficients["cumnbl5ug"]*(mstar-m)

	# DIFFERENCE BETWEEN ATE AND CDE #
	miest.rbal[i,5]<-miest.rbal[i,1]-miest.rbal[i,3]
	
	# COMPUTE BOOTSTRAP SEs #
	boot.ate.rbal<-boot.aje.rbal<-boot.cde.rbal<-boot.cme.rbal<-boot.dif1.rbal<-matrix(data=NA,nrow=nboot,ncol=1)

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

		boot.Cm<-cbind(1,
		   boot.phdcn$nbl5ug_1_r,
		   boot.phdcn$nbl5ug_1_r*boot.phdcn$ncondadvg_2,
		   boot.phdcn$nbl5ug_1_r*boot.phdcn$ncondadvg_3,
		   boot.phdcn$nbl5ug_1_r*boot.phdcn$npcage_b,
		   boot.phdcn$nbl5ug_1_r*boot.phdcn$npchsgrad_b,
		   boot.phdcn$nbl5ug_1_r*boot.phdcn$npcsomcol_b,
		   boot.phdcn$nbl5ug_1_r*boot.phdcn$npccolgrd_b,
		   boot.phdcn$nbl5ug_1_r*boot.phdcn$nownhome_b,
		   boot.phdcn$nbl5ug_1_r*boot.phdcn$nfemale_b,
		   boot.phdcn$nbl5ug_1_r*boot.phdcn$nblack_b,
		   boot.phdcn$nbl5ug_1_r*boot.phdcn$nhispan_b,
		   boot.phdcn$nbl5ug_1_r*boot.phdcn$nothrace_b,
		   boot.phdcn$nbl5ug_1_r*boot.phdcn$nfamsize_b,
		   boot.phdcn$nbl5ug_1_r*boot.phdcn$nlninc_1,
		   boot.phdcn$nbl5ug_1_r*boot.phdcn$npcemply_1,
		   boot.phdcn$nbl5ug_1_r*boot.phdcn$npcwelf_1,
		   boot.phdcn$nbl5ug_1_r*boot.phdcn$npcmarr_1,
		   boot.phdcn$nbl5ug_1_r*boot.phdcn$npcengl_1,
		   boot.phdcn$nbl5ug_1_r*boot.phdcn$ncondadvg_1,
		   boot.phdcn$nlninc_2_r,
		   boot.phdcn$nlninc_2_r*boot.phdcn$ncondadvg_2,
		   boot.phdcn$nlninc_2_r*boot.phdcn$ncondadvg_3,
		   boot.phdcn$nlninc_2_r*boot.phdcn$npcage_b,
		   boot.phdcn$nlninc_2_r*boot.phdcn$npchsgrad_b,
		   boot.phdcn$nlninc_2_r*boot.phdcn$npcsomcol_b,
		   boot.phdcn$nlninc_2_r*boot.phdcn$npccolgrd_b,
		   boot.phdcn$nlninc_2_r*boot.phdcn$nownhome_b,
		   boot.phdcn$nlninc_2_r*boot.phdcn$nfemale_b,
		   boot.phdcn$nlninc_2_r*boot.phdcn$nblack_b,
		   boot.phdcn$nlninc_2_r*boot.phdcn$nhispan_b,
		   boot.phdcn$nlninc_2_r*boot.phdcn$nothrace_b,
		   boot.phdcn$nlninc_2_r*boot.phdcn$nfamsize_b,
		   boot.phdcn$nlninc_2_r*boot.phdcn$nlninc_1,
		   boot.phdcn$nlninc_2_r*boot.phdcn$npcemply_1,
		   boot.phdcn$nlninc_2_r*boot.phdcn$npcwelf_1,
		   boot.phdcn$nlninc_2_r*boot.phdcn$npcmarr_1,
		   boot.phdcn$nlninc_2_r*boot.phdcn$npcengl_1,
		   boot.phdcn$nlninc_2_r*boot.phdcn$ncondadvg_1,
		   boot.phdcn$nlninc_2_r*boot.phdcn$nbl5ug_1,
		   boot.phdcn$npcemply_2_r,
		   boot.phdcn$npcemply_2_r*boot.phdcn$ncondadvg_2,
		   boot.phdcn$npcemply_2_r*boot.phdcn$ncondadvg_3,
		   boot.phdcn$npcemply_2_r*boot.phdcn$npcage_b,
		   boot.phdcn$npcemply_2_r*boot.phdcn$npchsgrad_b,
		   boot.phdcn$npcemply_2_r*boot.phdcn$npcsomcol_b,
		   boot.phdcn$npcemply_2_r*boot.phdcn$npccolgrd_b,
		   boot.phdcn$npcemply_2_r*boot.phdcn$nownhome_b,
		   boot.phdcn$npcemply_2_r*boot.phdcn$nfemale_b,
		   boot.phdcn$npcemply_2_r*boot.phdcn$nblack_b,
		   boot.phdcn$npcemply_2_r*boot.phdcn$nhispan_b,
		   boot.phdcn$npcemply_2_r*boot.phdcn$nothrace_b,
		   boot.phdcn$npcemply_2_r*boot.phdcn$nfamsize_b,
		   boot.phdcn$npcemply_2_r*boot.phdcn$nlninc_1,
		   boot.phdcn$npcemply_2_r*boot.phdcn$npcemply_1,
		   boot.phdcn$npcemply_2_r*boot.phdcn$npcwelf_1,
		   boot.phdcn$npcemply_2_r*boot.phdcn$npcmarr_1,
		   boot.phdcn$npcemply_2_r*boot.phdcn$npcengl_1,
		   boot.phdcn$npcemply_2_r*boot.phdcn$ncondadvg_1,
		   boot.phdcn$npcemply_2_r*boot.phdcn$nbl5ug_1,
		   boot.phdcn$npcwelf_2_r,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$ncondadvg_2,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$ncondadvg_3,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$npcage_b,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$npchsgrad_b,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$npcsomcol_b,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$npccolgrd_b,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$nownhome_b,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$nfemale_b,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$nblack_b,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$nhispan_b,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$nothrace_b,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$nfamsize_b,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$nlninc_1,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$npcemply_1,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$npcwelf_1,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$npcmarr_1,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$npcengl_1,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$ncondadvg_1,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$nbl5ug_1,
		   boot.phdcn$npcmarr_2_r,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$ncondadvg_2,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$ncondadvg_3,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$npcage_b,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$npchsgrad_b,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$npcsomcol_b,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$npccolgrd_b,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$nownhome_b,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$nfemale_b,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$nblack_b,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$nhispan_b,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$nothrace_b,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$nfamsize_b,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$nlninc_1,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$npcemply_1,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$npcwelf_1,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$npcmarr_1,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$npcengl_1,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$ncondadvg_1,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$nbl5ug_1,
		   boot.phdcn$npcengl_2_r,
		   boot.phdcn$npcengl_2_r*boot.phdcn$ncondadvg_2,
		   boot.phdcn$npcengl_2_r*boot.phdcn$ncondadvg_3,
		   boot.phdcn$npcengl_2_r*boot.phdcn$npcage_b,
		   boot.phdcn$npcengl_2_r*boot.phdcn$npchsgrad_b,
		   boot.phdcn$npcengl_2_r*boot.phdcn$npcsomcol_b,
		   boot.phdcn$npcengl_2_r*boot.phdcn$npccolgrd_b,
		   boot.phdcn$npcengl_2_r*boot.phdcn$nownhome_b,
		   boot.phdcn$npcengl_2_r*boot.phdcn$nfemale_b,
		   boot.phdcn$npcengl_2_r*boot.phdcn$nblack_b,
		   boot.phdcn$npcengl_2_r*boot.phdcn$nhispan_b,
		   boot.phdcn$npcengl_2_r*boot.phdcn$nothrace_b,
		   boot.phdcn$npcengl_2_r*boot.phdcn$nfamsize_b,
		   boot.phdcn$npcengl_2_r*boot.phdcn$nlninc_1,
		   boot.phdcn$npcengl_2_r*boot.phdcn$npcemply_1,
		   boot.phdcn$npcengl_2_r*boot.phdcn$npcwelf_1,
		   boot.phdcn$npcengl_2_r*boot.phdcn$npcmarr_1,
		   boot.phdcn$npcengl_2_r*boot.phdcn$npcengl_1,
		   boot.phdcn$npcengl_2_r*boot.phdcn$ncondadvg_1,
		   boot.phdcn$npcengl_2_r*boot.phdcn$nbl5ug_1,
		   boot.phdcn$nbl5ug_2_r,
		   boot.phdcn$nbl5ug_2_r*boot.phdcn$ncondadvg_3,
		   boot.phdcn$nbl5ug_2_r*boot.phdcn$npcage_b,
		   boot.phdcn$nbl5ug_2_r*boot.phdcn$npchsgrad_b,
		   boot.phdcn$nbl5ug_2_r*boot.phdcn$npcsomcol_b,
		   boot.phdcn$nbl5ug_2_r*boot.phdcn$npccolgrd_b,
		   boot.phdcn$nbl5ug_2_r*boot.phdcn$nownhome_b,
		   boot.phdcn$nbl5ug_2_r*boot.phdcn$nfemale_b,
		   boot.phdcn$nbl5ug_2_r*boot.phdcn$nblack_b,
		   boot.phdcn$nbl5ug_2_r*boot.phdcn$nhispan_b,
		   boot.phdcn$nbl5ug_2_r*boot.phdcn$nothrace_b,
		   boot.phdcn$nbl5ug_2_r*boot.phdcn$nfamsize_b,
		   boot.phdcn$nbl5ug_2_r*boot.phdcn$nlninc_1,
		   boot.phdcn$nbl5ug_2_r*boot.phdcn$npcemply_1,
		   boot.phdcn$nbl5ug_2_r*boot.phdcn$npcwelf_1,
		   boot.phdcn$nbl5ug_2_r*boot.phdcn$npcmarr_1,
		   boot.phdcn$nbl5ug_2_r*boot.phdcn$npcengl_1,
		   boot.phdcn$nbl5ug_2_r*boot.phdcn$ncondadvg_1,
		   boot.phdcn$nbl5ug_2_r*boot.phdcn$nbl5ug_1,
		   boot.phdcn$nbl5ug_2_r*boot.phdcn$nlninc_2,
		   boot.phdcn$nbl5ug_2_r*boot.phdcn$npcemply_2,
		   boot.phdcn$nbl5ug_2_r*boot.phdcn$npcwelf_2,
		   boot.phdcn$nbl5ug_2_r*boot.phdcn$npcmarr_2,
		   boot.phdcn$nbl5ug_2_r*boot.phdcn$npcengl_2,
		   boot.phdcn$nbl5ug_2_r*boot.phdcn$ncondadvg_2,
		   boot.phdcn$nlninc_3_r,
		   boot.phdcn$nlninc_3_r*boot.phdcn$ncondadvg_3,
		   boot.phdcn$nlninc_3_r*boot.phdcn$npcage_b,
		   boot.phdcn$nlninc_3_r*boot.phdcn$npchsgrad_b,
		   boot.phdcn$nlninc_3_r*boot.phdcn$npcsomcol_b,
		   boot.phdcn$nlninc_3_r*boot.phdcn$npccolgrd_b,
		   boot.phdcn$nlninc_3_r*boot.phdcn$nownhome_b,
		   boot.phdcn$nlninc_3_r*boot.phdcn$nfemale_b,
		   boot.phdcn$nlninc_3_r*boot.phdcn$nblack_b,
		   boot.phdcn$nlninc_3_r*boot.phdcn$nhispan_b,
		   boot.phdcn$nlninc_3_r*boot.phdcn$nothrace_b,
		   boot.phdcn$nlninc_3_r*boot.phdcn$nfamsize_b,
		   boot.phdcn$nlninc_3_r*boot.phdcn$nlninc_1,
		   boot.phdcn$nlninc_3_r*boot.phdcn$npcemply_1,
		   boot.phdcn$nlninc_3_r*boot.phdcn$npcwelf_1,
		   boot.phdcn$nlninc_3_r*boot.phdcn$npcmarr_1,
		   boot.phdcn$nlninc_3_r*boot.phdcn$npcengl_1,
		   boot.phdcn$nlninc_3_r*boot.phdcn$ncondadvg_1,
		   boot.phdcn$nlninc_3_r*boot.phdcn$nbl5ug_1,
		   boot.phdcn$nlninc_3_r*boot.phdcn$nlninc_2,
		   boot.phdcn$nlninc_3_r*boot.phdcn$npcemply_2,
		   boot.phdcn$nlninc_3_r*boot.phdcn$npcwelf_2,
		   boot.phdcn$nlninc_3_r*boot.phdcn$npcmarr_2,
		   boot.phdcn$nlninc_3_r*boot.phdcn$npcengl_2,
		   boot.phdcn$nlninc_3_r*boot.phdcn$ncondadvg_2,
		   boot.phdcn$nlninc_3_r*boot.phdcn$nbl5ug_2,
		   boot.phdcn$npcemply_3_r,
		   boot.phdcn$npcemply_3_r*boot.phdcn$ncondadvg_3,
		   boot.phdcn$npcemply_3_r*boot.phdcn$npcage_b,
		   boot.phdcn$npcemply_3_r*boot.phdcn$npchsgrad_b,
		   boot.phdcn$npcemply_3_r*boot.phdcn$npcsomcol_b,
		   boot.phdcn$npcemply_3_r*boot.phdcn$npccolgrd_b,
		   boot.phdcn$npcemply_3_r*boot.phdcn$nownhome_b,
		   boot.phdcn$npcemply_3_r*boot.phdcn$nfemale_b,
		   boot.phdcn$npcemply_3_r*boot.phdcn$nblack_b,
		   boot.phdcn$npcemply_3_r*boot.phdcn$nhispan_b,
		   boot.phdcn$npcemply_3_r*boot.phdcn$nothrace_b,
		   boot.phdcn$npcemply_3_r*boot.phdcn$nfamsize_b,
		   boot.phdcn$npcemply_3_r*boot.phdcn$nlninc_1,
		   boot.phdcn$npcemply_3_r*boot.phdcn$npcemply_1,
		   boot.phdcn$npcemply_3_r*boot.phdcn$npcwelf_1,
		   boot.phdcn$npcemply_3_r*boot.phdcn$npcmarr_1,
		   boot.phdcn$npcemply_3_r*boot.phdcn$npcengl_1,
		   boot.phdcn$npcemply_3_r*boot.phdcn$ncondadvg_1,
		   boot.phdcn$npcemply_3_r*boot.phdcn$nbl5ug_1,
		   boot.phdcn$npcemply_3_r*boot.phdcn$nlninc_2,
		   boot.phdcn$npcemply_3_r*boot.phdcn$npcemply_2,
		   boot.phdcn$npcemply_3_r*boot.phdcn$npcwelf_2,
		   boot.phdcn$npcemply_3_r*boot.phdcn$npcmarr_2,
		   boot.phdcn$npcemply_3_r*boot.phdcn$npcengl_2,
		   boot.phdcn$npcemply_3_r*boot.phdcn$ncondadvg_2,
		   boot.phdcn$npcemply_3_r*boot.phdcn$nbl5ug_2,
		   boot.phdcn$npcwelf_3_r,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$ncondadvg_3,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$npcage_b,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$npchsgrad_b,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$npcsomcol_b,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$npccolgrd_b,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$nownhome_b,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$nfemale_b,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$nblack_b,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$nhispan_b,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$nothrace_b,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$nfamsize_b,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$nlninc_1,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$npcemply_1,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$npcwelf_1,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$npcmarr_1,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$npcengl_1,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$ncondadvg_1,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$nbl5ug_1,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$nlninc_2,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$npcemply_2,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$npcwelf_2,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$npcmarr_2,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$npcengl_2,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$ncondadvg_2,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$nbl5ug_2,
		   boot.phdcn$npcmarr_3_r,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$ncondadvg_3,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$npcage_b,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$npchsgrad_b,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$npcsomcol_b,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$npccolgrd_b,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$nownhome_b,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$nfemale_b,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$nblack_b,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$nhispan_b,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$nothrace_b,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$nfamsize_b,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$nlninc_1,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$npcemply_1,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$npcwelf_1,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$npcmarr_1,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$npcengl_1,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$ncondadvg_1,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$nbl5ug_1,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$nlninc_2,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$npcemply_2,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$npcwelf_2,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$npcmarr_2,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$npcengl_2,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$ncondadvg_2,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$nbl5ug_2,
		   boot.phdcn$npcengl_3_r,
		   boot.phdcn$npcengl_3_r*boot.phdcn$ncondadvg_3,
		   boot.phdcn$npcengl_3_r*boot.phdcn$npcage_b,
		   boot.phdcn$npcengl_3_r*boot.phdcn$npchsgrad_b,
		   boot.phdcn$npcengl_3_r*boot.phdcn$npcsomcol_b,
		   boot.phdcn$npcengl_3_r*boot.phdcn$npccolgrd_b,
		   boot.phdcn$npcengl_3_r*boot.phdcn$nownhome_b,
		   boot.phdcn$npcengl_3_r*boot.phdcn$nfemale_b,
		   boot.phdcn$npcengl_3_r*boot.phdcn$nblack_b,
		   boot.phdcn$npcengl_3_r*boot.phdcn$nhispan_b,
		   boot.phdcn$npcengl_3_r*boot.phdcn$nothrace_b,
		   boot.phdcn$npcengl_3_r*boot.phdcn$nfamsize_b,
		   boot.phdcn$npcengl_3_r*boot.phdcn$nlninc_1,
		   boot.phdcn$npcengl_3_r*boot.phdcn$npcemply_1,
		   boot.phdcn$npcengl_3_r*boot.phdcn$npcwelf_1,
		   boot.phdcn$npcengl_3_r*boot.phdcn$npcmarr_1,
		   boot.phdcn$npcengl_3_r*boot.phdcn$npcengl_1,
		   boot.phdcn$npcengl_3_r*boot.phdcn$ncondadvg_1,
		   boot.phdcn$npcengl_3_r*boot.phdcn$nbl5ug_1,
		   boot.phdcn$npcengl_3_r*boot.phdcn$nlninc_2,
		   boot.phdcn$npcengl_3_r*boot.phdcn$npcemply_2,
		   boot.phdcn$npcengl_3_r*boot.phdcn$npcwelf_2,
		   boot.phdcn$npcengl_3_r*boot.phdcn$npcmarr_2,
		   boot.phdcn$npcengl_3_r*boot.phdcn$npcengl_2,
		   boot.phdcn$npcengl_3_r*boot.phdcn$ncondadvg_2,
		   boot.phdcn$npcengl_3_r*boot.phdcn$nbl5ug_2,
		   boot.phdcn$nbl5ug_3_r,
		   boot.phdcn$nbl5ug_3_r*boot.phdcn$ncondadvg_3,
		   boot.phdcn$nbl5ug_3_r*boot.phdcn$npcage_b,
		   boot.phdcn$nbl5ug_3_r*boot.phdcn$npchsgrad_b,
		   boot.phdcn$nbl5ug_3_r*boot.phdcn$npcsomcol_b,
		   boot.phdcn$nbl5ug_3_r*boot.phdcn$npccolgrd_b,
		   boot.phdcn$nbl5ug_3_r*boot.phdcn$nownhome_b,
		   boot.phdcn$nbl5ug_3_r*boot.phdcn$nfemale_b,
		   boot.phdcn$nbl5ug_3_r*boot.phdcn$nblack_b,
		   boot.phdcn$nbl5ug_3_r*boot.phdcn$nhispan_b,
		   boot.phdcn$nbl5ug_3_r*boot.phdcn$nothrace_b,
		   boot.phdcn$nbl5ug_3_r*boot.phdcn$nfamsize_b,
		   boot.phdcn$nbl5ug_3_r*boot.phdcn$nlninc_1,
		   boot.phdcn$nbl5ug_3_r*boot.phdcn$npcemply_1,
		   boot.phdcn$nbl5ug_3_r*boot.phdcn$npcwelf_1,
		   boot.phdcn$nbl5ug_3_r*boot.phdcn$npcmarr_1,
		   boot.phdcn$nbl5ug_3_r*boot.phdcn$npcengl_1,
		   boot.phdcn$nbl5ug_3_r*boot.phdcn$ncondadvg_1,
		   boot.phdcn$nbl5ug_3_r*boot.phdcn$nbl5ug_1,
		   boot.phdcn$nbl5ug_3_r*boot.phdcn$nlninc_2,
		   boot.phdcn$nbl5ug_3_r*boot.phdcn$npcemply_2,
		   boot.phdcn$nbl5ug_3_r*boot.phdcn$npcwelf_2,
		   boot.phdcn$nbl5ug_3_r*boot.phdcn$npcmarr_2,
		   boot.phdcn$nbl5ug_3_r*boot.phdcn$npcengl_2,
		   boot.phdcn$nbl5ug_3_r*boot.phdcn$ncondadvg_2,
		   boot.phdcn$nbl5ug_3_r*boot.phdcn$nbl5ug_2)

		boot.Cj<-cbind(1,
		   boot.phdcn$nlninc_2_r,
		   boot.phdcn$nlninc_2_r*boot.phdcn$ncondadvg_2,
		   boot.phdcn$nlninc_2_r*boot.phdcn$nbl5ug_2,
		   boot.phdcn$nlninc_2_r*boot.phdcn$ncondadvg_3,
		   boot.phdcn$nlninc_2_r*boot.phdcn$nbl5ug_3,
		   boot.phdcn$nlninc_2_r*boot.phdcn$npcage_b,
		   boot.phdcn$nlninc_2_r*boot.phdcn$npchsgrad_b,
		   boot.phdcn$nlninc_2_r*boot.phdcn$npcsomcol_b,
		   boot.phdcn$nlninc_2_r*boot.phdcn$npccolgrd_b,
		   boot.phdcn$nlninc_2_r*boot.phdcn$nownhome_b,
		   boot.phdcn$nlninc_2_r*boot.phdcn$nfemale_b,
		   boot.phdcn$nlninc_2_r*boot.phdcn$nblack_b,
		   boot.phdcn$nlninc_2_r*boot.phdcn$nhispan_b,
		   boot.phdcn$nlninc_2_r*boot.phdcn$nothrace_b,
		   boot.phdcn$nlninc_2_r*boot.phdcn$nfamsize_b,
		   boot.phdcn$nlninc_2_r*boot.phdcn$nlninc_1,
		   boot.phdcn$nlninc_2_r*boot.phdcn$npcemply_1,
		   boot.phdcn$nlninc_2_r*boot.phdcn$npcwelf_1,
		   boot.phdcn$nlninc_2_r*boot.phdcn$npcmarr_1,
		   boot.phdcn$nlninc_2_r*boot.phdcn$npcengl_1,
		   boot.phdcn$nlninc_2_r*boot.phdcn$ncondadvg_1,
		   boot.phdcn$nlninc_2_r*boot.phdcn$nbl5ug_1,
		   boot.phdcn$npcemply_2_r,
		   boot.phdcn$npcemply_2_r*boot.phdcn$ncondadvg_2,
		   boot.phdcn$npcemply_2_r*boot.phdcn$nbl5ug_2,
		   boot.phdcn$npcemply_2_r*boot.phdcn$ncondadvg_3,
		   boot.phdcn$npcemply_2_r*boot.phdcn$nbl5ug_3,
		   boot.phdcn$npcemply_2_r*boot.phdcn$npcage_b,
		   boot.phdcn$npcemply_2_r*boot.phdcn$npchsgrad_b,
		   boot.phdcn$npcemply_2_r*boot.phdcn$npcsomcol_b,
		   boot.phdcn$npcemply_2_r*boot.phdcn$npccolgrd_b,
		   boot.phdcn$npcemply_2_r*boot.phdcn$nownhome_b,
		   boot.phdcn$npcemply_2_r*boot.phdcn$nfemale_b,
		   boot.phdcn$npcemply_2_r*boot.phdcn$nblack_b,
		   boot.phdcn$npcemply_2_r*boot.phdcn$nhispan_b,
		   boot.phdcn$npcemply_2_r*boot.phdcn$nothrace_b,
		   boot.phdcn$npcemply_2_r*boot.phdcn$nfamsize_b,
		   boot.phdcn$npcemply_2_r*boot.phdcn$nlninc_1,
		   boot.phdcn$npcemply_2_r*boot.phdcn$npcemply_1,
		   boot.phdcn$npcemply_2_r*boot.phdcn$npcwelf_1,
		   boot.phdcn$npcemply_2_r*boot.phdcn$npcmarr_1,
		   boot.phdcn$npcemply_2_r*boot.phdcn$npcengl_1,
		   boot.phdcn$npcemply_2_r*boot.phdcn$ncondadvg_1,
		   boot.phdcn$npcemply_2_r*boot.phdcn$nbl5ug_1,
		   boot.phdcn$npcwelf_2_r,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$ncondadvg_2,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$nbl5ug_2,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$ncondadvg_3,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$nbl5ug_3,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$npcage_b,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$npchsgrad_b,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$npcsomcol_b,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$npccolgrd_b,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$nownhome_b,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$nfemale_b,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$nblack_b,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$nhispan_b,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$nothrace_b,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$nfamsize_b,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$nlninc_1,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$npcemply_1,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$npcwelf_1,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$npcmarr_1,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$npcengl_1,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$ncondadvg_1,
		   boot.phdcn$npcwelf_2_r*boot.phdcn$nbl5ug_1,
		   boot.phdcn$npcmarr_2_r,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$ncondadvg_2,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$nbl5ug_2,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$ncondadvg_3,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$nbl5ug_3,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$npcage_b,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$npchsgrad_b,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$npcsomcol_b,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$npccolgrd_b,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$nownhome_b,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$nfemale_b,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$nblack_b,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$nhispan_b,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$nothrace_b,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$nfamsize_b,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$nlninc_1,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$npcemply_1,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$npcwelf_1,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$npcmarr_1,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$npcengl_1,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$ncondadvg_1,
		   boot.phdcn$npcmarr_2_r*boot.phdcn$nbl5ug_1,
		   boot.phdcn$npcengl_2_r,
		   boot.phdcn$npcengl_2_r*boot.phdcn$ncondadvg_2,
		   boot.phdcn$npcengl_2_r*boot.phdcn$nbl5ug_2,
		   boot.phdcn$npcengl_2_r*boot.phdcn$ncondadvg_3,
		   boot.phdcn$npcengl_2_r*boot.phdcn$nbl5ug_3,
		   boot.phdcn$npcengl_2_r*boot.phdcn$npcage_b,
		   boot.phdcn$npcengl_2_r*boot.phdcn$npchsgrad_b,
		   boot.phdcn$npcengl_2_r*boot.phdcn$npcsomcol_b,
		   boot.phdcn$npcengl_2_r*boot.phdcn$npccolgrd_b,
		   boot.phdcn$npcengl_2_r*boot.phdcn$nownhome_b,
		   boot.phdcn$npcengl_2_r*boot.phdcn$nfemale_b,
		   boot.phdcn$npcengl_2_r*boot.phdcn$nblack_b,
		   boot.phdcn$npcengl_2_r*boot.phdcn$nhispan_b,
		   boot.phdcn$npcengl_2_r*boot.phdcn$nothrace_b,
		   boot.phdcn$npcengl_2_r*boot.phdcn$nfamsize_b,
		   boot.phdcn$npcengl_2_r*boot.phdcn$nlninc_1,
		   boot.phdcn$npcengl_2_r*boot.phdcn$npcemply_1,
		   boot.phdcn$npcengl_2_r*boot.phdcn$npcwelf_1,
		   boot.phdcn$npcengl_2_r*boot.phdcn$npcmarr_1,
		   boot.phdcn$npcengl_2_r*boot.phdcn$npcengl_1,
		   boot.phdcn$npcengl_2_r*boot.phdcn$ncondadvg_1,
		   boot.phdcn$npcengl_2_r*boot.phdcn$nbl5ug_1,
		   boot.phdcn$nlninc_3_r,
		   boot.phdcn$nlninc_3_r*boot.phdcn$ncondadvg_3,
		   boot.phdcn$nlninc_3_r*boot.phdcn$nbl5ug_3,
		   boot.phdcn$nlninc_3_r*boot.phdcn$npcage_b,
		   boot.phdcn$nlninc_3_r*boot.phdcn$npchsgrad_b,
		   boot.phdcn$nlninc_3_r*boot.phdcn$npcsomcol_b,
		   boot.phdcn$nlninc_3_r*boot.phdcn$npccolgrd_b,
		   boot.phdcn$nlninc_3_r*boot.phdcn$nownhome_b,
		   boot.phdcn$nlninc_3_r*boot.phdcn$nfemale_b,
		   boot.phdcn$nlninc_3_r*boot.phdcn$nblack_b,
		   boot.phdcn$nlninc_3_r*boot.phdcn$nhispan_b,
		   boot.phdcn$nlninc_3_r*boot.phdcn$nothrace_b,
		   boot.phdcn$nlninc_3_r*boot.phdcn$nfamsize_b,
		   boot.phdcn$nlninc_3_r*boot.phdcn$nlninc_1,
		   boot.phdcn$nlninc_3_r*boot.phdcn$npcemply_1,
		   boot.phdcn$nlninc_3_r*boot.phdcn$npcwelf_1,
		   boot.phdcn$nlninc_3_r*boot.phdcn$npcmarr_1,
		   boot.phdcn$nlninc_3_r*boot.phdcn$npcengl_1,
		   boot.phdcn$nlninc_3_r*boot.phdcn$ncondadvg_1,
		   boot.phdcn$nlninc_3_r*boot.phdcn$nbl5ug_1,
		   boot.phdcn$nlninc_3_r*boot.phdcn$nlninc_2,
		   boot.phdcn$nlninc_3_r*boot.phdcn$npcemply_2,
		   boot.phdcn$nlninc_3_r*boot.phdcn$npcwelf_2,
		   boot.phdcn$nlninc_3_r*boot.phdcn$npcmarr_2,
		   boot.phdcn$nlninc_3_r*boot.phdcn$npcengl_2,
		   boot.phdcn$nlninc_3_r*boot.phdcn$ncondadvg_2,
		   boot.phdcn$nlninc_3_r*boot.phdcn$nbl5ug_2,
		   boot.phdcn$npcemply_3_r,
		   boot.phdcn$npcemply_3_r*boot.phdcn$ncondadvg_3,
		   boot.phdcn$npcemply_3_r*boot.phdcn$nbl5ug_3,
		   boot.phdcn$npcemply_3_r*boot.phdcn$npcage_b,
		   boot.phdcn$npcemply_3_r*boot.phdcn$npchsgrad_b,
		   boot.phdcn$npcemply_3_r*boot.phdcn$npcsomcol_b,
		   boot.phdcn$npcemply_3_r*boot.phdcn$npccolgrd_b,
		   boot.phdcn$npcemply_3_r*boot.phdcn$nownhome_b,
		   boot.phdcn$npcemply_3_r*boot.phdcn$nfemale_b,
		   boot.phdcn$npcemply_3_r*boot.phdcn$nblack_b,
		   boot.phdcn$npcemply_3_r*boot.phdcn$nhispan_b,
		   boot.phdcn$npcemply_3_r*boot.phdcn$nothrace_b,
		   boot.phdcn$npcemply_3_r*boot.phdcn$nfamsize_b,
		   boot.phdcn$npcemply_3_r*boot.phdcn$nlninc_1,
		   boot.phdcn$npcemply_3_r*boot.phdcn$npcemply_1,
		   boot.phdcn$npcemply_3_r*boot.phdcn$npcwelf_1,
		   boot.phdcn$npcemply_3_r*boot.phdcn$npcmarr_1,
		   boot.phdcn$npcemply_3_r*boot.phdcn$npcengl_1,
		   boot.phdcn$npcemply_3_r*boot.phdcn$ncondadvg_1,
		   boot.phdcn$npcemply_3_r*boot.phdcn$nbl5ug_1,
		   boot.phdcn$npcemply_3_r*boot.phdcn$nlninc_2,
		   boot.phdcn$npcemply_3_r*boot.phdcn$npcemply_2,
		   boot.phdcn$npcemply_3_r*boot.phdcn$npcwelf_2,
		   boot.phdcn$npcemply_3_r*boot.phdcn$npcmarr_2,
		   boot.phdcn$npcemply_3_r*boot.phdcn$npcengl_2,
		   boot.phdcn$npcemply_3_r*boot.phdcn$ncondadvg_2,
		   boot.phdcn$npcemply_3_r*boot.phdcn$nbl5ug_2,
		   boot.phdcn$npcwelf_3_r,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$ncondadvg_3,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$nbl5ug_3,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$npcage_b,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$npchsgrad_b,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$npcsomcol_b,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$npccolgrd_b,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$nownhome_b,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$nfemale_b,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$nblack_b,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$nhispan_b,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$nothrace_b,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$nfamsize_b,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$nlninc_1,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$npcemply_1,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$npcwelf_1,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$npcmarr_1,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$npcengl_1,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$ncondadvg_1,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$nbl5ug_1,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$nlninc_2,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$npcemply_2,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$npcwelf_2,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$npcmarr_2,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$npcengl_2,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$ncondadvg_2,
		   boot.phdcn$npcwelf_3_r*boot.phdcn$nbl5ug_2,
		   boot.phdcn$npcmarr_3_r,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$ncondadvg_3,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$nbl5ug_3,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$npcage_b,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$npchsgrad_b,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$npcsomcol_b,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$npccolgrd_b,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$nownhome_b,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$nfemale_b,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$nblack_b,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$nhispan_b,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$nothrace_b,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$nfamsize_b,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$nlninc_1,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$npcemply_1,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$npcwelf_1,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$npcmarr_1,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$npcengl_1,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$ncondadvg_1,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$nbl5ug_1,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$nlninc_2,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$npcemply_2,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$npcwelf_2,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$npcmarr_2,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$npcengl_2,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$ncondadvg_2,
		   boot.phdcn$npcmarr_3_r*boot.phdcn$nbl5ug_2,
		   boot.phdcn$npcengl_3_r,
		   boot.phdcn$npcengl_3_r*boot.phdcn$ncondadvg_3,
		   boot.phdcn$npcengl_3_r*boot.phdcn$nbl5ug_3,
		   boot.phdcn$npcengl_3_r*boot.phdcn$npcage_b,
		   boot.phdcn$npcengl_3_r*boot.phdcn$npchsgrad_b,
		   boot.phdcn$npcengl_3_r*boot.phdcn$npcsomcol_b,
		   boot.phdcn$npcengl_3_r*boot.phdcn$npccolgrd_b,
		   boot.phdcn$npcengl_3_r*boot.phdcn$nownhome_b,
		   boot.phdcn$npcengl_3_r*boot.phdcn$nfemale_b,
		   boot.phdcn$npcengl_3_r*boot.phdcn$nblack_b,
		   boot.phdcn$npcengl_3_r*boot.phdcn$nhispan_b,
		   boot.phdcn$npcengl_3_r*boot.phdcn$nothrace_b,
		   boot.phdcn$npcengl_3_r*boot.phdcn$nfamsize_b,
		   boot.phdcn$npcengl_3_r*boot.phdcn$nlninc_1,
		   boot.phdcn$npcengl_3_r*boot.phdcn$npcemply_1,
		   boot.phdcn$npcengl_3_r*boot.phdcn$npcwelf_1,
		   boot.phdcn$npcengl_3_r*boot.phdcn$npcmarr_1,
		   boot.phdcn$npcengl_3_r*boot.phdcn$npcengl_1,
		   boot.phdcn$npcengl_3_r*boot.phdcn$ncondadvg_1,
		   boot.phdcn$npcengl_3_r*boot.phdcn$nbl5ug_1,
		   boot.phdcn$npcengl_3_r*boot.phdcn$nlninc_2,
		   boot.phdcn$npcengl_3_r*boot.phdcn$npcemply_2,
		   boot.phdcn$npcengl_3_r*boot.phdcn$npcwelf_2,
		   boot.phdcn$npcengl_3_r*boot.phdcn$npcmarr_2,
		   boot.phdcn$npcengl_3_r*boot.phdcn$npcengl_2,
		   boot.phdcn$npcengl_3_r*boot.phdcn$ncondadvg_2,
		   boot.phdcn$npcengl_3_r*boot.phdcn$nbl5ug_2)

		boot.Q<-rep(1,length(boot.phdcn$nsubid))

		boot.Mm<-c(sum(boot.Q),rep(0,ncol(boot.Cm)-1))

		boot.Mj<-c(sum(boot.Q),rep(0,ncol(boot.Cj)-1))

		boot.rbalwtm<-resbal(boot.Mm,boot.Cm,boot.Q,tol=0.01)$W
		boot.rbalwtj<-resbal(boot.Mj,boot.Cj,boot.Q,tol=0.01)$W

		boot.rbalwtm[boot.rbalwtm>quantile(boot.rbalwtm,probs=0.99)]<-quantile(boot.rbalwtm,probs=0.99)
		boot.rbalwtm[boot.rbalwtm<quantile(boot.rbalwtm,probs=0.01)]<-quantile(boot.rbalwtm,probs=0.01)
		boot.rbalwtj[boot.rbalwtj>quantile(boot.rbalwtj,probs=0.99)]<-quantile(boot.rbalwtj,probs=0.99)
		boot.rbalwtj[boot.rbalwtj<quantile(boot.rbalwtj,probs=0.01)]<-quantile(boot.rbalwtj,probs=0.01)

		boot.m1<-lm(nppvt_3~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
				        nlninc_1_r+npcemply_1_r+npcwelf_1_r+npcmarr_1_r+npcengl_1_r+nsubage_3_r+cumndadvg,data=boot.phdcn,weights=boot.rbalwtm)
	
		boot.m2<-lm(nppvt_3~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
				        nlninc_1_r+npcemply_1_r+npcwelf_1_r+npcmarr_1_r+npcengl_1_r+nsubage_3_r+cumndadvg+cumnbl5ug,data=boot.phdcn,weights=boot.rbalwtj)

		boot.ate.rbal[j,1]<-boot.m1$coefficients["cumndadvg"]*(astar-a)
		boot.aje.rbal[j,1]<-boot.m2$coefficients["cumndadvg"]*(astar-a)+boot.m2$coefficients["cumnbl5ug"]*(mstar-m)
		boot.cde.rbal[j,1]<-boot.m2$coefficients["cumndadvg"]*(astar-a)
		boot.cme.rbal[j,1]<-boot.m2$coefficients["cumnbl5ug"]*(mstar-m)
		boot.dif1.rbal[j,1]<-boot.ate.rbal[j,1]-boot.cde.rbal[j,1]
		}

	mivar.rbal[i,1]<-var(boot.ate.rbal)
	mivar.rbal[i,2]<-var(boot.aje.rbal)
	mivar.rbal[i,3]<-var(boot.cde.rbal)
	mivar.rbal[i,4]<-var(boot.cme.rbal)
	mivar.rbal[i,5]<-var(boot.dif1.rbal)
	}

### COMBINE MI ESTIMATES ###
rwrest<-rwrest.gen<-rwrest.race<-rwrest.inc<-rbalest<-matrix(data=NA,nrow=5,ncol=4)
for (i in 1:5) { 
	rwrest[i,1]<-round(mean(miest.rwr[,i]),digits=4)
	rwrest[i,2]<-round(sqrt(mean(mivar.rwr[,i])+(var(miest.rwr[,i])*(1+(1/nmi)))),digits=4)
	rwrest[i,3]<-round((rwrest[i,1]/rwrest[i,2]),digits=4)
	rwrest[i,4]<-round((pnorm(abs(rwrest[i,3]),0,1,lower.tail=FALSE)*2),digits=4)

	rwrest.gen[i,1]<-round(mean(miest.rwr.gen[,i]),digits=4)
	rwrest.gen[i,2]<-round(sqrt(mean(mivar.rwr.gen[,i])+(var(miest.rwr.gen[,i])*(1+(1/nmi)))),digits=4)
	rwrest.gen[i,3]<-round((rwrest.gen[i,1]/rwrest.gen[i,2]),digits=4)
	rwrest.gen[i,4]<-round((pnorm(abs(rwrest.gen[i,3]),0,1,lower.tail=FALSE)*2),digits=4)

	rwrest.race[i,1]<-round(mean(miest.rwr.race[,i]),digits=4)
	rwrest.race[i,2]<-round(sqrt(mean(mivar.rwr.race[,i])+(var(miest.rwr.race[,i])*(1+(1/nmi)))),digits=4)
	rwrest.race[i,3]<-round((rwrest.race[i,1]/rwrest.race[i,2]),digits=4)
	rwrest.race[i,4]<-round((pnorm(abs(rwrest.race[i,3]),0,1,lower.tail=FALSE)*2),digits=4)

	rwrest.inc[i,1]<-round(mean(miest.rwr.inc[,i]),digits=4)
	rwrest.inc[i,2]<-round(sqrt(mean(mivar.rwr.inc[,i])+(var(miest.rwr.inc[,i])*(1+(1/nmi)))),digits=4)
	rwrest.inc[i,3]<-round((rwrest.inc[i,1]/rwrest.inc[i,2]),digits=4)
	rwrest.inc[i,4]<-round((pnorm(abs(rwrest.inc[i,3]),0,1,lower.tail=FALSE)*2),digits=4)

	rbalest[i,1]<-round(mean(miest.rbal[,i]),digits=4)
	rbalest[i,2]<-round(sqrt(mean(mivar.rbal[,i])+(var(miest.rbal[,i])*(1+(1/nmi)))),digits=4)
	rbalest[i,3]<-round((rbalest[i,1]/rbalest[i,2]),digits=4)
	rbalest[i,4]<-round((pnorm(abs(rbalest[i,3]),0,1,lower.tail=FALSE)*2),digits=4)
	}

### PRINT RESULTS ###
rlabel<-c('ATE','AJE','CDE','CME','ATE-CDE')

output.rwr<-data.frame(rwrest,row.names=rlabel)
output.rwr.gen<-data.frame(rwrest.gen,row.names=rlabel)
output.rwr.race<-data.frame(rwrest.race,row.names=rlabel)
output.rwr.inc<-data.frame(rwrest.inc,row.names=rlabel)
output.rbal<-data.frame(rbalest,row.names=rlabel)

colnames(output.rwr)<-c('Est','SE','Z','P-value')
colnames(output.rwr.gen)<-c('Est','SE','Z','P-value')
colnames(output.rwr.race)<-c('Est','SE','Z','P-value')
colnames(output.rwr.inc)<-c('Est','SE','Z','P-value')
colnames(output.rbal)<-c('Est','SE','Z','P-value')

cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("===========================================\n")
cat("Linear and Additive RWR\n")
cat("===========================================\n")
print(output.rwr)
cat("===========================================\n")
cat(" \n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat(" \n")
cat("===========================================\n")
cat("RWR + Gender Moderation\n")
cat("===========================================\n")
print(output.rwr.gen)
cat("===========================================\n")
cat(" \n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat(" \n")
cat("===========================================\n")
cat("RWR + Racial Moderation\n")
cat("===========================================\n")
print(output.rwr.race)
cat("===========================================\n")
cat(" \n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat(" \n")
cat("===========================================\n")
cat("RWR + Income Moderation\n")
cat("===========================================\n")
print(output.rwr.inc)
cat("===========================================\n")
cat(" \n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat(" \n")
cat("===========================================\n")
cat("Residual Balancing\n")
cat("===========================================\n")
print(output.rbal)
cat("===========================================\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
print(Sys.time())

sink()
