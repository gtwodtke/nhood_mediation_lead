sink("D:\\projects\\nhood_mediation_lead\\programs\\appendix_a\\_LOGS\\01_create_figure_a1_log.txt")

print(Sys.time())

################################################
################################################
##                                            ##
## PROGRAM NAME: 01_create_figure_a1          ##
## AUTHOR: GW                                 ##
## DATE: 08/13/2020                           ##
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
nboot<-250
smth<-3
astar<-0.70
a<-(-0.90)
mstar<-0.65
m<-0.30
npr<-20
aRef<-seq(from=(-1.6),to=(1.3),length=npr)
mRef<-seq(from=0.1,to=0.8,length=npr)

##### COMPUTE EFFECT ESTIMATES #####
miest.rwr.lm.cde<-mivar.rwr.lm.cde<-miest.rwr.gam.cde<-mivar.rwr.gam.cde<-matrix(data=NA,nrow=nmi,ncol=npr)
miest.rwr.lm.cme<-mivar.rwr.lm.cme<-miest.rwr.gam.cme<-mivar.rwr.gam.cme<-matrix(data=NA,nrow=nmi,ncol=npr)

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
	m2.lm<-lm(nppvt_3~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
			   nsubage_3_r+nlninc_s_r+npcemply_s_r+npcwelf_s_r+npcmarr_s_r+npcengl_s_r+cumndadvg+cumnbl5ug+(cumndadvg*cumnbl5ug),data=phdcn)

	m2.gam<-gam(nppvt_3~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
			   nsubage_3_r+nlninc_s_r+npcemply_s_r+npcwelf_s_r+npcmarr_s_r+npcengl_s_r+te(cumndadvg,cumnbl5ug),data=phdcn,family=gaussian(link="identity"),gamma=smth)

	# COMPUTE ESTIMATES #
	for (j in 1:npr) {
		gcomp<-phdcn
		gcomp$cumnbl5ug<-mRef[j]
		gcomp$cumndadvg<-astar
		yhat_astar.lm<-predict(m2.lm,gcomp,type="response")
		yhat_astar.gam<-predict(m2.gam,gcomp,type="response")
		gcomp$cumndadvg<-a
		yhat_a.lm<-predict(m2.lm,gcomp,type="response")
		yhat_a.gam<-predict(m2.gam,gcomp,type="response")
		miest.rwr.lm.cde[i,j]<-mean(yhat_astar.lm)-mean(yhat_a.lm)
		miest.rwr.gam.cde[i,j]<-mean(yhat_astar.gam)-mean(yhat_a.gam)
		}

	for (j in 1:npr) {
		gcomp<-phdcn
		gcomp$cumndadvg<-aRef[j]
		gcomp$cumnbl5ug<-mstar
		yhat_mstar.lm<-predict(m2.lm,gcomp,type="response")
		yhat_mstar.gam<-predict(m2.gam,gcomp,type="response")
		gcomp$cumnbl5ug<-m
		yhat_m.lm<-predict(m2.lm,gcomp,type="response")
		yhat_m.gam<-predict(m2.gam,gcomp,type="response")
		miest.rwr.lm.cme[i,j]<-mean(yhat_mstar.lm)-mean(yhat_m.lm)
		miest.rwr.gam.cme[i,j]<-mean(yhat_mstar.gam)-mean(yhat_m.gam)
		}

	### COMPUTE BOOTSTRAP SEs ###
	boot.lm.cde<-boot.lm.cme<-boot.gam.cde<-boot.gam.cme<-matrix(data=NA,nrow=nboot,ncol=npr)
	
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

		boot.m2.lm<-lm(nppvt_3~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
					  nsubage_3_r+nlninc_s_r+npcemply_s_r+npcwelf_s_r+npcmarr_s_r+npcengl_s_r+cumndadvg+cumnbl5ug+(cumndadvg*cumnbl5ug),data=boot.phdcn)

		boot.m2.gam<-gam(nppvt_3~npcage_b_r+npchsgrad_b_r+npcsomcol_b_r+npccolgrd_b_r+nownhome_b_r+nfemale_b_r+nblack_b_r+nhispan_b_r+nothrace_b_r+nfamsize_b_r+
				   nsubage_3_r+nlninc_s_r+npcemply_s_r+npcwelf_s_r+npcmarr_s_r+npcengl_s_r+te(cumndadvg,cumnbl5ug),data=boot.phdcn,family=gaussian(link="identity"),gamma=smth)

		for (k in 1:npr) {
			boot.gcomp<-boot.phdcn
			boot.gcomp$cumnbl5ug<-mRef[k]
			boot.gcomp$cumndadvg<-astar
			boot.yhat_astar.lm<-predict(boot.m2.lm,boot.gcomp,type="response")
			boot.yhat_astar.gam<-predict(boot.m2.gam,boot.gcomp,type="response")
			boot.gcomp$cumndadvg<-a
			boot.yhat_a.lm<-predict(boot.m2.lm,boot.gcomp,type="response")
			boot.yhat_a.gam<-predict(boot.m2.gam,boot.gcomp,type="response")
			boot.lm.cde[j,k]<-mean(boot.yhat_astar.lm)-mean(boot.yhat_a.lm)
			boot.gam.cde[j,k]<-mean(boot.yhat_astar.gam)-mean(boot.yhat_a.gam)
			}

		for (k in 1:npr) {
			boot.gcomp<-boot.phdcn
			boot.gcomp$cumndadvg<-aRef[k]
			boot.gcomp$cumnbl5ug<-mstar
			boot.yhat_mstar.lm<-predict(boot.m2.lm,gcomp,type="response")
			boot.yhat_mstar.gam<-predict(boot.m2.gam,gcomp,type="response")
			boot.gcomp$cumnbl5ug<-m
			boot.yhat_m.lm<-predict(boot.m2.lm,gcomp,type="response")
			boot.yhat_m.gam<-predict(boot.m2.gam,gcomp,type="response")
			boot.lm.cme[j,k]<-mean(boot.yhat_mstar.lm)-mean(boot.yhat_m.lm)
			boot.gam.cme[j,k]<-mean(boot.yhat_mstar.gam)-mean(boot.yhat_m.gam)
			}
		}

	for (k in 1:npr) {
		mivar.rwr.lm.cde[i,k]<-var(boot.lm.cde[,k])
		mivar.rwr.lm.cme[i,k]<-var(boot.lm.cme[,k])
		mivar.rwr.gam.cde[i,k]<-var(boot.gam.cde[,k])
		mivar.rwr.gam.cme[i,k]<-var(boot.gam.cme[,k])
		}
	}

##### COMBINE MI ESTIMATES #####
rwrest.lm.cde<-rwrest.lm.cme<-rwrest.gam.cde<-rwrest.gam.cme<-matrix(data=NA,nrow=npr,ncol=3)

for (i in 1:npr) { 
	rwrest.lm.cde[i,1]<-round(mean(miest.rwr.lm.cde[,i]),digits=4)
	rwrest.lm.cde[i,2]<-rwrest.lm.cde[i,1]-1.96*round(sqrt(mean(mivar.rwr.lm.cde[,i])+(var(miest.rwr.lm.cde[,i])*(1+(1/nmi)))),digits=4)
	rwrest.lm.cde[i,3]<-rwrest.lm.cde[i,1]+1.96*round(sqrt(mean(mivar.rwr.lm.cde[,i])+(var(miest.rwr.lm.cde[,i])*(1+(1/nmi)))),digits=4)

	rwrest.lm.cme[i,1]<-round(mean(miest.rwr.lm.cme[,i]),digits=4)
	rwrest.lm.cme[i,2]<-rwrest.lm.cme[i,1]-1.96*round(sqrt(mean(mivar.rwr.lm.cme[,i])+(var(miest.rwr.lm.cme[,i])*(1+(1/nmi)))),digits=4)
	rwrest.lm.cme[i,3]<-rwrest.lm.cme[i,1]+1.96*round(sqrt(mean(mivar.rwr.lm.cme[,i])+(var(miest.rwr.lm.cme[,i])*(1+(1/nmi)))),digits=4)

	rwrest.gam.cde[i,1]<-round(mean(miest.rwr.gam.cde[,i]),digits=4)
	rwrest.gam.cde[i,2]<-rwrest.gam.cde[i,1]-1.96*round(sqrt(mean(mivar.rwr.gam.cde[,i])+(var(miest.rwr.gam.cde[,i])*(1+(1/nmi)))),digits=4)
	rwrest.gam.cde[i,3]<-rwrest.gam.cde[i,1]+1.96*round(sqrt(mean(mivar.rwr.gam.cde[,i])+(var(miest.rwr.gam.cde[,i])*(1+(1/nmi)))),digits=4)

	rwrest.gam.cme[i,1]<-round(mean(miest.rwr.gam.cme[,i]),digits=4)
	rwrest.gam.cme[i,2]<-rwrest.gam.cme[i,1]-1.96*round(sqrt(mean(mivar.rwr.gam.cme[,i])+(var(miest.rwr.gam.cme[,i])*(1+(1/nmi)))),digits=4)
	rwrest.gam.cme[i,3]<-rwrest.gam.cme[i,1]+1.96*round(sqrt(mean(mivar.rwr.gam.cme[,i])+(var(miest.rwr.gam.cme[,i])*(1+(1/nmi)))),digits=4)
	}

rwrest.lm.cde<-cbind(mRef,rwrest.lm.cde)
output.lm.cde<-data.frame(rwrest.lm.cde)
colnames(output.lm.cde)<-c('mRef','CDE','95LL','95UL')

rwrest.lm.cme<-cbind(aRef,rwrest.lm.cme)
output.lm.cme<-data.frame(rwrest.lm.cme)
colnames(output.lm.cme)<-c('aRef','CME','95LL','95UL')

rwrest.gam.cde<-cbind(mRef,rwrest.gam.cde)
output.gam.cde<-data.frame(rwrest.gam.cde)
colnames(output.gam.cde)<-c('mRef','CDE','95LL','95UL')

rwrest.gam.cme<-cbind(aRef,rwrest.gam.cme)
output.gam.cme<-data.frame(rwrest.gam.cme)
colnames(output.gam.cme)<-c('aRef','CME','95LL','95UL')

##### PRINT RESULTS #####
print(output.lm.cde)
cat(" \n")
print(output.lm.cme)
cat(" \n")
print(output.gam.cde)
cat(" \n")
print(output.gam.cme)
cat(" \n")

##### PLOT RESULTS #####
#dev.new(width=9,height=9)
tiff("D:\\projects\\nhood_mediation_lead\\figures\\figure_a1.tiff",
	width=9,
	height=9,
	units='in',
	res=600)

par(mfrow=c(2,2))

plot(1:4,
	panel.first = 
       	c(abline(h=0,col="grey"), 
		  lines(output.lm.cde[,1],output.lm.cde[,3],type="l",lty="dashed"),
		  lines(output.lm.cde[,1],output.lm.cde[,4],type="l",lty="dashed"),
		  lines(output.lm.cde[,1],output.lm.cde[,2],type="l",lty="solid")),
	main="CDE(m) based on Cross Product Interaction",
	xlab="m",
	ylab="CDE(m) Estimate",
	xlim=c(0.1,0.8),
	ylim=c(-0.8,0.8))

plot(1:4,
	panel.first = 
       	c(abline(h=0,col="grey"), 
		  lines(output.lm.cme[,1],output.lm.cme[,3],type="l",lty="dashed"),
		  lines(output.lm.cme[,1],output.lm.cme[,4],type="l",lty="dashed"),
		  lines(output.lm.cme[,1],output.lm.cme[,2],type="l",lty="solid")),
	main="CME(a) based on Cross Product Interaction",
	xlab="a",
	ylab="CME(a) Estimate",
	xlim=c(-1.6,1.3),
	ylim=c(-0.8,0.8))

plot(1:4,
	panel.first = 
       	c(abline(h=0,col="grey"), 
		  lines(output.gam.cde[,1],output.gam.cde[,3],type="l",lty="dashed"),
		  lines(output.gam.cde[,1],output.gam.cde[,4],type="l",lty="dashed"),
		  lines(output.gam.cde[,1],output.gam.cde[,2],type="l",lty="solid")),
	main="CDE(m) based on Tensor Product Interaction",
	xlab="m",
	ylab="CDE(m) Estimate",
	xlim=c(0.1,0.8),
	ylim=c(-0.8,0.8))

plot(1:4,
	panel.first = 
       	c(abline(h=0,col="grey"), 
		  lines(output.gam.cme[,1],output.gam.cme[,3],type="l",lty="dashed"),
		  lines(output.gam.cme[,1],output.gam.cme[,4],type="l",lty="dashed"),
		  lines(output.gam.cme[,1],output.gam.cme[,2],type="l",lty="solid")),
	main="CME(a) based on Tensor Product Interaction",
	xlab="a",
	ylab="CME(a) Estimate",
	xlim=c(-1.6,1.3),
	ylim=c(-0.8,0.8))

dev.off()

print(Sys.time())

sink()


