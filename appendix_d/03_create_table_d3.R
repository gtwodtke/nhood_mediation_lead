sink("D:\\projects\\nhood_mediation_lead\\programs\\appendix_d\\_LOGS\\03_create_table_d3_log.txt")
print(Sys.time())

################################################
################################################
##                                            ##
## PROGRAM NAME: 03_create_table_d3           ##
## AUTHOR: GW                                 ##
## DATE: 5/25/2021                            ##
## DESCRIPTION:                               ##
##                                            ##
##  creates table of wave 3 nh effects        ##
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
library(glmnet)

##### LOAD PHDCN #####
phdcnmi<-read.dta("D:\\projects\\nhood_mediation_lead\\data\\v02_phdcn_merged_mi.dta")
phdcnmi<-phdcnmi[which((phdcnmi$"_mj"!=0)),]

##### COMPUTE RWR EFFECT ESTIMATES #####
set.seed(8675309)
nmi<-50
nboot<-250
astar<-0.70
a<-(-0.90)
mstar<-0.65
m<-0.30

lasso.lambda1<-0.0089062
lasso.lambda2<-0.0097745
elast.alpha1<-1.0
elast.alpha2<-0.5
elast.lambda1<-0.0089062
elast.lambda2<-0.0170813

bvars<-c("npcage_b","npchsgrad_b","npcsomcol_b","npccolgrd_b","nownhome_b","nfemale_b","nblack_b","nhispan_b","nothrace_b","nfamsize_b")
c1vars<-c("nlninc_1","npcemply_1","npcwelf_1","npcmarr_1","npcengl_1")
c2vars<-c("nlninc_2","npcemply_2","npcwelf_2","npcmarr_2","npcengl_2")
c3vars<-c("nlninc_3","npcemply_3","npcwelf_3","npcmarr_3","npcengl_3")

m1.xvars<-c(bvars,c1vars,c2vars,c3vars,"nsubage_3_r","ncondadvg_1","nbl5ug_1","ncondadvg_2","nbl5ug_2","ncondadvg_3")
m2.xvars<-c(bvars,c1vars,c2vars,c3vars,"nsubage_3_r","ncondadvg_1","nbl5ug_1","ncondadvg_2","nbl5ug_2","ncondadvg_3","nbl5ug_3")

miest.rwr.lm<-mivar.rwr.lm<-matrix(data=NA,nrow=nmi,ncol=5)
miest.rwr.lasso<-mivar.rwr.lasso<-matrix(data=NA,nrow=nmi,ncol=5)
miest.rwr.elast<-mivar.rwr.elast<-matrix(data=NA,nrow=nmi,ncol=5)

for (i in 1:nmi) {

	# LOAD MI DATA #
	phdcn<-phdcnmi[which(phdcnmi$"_mj"==i),]

	# TRANSFORM VARS #
	phdcn$nppvt_3<-(phdcn$nppvt_3-mean(phdcn$nppvt_3))/sd(phdcn$nppvt_3)
	phdcn$nsubage_3_r<-residuals(lm(nsubage_3~.,data=phdcn[,c(bvars,c1vars,c2vars,c3vars,"ncondadvg_1","ncondadvg_2","ncondadvg_3","nbl5ug_1","nbl5ug_2","nbl5ug_3","nsubage_3")]))

	# BASE MODEL #
	m1.lm<-lm(nppvt_3~.,data=phdcn[,c(m1.xvars,"nppvt_3")])
	m2.lm<-lm(nppvt_3~.,data=phdcn[,c(m2.xvars,"nppvt_3")])
	
	# LASSO #
	y<-phdcn[,"nppvt_3"]
	x1<-phdcn[,m1.xvars]
	x2<-phdcn[,m2.xvars]
	m1.lasso<-glmnet(x1,y,family="gaussian",lambda=lasso.lambda1)
	m2.lasso<-glmnet(x2,y,family="gaussian",lambda=lasso.lambda2)

	# ELASTICNET #
	m1.elast<-glmnet(x1,y,family="gaussian",alpha=elast.alpha1,lambda=elast.lambda1)
	m2.elast<-glmnet(x2,y,family="gaussian",alpha=elast.alpha1,lambda=elast.lambda2)

	# TOTAL EFFECT OF NHOOD POVERTY #
	gcomp<-phdcn
	gcomp$ncondadvg_3<-astar
	yhat.astar.lm<-predict(m1.lm,gcomp,type="response")
	yhat.astar.lasso<-predict(m1.lasso,newx=as.matrix(gcomp[,m1.xvars]))
	yhat.astar.elast<-predict(m1.elast,newx=as.matrix(gcomp[,m1.xvars]))
	gcomp$ncondadvg_3<-a
	yhat.a.lm<-predict(m1.lm,gcomp,type="response")
	yhat.a.lasso<-predict(m1.lasso,newx=as.matrix(gcomp[,m1.xvars]))
	yhat.a.elast<-predict(m1.elast,newx=as.matrix(gcomp[,m1.xvars]))
	miest.rwr.lm[i,1]<-mean(yhat.astar.lm)-mean(yhat.a.lm)
	miest.rwr.lasso[i,1]<-mean(yhat.astar.lasso)-mean(yhat.a.lasso)
	miest.rwr.elast[i,1]<-mean(yhat.astar.elast)-mean(yhat.a.elast)

	# JOINT EFFECT OF NHOOD POVERTY AND ENVIRONMENTAL LEAD CONTAMINATION #
	gcomp<-phdcn
	gcomp$ncondadvg_3<-astar
	gcomp$nbl5ug_3<-mstar
	yhat.astar.mstar.lm<-predict(m2.lm,gcomp,type="response")
	yhat.astar.mstar.lasso<-predict(m2.lasso,newx=as.matrix(gcomp[,m2.xvars]))
	yhat.astar.mstar.elast<-predict(m2.elast,newx=as.matrix(gcomp[,m2.xvars]))
	gcomp$ncondadvg_3<-a
	gcomp$nbl5ug_3<-m
	yhat.a.m.lm<-predict(m2.lm,gcomp,type="response")
	yhat.a.m.lasso<-predict(m2.lasso,newx=as.matrix(gcomp[,m2.xvars]))
	yhat.a.m.elast<-predict(m2.elast,newx=as.matrix(gcomp[,m2.xvars]))
	miest.rwr.lm[i,2]<-mean(yhat.astar.mstar.lm)-mean(yhat.a.m.lm)
	miest.rwr.lasso[i,2]<-mean(yhat.astar.mstar.lasso)-mean(yhat.a.m.lasso)
	miest.rwr.elast[i,2]<-mean(yhat.astar.mstar.elast)-mean(yhat.a.m.elast)

	# CONTROLLED DIRECT EFFECT OF NHOOD POVERTY #
	gcomp<-phdcn
	gcomp$ncondadvg_3<-astar
	yhat.astar.mfix.lm<-predict(m2.lm,gcomp,type="response")
	yhat.astar.mfix.lasso<-predict(m2.lasso,newx=as.matrix(gcomp[,m2.xvars]))
	yhat.astar.mfix.elast<-predict(m2.elast,newx=as.matrix(gcomp[,m2.xvars]))
	gcomp$ncondadvg_3<-a
	yhat.a.mfix.lm<-predict(m2.lm,gcomp,type="response")
	yhat.a.mfix.lasso<-predict(m2.lasso,newx=as.matrix(gcomp[,m2.xvars]))
	yhat.a.mfix.elast<-predict(m2.elast,newx=as.matrix(gcomp[,m2.xvars]))
	miest.rwr.lm[i,3]<-mean(yhat.astar.mfix.lm)-mean(yhat.a.mfix.lm)
	miest.rwr.lasso[i,3]<-mean(yhat.astar.mfix.lasso)-mean(yhat.a.mfix.lasso)
	miest.rwr.elast[i,3]<-mean(yhat.astar.mfix.elast)-mean(yhat.a.mfix.elast)

	# CONTROLLED MEDIATOR EFFECT OF ENVIRONMENTAL LEAD CONTAMINATION #
	gcomp<-phdcn
	gcomp$nbl5ug_3<-mstar
	yhat.afix.mstar.lm<-predict(m2.lm,gcomp,type="response")
	yhat.afix.mstar.lasso<-predict(m2.lasso,newx=as.matrix(gcomp[,m2.xvars]))
	yhat.afix.mstar.elast<-predict(m2.elast,newx=as.matrix(gcomp[,m2.xvars]))
	gcomp$nbl5ug_3<-m
	yhat.afix.m.lm<-predict(m2.lm,gcomp,type="response")
	yhat.afix.m.lasso<-predict(m2.lasso,newx=as.matrix(gcomp[,m2.xvars]))
	yhat.afix.m.elast<-predict(m2.elast,newx=as.matrix(gcomp[,m2.xvars]))
	miest.rwr.lm[i,4]<-mean(yhat.afix.mstar.lm)-mean(yhat.afix.m.lm)
	miest.rwr.lasso[i,4]<-mean(yhat.afix.mstar.lasso)-mean(yhat.afix.m.lasso)
	miest.rwr.elast[i,4]<-mean(yhat.afix.mstar.elast)-mean(yhat.afix.m.elast)

	# DIFFERENCE BETWEEN ATE AND CDE #
	miest.rwr.lm[i,5]<-miest.rwr.lm[i,1]-miest.rwr.lm[i,3]
	miest.rwr.lasso[i,5]<-miest.rwr.lasso[i,1]-miest.rwr.lasso[i,3]
	miest.rwr.elast[i,5]<-miest.rwr.elast[i,1]-miest.rwr.elast[i,3]

	# COMPUTE BOOTSTRAP SEs #
	boot.ate.rwr.lm<-boot.aje.rwr.lm<-boot.cde.rwr.lm<-boot.cme.rwr.lm<-boot.dif1.rwr.lm<-matrix(data=NA,nrow=nboot,ncol=1)
	boot.ate.rwr.lasso<-boot.aje.rwr.lasso<-boot.cde.rwr.lasso<-boot.cme.rwr.lasso<-boot.dif1.rwr.lasso<-matrix(data=NA,nrow=nboot,ncol=1)
	boot.ate.rwr.elast<-boot.aje.rwr.elast<-boot.cde.rwr.elast<-boot.cme.rwr.elast<-boot.dif1.rwr.elast<-matrix(data=NA,nrow=nboot,ncol=1)
	
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

		boot.phdcn$nppvt_3<-(boot.phdcn$nppvt_3-mean(boot.phdcn$nppvt_3))/sd(boot.phdcn$nppvt_3)
		boot.phdcn$nsubage_3_r<-residuals(lm(nsubage_3~.,data=boot.phdcn[,c(bvars,c1vars,c2vars,c3vars,"ncondadvg_1","ncondadvg_2","ncondadvg_3","nbl5ug_1","nbl5ug_2","nbl5ug_3","nsubage_3")]))

		boot.m1.lm<-lm(nppvt_3~.,data=boot.phdcn[,c(m1.xvars,"nppvt_3")])
		boot.m2.lm<-lm(nppvt_3~.,data=boot.phdcn[,c(m2.xvars,"nppvt_3")])
	
		boot.y<-boot.phdcn[,"nppvt_3"]
		boot.x1<-boot.phdcn[,m1.xvars]
		boot.x2<-boot.phdcn[,m2.xvars]
		boot.m1.lasso<-glmnet(boot.x1,boot.y,family="gaussian",lambda=lasso.lambda1)
		boot.m2.lasso<-glmnet(boot.x2,boot.y,family="gaussian",lambda=lasso.lambda2)

		boot.m1.elast<-glmnet(boot.x1,boot.y,family="gaussian",alpha=elast.alpha1,lambda=elast.lambda1)
		boot.m2.elast<-glmnet(boot.x2,boot.y,family="gaussian",alpha=elast.alpha1,lambda=elast.lambda2)

		boot.gcomp<-boot.phdcn
		boot.gcomp$ncondadvg_3<-astar
		boot.yhat.astar.lm<-predict(boot.m1.lm,boot.gcomp,type="response")
		boot.yhat.astar.lasso<-predict(boot.m1.lasso,newx=as.matrix(boot.gcomp[,m1.xvars]))
		boot.yhat.astar.elast<-predict(boot.m1.elast,newx=as.matrix(boot.gcomp[,m1.xvars]))
		boot.gcomp$ncondadvg_3<-a
		boot.yhat.a.lm<-predict(boot.m1.lm,boot.gcomp,type="response")
		boot.yhat.a.lasso<-predict(boot.m1.lasso,newx=as.matrix(boot.gcomp[,m1.xvars]))
		boot.yhat.a.elast<-predict(boot.m1.elast,newx=as.matrix(boot.gcomp[,m1.xvars]))
		boot.ate.rwr.lm[j,1]<-mean(boot.yhat.astar.lm)-mean(boot.yhat.a.lm)
		boot.ate.rwr.lasso[j,1]<-mean(boot.yhat.astar.lasso)-mean(boot.yhat.a.lasso)
		boot.ate.rwr.elast[j,1]<-mean(boot.yhat.astar.elast)-mean(boot.yhat.a.elast)

		boot.gcomp<-boot.phdcn
		boot.gcomp$ncondadvg_3<-astar
		boot.gcomp$nbl5ug_3<-mstar
		boot.yhat.astar.mstar.lm<-predict(boot.m2.lm,boot.gcomp,type="response")
		boot.yhat.astar.mstar.lasso<-predict(boot.m2.lasso,newx=as.matrix(boot.gcomp[,m2.xvars]))
		boot.yhat.astar.mstar.elast<-predict(boot.m2.elast,newx=as.matrix(boot.gcomp[,m2.xvars]))
		boot.gcomp$ncondadvg_3<-a
		boot.gcomp$nbl5ug_3<-m
		boot.yhat.a.m.lm<-predict(boot.m2.lm,boot.gcomp,type="response")
		boot.yhat.a.m.lasso<-predict(boot.m2.lasso,newx=as.matrix(boot.gcomp[,m2.xvars]))
		boot.yhat.a.m.elast<-predict(boot.m2.elast,newx=as.matrix(boot.gcomp[,m2.xvars]))
		boot.aje.rwr.lm[j,1]<-mean(boot.yhat.astar.mstar.lm)-mean(boot.yhat.a.m.lm)
		boot.aje.rwr.lasso[j,1]<-mean(boot.yhat.astar.mstar.lasso)-mean(boot.yhat.a.m.lasso)
		boot.aje.rwr.elast[j,1]<-mean(boot.yhat.astar.mstar.elast)-mean(boot.yhat.a.m.elast)

		boot.gcomp<-boot.phdcn
		boot.gcomp$ncondadvg_3<-astar
		boot.yhat.astar.mfix.lm<-predict(boot.m2.lm,boot.gcomp,type="response")
		boot.yhat.astar.mfix.lasso<-predict(boot.m2.lasso,newx=as.matrix(boot.gcomp[,m2.xvars]))
		boot.yhat.astar.mfix.elast<-predict(boot.m2.elast,newx=as.matrix(boot.gcomp[,m2.xvars]))
		boot.gcomp$ncondadvg_3<-a
		boot.yhat.a.mfix.lm<-predict(boot.m2.lm,boot.gcomp,type="response")
		boot.yhat.a.mfix.lasso<-predict(boot.m2.lasso,newx=as.matrix(boot.gcomp[,m2.xvars]))
		boot.yhat.a.mfix.elast<-predict(boot.m2.elast,newx=as.matrix(boot.gcomp[,m2.xvars]))
		boot.cde.rwr.lm[j,1]<-mean(boot.yhat.astar.mfix.lm)-mean(boot.yhat.a.mfix.lm)
		boot.cde.rwr.lasso[j,1]<-mean(boot.yhat.astar.mfix.lasso)-mean(boot.yhat.a.mfix.lasso)
		boot.cde.rwr.elast[j,1]<-mean(boot.yhat.astar.mfix.elast)-mean(boot.yhat.a.mfix.elast)

		boot.gcomp<-boot.phdcn
		boot.gcomp$nbl5ug_3<-mstar
		boot.yhat.afix.mstar.lm<-predict(boot.m2.lm,boot.gcomp,type="response")
		boot.yhat.afix.mstar.lasso<-predict(boot.m2.lasso,newx=as.matrix(boot.gcomp[,m2.xvars]))
		boot.yhat.afix.mstar.elast<-predict(boot.m2.elast,newx=as.matrix(boot.gcomp[,m2.xvars]))
		boot.gcomp$nbl5ug_3<-m
		boot.yhat.afix.m.lm<-predict(boot.m2.lm,boot.gcomp,type="response")
		boot.yhat.afix.m.lasso<-predict(boot.m2.lasso,newx=as.matrix(boot.gcomp[,m2.xvars]))
		boot.yhat.afix.m.elast<-predict(boot.m2.elast,newx=as.matrix(boot.gcomp[,m2.xvars]))
		boot.cme.rwr.lm[j,1]<-mean(boot.yhat.afix.mstar.lm)-mean(boot.yhat.afix.m.lm)
		boot.cme.rwr.lasso[j,1]<-mean(boot.yhat.afix.mstar.lasso)-mean(boot.yhat.afix.m.lasso)
		boot.cme.rwr.elast[j,1]<-mean(boot.yhat.afix.mstar.elast)-mean(boot.yhat.afix.m.elast)

		boot.dif1.rwr.lm[j,1]<-boot.ate.rwr.lm[j,1]-boot.cde.rwr.lm[j,1]
		boot.dif1.rwr.lasso[j,1]<-boot.ate.rwr.lasso[j,1]-boot.cde.rwr.lasso[j,1]
		boot.dif1.rwr.elast[j,1]<-boot.ate.rwr.elast[j,1]-boot.cde.rwr.elast[j,1]
		}

	mivar.rwr.lm[i,1]<-var(boot.ate.rwr.lm)
	mivar.rwr.lm[i,2]<-var(boot.aje.rwr.lm)
	mivar.rwr.lm[i,3]<-var(boot.cde.rwr.lm)
	mivar.rwr.lm[i,4]<-var(boot.cme.rwr.lm)
	mivar.rwr.lm[i,5]<-var(boot.dif1.rwr.lm)

	mivar.rwr.lasso[i,1]<-var(boot.ate.rwr.lasso)
	mivar.rwr.lasso[i,2]<-var(boot.aje.rwr.lasso)
	mivar.rwr.lasso[i,3]<-var(boot.cde.rwr.lasso)
	mivar.rwr.lasso[i,4]<-var(boot.cme.rwr.lasso)
	mivar.rwr.lasso[i,5]<-var(boot.dif1.rwr.lasso)

	mivar.rwr.elast[i,1]<-var(boot.ate.rwr.elast)
	mivar.rwr.elast[i,2]<-var(boot.aje.rwr.elast)
	mivar.rwr.elast[i,3]<-var(boot.cde.rwr.elast)
	mivar.rwr.elast[i,4]<-var(boot.cme.rwr.elast)
	mivar.rwr.elast[i,5]<-var(boot.dif1.rwr.elast)
	}

### COMBINE MI ESTIMATES ###
rwrest.lm<-rwrest.lasso<-rwrest.elast<-matrix(data=NA,nrow=5,ncol=4)

for (i in 1:5) { 
	rwrest.lm[i,1]<-round(mean(miest.rwr.lm[,i]),digits=4)
	rwrest.lm[i,2]<-round(sqrt(mean(mivar.rwr.lm[,i])+(var(miest.rwr.lm[,i])*(1+(1/nmi)))),digits=4)
	rwrest.lm[i,3]<-round((rwrest.lm[i,1]/rwrest.lm[i,2]),digits=4)
	rwrest.lm[i,4]<-round((pnorm(abs(rwrest.lm[i,3]),0,1,lower.tail=FALSE)*2),digits=4)

	rwrest.lasso[i,1]<-round(mean(miest.rwr.lasso[,i]),digits=4)
	rwrest.lasso[i,2]<-round(sqrt(mean(mivar.rwr.lasso[,i])+(var(miest.rwr.lasso[,i])*(1+(1/nmi)))),digits=4)
	rwrest.lasso[i,3]<-round((rwrest.lasso[i,1]/rwrest.lasso[i,2]),digits=4)
	rwrest.lasso[i,4]<-round((pnorm(abs(rwrest.lasso[i,3]),0,1,lower.tail=FALSE)*2),digits=4)

	rwrest.elast[i,1]<-round(mean(miest.rwr.elast[,i]),digits=4)
	rwrest.elast[i,2]<-round(sqrt(mean(mivar.rwr.elast[,i])+(var(miest.rwr.elast[,i])*(1+(1/nmi)))),digits=4)
	rwrest.elast[i,3]<-round((rwrest.elast[i,1]/rwrest.elast[i,2]),digits=4)
	rwrest.elast[i,4]<-round((pnorm(abs(rwrest.elast[i,3]),0,1,lower.tail=FALSE)*2),digits=4)
	}

### PRINT RESULTS ###
rlabel<-c('ATE','AJE','CDE','CME','ATE-CDE')

output.rwr.lm<-data.frame(rwrest.lm,row.names=rlabel)
output.rwr.lasso<-data.frame(rwrest.lasso,row.names=rlabel)
output.rwr.elast<-data.frame(rwrest.elast,row.names=rlabel)

colnames(output.rwr.lm)<-c('Est','SE','Z','P-value')
colnames(output.rwr.lasso)<-c('Est','SE','Z','P-value')
colnames(output.rwr.elast)<-c('Est','SE','Z','P-value')

cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("===========================================\n")
cat("Linear and Additive Model (LAM)\n")
cat("===========================================\n")
print(output.rwr.lm)
cat("===========================================\n")
cat(" \n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat(" \n")
cat("===========================================\n")
cat("LAM with LASSO Regularization\n")
cat("===========================================\n")
print(output.rwr.lasso)
cat("===========================================\n")
cat(" \n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat(" \n")
cat("===========================================\n")
cat("LAM with Elasticnet Regularization\n")
cat("===========================================\n")
print(output.rwr.elast)
cat("===========================================\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")

print(Sys.time())

sink()
