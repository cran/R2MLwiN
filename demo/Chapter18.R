############################################################################
#     MLwiN MCMC Manual
#
# 18  Multivariate Normal Response Models and Missing Data . . . . . . . 263
#
#     Browne, W.J. (2009) MCMC Estimation in MLwiN, v2.13. Centre for
#     Multilevel Modelling, University of Bristol.
############################################################################
#     R script to replicate all analyses using R2MLwiN
#
#     Zhang, Z., Charlton, C., Parker, R, Leckie, G., and Browne, W.J.
#     Centre for Multilevel Modelling, 2012
#     http://www.bristol.ac.uk/cmm/software/R2MLwiN/
############################################################################

# 18.1 GCSE science data with complete records only . . . . . . . . . . .264

library(R2MLwiN)
## Input the MLwiN tutorial data set
# MLwiN folder
if(!exists("mlwin")) mlwin ="C:/Program Files (x86)/MLwiN v2.30/"
while (!file.access(mlwin,mode=0)==0||!file.access(mlwin,mode=1)==0||!file.access(mlwin,mode=4)==0){
    cat("Please specify the MLwiN folder including the MLwiN executable:\n")
    mlwin=scan(what=character(0),sep ="\n")
    mlwin=gsub("\\", "/",mlwin, fixed=TRUE)
}

# User's input if necessary

## Read gcsecomp1 data from runmlwin (Leckie&Charlton, 2011) data folder
library(foreign); indata =read.dta("http://www.bristol.ac.uk/cmm/media/runmlwin/gcsecomp1.dta")

## Alternatively converts gcsecomp1.ws under mlwin sample folder to gcsecomp1.dta
## MLwiN sample worksheet folder
#wsfile=paste(mlwin,"/samples/gcsecomp1.ws",sep="")
## the tutorial.dta will be save under the temporary folder
#inputfile=paste(tempdir(),"/gcsecomp1.dta",sep="")
#ws2foreign(wsfile, foreignfile=inputfile, MLwiNPath=mlwin)
#library(foreign); indata =read.dta(inputfile)
#summary(indata)
#cor(indata[,c("written","csework")])

# 18.2 Fitting single level multivariate models . . . . . . . . . . . . .265

formula="c(written,csework)~(0|cons)+(1|cons)"
levID='student'
##IGLS
estoptions= list(EstM=0)
(mymodel=runMLwiN(formula, levID, D='Multivariate Normal', indata, estoptions,MLwiNPath=mlwin))
##MCMC
estoptions= list(EstM=1)
(mymodel=runMLwiN(formula, levID, D='Multivariate Normal', indata, estoptions,MLwiNPath=mlwin))

# 18.3 Adding predictor variables . . . . . . . . . . . . . . . . . . . .270

formula="c(written,csework)~(0|cons+female)+(1|cons)"
levID='student'
estoptions= list(EstM=1)
(mymodel=runMLwiN(formula, levID, D='Multivariate Normal', indata, estoptions,MLwiNPath=mlwin))

# 18.4 A multilevel multivariate model . . . . . . . . . . . . . . . . . 271

formula="c(written,csework)~(0|cons+female)+(2|cons)+(1|cons)"
levID=c('school','student')
##Store residual chain at level 3: school
estoptions= list(EstM=1,resi.store=T,resi.store.levs=3)
(mymodel=runMLwiN(formula, levID, D='Multivariate Normal', indata, estoptions,MLwiNPath=mlwin))

lencateg = length(unique(indata[["school"]]))
resi=na.omit(mymodel["resi.chains"][,"resi_lev3"])
##u0 and u1 alternates for each iteration (each column)
resi=matrix(resi,nrow =lencateg*2)
label=1:nrow(resi)

##highlight
hipos=rep(0,6)
hipos[1]=which(levels(as.factor(indata[["school"]]))==68137)
hipos[2]=which(levels(as.factor(indata[["school"]]))==68201)
hipos[3]=which(levels(as.factor(indata[["school"]]))==68711)
hipos[4]=which(levels(as.factor(indata[["school"]]))==60427)
hipos[5]=which(levels(as.factor(indata[["school"]]))==22710)
hipos[6]=which(levels(as.factor(indata[["school"]]))==67105)

par(mfrow=c(2,1))
##Select u0
resi0=resi[label[which(label%%2==1)],]
resi0mean = apply(resi0,1,mean)
resi0sd = apply(resi0,1,sd)
rankno0=order(resi0mean)
resi0.lo=resi0mean-1.4*resi0sd
resi0.hi=resi0mean+1.4*resi0sd
caterpillar(y=resi0mean[rankno0],x=1:length(resi0mean),qtlow=resi0.lo[rankno0],qtup=resi0.hi[rankno0],ylim=c(-24,21),ylab="cons.written",xlab="rank")
abline(h=0,lty="dotted")
for(i in 1:6) points(x=which(rankno0==hipos[i]),y=resi0mean[rankno0[which(rankno0==hipos[i])]],pch=22,bg=i+1)

##Select u1
resi1=resi[label[which(label%%2==0)],]
resi1mean = apply(resi1,1,mean)
resi1sd = apply(resi1,1,sd)
rankno1=order(resi1mean)
resi1.lo=resi1mean-1.4*resi1sd
resi1.hi=resi1mean+1.4*resi1sd
caterpillar(y=resi1mean[rankno1],x=1:length(resi1mean),qtlow=resi1.lo[rankno1],qtup=resi1.hi[rankno1],ylim=c(-24,21),ylab="cons.csework",xlab="rank")
abline(h=0,lty="dotted")
for(i in 1:6) points(x=which(rankno1==hipos[i]),y=resi1mean[rankno1[which(rankno1==hipos[i])]],pch=22,bg=i+1)

par(mfrow=c(1,1))
plot(resi0mean,resi1mean,pch=24,bg="black",xlab="cons.written",ylab="cons.csework")
for(i in 1:6) points(x=resi0mean[rankno0[which(rankno0==hipos[i])]],y=resi1mean[rankno1[which(rankno1==hipos[i])]],pch=24,bg=i+1)

# 18.5 GCSE science data with missing records . . . . . . . . . . . . . .275

library(R2MLwiN)
## Read gcsemv1 data from runmlwin (Leckie&Charlton, 2011) data folder
library(foreign); indata =read.dta("http://www.bristol.ac.uk/cmm/media/runmlwin/gcsemv1.dta")

## Alternatively converts gcsemv1.ws under mlwin sample folder to gcsemv1.dta
## Input the MLwiN tutorial data set
# MLwiN folder (Modify the path as appropriate.
#mlwin ="C:/Stat-JR/MLwiN v2.25/"
## MLwiN sample worksheet folder
#wsfile=paste(mlwin,"/samples/gcsemv1.ws",sep="")
## the tutorial.dta will be save under the temporary folder
#inputfile=paste(tempdir(),"/gcsemv1.dta",sep="")
#ws2foreign(wsfile, foreignfile=inputfile, MLwiNPath=mlwin)
#library(foreign); indata =read.dta(inputfile)

formula="c(written,csework)~(0|cons+female)+(1|cons)"
levID='student'
estoptions= list(EstM=1)
(mymodel=runMLwiN(formula, levID, D='Multivariate Normal', indata, estoptions,MLwiNPath=mlwin))

formula="c(written,csework)~(0|cons+female)+(2|cons)+(1|cons)"
levID=c('school','student')
estoptions= list(EstM=1,mcmcMeth=list(dami=2))
(mymodel=runMLwiN(formula, levID, D='Multivariate Normal', indata, estoptions,MLwiNPath=mlwin))


# 18.6 Imputation methods for missing data . . . . . . . . . . . . . . . 280

# 18.7 Hungarian science exam dataset . . . . . . . . . . . . . . . . . .281

library(R2MLwiN)
## Input the MLwiN tutorial data set
# MLwiN folder
if(!exists("mlwin")) mlwin ="C:/Program Files (x86)/MLwiN v2.26/"
while (!file.access(mlwin,mode=0)==0||!file.access(mlwin,mode=1)==0||!file.access(mlwin,mode=4)==0){
    cat("Please specify the MLwiN folder including the MLwiN executable:\n")
    mlwin=scan(what=character(0),sep ="\n")
    mlwin=gsub("\\", "/",mlwin, fixed=TRUE)
}

# User's input if necessary
# MLwiN sample worksheet folder
wsfile=paste(mlwin,"/samples/hungary1.ws",sep="")
# the tutorial.dta will be save under the temporary folder
inputfile=paste(tempdir(),"/hungary1.dta",sep="")
ws2foreign(wsfile, foreignfile=inputfile, MLwiNPath=mlwin)
library(foreign); indata =read.dta(inputfile)
summary(indata)

formula="c(es_core,biol_core,biol_r3,biol_r4,phys_core,phys_r2)~(0|cons+female)+(2|cons)+(1|cons)"
levID=c('school','student')
estoptions= list(EstM=0)
(mymodel=runMLwiN(formula, levID, D='Multivariate Normal', indata, estoptions,MLwiNPath=mlwin))

estoptions= list(EstM=1,mcmcMeth=list(dami=c(0,1000,2000,3000,4000,5000)))
(mymodel=runMLwiN(formula, levID, D='Multivariate Normal', indata, estoptions,MLwiNPath=mlwin))
head(mymodel["MIdata"])

# Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . . .128





############################################################################
