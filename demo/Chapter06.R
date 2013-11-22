############################################################################
#     MLwiN MCMC Manual
#
# 6   Random Slopes Regression Models . . . . . . . . . . . . . . . . . . 71
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

library(R2MLwiN)
## Input the MLwiN tutorial data set
# MLwiN folder
if(!exists("mlwin")) mlwin ="C:/Program Files (x86)/MLwiN v2.27/"
while (!file.access(mlwin,mode=0)==0||!file.access(mlwin,mode=1)==0||!file.access(mlwin,mode=4)==0){
    cat("Please specify the MLwiN folder including the MLwiN executable:\n")
    mlwin=scan(what=character(0),sep ="\n")
    mlwin=gsub("\\", "/",mlwin, fixed=TRUE)
}

# User's input if necessary

## Read tutorial data from runmlwin (Leckie&Charlton, 2011) data folder
library(foreign); indata =read.dta("http://www.bristol.ac.uk/cmm/media/runmlwin/tutorial.dta")

## Alternatively converts tutorial.ws under mlwin sample folder to tutorial.dta
## MLwiN sample worksheet folder
#wsfile=paste(mlwin,"/samples/tutorial.ws",sep="")
## the tutorial.dta will be save under the temporary folder
#inputfile=paste(tempdir(),"/tutorial.dta",sep="")
#ws2foreign(wsfile, foreignfile=inputfile, MLwiNPath=mlwin)
#library(foreign); indata =read.dta(inputfile)

indata=cbind(indata,Untoggle(indata[["school"]],"school"))

## Define the model
tempstr1=paste("+",names(indata)[12:75],collapse="")
tempstr2=paste("+",names(indata)[12:75],":standlrt",collapse="")
formula=paste("normexam~(0|cons+standlrt",tempstr1,tempstr2,")+(1|cons)",sep="")
levID='student'
## Choose MCMC algoritm for estimation (IGLS will be used to obtain starting values for MCMC)
estoptions= list(EstM=1)
## Fit the model
(mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin, workdir = tempdir()))

## Define the model
formula="normexam~(0|cons+standlrt)+(2|cons+standlrt)+(1|cons)"
levID=c('school','student')

## Choose IGLS algoritm for estimation
estoptions= list(EstM=0)
## Fit the model
(mymodel0a=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin, workdir = tempdir()))

## Choose MCMC algoritm for estimation (IGLS will be used to obtain starting values for MCMC)
estoptions= list(EstM=1)
## Fit the model
(mymodel0=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin, workdir = tempdir()))

# 6.1 Prediction intervals for a random slopes regression model . . . . . 75

## Save level 2 residual chains
estoptions= list(EstM=1, mcmcMeth=list(iterations=5001),resi.store.levs=2)
## Fit the model
(mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin, workdir = tempdir()))

predLines(mymodel, indata, xname="standlrt", lev = 2, selected =NULL, probs=c(.025,.975), legend.space="right", legend.ncol=2)
windows()
predLines(mymodel, indata, xname="standlrt", lev = 2, selected =c(30,44,53,59), probs=c(.025,.975))

# 6.2 Alternative priors for variance matrices . . . . . . . . . . . . . .78

# 6.3 WinBUGS priors (Prior 2) . . . . . . . . . . . . . . . . . . . . . .78

## Change the starting values for Level 2 variance matrix to .1 on diagonal 0 otherwise.
estoptions= list(EstM=1, mcmcMeth=list(startval=list(RP.b=c(.1,0,.1,.554))))
## Fit the model
(mymodel1=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin, workdir = tempdir()))

# 6.4 Uniform prior . . . . . . . . . . . . . . . . . . . . . . . . . . . 79

## Diffuse priors (Uniform priors)
estoptions= list(EstM=1,mcmcMeth=list(priorcode=0))
## Fit the model
(mymodel2=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin, workdir = tempdir()))

# 6.5 Informative prior . . . . . . . . . . . . . . . . . . . . . . . . . 80

## Informative normal prior for Sigma_u
prior=list(rp2=list(estimate=matrix(c(.09,.018,.09,.015),2,2),size=65))
prior=prior2macro(prior,formula,levID,D='Normal', indata)
estoptions= list(EstM=1,mcmcMeth=list(priorParam=prior))
## Fit the model
(mymodel3=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin, workdir = tempdir()))

# 6.6 Results . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 81

cat("The mean parameter estimates\n")
aa=cbind(mymodel0a["FP"],mymodel0["FP"],mymodel1["FP"],mymodel2["FP"],mymodel3["FP"])
bb=cbind(mymodel0a["RP"],mymodel0["RP"],mymodel1["RP"],mymodel2["RP"],mymodel3["RP"])
ctable=round(rbind(aa,bb),3)
colnames(ctable)=c("IGLS","default","prior 2", "uniform", "prior 4")
print(ctable)

cat("The standard errors of parameter estimates\n")
cc=cbind(sqrt(diag(mymodel0a["FP.cov"])),sqrt(diag(mymodel0["FP.cov"])),sqrt(diag(mymodel1["FP.cov"])),sqrt(diag(mymodel2["FP.cov"])),sqrt(diag(mymodel3["FP.cov"])))
dd=cbind(sqrt(diag(mymodel0a["RP.cov"])),sqrt(diag(mymodel0["RP.cov"])),sqrt(diag(mymodel1["RP.cov"])),sqrt(diag(mymodel2["RP.cov"])),sqrt(diag(mymodel3["RP.cov"])))
sdtable=round(rbind(cc,dd),3)
colnames(sdtable)=c("IGLS","default","prior 2", "uniform", "prior 4")
print(sdtable)

# Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . . . 81





############################################################################
