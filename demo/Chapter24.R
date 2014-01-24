############################################################################
#     MLwiN MCMC Manual
#
# 24  Parameter expansion . . . . . . . . . . . . . . . . . . . . . . . .381
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

# 24.1 What is Parameter Expansion? . . . . . . . . . . . . . . . . . . .381

# 24.2 The tutorial example . . . . . . . . . . . . . . . . . . . . . . .383

library(R2MLwiN)
## Input the MLwiN tutorial data set
# MLwiN folder
if(!exists("mlwin")) mlwin ="C:/Program Files (x86)/MLwiN v2.29/"
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

## Define the model
formula="normexam~(0|cons+standlrt)+(2|cons)+(1|cons)"
levID=c('school','student')
estoptions= list(EstM=1)
(mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin))
summary(mymodel["chains"][,"RP2_var_cons"])
sixway(mymodel["chains"][,"RP2_var_cons"],"sigma2u2")

## Parameter expansion at level 2
estoptions= list(EstM=1, mcmcOptions=list(paex=c(2,1)))
(mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin))
sixway(mymodel["chains"][,"RP2_var_cons"],"sigma2u0")

# 24.3 Binary responses - Voting example . . . . . . . . . . . . . . . . 386


## Read tutorial data from runmlwin (Leckie&Charlton, 2011) data folder
library(foreign); indata =read.dta("http://www.bristol.ac.uk/cmm/media/runmlwin/bes83.dta")

## Alternatively converts bes83.ws under mlwin sample folder to bes83.dta
#wsfile=paste(mlwin,"/samples/bes83.ws",sep="")
## the tutorial.dta will be save under the temporary folder
#inputfile=paste(tempdir(),"/bes83.dta",sep="")
#ws2foreign(wsfile, foreignfile=inputfile, MLwiNPath=mlwin)
#library(foreign); indata =read.dta(inputfile)

## Define the model
formula="logit(votecons,cons)~(0|cons+defence+ unemp+ taxes+ privat)+(2|cons)"
levID=c('area','voter')
estoptions= list(EstM=1)
(mymodel=runMLwiN(formula, levID, D="Binomial", indata, estoptions,MLwiNPath=mlwin))
sixway(mymodel["chains"][,"RP2_var_cons"],acf.maxlag=500,"sigma2u0")

## Parameter expansion at level 2
estoptions= list(EstM=1, mcmcOptions=list(paex=c(2,1)))
(mymodel=runMLwiN(formula, levID, D="Binomial", indata, estoptions,MLwiNPath=mlwin))
sixway(mymodel["chains"][,"RP2_var_cons"],acf.maxlag=500,"sigma2u0")

# 24.4 The choice of prior distribution . . . . . . . . . . . . . . . . .390

## Uniform on the variance scale priors+Parameter expansion at level 2
estoptions= list(EstM=1, mcmcMeth=list(priorcode=0),mcmcOptions=list(paex=c(2,1)))
(mymodel=runMLwiN(formula, levID, D="Binomial", indata, estoptions,MLwiNPath=mlwin))
sixway(mymodel["chains"][,"RP2_var_cons"],acf.maxlag=100,"sigma2u0")

# 24.5 Parameter expansion and WinBUGS . . . . . . . . . . . . . . . . . 391

## winbugs executable
if(!exists("winbugs")) winbugs="C:/Program Files (x86)/WinBUGS14/WinBUGS14.exe"
while (!file.access(winbugs,mode=0)==0||!file.access(winbugs,mode=1)==0||!file.access(winbugs,mode=4)==0){
    cat("Please specify the path for the WinBUGS executable:\n")
    winbugs=scan(what=character(0),sep ="\n")
    winbugs=gsub("\\", "/",winbugs, fixed=TRUE)
}
# User's input if necessary

estoptions= list(EstM=1,mcmcMeth=list(priorcode=0),mcmcOptions=list(paex=c(2,1)),show.file=T)
mymodel=runMLwiN(formula, levID, D="Binomial", indata, estoptions,BUGO=c(version=4,n.chains=1,debug=F,seed=1,bugs=winbugs, OpenBugs = F),MLwiNPath=mlwin)
apply(mymodel[[1]],2,effectiveSize)
sixway(mymodel[[1]][,"sigma2.u2"],acf.maxlag=250,"sigma2.u2")
sixway(mymodel[[1]][,"sigma2.v2"],acf.maxlag=100,"sigma2.v2")

# 24.6 Parameter expansion and random slopes . . . . . . . . . . . . . . 396

## Read tutorial data from runmlwin (Leckie&Charlton, 2011) data folder
library(foreign); indata =read.dta("http://www.bristol.ac.uk/cmm/media/runmlwin/tutorial.dta")

## Alternatively converts tutorial.ws under mlwin sample folder to tutorial.dta
#wsfile=paste(mlwin,"/samples/tutorial.ws",sep="")
## the tutorial.dta will be save under the temporary folder
#inputfile=paste(tempdir(),"/tutorial.dta",sep="")
#ws2foreign(wsfile, foreignfile=inputfile, MLwiNPath=mlwin)
#library(foreign); indata =read.dta(inputfile)

## Define the model
formula="normexam~(0|cons+standlrt)+(2|cons+standlrt)+(1|cons)"
levID=c('school','student')
estoptions= list(EstM=1)
(mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin))


## Parameter expansion at level 2
estoptions= list(EstM=1, mcmcOptions=list(paex=c(2,1)))
(mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin))

# Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . . .399





############################################################################
