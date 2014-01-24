############################################################################
#     MLwiN MCMC Manual
#
# 23  Using Orthogonal fixed effect vectors . . . . . . . . . . . . . . .357
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

# 23.1 A simple example . . . . . . . . . . . . . . . . . . . . . . . . .358

# 23.2 Constructing orthogonal vectors . . . . . . . . . . . . . . . . . 359

# 23.3 A Binomial response example . . . . . . . . . . . . . . . . . . . 360

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

## Read bang1 data from runmlwin (Leckie&Charlton, 2011) data folder
library(foreign); indata =read.dta("http://www.bristol.ac.uk/cmm/media/runmlwin/bang1.dta")

## Alternatively converts bang1.ws under mlwin sample folder to bang1.dta
# MLwiN sample worksheet folder
#wsfile=paste(mlwin,"/samples/bang1.ws",sep="")
## the tutorial.dta will be save under the temporary folder
#inputfile=paste(tempdir(),"/bang1.dta",sep="")
#ws2foreign(wsfile, foreignfile=inputfile, MLwiNPath=mlwin)
#library(foreign); indata =read.dta(inputfile)

levels(indata[["lc"]])=c("nokids",     "onekid",     "twokids",    "threepluskids")
## winbugs executable
if(!exists("winbugs")) winbugs="C:/Program Files (x86)/WinBUGS14/WinBUGS14.exe"
while (!file.access(winbugs,mode=0)==0||!file.access(winbugs,mode=1)==0||!file.access(winbugs,mode=4)==0){
    cat("Please specify the path for the WinBUGS executable:\n")
    winbugs=scan(what=character(0),sep ="\n")
    winbugs=gsub("\\", "/",winbugs, fixed=TRUE)
}

# User's input if necessary

## Openbugs executable
#openbugs="C:/Program Files (x86)/OpenBUGS321/OpenBUGS.exe"


## Define the model
formula="logit(use,denomb)~(0|cons+age+lc[nokids]+urban)+(2|cons+urban)"
levID=c('district','woman')
estoptions= list(EstM=1)
(mymodel=runMLwiN(formula, levID, D="Binomial", indata, estoptions,MLwiNPath=mlwin))
trajectories(mymodel["chains"])

##Orthogonal update
estoptions= list(EstM=1, mcmcOptions=list(orth=1))
(mymodel=runMLwiN(formula, levID, D="Binomial", indata, estoptions,MLwiNPath=mlwin))
trajectories(mymodel["chains"])

# 23.4 A Poisson example . . . . . . . . . . . . . . . . . . . . . . . . 364

wsfile=paste(mlwin,"/samples/mmmec1.ws",sep="")
# the tutorial.dta will be save under the temporary folder
inputfile=paste(tempdir(),"/mmmec1.dta",sep="")
ws2foreign(wsfile, foreignfile=inputfile, MLwiNPath=mlwin)
library(foreign); indata =read.dta(inputfile)
indata[["logexp"]]=double2singlePrecision(log(indata[["exp"]]))
levels(indata[["nation"]])=c("Belgium", "W_Germany", "Denmark", "France", "UK", "Italy", "Ireland", "Luxembourg", "Netherlands")

## Define the model
formula="log(obs,logexp)~(0|nation[]+Belgium:uvbi+W_Germany:uvbi+Denmark:uvbi+France:uvbi+UK:uvbi+Italy:uvbi+Ireland:uvbi+Luxembourg:uvbi+Netherlands:uvbi)+(2|cons)"
levID=c('region','county')
## Choose option(s) for inference
estoptions= list(EstM=1,mcmcMeth=list(iterations=50000))
## Fit the model
(mymodel=runMLwiN(formula, levID, D="Poisson", indata, estoptions,MLwiNPath=mlwin))
sixway(mymodel["chains"][,"FP_Belgium"],acf.maxlag=5000,"beta_1")

##Orthogonal update
estoptions= list(EstM=1, mcmcMeth=list(iterations=50000), mcmcOptions=list(orth=1))
(mymodel=runMLwiN(formula, levID, D="Poisson", indata, estoptions,MLwiNPath=mlwin))
sixway(mymodel["chains"][,"FP_Belgium"],acf.maxlag=100,"beta_1")

# 23.5 An Ordered multinomial example . . . . . . . . . . . . . . . . . .368

## Read alevchem data from runmlwin (Leckie&Charlton, 2011) data folder
library(foreign); indata =read.dta("http://www.bristol.ac.uk/cmm/media/runmlwin/alevchem.dta")
# MLwiN sample worksheet folder

## Alternatively converts alevchem.ws under mlwin sample folder to alevchem.dta
#wsfile=paste(mlwin,"/samples/alevchem.ws",sep="")
## the tutorial.dta will be save under the temporary folder
#inputfile=paste(tempdir(),"/alevchem.dta",sep="")
#ws2foreign(wsfile, foreignfile=inputfile, MLwiNPath=mlwin)
#library(foreign);indata =read.dta(inputfile)
#names(indata)=gsub("-","_",names(indata))

indata["gcseav"]=double2singlePrecision(indata["gcse_tot"]/indata["gcse_no"]-6)
indata["gcse2"]=double2singlePrecision(indata["gcseav"]^2)
indata["gcse3"]=double2singlePrecision(indata["gcseav"]^3)

formula="logit(a_point,cons,A) ~ (0s|cons)+(0c|gcseav+gcse2+gender) +( 2c | cons)"
levID=c('estab','pupil')
##MCMC
estoptions= list(EstM=1)
## Fit the model
(mymodel=runMLwiN(formula, levID, D='Ordered Multinomial', indata, estoptions,MLwiNPath=mlwin))
trajectories(mymodel["chains"])

##Orthogonal update
estoptions= list(EstM=1, mcmcOptions=list(orth=1))
## Fit the model
(mymodel=runMLwiN(formula, levID, D='Ordered Multinomial', indata, estoptions,MLwiNPath=mlwin))
trajectories(mymodel["chains"])

# 23.6 The WinBUGS interface . . . . . . . . . . . . . . . . . . . . . . 372

## Read bang1 data from runmlwin (Leckie&Charlton, 2011) data folder
library(foreign); indata =read.dta("http://www.bristol.ac.uk/cmm/media/runmlwin/bang1.dta")

## Alternatively converts bang1.ws under mlwin sample folder to bang1.dta
# MLwiN sample worksheet folder
#wsfile=paste(mlwin,"/samples/bang1.ws",sep="")
## the tutorial.dta will be save under the temporary folder
#inputfile=paste(tempdir(),"/bang1.dta",sep="")
#ws2foreign(wsfile, foreignfile=inputfile, MLwiNPath=mlwin)
#library(foreign); indata =read.dta(inputfile)

levels(indata[["lc"]])=c("nokids",     "onekid",     "twokids",    "threepluskids")

## winbugs executable
if(!exists("winbugs")) winbugs="C:/Program Files (x86)/WinBUGS14/WinBUGS14.exe"
while (!file.access(winbugs,mode=0)==0||!file.access(winbugs,mode=1)==0||!file.access(winbugs,mode=4)==0){
    cat("Please specify the path for the WinBUGS executable:\n")
    winbugs=scan(what=character(0),sep ="\n")
    winbugs=gsub("\\", "/",winbugs, fixed=TRUE)
}

# User's input if necessary

## Openbugs executable
#openbugs="C:/Program Files (x86)/OpenBUGS321/OpenBUGS.exe"


## Define the model
formula="logit(use,denomb)~(0|cons+age+lc[nokids]+urban)+(2|cons+urban)"
levID=c('district','woman')

##Orthogonal update (WinBUGS)
estoptions= list(EstM=1, mcmcOptions=list(orth=1),show.file=T)
mymodel=runMLwiN(formula, levID, D="Binomial", indata, estoptions,BUGO=c(version=4,n.chains=1,debug=F,seed=1,bugs=winbugs, OpenBugs = F),MLwiNPath=mlwin)
apply(mymodel[[1]],2,effectiveSize)
sixway(mymodel[[1]][,"beta[1]"],"beta[1]")

# Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . . .379





############################################################################
