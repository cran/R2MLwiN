############################################################################
#     MLwiN MCMC Manual
#
# 3   Variance Components Models . . . . . . . . . . . . . . . . . . . . .35
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

# 3.1 A 2 level variance components model for the Tutorial dataset . . . .36

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

## Define the model
formula="normexam~(0|cons+standlrt)+(2|cons)+(1|cons)"
## The highest level comes first, then the second highest and so on
levID=c('school','student')
## Choose option(s) for inference
estoptions= list(EstM=1)
## Fit the model
(mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin, workdir = tempdir()))

estimates=mymodel["chains"]
par(mfrow=c(3,2))
plot(4501:nrow(estimates),estimates[4501:nrow(estimates),"deviance"],xlab="iteration",
ylab=expression(paste("Est. of deviance")),type="l")
plot(4501:nrow(estimates),estimates[4501:nrow(estimates),"FP_cons"],xlab="iteration",
ylab=expression(paste("Est. of ",beta[0])),type="l")
plot(4501:nrow(estimates),estimates[4501:nrow(estimates),"FP_standlrt"],xlab="iteration",
ylab=expression(paste("Est. of ",beta[1])),type="l")
plot(4501:nrow(estimates),estimates[4501:nrow(estimates),"RP2_var_cons"],xlab="iteration",
ylab=expression(paste("Est. of ",sigma[u0]^2)),type="l")
plot(4501:nrow(estimates),estimates[4501:nrow(estimates),"RP1_var_cons"],xlab="iteration",
ylab=expression(paste("Est. of ",sigma[e0]^2)),type="l")

sixway(mymodel["chains"][,"FP_standlrt"],"beta_1")
sixway(mymodel["chains"][,"RP2_var_cons"],"sigma2u0")

# 3.2 DIC and multilevel models . . . . . . . . . . . . . . . . . . . . . 41

# 3.3 Comparison between fixed and random school effects . . . . . . . . .41

## Define the model
formula="normexam~(0|cons+standlrt+girl)+(2|cons)+(1|cons)"
levID=c('school','student')
## Choose option(s) for inference
estoptions= list(EstM=1)
## Fit the model
(mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin, workdir = tempdir()))

# Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . . . 43





############################################################################
