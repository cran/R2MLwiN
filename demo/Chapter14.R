############################################################################
#     MLwiN MCMC Manual
#
# 14  Adjusting for Measurement Errors in Predictor Variables . . . . . .199
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
if(!exists("mlwin")) mlwin ="C:/Program Files (x86)/MLwiN v2.26/"
while (!file.access(mlwin,mode=0)==0||!file.access(mlwin,mode=1)==0||!file.access(mlwin,mode=4)==0){
    cat("Please specify the MLwiN folder including the MLwiN executable:\n")
    mlwin=scan(what=character(0),sep ="\n")
    mlwin=gsub("\\", "/",mlwin, fixed=TRUE)
}

# User's input if necessary

## Read tutorial data from runmlwin (Leckie&Charlton, 2011) data folder
library(foreign); indata =read.dta("http://www.bristol.ac.uk/cmm/media/runmlwin/tutorial.dta")

## Alternatively converts tutorial.ws under mlwin sample folder to tutorial.dta
# MLwiN sample worksheet folder
#wsfile=paste(mlwin,"/samples/tutorial.ws",sep="")
## the tutorial.dta will be save under the temporary folder
#inputfile=paste(tempdir(),"/tutorial.dta",sep="")
#ws2foreign(wsfile, foreignfile=inputfile, MLwiNPath=mlwin)
#library(foreign); indata =read.dta(inputfile)

# 14.1 Effects of measurement error on predictors . . . . . . . . . . . .200
set.seed(1)
error=double2singlePrecision(rnorm(length(indata[,"standlrt"]),0,sqrt(.2)))
obslrt=double2singlePrecision(indata[,"standlrt"]+error)
indata=cbind(indata,error,obslrt)

formula="normexam~(0|cons+standlrt)+(1|cons)"
levID='student'
estoptions= list(EstM=0)
mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions,MLwiNPath=mlwin)


formula="normexam~(0|cons+error)+(1|cons)"
levID='student'
estoptions= list(EstM=0)
mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions,MLwiNPath=mlwin)

formula="normexam~(0|cons+obslrt)+(1|cons)"
levID='student'
estoptions= list(EstM=0)
mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions,MLwiNPath=mlwin)
estoptions= list(EstM=1,merr=c(N=1,"obslrt",.2))
mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions,MLwiNPath=mlwin)

# 14.2 Measurement error modelling in multilevel models . . . . . . . . .205

formula="normexam~(0|cons+standlrt)+(2|cons+standlrt)+(1|cons)"
levID=c('school','student')
estoptions= list(EstM=1)
mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions,MLwiNPath=mlwin)

formula="normexam~(0|cons+obslrt)+(2|cons+obslrt)+(1|cons)"
levID=c('school','student')
estoptions= list(EstM=1)
mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions,MLwiNPath=mlwin)

formula="normexam~(0|cons+obslrt)+(2|cons+obslrt)+(1|cons)"
levID=c('school','student')
estoptions= list(EstM=1,merr=c(N=1,"obslrt",.2))
mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions,MLwiNPath=mlwin)

# 14.3 Measurement errors in binomial models . . . . . . . . . . . . . . 208


## Read bang1 data from runmlwin (Leckie&Charlton, 2011) data folder
library(foreign); indata =read.dta("http://www.bristol.ac.uk/cmm/media/runmlwin/bang1.dta")

## Alternatively converts bang1.ws under mlwin sample folder to bang1.dta
## Input the MLwiN tutorial data set
# MLwiN folder (Modify the path as appropriate.
#mlwin ="C:/Stat-JR/MLwiN v2.25/"
## MLwiN sample worksheet folder
#wsfile=paste(mlwin,"/samples/bang1.ws",sep="")
## the tutorial.dta will be save under the temporary folder
#inputfile=paste(tempdir(),"/bang1.dta",sep="")
#ws2foreign(wsfile, foreignfile=inputfile, MLwiNPath=mlwin)
#library(foreign); indata =read.dta(inputfile)

set.seed(1)
obsage=double2singlePrecision(indata[["age"]]+rnorm(length(indata[["age"]]),0,5))
indata=cbind(indata,obsage)

formula="logit(use,denomb)~(0|cons+age)"
levID=c('district','woman')
estoptions= list(EstM=1)
mymodel=runMLwiN(formula, levID, D="Binomial", indata, estoptions,MLwiNPath=mlwin)

formula="logit(use,denomb)~(0|cons+obsage)"
levID=c('district','woman')
estoptions= list(EstM=1)
mymodel=runMLwiN(formula, levID, D="Binomial", indata, estoptions,MLwiNPath=mlwin)
## Adjust for the measurement errors
estoptions= list(EstM=1,merr=c(N=1,"obsage",25))
mymodel=runMLwiN(formula, levID, D="Binomial", indata, estoptions,MLwiNPath=mlwin)

# 14.4 Measurement errors in more than one variable and
#      misclassifications . . . . . . . . . . . . . . . . . . . . . . . .211

# Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . . .128





############################################################################
