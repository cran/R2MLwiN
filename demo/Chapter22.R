############################################################################
#     MLwiN MCMC Manual
#
# 22  Using the Structured MVN framework for models . . . . . . . . . . .341
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

# 22.1 MCMC theory for Structured MVN models . . . . . . . . . . . . . . 341

# 22.2 Using the SMVN framework in practice . . . . . . . . . . . . . . .344

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
formula="normexam~(0|cons)+(2|cons)+(1|cons)"
levID=c('school','student')
estoptions= list(EstM=1)
(mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin))


## Structured MVN
estoptions= list(EstM=1, mcmcOptions=list(smvn=1))
(mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin))

# 22.3 Model Comparison and structured MVN models . . . . . . . . . . . .349

## Define the model
formula="normexam~(0|cons+standlrt)+(2|cons)+(1|cons)"
levID=c('school','student')

## Gibbs
estoptions= list(EstM=1)
(mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin))

## SMCMC
estoptions= list(EstM=1, mcmcOptions=list(smcm=1))
(mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin))

## Structured MVN
estoptions= list(EstM=1, mcmcOptions=list(smvn=1))
(mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin))

# 22.4 Assessing the need for the level 2 variance . . . . . . . . . . . 350

sixway(mymodel["chains"][,"RP2_var_cons"],"sigma2u0")

set.seed(1)
indata[["temp"]]=double2singlePrecision(rnorm(4059))

## Define the model
formula="temp~(0|cons+standlrt)+(2|cons)+(1|cons)"
levID=c('school','student')

##IGLS
estoptions= list(EstM=0)
(mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin))

## Gibbs
estoptions= list(EstM=1)
(mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin))

## Structured MVN
estoptions= list(EstM=1, mcmcOptions=list(smvn=1))
(mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin))

summary(mymodel["chains"][,"RP2_var_cons"])
sixway(mymodel["chains"][,"RP2_var_cons"],"sigma2u0")

# Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . . .355





############################################################################
