############################################################################
#     MLwiN MCMC Manual
#
# 21  Using Structured MCMC . . . . . . . . . . . . . . . . . . . . . . .327
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

# 21.1 SMCMC Theory . . . . . . . . . . . . . . . . . . . . . . . . . . .327

# 21.2 Fitting the model using MLwiN . . . . . . . . . . . . . . . . . . 330

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
# MLwiN sample worksheet folder
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
summary(mymodel["chains"][,"FP_cons"])
sixway(mymodel["chains"][,"FP_cons"],"beta_0")

## Structured MCMC
estoptions= list(EstM=1, mcmcOptions=list(smcm=1))
(mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin))
summary(mymodel["chains"][,"FP_cons"])
sixway(mymodel["chains"][,"FP_cons"],"beta_0")

# 21.3 A random intercepts model . . . . . . . . . . . . . . . . . . . . 334

formula="normexam~(0|cons+standlrt)+(2|cons)+(1|cons)"
levID=c('school','student')
estoptions= list(EstM=1, mcmcOptions=list(smcm=1))
(mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin))
trajectories(mymodel["chains"],Range=c(1,500))

# 21.4 Examining the residual chains . . . . . . . . . . . . . . . . . . 335

estoptions= list(EstM=1, resi.store=T, resi.store.levs=2,mcmcMeth=list(iterations=5001),mcmcOptions=list(smcm=1))
(mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin))
## Each row represents each iteration
resi=matrix(mymodel["resi.chains"][,1],ncol=65,byrow=T)
sixway(resi[,1],"school1")

# 21.5 Random slopes model theory . . . . . . . . . . . . . . . . . . . .336

# 21.6 Random Slopes model practice . . . . . . . . . . . . . . . . . . .338

formula="normexam~(0|cons+standlrt)+(2|cons+standlrt)+(1|cons)"
levID=c('school','student')
estoptions= list(EstM=1, mcmcOptions=list(smcm=1))
(mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin))

sixway(mymodel["chains"][,"FP_cons"],"beta_0")
sixway(mymodel["chains"][,"FP_standlrt"],"beta_1")

# Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . . .340





############################################################################
