############################################################################
#     MLwiN MCMC Manual
#
# 5   Prior Distributions, Starting Values and Random Number Seeds . . . .61
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

# 5.1 Prior distributions . . . . . . . . . . . . . . . . . . . . . . . . 61

# 5.2 Uniform on variance scale priors . . . . . . . . . . . . . . . . . .61

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

## IGLS
estoptions= list(EstM=0)
## Fit the model
(mymodel1=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin, workdir = tempdir()))

## Diffuse priors (Gamma priors)
estoptions= list(EstM=1)
## Fit the model
(mymodel2=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin))

## Diffuse priors (Uniform priors)
estoptions= list(EstM=1,mcmcMeth=list(priorcode=0))
## Fit the model
(mymodel3=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin))

aa=cbind(mymodel1["FP"],mymodel2["FP"],mymodel3["FP"])
bb=cbind(mymodel1["RP"],mymodel2["RP"],mymodel3["RP"])
ctable=round(rbind(aa,bb),3)
colnames(ctable)=c("IGLS","Gibbs1", "Gibbs2")
print(ctable)
rm(list=c("mymodel1","mymodel2","mymodel3"))

# 5.3 Using informative priors . . . . . . . . . . . . . . . . . . . . . .62

## Informative normal prior for beta_1
prior=list(fixe=list(standlrt=c(1,.01)))
prior=prior2macro(prior,formula,levID,D='Normal', indata)
estoptions= list(EstM=1,mcmcMeth=list(priorParam=prior))
## Fit the model
(mymodel4=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin))
sixway(mymodel4["chains"][,"FP_standlrt"],"beta_1")

## Informative normal prior for beta_1
prior=list(fixe=list(standlrt=c(1,.1)))
prior=prior2macro(prior,formula,levID,D='Normal', indata)
estoptions= list(EstM=1,mcmcMeth=list(priorParam=prior))
## Fit the model
(mymodel5=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin))
sixway(mymodel5["chains"][,"FP_standlrt"],"beta_1")

# 5.4 Specifying an informative prior for a random parameter . . . . . . .65

## Specifies an ingormative prior for sigma2u
prior=list(rp2=list(estimates=.2,size=100))
prior=prior2macro(prior,formula,levID,D='Normal', indata)
estoptions= list(EstM=1,mcmcMeth=list(priorParam=prior))
## Fit the model
(mymodel6=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin))
sixway(mymodel6["chains"][,"RP2_var_cons"],"sigma^2_u0")

# 5.5 Changing the random number seed and the parameter starting values  .66

## Set starting values for random and fixed parameter estimates
startval=list(FP.b=c(-2,5),RP.b=c(2,4))
estoptions= list(EstM=1,mcmcMeth=list(burnin=0, iterations=500,startval=startval))
## Fit the model
(mymodel7=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin))

rm(list=c("mymodel4","mymodel5","mymodel6","mymodel7"))

##Use different seeds
estoptions= list(EstM=1,mcmcMeth=list(seed=1))
(mymodel8=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin))

estoptions= list(EstM=1,mcmcMeth=list(seed=2))
(mymodel9=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin))

estoptions= list(EstM=1,mcmcMeth=list(seed=3))
(mymodel10=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin))

estoptions= list(EstM=1,mcmcMeth=list(seed=4))
(mymodel11=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin))

aa=cbind(mymodel8["FP"],mymodel9["FP"],mymodel10["FP"],mymodel11["FP"])
bb=cbind(mymodel8["RP"],mymodel9["RP"],mymodel10["RP"],mymodel11["RP"])
ctable=round(rbind(aa,bb),3)
colnames(ctable)=c("Seed1","Seed2", "Seed3","Seed4")
print(ctable)
rm(list=c("mymodel8","mymodel9","mymodel10","mymodel11"))

# 5.6 Improving the speed of MCMC Estimation . . . . . . . . . . . . . . .69

# Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . . . 70





############################################################################
