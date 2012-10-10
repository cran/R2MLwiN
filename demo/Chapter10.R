############################################################################
#     MLwiN MCMC Manual
#
# 10  Modelling Binary Responses . . . . . . . . . . . . . . . . . . . . 129
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

## Read bang1 data from runmlwin (Leckie&Charlton, 2011) data folder
library(foreign); indata =read.dta("http://www.bristol.ac.uk/cmm/media/runmlwin/bang1.dta")

## Alternatively converts bang1.ws under mlwin sample folder to bang1.dta
## MLwiN sample worksheet folder
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

# 10.1 Simple logistic regression model . . . . . . . . . . . . . . . . .130

## Define the model
formula="logit(use,denomb)~(0|cons+age)"
levID=c('district','woman')
## Choose option(s) for inference
estoptions= list(EstM=1)
## Fit the model
mymodel1=runMLwiN(formula, levID, D="Binomial", indata, estoptions,MLwiNPath=mlwin)
summary(mymodel1["chains"][["FP_age"]])
sixway(mymodel1["chains"][["FP_age"]],"beta_1")

## 15,000 iterations
estoptions= list(EstM=1,mcmcMeth=list(iterations=15000))
## Fit the model
mymodel2=runMLwiN(formula, levID, D="Binomial", indata, estoptions,MLwiNPath=mlwin)
sixway(mymodel1["chains"][["FP_age"]],"beta_1")

## Define the model
formula="logit(use,denomb)~(0|cons+age+lc[nokids])"
levID=c('district','woman')
## Change to 5000 iterations by default
estoptions= list(EstM=1)
mymodel3=runMLwiN(formula, levID, D="Binomial", indata, estoptions,MLwiNPath=mlwin)

# 10.2 Random effects logistic regression model . . . . . . . . . . . . .136

## Define the model
formula="logit(use,denomb)~(0|cons+age+lc[nokids])+(2|cons)"
levID=c('district','woman')
estoptions= list(EstM=1)
mymodel4=runMLwiN(formula, levID, D="Binomial", indata, estoptions,MLwiNPath=mlwin)
summary(mymodel4["chains"][["RP2_var_cons"]])
sixway(mymodel4["chains"][["RP2_var_cons"]],"sigma2u0")

# 10.3 Random coefficients for area type . . . . . . . . . . . . . . . . 139

formula="logit(use,denomb)~(0|cons+age+lc[nokids]+urban)+(2|cons)"
levID=c('district','woman')
estoptions= list(EstM=1)
mymodel5=runMLwiN(formula, levID, D="Binomial", indata, estoptions,MLwiNPath=mlwin)

formula="logit(use,denomb)~(0|cons+age+lc[nokids]+urban)+(2|cons+urban)"
levID=c('district','woman')
estoptions= list(EstM=1)
mymodel6=runMLwiN(formula, levID, D="Binomial", indata, estoptions,MLwiNPath=mlwin)

# 10.4 Probit regression . . . . . . . . . . . . . . . . . . . . . . . . 141

# 10.5 Running a probit regression in MLwiN . . . . . . . . . . . . . . .142

formula="probit(use,denomb)~(0|cons+age+lc[nokids]+urban)+(2|cons+urban)"
levID=c('district','woman')

## Gibbs
estoptions= list(EstM=1,mcmcMeth=list(fixM=1,residM=1))
mymodel7=runMLwiN(formula, levID, D="Binomial", indata, estoptions,MLwiNPath=mlwin)

## Univariate MH by default
estoptions= list(EstM=1)
mymodel8=runMLwiN(formula, levID, D="Binomial", indata, estoptions,MLwiNPath=mlwin)

cat("The mean parameter estimates\n")
aa=cbind(mymodel7["FP"],mymodel8["FP"])
ESS.aa=effectiveSize(mymodel7["chains"][,3:12])
bb=cbind(mymodel7["RP"],mymodel8["RP"])
ESS.bb=effectiveSize(mymodel8["chains"][,3:12])
ctable=round(rbind(aa,bb),3)
ctable=cbind(ctable[,1],round(ESS.aa),ctable[,2],round(ESS.bb))
colnames(ctable)=c("Gibbs","ESS(Gibbs)","Metropolis", "ESS(Metropolis)")
print(ctable)

cat("The standard errors of parameter estimates\n")
cc=cbind(sqrt(diag(mymodel7["FP.cov"])),sqrt(diag(mymodel8["FP.cov"])))
dd=cbind(sqrt(diag(mymodel7["RP.cov"])),sqrt(diag(mymodel8["RP.cov"])))
sdtable=round(rbind(cc,dd),3)
colnames(sdtable)=c("Gibbs","Metropolis")
print(sdtable)


# 10.6 Comparison with WinBUGS . . . . . . . . . . . . . . . . . . . . . 144

## Define the model
formula="logit(use,denomb)~(0|cons+age)+(2|cons)"
levID=c('district','woman')
estoptions= list(EstM=0)
mymodel9=runMLwiN(formula, levID, D="Binomial", indata, estoptions,BUGO=c(version=4,n.chains=1,bugs=winbugs, OpenBugs = F),MLwiNPath=mlwin)
summary(mymodel9["chains.bugs"][[1]][,"beta[1]"])
sixway(mymodel9["chains.bugs"][[1]][,"beta[1]"],"beta[1]")

estoptions= list(EstM=1)
mymodel10=runMLwiN(formula, levID, D="Binomial", indata, estoptions,MLwiNPath=mlwin)
summary(mymodel10["chains"][["FP_cons"]])
sixway(mymodel10["chains"][["FP_cons"]],"beta0")

# Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . . .128





############################################################################
