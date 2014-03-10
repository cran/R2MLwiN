############################################################################
#     MLwiN MCMC Manual
#
# 12  Unordered Categorical Responses . . . . . . . . . . . . . . . . . .167
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
if(!exists("mlwin")) mlwin ="C:/Program Files (x86)/MLwiN v2.30/"
while (!file.access(mlwin,mode=0)==0||!file.access(mlwin,mode=1)==0||!file.access(mlwin,mode=4)==0){
    cat("Please specify the MLwiN folder including the MLwiN executable:\n")
    mlwin=scan(what=character(0),sep ="\n")
    mlwin=gsub("\\", "/",mlwin, fixed=TRUE)
}

# User's input if necessary


## Read bang data from runmlwin (Leckie&Charlton, 2011) data folder
library(foreign); indata =read.dta("http://www.bristol.ac.uk/cmm/media/runmlwin/bang.dta")

## Alternatively converts bang.ws under mlwin sample folder to bang.dta
## MLwiN sample worksheet folder
#wsfile=paste(mlwin,"/samples/bang.ws",sep="")
## the tutorial.dta will be save under the temporary folder
#inputfile=paste(tempdir(),"/bang.dta",sep="")
#ws2foreign(wsfile, foreignfile=inputfile, MLwiNPath=mlwin)
#library(foreign); indata =read.dta(inputfile)

# 12.1 Fitting a first single-level multinomial model . . . . . . . . . .169

## Define the model
formula="log(use4,cons,use4_4)~ (0|cons)"
levID=c('woman')
estoptions= list(EstM=1)
## Fit the model
(mymodel=runMLwiN(formula, levID, D='Unordered Multinomial', indata, estoptions,MLwiNPath=mlwin))

cat(paste("Pr(y = 1) =", round(exp(mymodel["FP"]["FP_cons_use4_1"])/(1+exp(mymodel["FP"]["FP_cons_use4_1"])+exp(mymodel["FP"]["FP_cons_use4_2"])+exp(mymodel["FP"]["FP_cons_use4_3"])),4),"\n"))
cat(paste("Pr(y = 2) =", round(exp(mymodel["FP"]["FP_cons_use4_2"])/(1+exp(mymodel["FP"]["FP_cons_use4_1"])+exp(mymodel["FP"]["FP_cons_use4_2"])+exp(mymodel["FP"]["FP_cons_use4_3"])),4),"\n"))
cat(paste("Pr(y = 3) =", round(exp(mymodel["FP"]["FP_cons_use4_3"])/(1+exp(mymodel["FP"]["FP_cons_use4_1"])+exp(mymodel["FP"]["FP_cons_use4_2"])+exp(mymodel["FP"]["FP_cons_use4_3"])),4),"\n"))

# 12.2 Adding predictor variables . . . . . . . . . . . . . . . . . . . .173

## Define the model
formula="log(use4,cons,use4_4)~ (0|cons+lc[lc0])"
levID=c('woman')
estoptions= list(EstM=1)
## Fit the model
(mymodel=runMLwiN(formula, levID, D='Unordered Multinomial', indata, estoptions,MLwiNPath=mlwin))

cat(paste("Pr(y = 3) =", round(exp(mymodel["FP"]["FP_cons_use4_3"])/(1+exp(mymodel["FP"]["FP_cons_use4_1"])+exp(mymodel["FP"]["FP_cons_use4_2"])+exp(mymodel["FP"]["FP_cons_use4_3"])),4),"\n"))
cat(paste("Pr(y = 3) =", round(exp(mymodel["FP"]["FP_cons_use4_3"]+mymodel["FP"]["FP_lc2_use4_3"])/(1+exp(mymodel["FP"]["FP_cons_use4_1"]+mymodel["FP"]["FP_lc2_use4_1"])+
exp(mymodel["FP"]["FP_cons_use4_2"]+mymodel["FP"]["FP_lc2_use4_2"])+exp(mymodel["FP"]["FP_cons_use4_3"]+mymodel["FP"]["FP_lc2_use4_3"])),4),"\n"))

# 12.3 Interval estimates for conditional probabilities . . . . . . . . .175

chains=mymodel["chains"]
pred1=exp(chains[,"FP_cons_use4_3"])/(1+exp(chains[,"FP_cons_use4_1"])+exp(chains[,"FP_cons_use4_2"])+exp(chains[,"FP_cons_use4_3"]))
summary(pred1)
sixway(pred1,"prob1")

pred2=exp(chains[,"FP_cons_use4_3"]+chains[,"FP_lc2_use4_3"])/(1+exp(chains[,"FP_cons_use4_1"]+chains[,"FP_lc2_use4_1"])+
exp(chains[,"FP_cons_use4_2"]+chains[,"FP_lc2_use4_2"])+exp(chains[,"FP_cons_use4_3"]+chains[,"FP_lc2_use4_3"]))
summary(pred2)
sixway(pred2,"prob1")

# 12.4 Adding district level random effects . . . . . . . . . . . . . . .177

## Define the model
formula="log(use4,cons,use4_4)~(0|cons+lc[lc0])+(2|cons)"
levID=c('district','woman')

#Uses IGLS
estoptions= list(EstM=0, nonlinear=c(1,2))
## Fit the model
(mymodel=runMLwiN(formula, levID, D='Unordered Multinomial', indata, estoptions,MLwiNPath=mlwin))

## Uses MCMC
estoptions= list(EstM=1, nonlinear=c(1,2))
## Fit the model
(mymodel=runMLwiN(formula, levID, D='Unordered Multinomial', indata, estoptions,MLwiNPath=mlwin))
sixway(mymodel["chains"][,"RP2_var_cons_use4_1"],"sigma2v0")

RP3.cons=matrix(,3,3)
RP3.cons[upper.tri(RP3.cons,diag=T)]=mymodel["RP"][1:6]
RP3.cons[lower.tri(RP3.cons)]=RP3.cons[upper.tri(RP3.cons)]
round(cov2cor(RP3.cons),3)

# Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . . .128





############################################################################
