############################################################################
#     MLwiN MCMC Manual
#
# 13  Ordered Categorical Responses . . . . . . . . . . . . . . . . . . .181
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

# 13.1 A level chemistry dataset . . . . . . . . . . . . . . . . . . . . 181

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

hist(indata[["gcseav"]],breaks=20)

# 13.2 Normal response models . . . . . . . . . . . . . . . . . . . . . .184

## Define the model
formula="a_point ~ (0|cons)+(1|cons )"
levID='pupil'
estoptions= list(EstM=1)
## Fit the model
(mymodel=runMLwiN(formula, levID, D='Normal', indata, estoptions,MLwiNPath=mlwin))

## Define the model
formula="a_point ~ (0|cons+gcseav+gcse2+gcse3+gender)+(1|cons )"
levID='pupil'
estoptions= list(EstM=1, resi.store=T)
## Fit the model
(mymodel=runMLwiN(formula, levID, D='Normal', indata, estoptions,MLwiNPath=mlwin))

resi=mymodel["residual"]
FP=mymodel["FP"]
predCurves(mymodel, indata, xname="gcseav", group="gender")

# 13.3 Ordered multinomial modelling . . . . . . . . . . . . . . . . . . 186

##Define the model
formula="logit(a_point,cons,A) ~ (0s|cons)"
levID=c('pupil')
##IGLS
estoptions= list(EstM=0)
## Fit the model
(mymodel=runMLwiN(formula, levID, D='Ordered Multinomial', indata, estoptions,MLwiNPath=mlwin))

##MCMC
estoptions= list(EstM=1)
## Fit the model
(mymodel=runMLwiN(formula, levID, D='Ordered Multinomial', indata, estoptions,MLwiNPath=mlwin))

# 13.4 Adding predictor variables . . . . . . . . . . . . . . . . . . . .191
formula="logit(a_point,cons,A) ~ (0s|cons)+(0c|gcseav+gcse2+gcse3+gender)"
levID=c('pupil')
##MCMC
estoptions= list(EstM=1)
## Fit the model
(mymodel=runMLwiN(formula, levID, D='Ordered Multinomial', indata, estoptions,MLwiNPath=mlwin))

# 13.5 Multilevel ordered response modelling . . . . . . . . . . . . . . 192
formula="logit(a_point,cons,A) ~ (0s|cons)+(0c|gcseav+gcse2+gender) +( 2c | cons)"
levID=c('estab','pupil')
##MCMC
estoptions= list(EstM=1)
## Fit the model
(mymodel=runMLwiN(formula, levID, D='Ordered Multinomial', indata, estoptions,MLwiNPath=mlwin))


formula="logit(a_point,cons,A) ~ (0s|cons)+(0c|gcseav+gcse2+gender) +( 2c | cons+gcseav )"
levID=c('estab','pupil')
##MCMC
estoptions= list(EstM=1)
## Fit the model
(mymodel=runMLwiN(formula, levID, D='Ordered Multinomial', indata, estoptions,MLwiNPath=mlwin))
sixway(mymodel["chains"][,"RP2_var_cons_12345"],acf.maxlag = 300,"sigma2v6")

##Increases iterations to 50,000
estoptions= list(EstM=1,mcmcMeth=list(iterations=50000))
## Fit the model
(mymodel=runMLwiN(formula, levID, D='Ordered Multinomial', indata, estoptions,MLwiNPath=mlwin))
sixway(mymodel["chains"][,"RP2_var_cons_12345"],acf.maxlag = 300,"sigma2v6")

# Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . . .128





############################################################################
