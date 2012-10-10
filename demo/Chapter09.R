############################################################################
#     MLwiN MCMC Manual
#
# 9   Modelling Complex Variance at Level 1 / Heteroscedasticity. . . . .111
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
## MLwiN sample worksheet folder
#wsfile=paste(mlwin,"/samples/tutorial.ws",sep="")
## the tutorial.dta will be save under the temporary folder
#inputfile=paste(tempdir(),"/tutorial.dta",sep="")
#ws2foreign(wsfile, foreignfile=inputfile, MLwiNPath=mlwin)
#library(foreign); indata =read.dta(inputfile)

boy.normexam=indata[["normexam"]][which(indata[["girl"]]==0)]
girl.normexam=indata[["normexam"]][which(indata[["girl"]]==1)]
tab1=cbind(c(length(boy.normexam),mean(boy.normexam),sd(boy.normexam)),
c(length(girl.normexam),mean(girl.normexam),sd(girl.normexam)),
c(length(indata[["normexam"]]),mean(indata[["normexam"]]),sd(indata[["normexam"]])))
colnames(tab1)=c("0","1","TOTAL")
rownames(tab1)=c("N","MEANS","SDs")
formatC(round(tab1,6))

c5=indata[["standlrt"]]
intakecat=rep(0,length(c5))
intakecat[which(c5>-1)]=1
intakecat[which(c5>-.5)]=2
intakecat[which(c5>-.1)]=3
intakecat[which(c5>.3)]=4
intakecat[which(c5>.7)]=5
intakecat[which(c5>1.1)]=6
normexam=indata[["normexam"]]
tab2=cbind(c(sum(intakecat==0),mean(normexam[intakecat==0]),sd(normexam[intakecat==0])),
c(sum(intakecat==1),mean(normexam[intakecat==1]),sd(normexam[intakecat==1])),
c(sum(intakecat==2),mean(normexam[intakecat==2]),sd(normexam[intakecat==2])),
c(sum(intakecat==3),mean(normexam[intakecat==3]),sd(normexam[intakecat==3])),
c(sum(intakecat==4),mean(normexam[intakecat==4]),sd(normexam[intakecat==4])),
c(sum(intakecat==5),mean(normexam[intakecat==5]),sd(normexam[intakecat==5])),
c(sum(intakecat==6),mean(normexam[intakecat==6]),sd(normexam[intakecat==6])),
c(length(intakecat),mean(normexam),sd(normexam)))
colnames(tab2)=c("0","1","2","3","4","5","6","TOTAL")
rownames(tab2)=c("N","MEANS","SDs")
formatC(round(tab2,6))

# 9.1 MCMC algorithm for a 1 level Normal model with complex variation . 113

# 9.2 Setting up the model in MLwiN . . . . . . . . . . . . . . . . . . .115

## Define the model
formula="normexam~(0|cons+standlrt)+(1|cons+standlrt)"
## The highest level comes first, then the second highest and so on
levID=c('student')
## Choose option(s) for inference
estoptions= list(EstM=1)
## Fit the model
mymodel1=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin, workdir = tempdir())
trajectories(mymodel1["chains"],Range=c(4501,5000))

l1varfn= mymodel1["RP"]["RP1_var_cons"]+2*mymodel1["RP"]["RP1_cov_cons_standlrt"]*indata[["standlrt"]]+
mymodel1["RP"]["RP1_var_standlrt"]*indata[["standlrt"]]^2
plot(sort(indata[["standlrt"]]),l1varfn[order(indata[["standlrt"]])],xlab="standlrt",ylab="l1varfn",type="l")
abline(v=0,lty="dotted")

# 9.3 Complex variance functions in multilevel models . . . . . . . . . .119

formula="normexam~(0|cons+standlrt)+(1|cons)+(2|cons+standlrt)"
## The highest level comes first, then the second highest and so on
levID=c('school','student')
## Choose option(s) for inference
estoptions= list(EstM=1)
## Fit the model
mymodel2=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin, workdir = tempdir())

l2varfn= mymodel2["RP"]["RP2_var_cons"]+2*mymodel2["RP"]["RP2_cov_cons_standlrt"]*indata[["standlrt"]]+
mymodel2["RP"]["RP2_var_standlrt"]*indata[["standlrt"]]^2
l1varfn=mymodel2["RP"]["RP1_var_cons"]
plot(sort(indata[["standlrt"]]),l2varfn[order(indata[["standlrt"]])],xlab="standlrt",ylab="varfns",ylim=c(0,.6),type="l")
abline(h=l1varfn)
abline(v=0,lty="dotted")

formula="normexam~(0|cons+standlrt)+(1|cons+standlrt)+(2|cons+standlrt)"
levID=c('school','student')
estoptions= list(EstM=1)
## Fit the model
mymodel3=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin, workdir = tempdir())
## Remove term standlrt/standlrt from the level 1 covariance matrix
clre=matrix(,nrow=3,ncol=1)
clre[1,1]=1; clre[2,1]='standlrt'; clre[3,1]='standlrt'
estoptions= list(EstM=1,clre=clre)
## Fit the model
mymodel4=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin, workdir = tempdir())

# 9.4 Relationship with gender . . . . . . . . . . . . . . . . . . . . . 123

formula="normexam~(0|cons+standlrt+girl)+(2|cons+standlrt)+(1|cons+standlrt+girl)"
levID=c('school','student')
## Remove term standlrt/standlrt and girl/girl from the level 1 covariance matrix
clre=matrix(,nrow=3,ncol=2)
clre[1,1]=1; clre[2,1]='standlrt'; clre[3,1]='standlrt'
clre[1,2]=1; clre[2,2]='girl'; clre[3,2]='girl'
estoptions= list(EstM=1,clre=clre)
## Fit the model
mymodel5=runMLwiN(formula, levID, D="Normal", indata, estoptions,MLwiNPath=mlwin, workdir = tempdir())

l2varfn= mymodel5["RP"]["RP2_var_cons"]+2*mymodel5["RP"]["RP2_cov_cons_standlrt"]*indata[["standlrt"]]+
mymodel5["RP"]["RP2_var_standlrt"]*indata[["standlrt"]]^2
l1varfnboys=mymodel5["RP"]["RP1_var_cons"]+2*mymodel5["RP"]["RP1_cov_cons_standlrt"]*indata[["standlrt"]]
l1varfngirls=mymodel5["RP"]["RP1_var_cons"]+2*mymodel5["RP"]["RP1_cov_cons_standlrt"]*indata[["standlrt"]]+
2*mymodel5["RP"]["RP1_cov_cons_girl"]+2*mymodel5["RP"]["RP1_cov_standlrt_girl"]*indata[["standlrt"]]
plot(sort(indata[["standlrt"]]),l2varfn[order(indata[["standlrt"]])],xlab="standlrt",ylab="varfns",ylim=c(0,.8),type="l")
lines(sort(indata[["standlrt"]]),l1varfnboys[order(indata[["standlrt"]])])
lines(sort(indata[["standlrt"]]),l1varfngirls[order(indata[["standlrt"]])])
abline(v=0,lty="dotted")

# 9.5 Alternative log precision formulation . . . . . . . . . . . . . . .126

estoptions= list(EstM=1,clre=clre,mcmcMeth=list(lclo=1))
## Fit the model
mymodel6=runMLwiN(formula, levID, D="Normal", indata, estoptions,MLwiNPath=mlwin, workdir = tempdir())

l2varfn= mymodel6["RP"]["RP2_var_cons"]+2*mymodel6["RP"]["RP2_cov_cons_standlrt"]*indata[["standlrt"]]+
mymodel6["RP"]["RP2_var_standlrt"]*indata[["standlrt"]]^2
l1varfnboys=1/exp(mymodel6["RP"]["RP1_var_cons"]+2*mymodel6["RP"]["RP1_cov_cons_standlrt"]*indata[["standlrt"]])
l1varfngirls=1/exp(mymodel6["RP"]["RP1_var_cons"]+2*mymodel6["RP"]["RP1_cov_cons_standlrt"]*indata[["standlrt"]]+
2*mymodel6["RP"]["RP1_cov_cons_girl"]+2*mymodel6["RP"]["RP1_cov_standlrt_girl"]*indata[["standlrt"]])
plot(sort(indata[["standlrt"]]),l2varfn[order(indata[["standlrt"]])],xlab="standlrt",ylab="varfns",ylim=c(0,.8),type="l")
lines(sort(indata[["standlrt"]]),l1varfnboys[order(indata[["standlrt"]])])
lines(sort(indata[["standlrt"]]),l1varfngirls[order(indata[["standlrt"]])])
abline(v=0,lty="dotted")

# Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . . .128





############################################################################
