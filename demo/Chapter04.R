############################################################################
#     MLwiN MCMC Manual
#
# 4   Other Features of Variance Components Models . . . . . . . . . . . .45
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

## Gibbs
estoptions= list(EstM=1)
## Fit the model
(mymodel2=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin))

# 4.1 Metropolis Hastings (MH) sampling for the variance components model 46

# 4.2 Metropolis-Hastings settings . . . . . . . . . . . . . . . . . . . .47

# 4.3 Running the variance components with Metropolis Hastings . . . . . .48

## MH Adaptive with defaults
estoptions= list(EstM=1,mcmcMeth=list(fixM=2,residM=2,Lev1VarM=2))
## Fit the model
(mymodel3=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin))
sixway(mymodel3["chains"][,"FP_standlrt"],"beta_1")

## MH Scale Factor =5.8
estoptions= list(EstM=1,mcmcMeth=list(fixM=2,residM=2,Lev1VarM=2, adaption=0))
## Fit the model
(mymodel4=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin))

aa=cbind(mymodel1["FP"],mymodel2["FP"],mymodel4["FP"],mymodel3["FP"])
bb=cbind(mymodel1["RP"],mymodel2["RP"],mymodel4["RP"],mymodel3["RP"])
ctable=round(rbind(aa,bb),3)
colnames(ctable)=c("IGLS","Gibbs", "MH", "MH Adaptive")
print(ctable)
rm(list=c("mymodel1","mymodel2","mymodel3","mymodel4"))
# 4.4 MH cycles per Gibbs iteration . . . . . . . . . . . . . . . . . . . 49

# 4.5 Block updating MH sampling . . . . . . . . . . . . . . . . . . . . .49
## MH Adaptive with defaults
estoptions= list(EstM=1,mcmcMeth=list(fixM=3,residM=2,Lev1VarM=2, rate=40))
## Fit the model
(mymodel5=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin))

estimates=mymodel5["chains"]
par(mfrow=c(3,2))
plot(4951:nrow(estimates),estimates[4951:nrow(estimates),"deviance"],xlab="iteration",
ylab=expression(paste("Est. of deviance")),type="l")
plot(4951:nrow(estimates),estimates[4951:nrow(estimates),"FP_cons"],xlab="iteration",
ylab=expression(paste("Est. of ",beta[0])),type="l")
plot(4951:nrow(estimates),estimates[4951:nrow(estimates),"FP_standlrt"],xlab="iteration",
ylab=expression(paste("Est. of ",beta[1])),type="l")
plot(4951:nrow(estimates),estimates[4951:nrow(estimates),"RP2_var_cons"],xlab="iteration",
ylab=expression(paste("Est. of ",sigma[u0]^2)),type="l")
plot(4951:nrow(estimates),estimates[4951:nrow(estimates),"RP1_var_cons"],xlab="iteration",
ylab=expression(paste("Est. of ",sigma[e0]^2)),type="l")
rm(mymodel5)

# 4.6 Residuals in MCMC . . . . . . . . . . . . . . . . . . . . . . . . . 51

estoptions= list(EstM=1,resi.store=TRUE,resi.store.levs=2,mcmcMeth=list(iterations=5001))
(mymodel6=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin))

lencateg = length(unique(indata[["school"]]))
resi.chain2 = mymodel6["resi.chains"][,1]
resi.chain2 = matrix(resi.chain2, nrow =lencateg)
sixway(resi.chain2[1,],name="u0_1")

# 4.7 Comparing two schools . . . . . . . . . . . . . . . . . . . . . . . 54

dif=resi.chain2[1,]-resi.chain2[2,]
sixway(dif,name="dif")
prop = (dif>0)
mean(prop)

# 4.8 Calculating ranks of schools . . . . . . . . . . . . . . . . . . . .55

u0rank = apply(resi.chain2,2,rank)
u0rankmn = apply(u0rank, 1,mean)
u0ranklo = apply(u0rank, 1, function(x) quantile(x,.025))
u0rankmd = apply(u0rank, 1,median)
u0rankhi = apply(u0rank, 1, function(x) quantile(x,.975))

plot(1:65,u0rankmd,ylim=c(0.5,65.5),pch=15, xlab="School",ylab="Rank")
points(1:65,u0ranklo,pch=24,bg="grey")
points(1:65,u0rankhi,pch=25,bg="grey")
for(i in 1:65) lines(rep(i,2),c(u0ranklo[i],u0rankhi[i]))

## common caterpillar plot

rankno = order(u0rankmn)
plot(1:65,u0rankmn[rankno],ylim=c(0.5,65.5),pch=15, xlab="School",ylab="Rank")
points(1:65,u0ranklo[rankno],pch=24,bg="grey")
points(1:65,u0rankhi[rankno],pch=25,bg="grey")
for(i in 1:65) {lines(rep(i,2),c(u0ranklo[rankno[i]],u0rankhi[rankno[i]]))}

## Alternatively
caterpillarR(mymodel6["residual"], lev=2)

# 4.9 Estimating a function of parameters . . . . . . . . . . . . . . . . 58
estimates=mymodel6["chains"]
isc = estimates[,"RP2_var_cons"]/(estimates[,"RP2_var_cons"]+estimates[,"RP1_var_cons"])
summary(isc)
sixway(isc,"isc")
rm(mymodel6)

# Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . . . 60





############################################################################
