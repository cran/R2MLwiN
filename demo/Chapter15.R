############################################################################
#     MLwiN MCMC Manual
#
# 15  Cross Classified Models . . . . . . . . . . . . . . . . . . . . . .215
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

# 15.1 Classifications and levels . . . . . . . . . . . . . . . . . . . .216

# 15.2 Notation . . . . . . . . . . . . . . . . . . . . . . . . . . . . .217

# 15.3 The Fife educational dataset . . . . . . . . . . . . . . . . . . .217

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

# MLwiN sample worksheet folder
wsfile=paste(mlwin,"/samples/xc1.ws",sep="")
# the tutorial.dta will be save under the temporary folder
inputfile=paste(tempdir(),"/xc1.dta",sep="")
ws2foreign(wsfile, foreignfile=inputfile, MLwiNPath=mlwin)
library(foreign); indata =read.dta(inputfile)

formula="ATTAIN~(0|CONS)+(3|CONS)+(1|CONS)"
levID=c('SID','PID','PUPIL')
estoptions= list(EstM=1)
(mymodel=runMLwiN(formula, levID, D='Normal', indata, estoptions,MLwiNPath=mlwin))

# 15.4 A Cross-classified model . . . . . . . . . . . . . . . . . . . . .220

formula="ATTAIN~(0|CONS)+(3|CONS)+(2|CONS)+(1|CONS)"
levID=c('SID','PID','PUPIL')
xclass=list("classes"=c(2,3),"N1"=c(1,1))
estoptions= list(xclass=xclass,EstM=1,notation='class',resi.store=T,resi.store.levs=c(2,3))
(mymodel=runMLwiN(formula, levID, D='Normal', indata, estoptions,MLwiNPath=mlwin))

# 15.5 Residuals . . . . . . . . . . . . . . . . . . . . . . . . . . . . 223

lencateg = length(unique(indata[["SID"]]))
resi.chain0=na.omit(mymodel["resi.chains"][,"resi_lev3"])
resi.chain0=matrix(resi.chain0, nrow =lencateg)
residual0 = apply(resi.chain0,1,mean)
rankno=order(residual0)
plot(x=1:lencateg,y=residual0[rankno],pch=24,bg="black",xlab="rank",ylab="cons")
abline(h=0,lty="dotted")

## Common caterpillar
#lencateg = length(unique(indata[["SID"]]))
#resi.chain0=na.omit(mymodel["resi.chains"][,"resi_lev3"])
#resi.chain0=matrix(resi.chain0, nrow =lencateg)
#u0rank = apply(resi.chain0,2,rank)
#u0rankmn = apply(u0rank, 1,mean)
#u0ranklo = apply(u0rank, 1, function(x) quantile(x,.025))
#u0rankmd = apply(u0rank, 1,median)
#u0rankhi = apply(u0rank, 1, function(x) quantile(x,.975))
#rankno = order(u0rankmn)
#caterpillar(y=u0rankmn[rankno],x=1:lencateg,qtlow=u0ranklo[rankno],qtup=u0rankhi[rankno]],ylim=c(0,20))

lencateg = length(unique(indata[["PID"]]))
resi.chain1=na.omit(mymodel["resi.chains"][,"resi_lev2"])
resi.chain1=matrix(resi.chain1, nrow =lencateg)
residual1 = apply(resi.chain1,1,mean)
rankno=order(residual1)
plot(x=1:length(residual1),y=residual1[rankno],pch=24,bg="black",xlab="rank",ylab="cons")
abline(h=0,lty="dotted")

## Common caterpillar
#lencateg = length(unique(indata[["PID"]]))
#resi.chain1=na.omit(mymodel["resi.chains"][,"resi_lev2"])
#resi.chain1=matrix(resi.chain1, nrow =lencateg)
#u0rank = apply(resi.chain1,2,rank)
#u0rankmn = apply(u0rank, 1,mean)
#u0ranklo = apply(u0rank, 1, function(x) quantile(x,.025))
#u0rankmd = apply(u0rank, 1,median)
#u0rankhi = apply(u0rank, 1, function(x) quantile(x,.975))
#rankno = order(u0rankmn)
#caterpillar(y=u0rankmn[rankno],x=1:lencateg,qtlow=u0ranklo[rankno],qtup=u0rankhi[rankno],ylim=c(0,150))

# 15.6 Adding predictors to the model . . . . . . . . . . . . . . . . . .225

formula="ATTAIN~(0|CONS+VRQ)+(3|CONS)+(2|CONS)+(1|CONS)"
levID=c('SID','PID','PUPIL')
xclass=list("classes"=c(2,3),"N1"=c(1,1))
estoptions= list(xclass=xclass,EstM=1,notation='class',resi.store=T,resi.store.levs=c(2,3))
(mymodel=runMLwiN(formula, levID, D='Normal', indata, estoptions,MLwiNPath=mlwin))

formula="ATTAIN~(0|CONS+VRQ+SC+FED+MED+CHOICE)+(3|CONS)+(2|CONS)+(1|CONS)"
levID=c('SID','PID','PUPIL')
xclass=list("classes"=c(2,3),"N1"=c(1,1))
estoptions= list(xclass=xclass,EstM=1,notation='class',resi.store=T,resi.store.levs=c(2,3))
(mymodel=runMLwiN(formula, levID, D='Normal', indata, estoptions,MLwiNPath=mlwin))

lencateg = length(unique(indata[["SID"]]))
resi.chain0=na.omit(mymodel["resi.chains"][,"resi_lev3"])
resi.chain0=matrix(resi.chain0, nrow =lencateg)
residual0 = apply(resi.chain0,1,mean)
rankno=order(residual0)
plot(x=1:lencateg,y=residual0[rankno],pch=24,bg="black",xlab="rank",ylab="cons")
abline(h=0,lty="dotted")

indata[["school19"]]=as.integer(indata[["SID"]]==19)
formula="ATTAIN~(0|CONS+VRQ+SC+FED+MED+CHOICE+school19)+(2|CONS)+(1|CONS)"
levID=c('SID','PID','PUPIL')
xclass=list("classes"=c(2,3),"N1"=c(1,1))
estoptions= list(xclass=xclass,EstM=1,notation='class')
(mymodel=runMLwiN(formula, levID, D='Normal', indata, estoptions,MLwiNPath=mlwin))

# 15.7 Current restrictions for cross-classified models . . . . . . . . .229

# Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . . .128





############################################################################
