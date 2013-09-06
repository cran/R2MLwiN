############################################################################
#     MLwiN MCMC Manual
#
# 16  Multiple Membership Models . . . . . . . . . . . . . . . . . . . . 231
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

# 16.1 Notation and weightings . . . . . . . . . . . . . . . . . . . . . 232

# 16.2 Office workers salary dataset . . . . . . . . . . . . . . . . . . 232

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

## Read wage1 data from runmlwin (Leckie&Charlton, 2011) data folder
library(foreign); indata =read.dta("http://www.bristol.ac.uk/cmm/media/runmlwin/wage1.dta")

## Alternatively converts wage1.ws under mlwin sample folder to wage1.dta
## MLwiN sample worksheet folder
#wsfile=paste(mlwin,"/samples/wage1.ws",sep="")
## the tutorial.dta will be save under the temporary folder
#inputfile=paste(tempdir(),"/wage1.dta",sep="")
#ws2foreign(wsfile, foreignfile=inputfile, MLwiNPath=mlwin)
#library(foreign); indata =read.dta(inputfile)
#names(indata)=gsub("-","_",names(indata))

summary(indata)
hist(indata[["earnings"]])
hist(indata[["logearn"]],breaks=20)

formula="logearn~(0|cons+age_40+numjobs)+(1|cons)"
levID='id'
estoptions= list(EstM=1)
(mymodel=runMLwiN(formula, levID, D='Normal', indata, estoptions,MLwiNPath=mlwin))

formula="logearn~(0|cons+age_40+numjobs+sex+parttime)+(1|cons)"
levID='id'
estoptions= list(EstM=1)
(mymodel=runMLwiN(formula, levID, D='Normal', indata, estoptions,MLwiNPath=mlwin))
round(cor(indata[,c("parttime","sex","numjobs")]),4)

# 16.4 Fitting multiple membership models to the dataset . . . . . . . . 237

tabulate(indata[["numjobs"]])

formula="logearn~(0|cons+age_40+sex+parttime)+(2|cons)+(1|cons)"
levID=c('company','id')
estoptions= list(EstM=1)
(mymodel=runMLwiN(formula, levID, D='Normal', indata, estoptions,MLwiNPath=mlwin))

##Multiple membership
xclass=list("class"=2,"N1"=4,"weight"='weight1',"id"=NA)
estoptions= list(EstM=1,xclass=xclass,notation='class',resi.store=T,resi.store.levs=2)
(mymodel=runMLwiN(formula, levID, D='Normal', indata, estoptions,MLwiNPath=mlwin))

# 16.5 Residuals in multiple membership models . . . . . . . . . . . . . 240

lencateg = length(unique(indata[["company"]]))
resi0=na.omit(mymodel["resi.chains"][,"resi_lev2"])
resi0=matrix(resi0, nrow =lencateg)
resi0mean = apply(resi0,1,mean)
resi0sd = apply(resi0,1,sd)

rankno=order(resi0mean)
resi0.lo=resi0mean-1.4*resi0sd
resi0.hi=resi0mean+1.4*resi0sd
caterpillar(y=resi0mean[rankno],x=1:length(resi0mean),qtlow=resi0.lo[rankno],qtup=resi0.hi[rankno],ylim=c(-1,1.3))
abline(h=0,lty="dotted")

aa=qqnorm(resi0mean,plot.it=F)
plot(x=aa$x[rankno],y=resi0mean[rankno],pch=24,bg="black",xlab="nscore",ylab="cons")
abline(h=0,lty="dotted")

indata[["companyno54"]]=(indata[["company"]]==54)+(indata[["company2"]]==54)+(indata[["company3"]]==54)+(indata[["company4"]]==54)
indata[["companyno67"]]=(indata[["company"]]==67)+(indata[["company2"]]==67)+(indata[["company3"]]==67)+(indata[["company4"]]==67)

##New model
formula="logearn~(0|cons+age_40+sex+parttime+companyno54+companyno67)+(2|cons)+(1|cons)"
levID=c('company','id')

##Multiple membership
xclass=list("class"=2,"N1"=4,"weight"='weight1',"id"=NA)
estoptions= list(EstM=1,xclass=xclass,notation='class')
(mymodel=runMLwiN(formula, levID, D='Normal', indata, estoptions,MLwiNPath=mlwin))

#  16.6 Alternative weights for multiple membership models . . . . . . . .243


## New weights
xclass=list("class"=2,"N1"=4,"weight"='ew1',"id"=NA)
estoptions= list(EstM=1,xclass=xclass,notation='class')
(mymodel=runMLwiN(formula, levID, D='Normal', indata, estoptions,MLwiNPath=mlwin))

# 16.7 Multiple membership multiple classification (MMMC) models . . . . 244

# Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . . .128





############################################################################
