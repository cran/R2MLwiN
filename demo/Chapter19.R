############################################################################
#     MLwiN MCMC Manual
#
# 19  Mixed Response Models and Correlated Residuals . . . . . . . . . . 287
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

# 19.1 Mixed response models . . . . . . . . . . . . . . . . . . . . . . 287

# 19.2 The JSP mixed response example . . . . . . . . . . . . . . . . . .289

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

# MLwiN sample worksheet folder
wsfile=paste(mlwin,"/samples/jspmix1.ws",sep="")
# the jspmix1.dta will be save under the temporary folder
inputfile=paste(tempdir(),"/jspmix1.dta",sep="")
ws2foreign(wsfile, foreignfile=inputfile, MLwiNPath=mlwin)
library(foreign); indata =read.dta(inputfile)

tab1=matrix(,3,3)
colnames(tab1)=c("0","1","TOTALS")
rownames(tab1)=c("N","MEANS","SDs")
tab1[1,1:2]=colSums(table(indata[["english"]],indata[["behaviour"]]))
tab1[1,3]=sum(tab1[1,1:2])
tab1[2,1]=mean(indata[["english"]][indata[["behaviour"]]==0])
tab1[2,2]=mean(indata[["english"]][indata[["behaviour"]]==1])
tab1[2,3]=mean(indata[["english"]])
tab1[3,1]=sd(indata[["english"]][indata[["behaviour"]]==0])
tab1[3,2]=sd(indata[["english"]][indata[["behaviour"]]==1])
tab1[3,3]=sd(indata[["english"]])
formatC(tab1)

round(cor(indata[,c("sex","fluent","ravens","english","behaviour")]),4)

# 19.3 Setting up a single level mixed response model . . . . . . . . . .291

formula="c(english,probit(behaviour,denomb))~(0s|cons+sex+ravens)+(0c|fluent{1,0})+(1s|cons.english)"
levID= 'id'
estoptions= list(EstM=1,mcmcMeth=list(fixM=1,residM=1,Lev1VarM=1))
mymodel=runMLwiN(formula, levID, D=c("Mixed","Normal","Binomial"), indata, estoptions,MLwiNPath=mlwin)

# 19.4 Multilevel mixed response model . . . . . . . . . . . . . . . . . 294

formula="c(english,probit(behaviour,denomb))~(0s|cons+sex+ravens)+(0c|fluent{1,0})+(2s|cons)+(1s|cons.english)"
levID=c('school', 'id')
estoptions= list(EstM=1,mcmcMeth=list(fixM=1,residM=1,Lev1VarM=1))
mymodel=runMLwiN(formula, levID, D=c("Mixed","Normal","Binomial"), indata, estoptions,MLwiNPath=mlwin)


# 19.5 Rats dataset . . . . . . . . . . . . . . . . . . . . . . . . . . .295

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
# MLwiN sample worksheet folder
wsfile=paste(mlwin,"/samples/rats.ws",sep="")
# the tutorial.dta will be save under the temporary folder
inputfile=paste(tempdir(),"/rats.dta",sep="")
ws2foreign(wsfile, foreignfile=inputfile, MLwiNPath=mlwin)
library(foreign); indata =read.dta(inputfile)


formula="c(y8,y15,y22,y29,y36)~(0|cons)+(1|cons)"
levID=c('rat')
estoptions= list(EstM=1)
mymodel=runMLwiN(formula, levID, D='Multivariate Normal', indata, estoptions,MLwiNPath=mlwin)

sixway(mymodel["chains"][["RP1_var_cons_y8"]],"sigma2u0")

covM1=matrix(,5,5)
colnames(covM1)=rownames(covM1)=c("cons.y8","cons.y15","cons.y22","cons.y29","cons.y36")
covM1[upper.tri(covM1,diag=T)]= mymodel["RP"]
#covM1[lower.tri(covM1)]=t(covM1)[lower.tri(covM1)]
round(t(covM1),3)
round(cov2cor(t(covM1)),3)

# 19.6 Fitting an autoregressive structure to the variance matrix . . . .298

estoptions= list(EstM=1,mcmcMeth=list(iterations=50000),mcmcOptions=list(mcco=4))
mymodel=runMLwiN(formula, levID, D='Multivariate Normal', indata, estoptions,MLwiNPath=mlwin)

covM2=matrix(,5,5)
colnames(covM2)=rownames(covM2)=c("cons.y8","cons.y15","cons.y22","cons.y29","cons.y36")
covM2[upper.tri(covM2,diag=T)]= mymodel["RP"]
#covM2[lower.tri(covM2)]=t(covM2)[lower.tri(covM2)]
round(t(covM2),3)
round(cov2cor(t(covM2)),3)

# Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . . .128





############################################################################
