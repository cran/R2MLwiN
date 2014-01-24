############################################################################
#     MLwiN MCMC Manual
#
# 7   Using the WinBUGS Interface in MLwiN . . . . . . . . . . . . . . . .83
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

# 7.1 Variance components models in WinBUGS . . . . . . . . . . . . . . . 84

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

## winbugs executable
if(!exists("winbugs")) winbugs="C:/Program Files (x86)/WinBUGS14/WinBUGS14.exe"
while (!file.access(winbugs,mode=0)==0||!file.access(winbugs,mode=1)==0||!file.access(winbugs,mode=4)==0){
    cat("Please specify the path for the WinBUGS executable:\n")
    winbugs=scan(what=character(0),sep ="\n")
    winbugs=gsub("\\", "/",winbugs, fixed=TRUE)
}

# User's input if necessary

## Openbugs executable
if(!exists("openbugs")) openbugs="C:/Program Files (x86)/OpenBUGS321/OpenBUGS.exe"
while (!file.access(openbugs,mode=0)==0||!file.access(openbugs,mode=1)==0||!file.access(openbugs,mode=4)==0){
    cat("Please specify the path for the OpenBUGS executable:\n")
    openbugs=scan(what=character(0),sep ="\n")
    openbugs=gsub("\\", "/",openbugs, fixed=TRUE)
}

## Define the model
formula="normexam~(0|cons+standlrt)+(2|cons)+(1|cons)"
## The highest level comes first, then the second highest and so on
levID=c('school','student')

## Uses the results from IGLS to create initial values for bugs
estoptions= list(EstM=0, show.file=T)
## Fit the model by calling winbugs using the rbugs package
mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions,BUGO=c(version=4,n.chains=1,debug=F,seed=1,bugs=winbugs, OpenBugs = F), MLwiNPath=mlwin, workdir = tempdir())

## Alternatively uses openbugs
## Uses the results from IGLS to create initial values for bugs
estoptions= list(EstM=0, show.file=T)
## Fit the model by calling openbugs using the rbugs package
mymodel1=runMLwiN(formula, levID, D="Normal", indata, estoptions,BUGO=c(version=4,n.chains=1,debug=F,seed=1,bugs=openbugs, OpenBugs = T), MLwiNPath=mlwin, workdir = tempdir())
summary(mymodel1[[1]][,"beta[2]"])
sixway(mymodel1[[1]][,"beta[2]"])

# 7.2 So why have a WinBUGS interface ? . . . . . . . . . . . . . . . . . 92
# 7.3 t distributed school residuals . . . . . . . . . . . . . . . . . . .92

## Download the model, initial, data files
download.file("http://www.bristol.ac.uk/cmm/media/runmlwin/tutorial1_model.txt", paste(tempdir(),"/tutorial1_model.txt",sep=""), method="auto")
file.show(paste(tempdir(),"/tutorial1_model.txt",sep="")); modelfile=paste(tempdir(),"/tutorial1_model.txt",sep="")
download.file("http://www.bristol.ac.uk/cmm/media/runmlwin/tutorial1_inits.txt", paste(tempdir(),"/tutorial1_inits.txt",sep=""), method="auto")
file.show(paste(tempdir(),"/tutorial1_inits.txt",sep="")); initfile=paste(tempdir(),"/tutorial1_inits.txt",sep="")
download.file("http://www.bristol.ac.uk/cmm/media/runmlwin/tutorial1_data.txt", paste(tempdir(),"/tutorial1_data.txt",sep=""), method="auto")
datafile=paste(tempdir(),"/tutorial1_data.txt",sep="")
bugEst=paste(tempdir(),"/tutorial1_log.txt",sep="")

chains.bugs1=mlwin2bugs(D="t",levID, datafile, initfile, modelfile, bugEst, fact=NULL, addmore=NULL, n.chains = 1, n.iter = 5500, n.burnin=500, n.thin=1, debug=T, bugs=winbugs,
        bugsWorkingDir=tempdir(), OpenBugs = F)
## Close winbugs manually
summary(chains.bugs1)
sixway(chains.bugs1[[1]][,"df"],"df")

chains.bugs2=mlwin2bugs(D="t",levID, datafile, initfile, modelfile, bugEst, fact=NULL, addmore=NULL, n.chains = 1, n.iter = 12000, n.burnin=2000, n.thin=1, debug=T, bugs=winbugs,
        bugsWorkingDir=tempdir(), OpenBugs = F)
## Close winbugs manually
summary(chains.bugs2)
sixway(chains.bugs2[[1]][,"df"],"df")

# Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . . . 96





############################################################################
