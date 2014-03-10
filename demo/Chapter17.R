############################################################################
#     MLwiN MCMC Manual
#
# 17  Modelling Spatial Data . . . . . . . . . . . . . . . . . . . . . . 247
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

# 17.1 Scottish lip cancer dataset . . . . . . . . . . . . . . . . . . . 247

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

# MLwiN sample worksheet folder
wsfile=paste(mlwin,"/samples/lips1.ws",sep="")
# the tutorial.dta will be save under the temporary folder
inputfile=paste(tempdir(),"/lips1.dta",sep="")
ws2foreign(wsfile, foreignfile=inputfile, MLwiNPath=mlwin)
library(foreign); indata =read.dta(inputfile)
summary(indata)

# 17.2 Fixed effects models . . . . . . . . . . . . . . . . . . . . . . .248

formula="log(obs,offs)~(0|cons)"
levID=c('neigh1','area','area')
estoptions= list(EstM=1,notation="class")
(mymodel=runMLwiN(formula, levID, D='Poisson', indata, estoptions,MLwiNPath=mlwin))

formula="log(obs,offs)~(0|cons+perc_aff)"
levID=c('neigh1','area','area')
estoptions= list(EstM=1,notation="class")
(mymodel=runMLwiN(formula, levID, D='Poisson', indata, estoptions,MLwiNPath=mlwin))

# 17.3 Random effects models . . . . . . . . . . . . . . . . . . . . . . 251

formula="log(obs,offs)~(0|cons+perc_aff)+(2|cons)"
levID=c('neigh1','area','area')
estoptions= list(EstM=1,notation="class",mcmcMeth=list(iterations=50000))
(mymodel=runMLwiN(formula, levID, D='Poisson', indata, estoptions,MLwiNPath=mlwin))

# 17.4 A spatial multiple-membership (MM) model . . . . . . . . . . . . .252

formula="log(obs,offs)~(0|cons+perc_aff)+(2|cons)+(3|cons)"
levID=c('neigh1','area','area')
xclass=list("class"=3,"N1"=11,"weight"='weight1',"id"=NA)
estoptions= list(xclass=xclass,EstM=1,notation="class",mcmcMeth=list(iterations=50000))
(mymodel=runMLwiN(formula, levID, D='Poisson', indata, estoptions,MLwiNPath=mlwin))

# 17.5 Other spatial models . . . . . . . . . . . . . . . . . . . . . . .255

# 17.6 Fitting a CAR model in MLwiN . . . . . . . . . . . . . . . . . . .255

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

formula="log(obs,offs)~(0|perc_aff)+(3|cons)"
levID=c('area','area','area')
xclass=list("class"=3,"N1"=11,"weight"='wcar1',"id"='neigh1',"car"=TRUE)
estoptions= list(xclass=xclass,EstM=1,notation="class",mcmcMeth=list(iterations=50000))
(mymodel=runMLwiN(formula, levID, D='Poisson', indata, estoptions, BUGO=c(version=4,n.chains=1,bugs=winbugs, OpenBugs = FALSE), MLwiNPath=mlwin))


# 17.7 Including exchangeable random effects . . . . . . . . . . . . . . 259

formula="log(obs,offs)~(0|perc_aff)+(2|cons)+(3|cons)"
levID=c('area','area','area')
xclass=list("class"=3,"N1"=11,"weight"='wcar1',"id"='neigh1',"car"=TRUE)
estoptions= list(xclass=xclass,EstM=1,notation="class",mcmcMeth=list(iterations=50000))
(mymodel=runMLwiN(formula, levID, D='Poisson', indata, estoptions, MLwiNPath=mlwin))

# 17.8 Further reading on spatial modelling . . . . . . . . . . . . . . .260

# Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . . .128





############################################################################
