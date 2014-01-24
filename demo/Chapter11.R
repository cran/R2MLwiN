############################################################################
#     MLwiN MCMC Manual
#
# 11  Poisson Response Modelling . . . . . . . . . . . . . . . . . . . . 153
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
    mlwin=scan(what=character(0),sep ="\n")
    mlwin=gsub("\\", "/",mlwin, fixed=TRUE)
}

# User's input if necessary


# MLwiN sample worksheet folder
wsfile=paste(mlwin,"/samples/mmmec1.ws",sep="")
# the tutorial.dta will be save under the temporary folder
inputfile=paste(tempdir(),"/mmmec1.dta",sep="")
ws2foreign(wsfile, foreignfile=inputfile, MLwiNPath=mlwin)
library(foreign); indata =read.dta(inputfile)
indata[["logexp"]]=double2singlePrecision(log(indata[["exp"]]))
levels(indata[["nation"]])=c("Belgium", "W_Germany", "Denmark", "France", "UK", "Italy", "Ireland", "Luxembourg", "Netherlands")

# 11.1 Simple Poisson regression model . . . . . . . . . . . . . . . . . 155

## Define the model
formula="log(obs,logexp)~(0|cons+uvbi)"
levID=c('nation','region','county')
## Choose option(s) for inference
estoptions= list(EstM=1,mcmcMeth=list(iterations=50000))
## Fit the model
(mymodel1=runMLwiN(formula, levID, D="Poisson", indata, estoptions,MLwiNPath=mlwin))
summary(mymodel1["chains"][,"FP_uvbi"])
sixway(mymodel1["chains"][,"FP_uvbi"],"beta_1")

# 11.2 Adding in region level random effects . . . . . . . . . . . . . . 157

## Define the model
formula="log(obs,logexp)~(0|cons+uvbi)+(2|cons)"
levID=c('region','county')
## Choose option(s) for inference
estoptions= list(EstM=1,mcmcMeth=list(iterations=50000,seed=13))
## Fit the model
(mymodel2=runMLwiN(formula, levID, D="Poisson", indata, estoptions,MLwiNPath=mlwin))
summary(mymodel2["chains"][,"FP_uvbi"])
sixway(mymodel2["chains"][,"FP_uvbi"],"beta_1")

# 11.3 Including nation effects in the model . . . . . . . . . . . . . . 159

## Define the model
formula="log(obs,logexp)~(0|cons+uvbi)+(2|cons)+(3|cons)"
levID=c('nation','region','county')
## Choose option(s) for inference
estoptions= list(EstM=1,mcmcMeth=list(iterations=50000,seed=13))
## Fit the model
(mymodel3=runMLwiN(formula, levID, D="Poisson", indata, estoptions,MLwiNPath=mlwin))

## Define the model
formula="log(obs,logexp)~(0|uvbi+nation[])+(2|cons)"
levID=c('nation','region','county')
## Choose option(s) for inference
estoptions= list(EstM=1,mcmcMeth=list(iterations=50000))
## Fit the model
(mymodel4=runMLwiN(formula, levID, D="Poisson", indata, estoptions,MLwiNPath=mlwin))

# 11.4 Interaction with UV exposure . . . . . . . . . . . . . . . . . . .161

## Define the model
formula="log(obs,logexp)~(0|nation[]+Belgium:uvbi+W_Germany:uvbi+Denmark:uvbi+France:uvbi+UK:uvbi+Italy:uvbi+Ireland:uvbi+Luxembourg:uvbi+Netherlands:uvbi)+(2|cons)"
levID=c('region','county')
## Choose option(s) for inference
estoptions= list(EstM=1,mcmcMeth=list(iterations=50000))
## Fit the model
(mymodel5=runMLwiN(formula, levID, D="Poisson", indata, estoptions,MLwiNPath=mlwin))
sixway(mymodel5["chains"][,"FP_Belgium"],acf.maxlag=5000,"beta_1")

# 11.5 Problems with univariate updating Metropolis procedures . . . . . 163

## NOTE THAT WE RUN 50,000 rather than 500,000 HERE
estoptions= list(EstM=1,mcmcMeth=list(iterations=50000,thinning=10))
## Fit the model
(mymodel6=runMLwiN(formula, levID, D="Poisson", indata, estoptions,MLwiNPath=mlwin))
sixway(mymodel6["chains"][,"FP_Belgium"],"beta_1")

## Half of million interations (could take a few hours to run)
## Increasing memory size of the worksheet
##HOWEVER THE RESULT IS ATTACHED BELOW FOR 500,000
#estoptions= list(EstM=1,mcmcMeth=list(iterations=500000,thinning=10),mem.init=c(2,50000,2500,20,20))
#-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
#MLwiN multilevel model (Poisson)
#Estimation algorithm:  MCMC        Elapsed time : 5232.41s
#Number of obs:  354               Number of iter.: 5e+05
#Bayesian Deviance Information Criterion (DIC)
#Dbar      D(thetabar)    pD      DIC
#2028.126   1964.748   63.379     2091.505
#-----------------------------------------------------------------------------------------------------------------------------------------------------
#The model formula:
#log(obs,logexp)~(0|nation[]+Belgium:uvbi+W_Germany:uvbi+Denmark:uvbi+France:uvbi+UK:uvbi+Italy:uvbi+Ireland:uvbi+Luxembourg:uvbi+Netherlands:uvbi)+(2|cons)
#Level 2: region     Level 1: county
#-----------------------------------------------------------------------------------------------------------------------------------------------------
#The fixed part estimates:
#                            Coef.        Std. Err.            t            p-value       [95% Cred.      Interval]            ESS
#Belgium                   0.72601          0.74086         0.98             0.3271         -0.70808        2.17766         2162.6
#W_Germany                 0.48100          0.12357         3.89          9.915e-05          0.24133        0.72734        13759.1
#Denmark                   0.30241          0.87364         0.35             0.7293         -1.42342        1.99139         1099.5
#France                   -0.59374          0.05492       -10.80          3.296e-27         -0.70235       -0.48629       132202.3
#UK                        0.61846          0.20708         2.99           0.002822          0.21067        1.02434         6840.4
#Italy                     0.28228          0.10549         2.67           0.007486          0.07327        0.48864        38617.2
#Ireland                  -0.49662          1.29626        -0.38             0.7016         -3.02488        2.03626         5117.8
#Luxembourg               18.02608         17.38725         1.04             0.2999        -13.42265       55.35230           82.1
#Netherlands              -0.33234          0.91690        -0.36              0.717         -2.16868        1.45044         1218.3
#Belgium:uvbi              0.27301          0.24976         1.09             0.2743         -0.21050        0.76254         2168.7
#W.Germany:uvbi           -0.01299          0.03300        -0.39             0.6938         -0.07706        0.05240        15212.4
#Denmark:uvbi             -0.08546          0.15423        -0.55             0.5795         -0.38985        0.21233         1097.0
#France:uvbi               0.01306          0.01813         0.72             0.4714         -0.02241        0.04881       146459.0
#UK:uvbi                   0.14281          0.04262         3.35          0.0008066          0.05914        0.22655         7015.9
#Italy:uvbi               -0.08742          0.01588        -5.51          3.626e-08         -0.11840       -0.05604        41203.0
#Ireland:uvbi              0.00746          0.26141         0.03             0.9771         -0.49894        0.52220         5134.2
#Luxembourg:uvbi           7.86721          7.59078         1.04                0.3         -5.82712       24.18936           82.3
#Netherlands:uvbi         -0.10952          0.21999        -0.50             0.6186         -0.54969        0.32003         1224.9
#-----------------------------------------------------------------------------------------------------------------------------------------------------
#The random part estimates at the region level:
#                   Coef.         Std. Err.       [95% Cred.      Interval]           ESS
#var_cons         0.03709           0.00923          0.02229        0.05825       73183.8
#-----------------------------------------------------------------------------------------------------------------------------------------------------
#The random part estimates at the county level:
#                   Coef.         Std. Err.       [95% Cred.      Interval]       ESS
#bcons_1          1.00000           0.00000          1.00000        1.00000       0.0
#-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

# Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . . .128





############################################################################
