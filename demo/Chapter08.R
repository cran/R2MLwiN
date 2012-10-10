############################################################################
#     MLwiN MCMC Manual
#
# 8   Running a Simulation Study in MLwiN . . . . . . . . . . . . . . . . 97
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

# 8.1 JSP dataset simulation study . . . . . . . . . . . . . . . . . . . .97

# 8.2 Setting up the structure of the dataset . . . . . . . . . . . . . . 98

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

set.seed(1)
pupil=1:108
school=c( rep(1,18),rep(2,18),rep(3,18),rep(4,18),rep(5,18),rep(6,18))
cons=rep(1,108)
u=rnorm(108,0,sqrt(10))
e=rnorm(108,0,sqrt(40))
resp=30*cons+u+e
indata=as.data.frame(cbind(pupil,school,cons,resp))
indata=double2singlePrecision(indata)
levID=c('school','pupil')
formula="resp~(0|cons)+(2|cons)+(1|cons)"
estoptions= list(EstM=1)
mymodel=runMLwiN(formula, levID, D='Normal', indata, estoptions,MLwiNPath=mlwin)

# 8.3 Generating simulated datasets based on true values . . . . . . . . 102

# 8.4 Fitting the model to the simulated datasets . . . . . . . . . . . .106

## Note: To replicate the analysis presented in the manual as cloesly as
## possible, increase the maximum number of iterations from 10 to 100.

ns=10
aa=cc=array(,c(9,5,ns))
for (ii in 1:ns){
    u=rnorm(108,0,sqrt(10))
    e=rnorm(108,0,sqrt(40))
    resp=double2singlePrecision(30*cons+u+e)
    indata=data.frame(cbind(pupil,school,cons,resp))
    estoptions= list(EstM=0)
    mymodel0=runMLwiN(formula, levID, D='Normal', indata, estoptions,MLwiNPath=mlwin)
    cc[,,ii]=as.matrix(mymodel0["estIGLS"])
    estoptions= list(EstM=1)
    mymodel=runMLwiN(formula, levID, D='Normal', indata, estoptions,MLwiNPath=mlwin)
    aa[,,ii]=as.matrix(mymodel["estMCMC"])
}
bb=sapply(1:ns,function(x)  na.omit(stack(as.data.frame(aa[,,x])))$values)
qbeta0.mcmc=quantile(bb[1,],probs=c(.025,.5,.975))
qu0.mcmc=quantile(bb[3,],probs=c(.025,.5,.975))
qe0.mcmc=quantile(bb[4,],probs=c(.025,.5,.975))
qDev.mcmc=quantile(bb[10,],probs=c(.025,.5,.975))

dd=sapply(1:ns,function(x)  na.omit(stack(as.data.frame(cc[,,x])))$values)
qbeta0.igls=quantile(dd[1,],probs=c(.025,.5,.975))
qu0.igls=quantile(dd[3,],probs=c(.025,.5,.975))
qe0.igls=quantile(dd[4,],probs=c(.025,.5,.975))
qDev.igls=quantile(dd[10,],probs=c(.025,.5,.975))

qigls=rbind(qbeta0.igls,qu0.igls,qe0.igls)
rownames(qigls)=c("beta0","u0","e0"); colnames(qigls)=c("2.5%","50%","97.5%")
qmcmc=rbind(qbeta0.mcmc,qu0.mcmc,qe0.mcmc)
rownames(qmcmc)=c("beta0","u0","e0"); colnames(qmcmc)=c("2.5%","50%","97.5%")

# 8.5 Analysing the simulation results . . . . . . . . . . . . . . . . . 109
cat("quantiles of estimates using MCMC\n")
qmcmc
cat("quantiles of estimates using IGLS\n")
qigls


# Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . . . 96





############################################################################
