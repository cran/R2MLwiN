############################################################################
#     MLwiN MCMC Manual
#
# 6   Random Slopes Regression Models . . . . . . . . . . . . . . . . . . 71
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
# MLwiN folder
mlwin <- getOption("MLwiN_path")
while (!file.access(mlwin, mode = 1) == 0) {
  cat("Please specify the root MLwiN folder or the full path to the MLwiN executable:\n")
  mlwin <- scan(what = character(0), sep = "\n")
  mlwin <- gsub("\\", "/", mlwin, fixed = TRUE)
}
options(MLwiN_path = mlwin)

## Read tutorial data
data(tutorial, package = "R2MLwiN")

## Choose MCMC algoritm for estimation (IGLS will be used to obtain starting values for MCMC)
(mymodel <- runMLwiN(normexam ~ 1 + standlrt + school + school:standlrt + (1 | student), estoptions = list(EstM = 1), 
  data = tutorial))

## Define the model Choose IGLS algoritm for estimation Fit the model
(mymodel0a <- runMLwiN(normexam ~ 1 + standlrt + (1 + standlrt | school) + (1 | student), data = tutorial))

## Choose MCMC algoritm for estimation (IGLS will be used to obtain starting values for MCMC)
(mymodel0 <- runMLwiN(normexam ~ 1 + standlrt + (1 + standlrt | school) + (1 | student), estoptions = list(EstM = 1), 
  data = tutorial))

# 6.1 Prediction intervals for a random slopes regression model . . . . . 75

## Save level 2 residual chains
(mymodel <- runMLwiN(normexam ~ 1 + standlrt + (1 + standlrt | school) + (1 | student), estoptions = list(EstM = 1, 
  mcmcMeth = list(iterations = 5001), resi.store.levs = 2), data = tutorial))

predLines(mymodel, xname = "standlrt", lev = 2, selected = NULL, probs = c(0.025, 0.975), legend.space = "right", 
  legend.ncol = 2)
dev.new()
predLines(mymodel, xname = "standlrt", lev = 2, selected = c(30, 44, 53, 59), probs = c(0.025, 0.975))

# 6.2 Alternative priors for variance matrices . . . . . . . . . . . . . .78

# 6.3 WinBUGS priors (Prior 2) . . . . . . . . . . . . . . . . . . . . . .78

## Change the starting values for Level 2 variance matrix to .1 on diagonal 0 otherwise.
RP.b <- c(0.1, 0, 0.1, 0.554)
names(RP.b) <- c("RP2_var_Intercept", "RP2_cov_Intercept_standlrt", "RP2_var_standlrt", "RP1_var_Intercept")

## Fit the model
(mymodel1 <- runMLwiN(normexam ~ 1 + standlrt + (1 + standlrt | school) + (1 | student), estoptions = list(EstM = 1, 
  startval = list(RP.b = RP.b)), data = tutorial))

# 6.4 Uniform prior . . . . . . . . . . . . . . . . . . . . . . . . . . . 79

## Diffuse priors (Uniform priors)
(mymodel2 <- runMLwiN(normexam ~ 1 + standlrt + (1 + standlrt | school) + (1 | student), estoptions = list(EstM = 1, 
  mcmcMeth = list(priorcode = 0)), data = tutorial))

# 6.5 Informative prior . . . . . . . . . . . . . . . . . . . . . . . . . 80

## Informative normal prior for Sigma_u
(mymodel3 <- runMLwiN(normexam ~ 1 + standlrt + (1 + standlrt | school) + (1 | student), estoptions = list(EstM = 1, 
  mcmcMeth = list(priorParam = list(rp2 = list(estimate = matrix(c(0.09, 0.018, 0.018, 0.015), 2, 2), size = 65)))), 
  data = tutorial))

# 6.6 Results . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 81

if (!require(texreg)) install.packages("texreg")
library(texreg)
screenreg(list(mymodel0a, mymodel0, mymodel1, mymodel2, mymodel3), custom.model.names=c("IGLS", "default", "prior 2", "uniform", "prior 4"), groups = list("Fixed Part" = 1:2, "Level-2" = 3:5, "Level-1" = 6:6),
 stars = numeric(0), include.nobs=FALSE, include.loglik=FALSE, include.deviance=FALSE, include.dbar=FALSE, include.dthetabar=FALSE, include.pd=FALSE, include.dic=FALSE)

# Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . . . 81





############################################################################
