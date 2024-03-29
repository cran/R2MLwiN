############################################################################
#     MLwiN MCMC Manual
#
# 10  Modelling Binary Responses . . . . . . . . . . . . . . . . . . . . 129
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

# Change contrasts if wish to avoid warning indicating that, by default,
# specified contrasts for ordered predictors will be ignored by runMLwiN
# (they will be fitted as "contr.treatment" regardless of this setting). To
# enable specified contrasts, set allowcontrast to TRUE (this will be the
# default in future package releases). NB at the end of this script, the
# specification for contrasts is changed back.
my_contrasts <- options("contrasts")$contrasts
options(contrasts = c(unordered = "contr.treatment",
                      ordered = "contr.treatment"))

# As an alternative to changing contrasts, can instead use C() to specify
# contrasts for ordered predictors in formula object, e.g.:

# (mymodel3 <- runMLwiN(logit(use) ~ 1 + age + C(lc, "contr.treatment"),
#                       D = "Binomial",
#                       estoptions = list(EstM = 1),
#                       data = bang1,
#                       allowcontrast = TRUE))

# User's input if necessary

## Read bang1 data
data(bang1, package = "R2MLwiN")

# 10.1 Simple logistic regression model . . . . . . . . . . . . . . . . .130

(mymodel1 <- runMLwiN(logit(use) ~ 1 + age, D = "Binomial", estoptions = list(EstM = 1), data = bang1))
summary(mymodel1@chains[, "FP_age"])
sixway(mymodel1@chains[, "FP_age", drop = FALSE], "beta_1")

## 15,000 iterations
(mymodel2 <- runMLwiN(logit(use) ~ 1 + age, D = "Binomial", estoptions = list(EstM = 1, mcmcMeth = list(iterations = 15000)), 
  data = bang1))
sixway(mymodel1@chains[, "FP_age", drop = FALSE], "beta_1")

## Change to 5000 iterations by default
(mymodel3 <- runMLwiN(logit(use) ~ 1 + age + lc, D = "Binomial", estoptions = list(EstM = 1), data = bang1))

# 10.2 Random effects logistic regression model . . . . . . . . . . . . .136

(mymodel4 <- runMLwiN(logit(use) ~ 1 + age + lc + (1 | district), D = "Binomial", estoptions = list(EstM = 1), 
  data = bang1))
summary(mymodel4@chains[, "RP2_var_Intercept"])
sixway(mymodel4@chains[, "RP2_var_Intercept", drop = FALSE], "sigma2u0")

# 10.3 Random coefficients for area type . . . . . . . . . . . . . . . . 139

(mymodel5 <- runMLwiN(logit(use) ~ 1 + age + lc + urban + (1 | district), D = "Binomial", estoptions = list(EstM = 1), 
  data = bang1))

(mymodel6 <- runMLwiN(logit(use) ~ 1 + age + lc + urban + (1 + urban | district), D = "Binomial", estoptions = list(EstM = 1), 
  data = bang1))

# 10.4 Probit regression . . . . . . . . . . . . . . . . . . . . . . . . 141

# 10.5 Running a probit regression in MLwiN . . . . . . . . . . . . . . .142

## Gibbs
(mymodel7 <- runMLwiN(probit(use) ~ 1 + age + lc + urban + (1 + urban | district), D = "Binomial", estoptions = list(EstM = 1, 
  mcmcMeth = list(fixM = 1, residM = 1)), data = bang1))

## Univariate MH by default
(mymodel8 <- runMLwiN(probit(use) ~ 1 + age + lc + urban + (1 + urban | district), D = "Binomial", estoptions = list(EstM = 1), 
  data = bang1))

if (!require(texreg)) {
  warning("texreg package required to use screenreg() function")
} else {
  screenreg(list(mymodel7, mymodel8), custom.model.names=c("Gibbs", "Metropolis"), groups = list("Fixed Part" = 1:6, "Level-2" = 7:9, "Level-1" = 10:10),
   stars = numeric(0), include.nobs=FALSE, include.loglik=FALSE, include.deviance=FALSE, include.dbar=FALSE, include.dthetabar=FALSE, include.pd=FALSE, include.dic=FALSE)
}

if (!require(coda)) {
  warning("package coda required to run this example")
} else {
  cat("The effective sample sizes\n")
  ESS.aa <- effectiveSize(mymodel7@chains[, 2:11])
  ESS.bb <- effectiveSize(mymodel8@chains[, 2:11])
  ctable <- cbind(round(ESS.aa), round(ESS.bb))
  colnames(ctable) <- c("ESS(Gibbs)", "ESS(Metropolis)")
  print(ctable)
}

# 10.6 Comparison with WinBUGS . . . . . . . . . . . . . . . . . . . . . 144

mymodel9 <- runMLwiN(logit(use) ~ 1 + age + (1 | district), D = "Binomial", estoptions = list(EstM = 1), BUGO = c(version = 4, 
  n.chains = 1, debug = FALSE, seed = 1, OpenBugs = TRUE), data = bang1)

summary(mymodel9)
summary(mymodel9[, "beta[1]"])
sixway(mymodel9[, "beta[1]", drop = FALSE])

(mymodel10 <- runMLwiN(logit(use) ~ 1 + age + (1 | district), D = "Binomial", estoptions = list(EstM = 1), 
  data = bang1))

summary(mymodel10@chains[, "FP_Intercept"])
sixway(mymodel10@chains[, "FP_Intercept", drop = FALSE], "beta_0")

# Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . . .128

# Addendum: changing contrasts back to pre-existing . . . . . . . . . . . NA

# Following re-specification of contrast settings towards the start of this
# script, change contrasts back to pre-existing:
options(contrasts = my_contrasts)

############################################################################  
