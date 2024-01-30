############################################################################
#     MLwiN MCMC Manual
#
# 23  Using Orthogonal fixed effect vectors . . . . . . . . . . . . . . .357
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

# 23.1 A simple example . . . . . . . . . . . . . . . . . . . . . . . . .358

# 23.2 Constructing orthogonal vectors . . . . . . . . . . . . . . . . . 359

# 23.3 A Binomial response example . . . . . . . . . . . . . . . . . . . 360

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

# (mymodel <- runMLwiN(logit(use) ~ 1 + age + C(lc, "contr.treatment") + urban +
#                        (1 + urban | district),
#                      D = "Binomial",
#                      estoptions = list(EstM = 1),
#                      data = bang1,
#                      allowcontrast = TRUE))

data(bang1, package="R2MLwiN")

## Define the model

(mymodel <- runMLwiN(logit(use) ~ 1 + age + lc + urban + (1 + urban | district), D = "Binomial", estoptions = list(EstM = 1),
  data = bang1))

trajectories(mymodel)

## Orthogonal update

(mymodel <- runMLwiN(logit(use) ~ 1 + age + lc + urban + (1 + urban | district), D = "Binomial", estoptions = list(EstM = 1,
  mcmcOptions = list(orth = 1)), data = bang1))

trajectories(mymodel)

# 23.4 A Poisson example . . . . . . . . . . . . . . . . . . . . . . . . 364

## Read mmmec data
data(mmmec, package = "R2MLwiN")

(mymodel <- runMLwiN(log(obs) ~ 0 + nation + nation:uvbi + offset(log(exp)) + (1 | region), D = "Poisson", estoptions = list(EstM = 1,
  mcmcMeth = list(iterations = 50000)), data = mmmec))

sixway(mymodel@chains[, "FP_nationBelgium", drop = FALSE], acf.maxlag = 5000, "beta_1")

## Orthogonal update

(mymodel <- runMLwiN(log(obs) ~ 0 + nation + nation:uvbi + offset(log(exp)) + (1 | region), D = "Poisson", estoptions = list(EstM = 1,
  mcmcMeth = list(iterations = 50000), mcmcOptions = list(orth = 1)), data = mmmec))

sixway(mymodel@chains[, "FP_nationBelgium", drop = FALSE], acf.maxlag = 100, "beta_1")

# 23.5 An Ordered multinomial example . . . . . . . . . . . . . . . . . .368

## Read alevchem data
data(alevchem, package = "R2MLwiN")

# Note: Establishment codes on their own do not uniquely identify schools.
# Schools are instead uniquely identified by LEA code, establishment ID
# combination. Thus, here we generated a unique school ID.
alevchem$school <- as.numeric(factor(paste0(alevchem$lea, alevchem$estab)))

alevchem$gcseav <- alevchem$gcse_tot/alevchem$gcse_no - 6

## MCMC
(mymodel <- runMLwiN(logit(a_point, cons, 6) ~ 1 + gcseav[1:5] + I(gcseav^2)[1:5] + gender[1:5] + (1[1:5] | school),
  D = "Ordered Multinomial", estoptions = list(EstM = 1), data = alevchem))

trajectories(mymodel)

## Orthogonal update
(mymodel <- runMLwiN(logit(a_point, cons, 6) ~ 1 + gcseav[1:5] + I(gcseav^2)[1:5] + gender[1:5] + (1[1:5] | school),
  D = "Ordered Multinomial", estoptions = list(EstM = 1, mcmcOptions = list(orth = 1)), data = alevchem))

trajectories(mymodel)

# 23.6 The WinBUGS interface . . . . . . . . . . . . . . . . . . . . . . 372

## Read bang1 data
data(bang1, package = "R2MLwiN")

## Orthogonal update (WinBUGS)

mymodel <- runMLwiN(logit(use) ~ 1 + age + lc + urban + (1 + urban | district), D = "Binomial", estoptions = list(EstM = 1,
  mcmcOptions = list(orth = 1), show.file = TRUE), BUGO = c(version = 4, n.chains = 1, debug = FALSE, seed = 1,
  OpenBugs = TRUE), data = bang1)

summary(mymodel)
if (!require(coda)) {
  warning("package coda required to run this example")
} else {
  effectiveSize(mymodel)
}
sixway(mymodel[, "beta[1]", drop = FALSE])

# Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . . .379

# Addendum: changing contrasts back to pre-existing . . . . . . . . . . . NA

# Following re-specification of contrast settings towards the start of this
# script, change contrasts back to pre-existing:
options(contrasts = my_contrasts)

############################################################################
