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
if(!exists("mlwin")) mlwin ="C:/Program Files (x86)/MLwiN v2.30/"
while (!file.access(mlwin,mode=0)==0||!file.access(mlwin,mode=1)==0||!file.access(mlwin,mode=4)==0){
    cat("Please specify the MLwiN folder including the MLwiN executable:\n")
    mlwin=scan(what=character(0),sep ="\n")
    mlwin=gsub("\\", "/",mlwin, fixed=TRUE)
}

# User's input if necessary

# 8.3 Generating simulated datasets based on true values . . . . . . . . 102

# 8.4 Fitting the model to the simulated datasets . . . . . . . . . . . .106

set.seed(1)
pupil = 1:108
school = c(rep(1, 18), rep(2, 18), rep(3, 18), rep(4, 18), rep(5, 18), rep(6, 18))
cons = rep(1, 108)
levID = c("school", "pupil")
formula = "resp ~ (0|cons) + (2|cons) + (1|cons)"

ns <- 100
IGLS_array = MCMC_array = array(, c(9, 5, ns))
MCMC_median <- data.frame(RP2_var_cons = rep(0, ns), RP1_var_cons = rep(0, ns))
CounterMCMC <- rep(0, 3)
Actual <- c(30, 10, 40)
for(i in 1:ns){
  u_short <- rnorm(6, 0, sqrt(Actual[2]))
  u <- rep(u_short, each = 18, len = 108)
  e <- rnorm(108, 0, sqrt(Actual[3]))
  resp <- Actual[1] * cons + u + e
  indata = data.frame(cbind(pupil, school, cons, resp))
  simModelIGLS <- runMLwiN(formula, levID, D = "Normal", indata, estoptions = list(EstM = 0), MLwiNPath=mlwin, workdir = tempdir())
  IGLS_array[,,i] <- as.matrix(simModelIGLS["estIGLS"])
  simModelMCMC <- runMLwiN(formula, levID, D = "Normal", indata, estoptions = list(EstM = 1), MLwiNPath=mlwin, workdir = tempdir())
  MCMC_array[,,i] <- as.matrix(simModelMCMC["estMCMC"])
  MCMC_median[i, ] <- c(median(simModelMCMC["chains"][,"RP2_var_cons"]), median(simModelMCMC["chains"][,"RP1_var_cons"]))
  if (Actual[1] > quantile(simModelMCMC["chains"][,"FP_cons"], 0.025) & Actual[1] < quantile(simModelMCMC["chains"][,"FP_cons"], 0.975)) {
    CounterMCMC[1] <- CounterMCMC[1] + 1
  }
  if (Actual[2] > quantile(simModelMCMC["chains"][,"RP2_var_cons"], 0.025) & Actual[2] < quantile(simModelMCMC["chains"][,"RP2_var_cons"], 0.975)) {
    CounterMCMC[2] <- CounterMCMC[2] + 1
  }
  if (Actual[3] > quantile(simModelMCMC["chains"][,"RP1_var_cons"], 0.025) & Actual[3] < quantile(simModelMCMC["chains"][,"RP1_var_cons"], 0.975)) {
    CounterMCMC[3] <- CounterMCMC[3] + 1
  }
}

aa <- sapply(1:ns, function(x) na.omit(stack(as.data.frame(IGLS_array[,,x])))$values)
counterIGLS <- rep(0,3)
for (i in 1:ns) {
  if (Actual[1] > aa[1,i] - 1.96 * sqrt(aa[2,i]) & Actual[1] < aa[1,i] + 1.96 * sqrt(aa[2,i])) {
    counterIGLS[1] <- counterIGLS[1] + 1
  }
  if (Actual[2] > aa[3,i] - 1.96 * sqrt(aa[5,i]) & Actual[2] < aa[3,i] + 1.96 * sqrt(aa[5,i])) {
    counterIGLS[2] <- counterIGLS[2] + 1
  }
  if (Actual[3] > aa[4,i] - 1.96 * sqrt(aa[7,i]) & Actual[3] < aa[4,i] + 1.96 * sqrt(aa[7,i])) {
    counterIGLS[3] <- counterIGLS[3] + 1
  }
}
Percent_interval_coverage <- (counterIGLS / ns) * 100
Mean_across_simus <- round(c(mean(aa[1,]), mean(aa[3,]), mean(aa[4,])), 2)
Percent_bias <- round(-100 * (1 - Mean_across_simus / Actual), 2)
IGLS_results <- cbind(Mean_across_simus, Actual, Percent_bias, Percent_interval_coverage)
rownames(IGLS_results) = c("beta0", "sigma2_u", "sigma2_e")
Percent_interval_coverage <- (CounterMCMC / ns) * 100
bb <- sapply(1:ns, function(x) na.omit(stack(as.data.frame(MCMC_array[,,x])))$values)
Mean_across_simus <- round(c(mean(bb[1,]), mean(bb[3,]), mean(bb[4,])), 2)
Percent_bias <- round(-100 * (1 - Mean_across_simus / Actual), 2)
MCMC_results <- cbind(Mean_across_simus, Actual, Percent_bias, Percent_interval_coverage)
rownames(MCMC_results) = c("beta0", "sigma2_u", "sigma2_e")

# 8.5 Analysing the simulation results . . . . . . . . . . . . . . . . . 109

cat("Simulation results using IGLS\n"); IGLS_results
cat("Simulation results using MCMC\n"); MCMC_results

# Investigating median estimates with Gamma(epsilon,epsilon) priors

Mean_across_simus <- round(c(mean(MCMC_median$RP2_var_cons), mean(MCMC_median$RP1_var_cons)), 2)
Actual <- tail(Actual, -1)
Percent_bias <- round(-100 * (1 - Mean_across_simus / Actual), 2)
Percent_interval_coverage <- tail(Percent_interval_coverage, -1)
MCMC_results2 <- cbind(Mean_across_simus, Actual, Percent_bias, Percent_interval_coverage)
rownames(MCMC_results2) <- c("sigma2_u", "sigma2_e")
cat("Simulation results based on median MCMC estimates\n"); MCMC_results2

# Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . . . 96





############################################################################
