\docType{data}
\name{height}
\alias{height}
\title{Height data.}
\format{A data frame with 100 observations on the following variable:
  \describe{
    \item{\code{height}}{Heights of 100 adult males measured in centimetres.}
}}
\source{
Rasbash, J., Charlton, C., Browne, W.J., Healy, M. and Cameron, B. (2009) \emph{MLwiN Version 2.1.} Centre for Multilevel Modelling, University of Bristol.
Rasbash, J., Steele, F., Browne, W.J. and Goldstein, H. (2012) \emph{A User's Guide to MLwiN Version 2.26.} Centre for Multilevel Modelling, University of Bristol.
}

\description{
  Height data for 100 adult males.}
\details{
  The \code{height} dataset is one of the sample datasets provided with the multilevel-modelling software package MLwiN (Rasbash et al., 2009); see Rasbash et al. (2012) for further details.}


\examples{
\dontrun{
# from demo(UserGuide16)

data(height, package = "R2MLwiN")
summary(height)

hist(height$height)

1 - pnorm((200 - mean(height$height)) / sd(height$height))

heightsim1 <- function() {
  heightsim <- 175.35 + 10.002 * qnorm(runif(100))
  c(pmean = mean(heightsim), pvar = var(heightsim))
}

set.seed(1)

# Note: To obtain estimates as close as possible to the MLwiN manual,
# increase the number of reps to 10000.

simdata1 <- as.data.frame(t(replicate(1000, heightsim1())))
simdata1$iteration <- 1:nrow(simdata1)

plot(simdata1$iteration, simdata1$pmean, type = "l")

plot(density(simdata1$pmean))

quantile(simdata1$pmean, c(0.025, 0.975))

plot(simdata1$iteration, simdata1$pvar, type = "l")

plot(density(simdata1$pvar))

quantile(simdata1$pvar, c(0.025, 0.975))

heightsim2 <- function(variable) {
  samp <- sample(variable, replace = TRUE)
  c(npmean = mean(samp), npvar = var(samp))
}

simdata2 <- as.data.frame(t(replicate(1000, heightsim2(height$height))))
simdata2$iteration <- 1:nrow(simdata2)

plot(simdata2$iteration, simdata2$npmean, type = "l")

plot(density(simdata2$npmean))

quantile(simdata2$npmean, c(0.025, 0.975))

plot(simdata2$iteration, simdata2$npvar, type = "l")

plot(density(simdata2$npvar))

quantile(simdata2$npvar, c(0.025, 0.975))
}
}
\keyword{datasets}

