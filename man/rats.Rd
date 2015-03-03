\docType{data}
\name{rats}
\alias{rats}
\title{Weights of 30 rats, measured weekly over 5 weeks.}
\format{A data frame with 30 observations on the following 7 variables:
  \describe{
    \item{\code{y8}}{Weight on day 8.}
    \item{\code{y15}}{Weight on day 15.}
    \item{\code{y22}}{Weight on day 22.}
    \item{\code{y29}}{Weight on day 29.}
    \item{\code{y36}}{Weight on day 36.}
    \item{\code{cons}}{Constant(=1).}
    \item{\code{rat}}{Rat ID}
}}
\source{
Browne, W. J. (2012) \emph{MCMC Estimation in MLwiN Version 2.26.} University of Bristol: Centre for Multilevel Modelling.

Gelfand, A. E., Hills, S.E., Racine-Poon, A., Smith, A.F.M. (1990) Illustration of Bayesian inference in normal data models using Gibbs sampling. \emph{Journal of the American Statistical Association} 85: 972-985.

Rasbash, J., Charlton, C., Browne, W.J., Healy, M. and Cameron, B. (2009) \emph{MLwiN Version 2.1.} Centre for Multilevel Modelling, University of Bristol.
}

\description{
  Weights of 30 rats, measured weekly over 5 weeks.}
\details{
  The \code{rats} dataset is one of the sample datasets provided with the multilevel-modelling software package MLwiN (Rasbash et al., 2009); see Browne (2012) and Gelfand (1990) for further details.}

\examples{
\dontrun{

data(rats, package = "R2MLwiN")

(mymodel <- runMLwiN(c(y8, y15, y22, y29, y36) ~ 1 + (1 | rat), 
  D = "Multivariate Normal", estoptions = list(EstM = 1),
  data = rats))

}
}
\keyword{datasets}

