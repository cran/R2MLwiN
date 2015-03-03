\docType{data}
\name{mmmec}
\alias{mmmec}
\title{EC data on UV radiation exposure & malignant melanoma.}
\format{A data frame with 354 observations on the following variables:
  \describe{
    \item{\code{nation}}{Nation ID: a factor with levels corresponding to each country.}
    \item{\code{region}}{Region (within-nation) ID.}
    \item{\code{county}}{County (within-region) ID.}
    \item{\code{obs}}{Number of male deaths due to malignant melanoma between 1971 and 1980.}
    \item{\code{exp}}{Expected number of deaths - proportional to county population.}
    \item{\code{cons}}{Constant (=1).}
    \item{\code{uvbi}}{County-level measurement of UV B radiation, centered on the mean.}
}}
\source{
Browne, W. J. (2012) \emph{MCMC Estimation in MLwiN Version 2.26.} University of Bristol: Centre for Multilevel Modelling.

Langford, I. H., Bentham, G., McDonald, A-L. (1998) Multi-level modelling of geographically aggregated health data: a case study on malignant melanoma mortality and UV exposure in the European Community. \emph{Statistics in Medicine} 17: 41-57.

Rasbash, J., Charlton, C., Browne, W.J., Healy, M. and Cameron, B. (2009) \emph{MLwiN Version 2.1.} Centre for Multilevel Modelling, University of Bristol.
}

\description{
  EC data on UV radiation exposure & malignant melanoma, consisting of 354 counties across 79 regions across 9 nations.}
\details{
  The \code{mmmec} dataset is one of the sample datasets provided with the multilevel-modelling software package MLwiN (Rasbash et al., 2009). Further information can be found in Langford et al. (1998) and Browne (2012).}

\seealso{
See \code{mlmRev} package for an alternative format of the same dataset, with fewer variables.
}

\examples{
\dontrun{

data(mmmec, package = "R2MLwiN")

(mymodel3 <- runMLwiN(log(obs) ~ 1 + uvbi + offset(log(exp)) + (1 | nation) + (1 | region), 
 D = "Poisson", estoptions = list(EstM = 1), data = mmmec))

}
}
\keyword{datasets}

