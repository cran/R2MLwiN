\docType{data}
\name{lips1}
\alias{lips1}
\title{Lips data}
\format{A data frame with 56 observations on the following 41 variables:
  \describe{
    \item{\code{area}}{Region ID.}
    \item{\code{cons}}{Constant (=1).}
    \item{\code{obs}}{Observed cases of lip cancer.}
    \item{\code{exp}}{Expected count.}
    \item{\code{perc_aff}}{Percentage of the region who work in agriculture, fishing and forestry.}
    \item{\code{offs}}{Log of the expected count.}
    \item{\code{pcons}}{Constant (=1).}
    \item{\code{denom}}{Constant (=1).}
    \item{\code{neigh1}}{First neighbours.}
    \item{\code{neigh2}}{Second neighbours.}
    \item{\code{neigh3}}{Third neighbours.}
    \item{\code{neigh4}}{Fourth neighbours.}
    \item{\code{neigh5}}{Fifth neighbours.}
    \item{\code{neigh6}}{Sixth neighbours.}
    \item{\code{neigh7}}{Seventh neighbours.}
    \item{\code{neigh8}}{Eighth neighbours.}
    \item{\code{neigh9}}{Ninth neighbours.}
    \item{\code{neigh10}}{Tenth neightbours.}
    \item{\code{neigh11}}{Eleventh neightbours.}
    \item{\code{weight1}}{First neighbours' weights.}
    \item{\code{weight2}}{Second neighbours' weights.}
    \item{\code{weight3}}{Third neighbours' weights.}
    \item{\code{weight4}}{Fourth neighbours' weights.}
    \item{\code{weight5}}{Fifth neighbours' weights.}
    \item{\code{weight6}}{Sixth neighbours' weights.}
    \item{\code{weight7}}{Seventh neighbours' weights.}
    \item{\code{weight8}}{Eighth neighbours' weights.}
    \item{\code{weight9}}{Ninth neighbours' weights.}
    \item{\code{weight10}}{Tenth neightbours' weights.}
    \item{\code{weight11}}{Eleventh neightbours' weights.}
    \item{\code{wcar1}}{First neighbours' CAR weights.}
    \item{\code{wcar2}}{Second neighbours' CAR weights.}
    \item{\code{wcar3}}{Third neighbours' CAR weights.}
    \item{\code{wcar4}}{Fourth neighbours' CAR weights.}
    \item{\code{wcar5}}{Fifth neighbours' CAR weights.}
    \item{\code{wcar6}}{Sixth neighbours' CAR weights.}
    \item{\code{wcar7}}{Seventh neighbours' CAR weights.}
    \item{\code{wcar8}}{Eighth neighbours' CAR weights.}
    \item{\code{wcar9}}{Ninth neighbours' CAR weights.}
    \item{\code{wcar10}}{Tenth neightbours' CAR weights.}
    \item{\code{wcar11}}{Eleventh neightbours' CAR weights.}
}}
\source{
Browne, W. J. (2012) \emph{MCMC Estimation in MLwiN Version 2.26.} University of Bristol: Centre for Multilevel Modelling.

Clayton, D., Kaldor, J. (1987) Empirical Bayes estimates of age-standardized relative risks for use in disease mapping. \emph{Biometrics} 43: 671-681.

Rasbash, J., Charlton, C., Browne, W.J., Healy, M. and Cameron, B. (2009) \emph{MLwiN Version 2.1.} Centre for Multilevel Modelling, University of Bristol.
}

\description{
  Observed counts of male lip cancer for the 56 regions of Scotland over the period 1975-1980.}
\details{
  The \code{lips1} dataset is one of the sample datasets provided with the multilevel-modelling software package MLwiN (Rasbash et al., 2009), and was analysed in Clayton & Kaldor (1987); see also Browne (2012) for more details.}

\examples{
\dontrun{

data(lips1, package = "R2MLwiN")

(mymodel <- runMLwiN(log(obs) ~ 1 + perc_aff + offset(offs) + (0 | neigh1) + (1 | area),
 D = "Poisson", estoptions = list(EstM = 1), data = lips1))

}
}
\keyword{datasets}

