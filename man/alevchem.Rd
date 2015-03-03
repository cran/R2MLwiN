\docType{data}
\name{alevchem}
\alias{alevchem}
\title{Chemistry A-level results from one exam board}
\format{A data frame with 2166 observations on the following 8 variables:
  \describe{
    \item{\code{lea}}{Local Education Authority ID.}
    \item{\code{estab}}{Establishment (institution) ID.}
    \item{\code{pupil}}{Pupil ID.}
    \item{\code{a_point}}{A-level point score (an ordered factor with levels: \code{F}, \code{E}, \code{D}, \code{C}, \code{B}, \code{A}).}
    \item{\code{gcse_tot}}{Total GCSE point score.}
    \item{\code{gcse_no}}{Number of GCSEs taken.}
    \item{\code{cons}}{Constant of ones}
    \item{\code{gender}}{Pupil's gender (a factor with levels: \code{male}, \code{female}).}
}}

\source{
Browne, W. J. (2012) \emph{MCMC Estimation in MLwiN Version 2.26.} University of Bristol: Centre for Multilevel Modelling.

Rasbash, J., Charlton, C., Browne, W.J., Healy, M. and Cameron, B. (2009) \emph{MLwiN Version 2.1.} Centre for Multilevel Modelling, University of Bristol.

Rasbash, J., Steele, F., Browne, W.J. and Goldstein, H. (2012) \emph{A User's Guide to MLwiN Version 2.26.} Centre for Multilevel Modelling, University of Bristol.

Yang, M., Woodhouse, G. (2001) Progress from GCSE to A and AS level: institutional and gender differences, and trends over time. \emph{British Educational Research Journal} 27: 245-267.
}

\description{
  Chemistry A-level results from one exam board; subset from Yang & Woodhouse, 2001. See also Rasbash et al. (2012) and Browne (2012).}
\details{
  The \code{alevchem} dataset is one of the sample datasets provided with the multilevel-modelling software package MLwiN (Rasbash et al., 2009).}


\examples{
\dontrun{

data(alevchem, package = "R2MLwiN")

alevchem$gcseav <- double2singlePrecision(alevchem$gcse_tot/alevchem$gcse_no - 6)

# Note: Establishment codes on their own do not uniquely identify schools.
# Schools are instead uniquely identified by LEA code, establishment ID
# combination. Thus, here we generated a unique school ID.

alevchem$school <- as.numeric(factor(paste0(alevchem$lea, alevchem$estab)))

(mymodel <- runMLwiN(logit(a_point, cons, 6) ~ 1 + gcseav[1:5] + I(gcseav^2)[1:5] +
  gender[1:5] + (1[1:5] + gcseav[1:5] | school), 
  D = "Ordered Multinomial", estoptions = list(EstM = 1), data = alevchem))

}
}
\keyword{datasets}

