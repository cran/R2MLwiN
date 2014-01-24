\docType{data}
\name{xc}
\alias{xc}
\title{Examination scores of 16-year olds in Fife, Scotland.}
\format{A data frame with 3435 observations on the following 11 variables:
  \describe{
    \item{\code{VRQ}}{A verbal reasoning score resulting from tests pupils took when they entered secondary school.}
    \item{\code{ATTAIN}}{Attainment score of pupils at age sixteen.}
    \item{\code{PID}}{Primary school identifying code.}
    \item{\code{SEX}}{Pupils' gender, with levels \code{Male} and \code{Female}.}
    \item{\code{SC}}{Pupils' social class (scaled from low to high).}
    \item{\code{SID}}{Secondary school identifying code.}
    \item{\code{FED}}{Fathers' education.}
    \item{\code{CHOICE}}{Choice number of secondary school attended (where 1 is first choice, etc.)}
    \item{\code{MED}}{Mothers' education.}
    \item{\code{CONS}}{A column of ones. If included as an explanatory variable in a regression model (e.g. in MLwiN), its coefficient is the intercept.}
    \item{\code{PUPIL}}{Pupil identifying code.}
}}
\source{
Rasbash, J., Browne, W. J., Healy, M., Cameron, B., Charlton, C. M. J. (2013) MLwiN v2.27. University of Bristol: Centre for Multilevel Modelling.
}

\description{
  A dataset of examination scores of 16-year olds in Fife, Scotland, in which the secondary school the pupil attended is cross-classified by the primary school the pupil attended.}
\details{
  The \code{xc} dataset is one of the sample datasets provided with the multilevel-modelling software package MLwiN (Rasbash et al., 2013). The data are cross-classified in that not all children who attended the same primary school subsequently entered the same secondary school.)}

\seealso{
See \code{mlmRev} package for an alternative format of the same dataset.
}

\examples{
\dontrun{
# NB: change path as appropriate
MLwiN <- "C:/Program Files (x86)/MLwiN v2.29/"
data(xc)
# Fit cross-classified model, partitioning variance in ATTAIN across
# and within primary (PID) and secondary (SID) school
F1 = "ATTAIN ~ (0|CONS) + (3|CONS) + (2|CONS) + (1|CONS)"
ID = c("SID", "PID", "PUPIL")
MyXC <- list("class" = c(3, 2),"N1" = c(1, 1))
(XCModel <- runMLwiN(Formula = F1, levID = ID, indata = xc, 
estoptions = list(xclass = MyXC, EstM = 1), MLwiNPath = MLwiN))
}
}
\keyword{datasets}

