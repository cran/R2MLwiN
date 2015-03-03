\docType{data}
\name{xc}
\alias{xc}
\title{Examination scores of 16-year olds in Fife, Scotland.}
\format{A data frame with 3435 observations on the following 11 variables:
  \describe{
    \item{\code{vrq}}{A verbal reasoning score resulting from tests pupils took when they entered secondary school.}
    \item{\code{attain}}{Attainment score of pupils at age sixteen.}
    \item{\code{pid}}{Primary school identifying code.}
    \item{\code{sex}}{Pupils' gender: a factor with levels \code{Male} and \code{Female}.}
    \item{\code{sc}}{Pupils' social class (scaled from low to high).}
    \item{\code{sid}}{Secondary school identifying code.}
    \item{\code{fed}}{Fathers' education.}
    \item{\code{choice}}{Choice number of secondary school attended (where 1 is first choice, etc.)}
    \item{\code{med}}{Mothers' education.}
    \item{\code{cons}}{A column of ones. If included as an explanatory variable in a regression model (e.g. in MLwiN), its coefficient is the intercept.}
    \item{\code{pupil}}{Pupil identifying code.}
}}
\source{
Paterson, L. (1991) Socio economic status and educational attainment: a multidimensional and multilevel study. \emph{Evaluation and Research in Education}, 5, 97-121.
Rasbash, J., Charlton, C., Browne, W.J., Healy, M. and Cameron, B. (2009) \emph{MLwiN Version 2.1.} Centre for Multilevel Modelling, University of Bristol.
Rasbash, J., Steele, F., Browne, W.J., Goldstein, H. (2012) \emph{A User's Guide to MLwiN v2.26}. University of Bristol: Centre for Multilevel Modelling.
}

\description{
  A dataset of examination scores of 16-year olds in Fife, Scotland, in which the secondary school the pupil attended is cross-classified by the primary school the pupil attended.}
\details{
  The \code{xc} dataset is one of the sample datasets provided with the multilevel-modelling software package MLwiN (Rasbash et al., 2009), analysed by Paterson (1991). The data are cross-classified in that not all children who attended the same primary school subsequently entered the same secondary school. See also Rasbash et al. (2012).
  }

\seealso{
See \code{mlmRev} package for an alternative format of the same dataset.
}

\examples{
\dontrun{

data(xc, package = "R2MLwiN")

(mymodel <- runMLwiN(attain ~ 1 + (1 | sid) + (1 | pid) + (1 | pupil),
 estoptions = list(xc = TRUE, EstM = 1), data = xc))

}
}
\keyword{datasets}

