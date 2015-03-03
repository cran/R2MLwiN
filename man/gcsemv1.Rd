\docType{data}
\name{gcsemv1}
\alias{gcsemv1}
\title{Pupils' marks from GCSE exams (UK, 1989).}
\format{A data frame with 1905 observations on the following variables:
  \describe{
    \item{\code{school}}{School identification (level 2 unit).}
    \item{\code{student}}{Student identification (level 1 unit).}
    \item{\code{female}}{Gender: a factor with levels \code{Female} and \code{Male}.}
    \item{\code{agemths}}{Age in months.}
    \item{\code{written}}{Score on the written component.}
    \item{\code{csework}}{Score on the coursework component.}
    \item{\code{cons}}{Constant (= 1).}
}}
\source{
Rasbash, J., Charlton, C., Browne, W.J., Healy, M. and Cameron, B. (2009) \emph{MLwiN Version 2.1.} Centre for Multilevel Modelling, University of Bristol.

Rasbash, J., Steele, F., Browne, W.J. and Goldstein, H. (2012) \emph{A User's Guide to MLwiN Version 2.26.} Centre for Multilevel Modelling,
University of Bristol.
}

\description{
  GCSE exam results, taken from 73 schools in England, consisting of 1905 pupils.}
\details{
  The \code{gcsemv1} dataset is one of the sample datasets provided with the multilevel-modelling software package MLwiN (Rasbash et al., 2009); for further details see Rasbash et al. (2012).}

\seealso{
See \code{mlmRev} package for an alternative format of the same dataset, with fewer variables.
}

\examples{
\dontrun{

data(gcsemv1, package = "R2MLwiN")

(mymodel <- runMLwiN(c(written, csework) ~ 1 + female + (1 | school) + (1 | student), 
  D = "Multivariate Normal", estoptions = list(EstM = 1), data = gcsemv1))

}
}
\keyword{datasets}

