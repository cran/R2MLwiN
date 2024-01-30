#' Pupils' marks from GCSE exams (UK, 1989); complete cases only.
#' 
#' Pupils' marks from GCSE exams, consisting of 1523 pupils across 73 schools.
#' See Browne (2012) for further details.
#' 
#' The \code{gcsecomp1} dataset is one of the sample datasets provided with the
#' multilevel-modelling software package MLwiN (Rasbash et al., 2009).
#' 
#' @docType data
#' @format A data frame with 1523 observations on the following 6 variables:
#' \describe{
#' \item{school}{Identifying code for each school (level 2 unit).}
#' \item{student}{Identifying code for each pupil (level 1 unit).}
#' \item{female}{Gender of pupil: a factor with levels \code{Male} and
#' \code{Female}.}
#' \item{written}{Exam score.}
#' \item{csework}{Coursework score.}
#' \item{cons}{Constant (=1).} }
#' @source Browne, W. J. (2012) \emph{MCMC Estimation in MLwiN Version 2.26.}
#' University of Bristol: Centre for Multilevel Modelling.
#' 
#' Rasbash, J., Charlton, C., Browne, W.J., Healy, M. and Cameron, B. (2009)
#' \emph{MLwiN Version 2.1.} Centre for Multilevel Modelling, University of
#' Bristol.
#' @keywords datasets
#' @examples
#' 
#' \dontrun{
#' 
#' data(gcsecomp1, package="R2MLwiN")
#' 
#' (mymodel <- runMLwiN(c(written, csework) ~ 1 + female + (1 | school) + (1 | student), 
#'   D = "Multivariate Normal", estoptions = list(EstM = 1), data = gcsecomp1))
#' 
#' }
#' 
"gcsecomp1"