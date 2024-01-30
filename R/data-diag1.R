#' Examination dataset
#' 
#' Examination data for 907 students within 18 schools.
#' 
#' The \code{diag1} dataset is one of the sample datasets provided with the
#' multilevel-modelling software package MLwiN (Rasbash et al., 2009),
#' originally analysed in Aitkin & Longford (1986), and described further in
#' Rasbash et al. (2012).
#' 
#' @docType data
#' @format A data frame with 907 observations on the following 9 variables:
#' \describe{
#' \item{school}{School identifier.}
#' \item{sex}{Pupil gender.}
#' \item{vrq}{Verbal Reasoning quotient.}
#' \item{ilea}{O-level/CSE examination results.}
#' \item{type}{School type: a factor with levels \code{Comprehensive} and
#' \code{Grammar}.}
#' \item{pupil}{Pupil identifier.}
#' \item{cons}{Constant (=1).}
#' \item{n_ilea}{O-level/CSE examination results (normal scores).}
#' \item{n_vrq}{Verbal Reasoning quotient (normal scores).}
#' }
#' @source Rasbash, J., Charlton, C., Browne, W.J., Healy, M. and Cameron, B.
#' (2009) \emph{MLwiN Version 2.1.} Centre for Multilevel Modelling, University
#' of Bristol.
#' 
#' Rasbash, J., Steele, F., Browne, W.J., Goldstein, H. (2012) \emph{A User's
#' Guide to MLwiN v2.26}. University of Bristol: Centre for Multilevel
#' Modelling.
#' 
#' Aitkin, M. & Longford, N. (1986). Statistical modelling in school
#' effectiveness studies (with discussion). Journal of the Royal Statistical
#' Society, Series A, 149:1-43.
#' @keywords datasets
#' @examples
#' 
#' \dontrun{
#' 
#' data(diag1, package = "R2MLwiN")
#' 
#' (mymodel <- runMLwiN(n_ilea ~ 1 + n_vrq + (1 + n_vrq | school) + (1 | pupil),
#'   estoptions = list(resi.store = TRUE, resioptions = c("standardised",
#'   "leverage", "influence", "deletion")), data = diag1))
#' 
#' }
#' 
"diag1"