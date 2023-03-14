#' Pupils' marks from GCSE exams (UK, 1989).
#' 
#' GCSE exam results, taken from 73 schools in England, consisting of 1905
#' pupils.
#' 
#' The \code{gcsemv1} dataset is one of the sample datasets provided with the
#' multilevel-modelling software package MLwiN (Rasbash et al., 2009); for
#' further details see Rasbash et al. (2012).
#' 
#' @docType data
#' @format A data frame with 1905 observations on the following variables:
#' \describe{ \item{list("school")}{School identification (level 2 unit).}
#' \item{list("student")}{Student identification (level 1 unit).}
#' \item{list("female")}{Gender: a factor with levels \code{Female} and
#' \code{Male}.} \item{list("agemths")}{Age in months.}
#' \item{list("written")}{Score on the written component.}
#' \item{list("csework")}{Score on the coursework component.}
#' \item{list("cons")}{Constant (= 1).} }
#' @seealso See \code{mlmRev} package for an alternative format of the same
#' dataset, with fewer variables.
#' @source Rasbash, J., Charlton, C., Browne, W.J., Healy, M. and Cameron, B.
#' (2009) \emph{MLwiN Version 2.1.} Centre for Multilevel Modelling, University
#' of Bristol.
#' 
#' Rasbash, J., Steele, F., Browne, W.J. and Goldstein, H. (2012) \emph{A
#' User's Guide to MLwiN Version 2.26.} Centre for Multilevel Modelling,
#' University of Bristol.
#' @keywords datasets
#' @examples
#' 
#' \dontrun{
#' 
#' data(gcsemv1, package = "R2MLwiN")
#' 
#' (mymodel <- runMLwiN(c(written, csework) ~ 1 + female + (1 | school) + (1 | student), 
#'   D = "Multivariate Normal", estoptions = list(EstM = 1), data = gcsemv1))
#' 
#' }
#' 
"gcsemv1"