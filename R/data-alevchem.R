#' Chemistry A-level results from one exam board
#' 
#' Chemistry A-level results from one exam board; subset from Yang & Woodhouse,
#' 2001. See also Rasbash et al. (2012) and Browne (2012).
#' 
#' The \code{alevchem} dataset is one of the sample datasets provided with the
#' multilevel-modelling software package MLwiN (Rasbash et al., 2009).
#' 
#' @docType data
#' @format A data frame with 2166 observations on the following 8 variables:
#' \describe{ \item{list("lea")}{Local Education Authority ID.}
#' \item{list("estab")}{Establishment (institution) ID.}
#' \item{list("pupil")}{Pupil ID.} \item{list("a_point")}{A-level point score
#' (an ordered factor with levels: \code{F}, \code{E}, \code{D}, \code{C},
#' \code{B}, \code{A}).} \item{list("gcse_tot")}{Total GCSE point score.}
#' \item{list("gcse_no")}{Number of GCSEs taken.} \item{list("cons")}{Constant
#' of ones} \item{list("gender")}{Pupil's gender (a factor with levels:
#' \code{male}, \code{female}).} }
#' @source Browne, W. J. (2012) \emph{MCMC Estimation in MLwiN Version 2.26.}
#' University of Bristol: Centre for Multilevel Modelling.
#' 
#' Rasbash, J., Charlton, C., Browne, W.J., Healy, M. and Cameron, B. (2009)
#' \emph{MLwiN Version 2.1.} Centre for Multilevel Modelling, University of
#' Bristol.
#' 
#' Rasbash, J., Steele, F., Browne, W.J. and Goldstein, H. (2012) \emph{A
#' User's Guide to MLwiN Version 2.26.} Centre for Multilevel Modelling,
#' University of Bristol.
#' 
#' Yang, M., Woodhouse, G. (2001) Progress from GCSE to A and AS level:
#' institutional and gender differences, and trends over time. \emph{British
#' Educational Research Journal} 27: 245-267.
#' @keywords datasets
#' @examples
#' 
#' \dontrun{
#' 
#' data(alevchem, package = "R2MLwiN")
#' 
#' alevchem$gcseav <- alevchem$gcse_tot/alevchem$gcse_no - 6
#' 
#' # Note: Establishment codes on their own do not uniquely identify schools.
#' # Schools are instead uniquely identified by LEA code, establishment ID
#' # combination. Thus, here we generated a unique school ID.
#' 
#' alevchem$school <- as.numeric(factor(paste0(alevchem$lea, alevchem$estab)))
#' 
#' (mymodel <- runMLwiN(logit(a_point, cons, 6) ~ 1 + gcseav[1:5] + I(gcseav^2)[1:5] +
#'   gender[1:5] + (1[1:5] + gcseav[1:5] | school), 
#'   D = "Ordered Multinomial", estoptions = list(EstM = 1), data = alevchem))
#' 
#' }
#' 
"alevchem"