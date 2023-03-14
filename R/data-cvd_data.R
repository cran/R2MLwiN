#' Data from the 1998 Scottish Health Survey on cardiovascular disease status of
#' 8804 respondents
#' 
#' Data from the 1998 Scottish Health Survey, with 8804 respondents aged between
#' 18 and 64. The outcome, \code{cvddef}, is a self-report of a doctor-diagnosed
#' cardiovascular disease (CVD) condition (angina, diabetes, hypertension, acute
#' myocardial infarction, etc.). This is a binary response, whether (1) or not
#' (0) respondents have CVD condition.
#' 
#' The \code{data-cvd} dataset is one of the example datasets analysed in
#' Leyland and Groenewegen (2020), and provided with the
#' multilevel-modelling software package MLwiN (Rasbash et al., 2009).
#' 
#' @docType data
#' @format A data frame with 8804 observations on the following 9 variables:
#' \describe{
#' \item{list("age")}{Age.}
#' \item{list("sex")}{Gender (factor with levels: \code{male}, \code{female}).}
#' \item{list("sc")}{Social class (factor with levels: \code{sc_12}, \code{sc_3}, \code{sc_45}).}
#' \item{list("cvddef")}{Self-reported cardiovascular disease (\code{0} = does not have
#' condition, \code{1} = has condition)}
#' \item{list("carstair")}{Carstairs score.}
#' \item{list("smoke")}{Smoking frequency (factor with levels: \code{smk_lite}, \code{smk_mod},
#' \code{smk_hvy}, \code{smk_ex}, \code{smk_nevr}).}
#' \item{list("id")}{Respondent identifier.}
#' \item{list("area")}{Postcode sector} }
#' 
#' @source
#' 
#' Browne, W. J. (2012) \emph{MCMC Estimation in MLwiN Version 2.26.}
#' University of Bristol: Centre for Multilevel Modelling.
#' 
#' Leyland A.H. (2005) Socioeconomic gradients in the prevalence of cardiovascular
#' disease in Scotland: the roles of composition and context.
#' \emph{J Epidemiol Community Health} 59:799–803
#'
#' Leyland, A.H., Groenewegen, P.P. (2020). Untangling Context and Composition.
#' In: \emph{Multilevel Modelling for Public Health and Health Services Research}.
#' Springer, Cham. \href{https://doi.org/10.1007/978-3-030-34801-4_13}
#' 
#' Rasbash, J., Charlton, C., Browne, W.J., Healy, M. and Cameron, B. (2009)
#' \emph{MLwiN Version 2.1.} Centre for Multilevel Modelling, University of
#' Bristol.
#' 
#' Rasbash, J., Steele, F., Browne, W.J. and Goldstein, H. (2012) \emph{A
#' User's Guide to MLwiN Version 2.26.} Centre for Multilevel Modelling,
#' University of Bristol.
#'
#' @keywords datasets
"cvd_data"