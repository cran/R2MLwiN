#' Sub-sample from the 1989 Bangladesh Fertility Survey (see Huq & Cleveland,
#' 1990)
#' 
#' A subset of data from the 1989 Bangladesh Fertility Survey, consisting of
#' 2867 women across 61 districts.
#' 
#' The \code{bang} dataset is one of the sample datasets provided with the
#' multilevel-modelling software package MLwiN (Rasbash et al., 2009), and is a
#' subset of data from the 1989 Bangladesh Fertility Survey (Huq and Cleland,
#' 1990) used by Rasbash et al. (2012) as an example when fitting logistic
#' models for binary and binomial responses. The full sample was analysed in
#' Amin et al. (1997).
#' 
#' @docType data
#' @format A data frame with 2867 observations on the following 12 variables:
#' \describe{
#' \item{woman}{Identifying code for each woman (level 1 unit).}
#' \item{district}{Identifying code for each district (level 2 unit).}
#' \item{use}{Contraceptive use status at time of survey; a factor with levels
#' \code{Not_using} and \code{Using}.}
#' \item{use4}{Contraceptive use status and method (a factor with levels:
#' \code{Sterilization}, \code{Modern_reversible_method},
#' \code{Traditional_method}, \code{Not_using_contraception}).}
#' \item{lc}{Number of living children at time of survey; a factor with ordered
#' levels \code{None}, \code{One_child}, \code{Two_children},
#' \code{Three_plus}.}
#' \item{age}{Age of woman at time of survey (in years), centred on sample mean
#' of 30 years.}
#' \item{urban}{Type of region of residence; levels are \code{Rural} and
#' \code{Urban}.}
#' \item{educ}{Woman's level of education (a factor with ordered levels
#' \code{None}, \code{Lower_primary}, \code{Upper_primary},
#' \code{Secondary_and_above}.}
#' \item{hindu}{Woman's religion; levels are \code{Muslim} and \code{Hindu}.}
#' \item{d_lit}{Proportion of women in district who are literate.}
#' \item{d_pray}{Proportion of Muslim women in district who pray every day (a
#' measure of religiosity).}
#' \item{cons}{Constant of ones.}
#' }
#' @seealso See \code{mlmRev} package for an alternative format of the same
#' dataset, with fewer variables.
#' @source Amin, S., Diamond, I., Steele, F. (1997) Contraception and
#' religiosity in Bangladesh. In: G. W. Jones, J. C. Caldwell, R. M. Douglas,
#' R. M. D'Souza (eds) \emph{The Continuing Demographic Transition}, 268--289.
#' Oxford: Oxford University Press.
#' 
#' Huq, N. M., Cleland, J. (1990) \emph{Bangladesh fertility survey, 1989.}
#' Dhaka: National Institute of Population Research and Training (NIPORT).
#' 
#' Rasbash, J., Charlton, C., Browne, W.J., Healy, M. and Cameron, B. (2009)
#' \emph{MLwiN Version 2.1.} Centre for Multilevel Modelling, University of
#' Bristol.
#' 
#' Rasbash, J., Steele, F., Browne, W.J. and Goldstein, H. (2012) \emph{A
#' User's Guide to MLwiN Version 2.26.} Centre for Multilevel Modelling,
#' University of Bristol.
#' @keywords datasets
#' @examples
#' 
#' \dontrun{
#' 
#' data(bang, package = "R2MLwiN")
#' 
#' bang$use4 <- relevel(bang$use4, 4)
#' 
#' # Change contrasts if wish to avoid warning indicating that, by default,
#' # specified contrasts for ordered predictors will be ignored by runMLwiN
#' # (they will be fitted as "contr.treatment" regardless of this setting). To
#' # enable specified contrasts, set allowcontrast to TRUE (this will be the
#' # default in future package releases).
#' my_contrasts <- options("contrasts")$contrasts
#' options(contrasts = c(unordered = "contr.treatment",
#'                       ordered = "contr.treatment"))
#' 
#' # As an alternative to changing contrasts, can instead use C() to specify
#' # contrasts for ordered predictors in formula object, e.g.:
#' 
#' # F1 <- log(use4, cons) ~ 1 + C(lc, "contr.treatment") + (1 | district)
#' 
#' # (mymodel <- runMLwiN(Formula = F1, 
#' #                      D = "Unordered Multinomial",
#' #                      estoptions = list(EstM = 1, nonlinear = c(1, 2)),
#' #                      data = bang,
#' #                      allowcontrast = TRUE))
#' 
#' F1 <- log(use4, cons) ~ 1 + lc + (1 | district)
#' 
#' (mymodel <- runMLwiN(Formula = F1, 
#'                      D = "Unordered Multinomial",
#'                      estoptions = list(EstM = 1, nonlinear = c(1, 2)),
#'                      data = bang))
#' 
#' # Change contrasts back to pre-existing:
#' options(contrasts = my_contrasts)
#' 
#' }
#' 
"bang"