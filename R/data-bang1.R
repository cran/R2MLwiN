#' Sub-sample from the 1989 Bangladesh Fertility Survey
#' 
#' A subset of data from the 1989 Bangladesh Fertility Survey, consisting of
#' 1934 women across 60 districts.
#' 
#' The \code{bang1} dataset is one of the sample datasets provided with the
#' multilevel-modelling software package MLwiN (Rasbash et al., 2009), and is a
#' subset of data from the 1989 Bangladesh Fertility Survey (Huq and Cleland,
#' 1990) used by Browne (2012) as an example when fitting logistic models for
#' binary and binomial responses. The full sample was analysed in Amin et al.
#' (1997).
#' 
#' @docType data
#' @format A data frame with 1934 observations on the following 11 variables:
#' \describe{
#' \item{woman}{Identifying code for each woman (level 1 unit).}
#' \item{district}{Identifying code for each district (level 2 unit).}
#' \item{use}{Contraceptive use status at time of survey; a factor with levels
#' \code{Not_using} and \code{Using}.}
#' \item{lc}{Number of living children at time of survey; an ordered factor with
#' levels \code{None}, \code{One_child}, \code{Two_children},
#' \code{Three_plus}.}
#' \item{age}{Age of woman at time of survey (in years), centred on sample mean
#' of 30 years.}
#' \item{urban}{Type of region of residence; a factor with levels \code{Rural}
#' and \code{Urban}.}
#' \item{educ}{Woman's level of education; an ordered factor with levels
#' \code{None}, \code{Lower_primary}, \code{Upper_primary},
#' \code{Secondary_and_above}.}
#' \item{hindu}{Woman's religion; a factor with levels \code{Muslim} and
#' \code{Hindu}.}
#' \item{d_illit}{Proportion of women in district who are literate.}
#' \item{d_pray}{Proportion of Muslim women in district who pray every day (a
#' measure of religiosity).}
#' \item{cons}{A column of ones. If included as an explanatory variable in a
#' regression model (e.g. in MLwiN), its coefficient is the intercept.}
#' }
#' @seealso See \code{mlmRev} package for an alternative format of the same
#' dataset, with fewer variables.
#' @source Amin, S., Diamond, I., Steele, F. (1997) Contraception and
#' religiosity in Bangladesh. In: G. W. Jones, J. C. Caldwell, R. M. Douglas,
#' R. M. D'Souza (eds) \emph{The Continuing Demographic Transition}, 268--289.
#' Oxford: Oxford University Press.
#' 
#' Browne, W. J. (2012) \emph{MCMC Estimation in MLwiN Version 2.26.}
#' University of Bristol: Centre for Multilevel Modelling.
#' 
#' Huq, N. M., Cleland, J. (1990) \emph{Bangladesh fertility survey, 1989.}
#' Dhaka: National Institute of Population Research and Training (NIPORT).
#' 
#' Rasbash, J., Charlton, C., Browne, W.J., Healy, M. and Cameron, B. (2009)
#' \emph{MLwiN Version 2.1.} Centre for Multilevel Modelling, University of
#' Bristol.
#' @keywords datasets
#' @examples
#' 
#' \dontrun{
#' 
#' data(bang1, package = "R2MLwiN")
#' 
#' bang1$denomb <- 1
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
#' # F1 <- logit(use, denomb) ~ 1 + age + C(lc, "contr.treatment") + urban +
#' #   (1 + urban | district)
#' 
#' # (mymodel <- runMLwiN(Formula = F1,
#' #                      D = "Binomial",
#' #                      estoptions = list(EstM = 1),
#' #                      data = bang1,
#' #                      allowcontrast = TRUE))
#' 
#' F1 <- logit(use, denomb) ~ 1 + age + lc + urban + (1 + urban | district)
#' 
#' (mymodel <- runMLwiN(Formula = F1,
#'                      D = "Binomial",
#'                      estoptions = list(EstM = 1),
#'                      data = bang1))
#'
#' # Change contrasts back to pre-existing:
#' options(contrasts = my_contrasts)
#' 
#' }
#' 
"bang1"