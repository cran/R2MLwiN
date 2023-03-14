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
#' \describe{ \item{list("woman")}{Identifying code for each woman (level 1
#' unit).} \item{list("district")}{Identifying code for each district (level 2
#' unit).} \item{list("use")}{Contraceptive use status at time of survey; a
#' factor with levels \code{Not_using} and \code{Using}.}
#' \item{list("use4")}{Contraceptive use status and method (a factor with
#' levels: \code{Sterilization}, \code{Modern_reversible_method},
#' \code{Traditional_method}, \code{Not_using_contraception}).}
#' \item{list("lc")}{Number of living children at time of survey; a factor with
#' ordered levels \code{None}, \code{One_child}, \code{Two_children},
#' \code{Three_plus}.} \item{list("age")}{Age of woman at time of survey (in
#' years), centred on sample mean of 30 years.} \item{list("urban")}{Type of
#' region of residence; levels are \code{Rural} and \code{Urban}.}
#' \item{list("educ")}{Woman's level of education (a factor with ordered levels
#' \code{None}, \code{Lower_primary}, \code{Upper_primary},
#' \code{Secondary_and_above}.} \item{list("hindu")}{Woman's religion; levels
#' are \code{Muslim} and \code{Hindu}.} \item{list("d_lit")}{Proportion of
#' women in district who are literate.} \item{list("d_pray")}{Proportion of
#' Muslim women in district who pray every day (a measure of religiosity).}
#' \item{list("cons")}{Constant of ones.} }
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
#' (mymodel <- runMLwiN(log(use4, cons) ~ 1 + lc + (1 | district), 
#'   D = "Unordered Multinomial", estoptions = list(EstM = 1, nonlinear = c(1, 2)), data = bang))
#' 
#' }
#' 
"bang"