#' Dataset of pupils' test scores, a subset of the Junior School Project.
#' 
#' An educational dataset of pupils' test scores, a subset of the Junior School
#' Project (Mortimore et al., 1988).
#' 
#' A subset of the Junior School Project (Mortimore et al., 1988), the
#' \code{jspmix1} dataset is one of the sample datasets provided with the
#' multilevel-modelling software package MLwiN (Rasbash et al., 2009), and is
#' used in Browne (2012) as an example of modelling mixed responses. It
#' consists of test scores for 1119 pupils across 47 schools. Note that the
#' \code{behaviour} variable originally had three categories, and the middle
#' 50\% and top 25\% have been combined to produce a binary variable.)
#' 
#' @docType data
#' @format A data frame with 1119 observations on the following 8 variables:
#' \describe{
#' \item{school}{School identifying code.}
#' \item{id}{Pupil identifying code.}
#' \item{sex}{Sex of pupil; a factor with levels \code{female} and \code{male}.}
#' \item{fluent}{Fluency in English indicator, where \code{0} = beginner,
#' \code{1} = intermediate, \code{2} = fully fluent; measured in Year 1.}
#' \item{ravens}{Test score, out of 40; measured in Year 1.}
#' \item{english}{Pupils' English test score, out of 100; measured in Year 3.}
#' \item{behaviour}{Pupils' behaviour score, where \code{lowerquarter} = pupil
#' rated in bottom 25\%, and \code{upper} otherwise; measured in Year 3.}
#' \item{cons}{A column of ones. If included as an explanatory variable in a
#' regression model (e.g. in MLwiN), its coefficient is the intercept.}
#' }
#' @source Browne, W. J. (2012) \emph{MCMC Estimation in MLwiN Version 2.26.}
#' University of Bristol: Centre for Multilevel Modelling.
#' 
#' Mortimore, P., Sammons, P., Stoll, L., Lewis, D., Ecob, R. (1988)
#' \emph{School Matters}. Wells: Open Books.
#' 
#' Rasbash, J., Charlton, C., Browne, W.J., Healy, M. and Cameron, B. (2009)
#' \emph{MLwiN Version 2.1.} Centre for Multilevel Modelling, University of
#' Bristol.
#' @keywords datasets
#' @examples
#' 
#' \dontrun{
#' 
#' data(jspmix1, package = "R2MLwiN")
#' 
#' jspmix1$denomb <- jspmix1$cons
#' 
#' (mymodel <- runMLwiN(c(english, probit(behaviour, denomb)) ~ 
#'   1 + sex + ravens + fluent[1] + (1 | school) + (1[1] | id), 
#'   D = c("Mixed", "Normal", "Binomial"), 
#'   estoptions = list(EstM = 1, mcmcMeth = list(fixM = 1, residM = 1, Lev1VarM = 1)), 
#'   data = jspmix1))
#' 
#' }
#' 
"jspmix1"