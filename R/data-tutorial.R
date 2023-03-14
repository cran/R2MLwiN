#' Exam results for six inner London Education Authorities
#' 
#' A subset of data from a much larger dataset of examination results from six
#' inner London Education Authorities (school boards).
#' 
#' The \code{tutorial} dataset is one of the sample datasets provided with the
#' multilevel-modelling software package MLwiN (Rasbash et al., 2009), and is a
#' subset of data from a much larger dataset of examination results from six
#' inner London Education Authorities (school boards). The original analysis
#' (Goldstein et al., 1993) sought to establish whether some secondary schools
#' had better student exam performance at 16 than others, after taking account
#' of variations in the characteristics of students when they started secondary
#' school; i.e., the analysis investigated the extent to which schools `added
#' value' (with regard to exam performance), and then examined what factors
#' might be associated with any such differences. See also Rasbash et al.
#' (2012) and Browne (2012).
#' 
#' @docType data
#' @format A data frame with 4059 observations on the following 10 variables:
#' \describe{ \item{list("school")}{Numeric school identifier.}
#' \item{list("student")}{Numeric student identifier.}
#' \item{list("normexam")}{Students' exam score at age 16, normalised to have
#' approximately a standard Normal distribution.} \item{list("cons")}{A column
#' of ones. If included as an explanatory variable in a regression model (e.g.
#' in MLwiN), its coefficient is the intercept.}
#' \item{list("standlrt")}{Students' score at age 11 on the London Reading Test
#' (LRT), standardised using Z-scores.} \item{list("sex")}{Sex of pupil; a
#' factor with levels \code{boy}, \code{girl}.} \item{list("schgend")}{Schools'
#' gender; a factor with levels corresponding to mixed school
#' (\code{mixedsch}), boys' school (\code{boysch}), and girls' school
#' (\code{girlsch}).} \item{list("avslrt")}{Average LRT score in school.}
#' \item{list("schav")}{Average LRT score in school, coded into 3 categories:
#' \code{low} = bottom 25\%, \code{mid} = middle 50\%, \code{high} = top 25\%.}
#' \item{list("vrband")}{Students' score in test of verbal reasoning at age 11,
#' a factor with 3 levels: \code{vb1} = top 25\%, \code{vb2} = middle 50\%,
#' \code{vb3} = bottom 25\%.} }
#' @seealso See \code{mlmRev} package for an alternative format of the same
#' dataset.
#' @source Browne, W. J. (2012) \emph{MCMC Estimation in MLwiN Version 2.26.}
#' University of Bristol: Centre for Multilevel Modelling.
#' 
#' Goldstein, H., Rasbash, J., Yang, M., Woodhouse, G., Pan, H., Nuttall, D.,
#' Thomas, S. (1993) A multilevel analysis of school examination results.
#' \emph{Oxford Review of Education}, \bold{19}, 425--433.
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
#' data(tutorial, package = "R2MLwiN")
#' 
#' # Fit 2-level variance components model, using IGLS (default estimation method)
#' (VarCompModel <- runMLwiN(normexam ~ 1 + (1 | school) + (1 | student), data = tutorial))
#' 
#' # print variance partition coefficient (VPC)
#' print(VPC <- coef(VarCompModel)[["RP2_var_Intercept"]] /
#'              (coef(VarCompModel)[["RP1_var_Intercept"]] +
#'              coef(VarCompModel)[["RP2_var_Intercept"]]))
#' 
#' # Fit same model using MCMC
#' (VarCompMCMC <- runMLwiN(normexam ~ 1 + (1 | school) + (1 | student),
#'  estoptions = list(EstM = 1), data = tutorial))
#' 
#' # return diagnostics for VPC
#' VPC_MCMC <- VarCompMCMC@chains[,"RP2_var_Intercept"] /
#'             (VarCompMCMC@chains[,"RP1_var_Intercept"] +
#'             VarCompMCMC@chains[,"RP2_var_Intercept"])
#' sixway(VPC_MCMC, name = "VPC")
#' 
#' # Adding predictor, allowing its coefficient to vary across groups (i.e. random slopes)
#' (standlrtRS_MCMC <- runMLwiN(normexam ~ 1 + standlrt + (1 + standlrt | school) + (1 | student),
#'  estoptions = list(EstM = 1), data = tutorial))
#' 
#' # Example modelling complex level 1 variance
#' # fit log of precision at level 1 as a function of predictors
#' (standlrtC1V_MCMC <- runMLwiN(normexam ~ 
#'   1 + standlrt + (school | 1 + standlrt) + (1 + standlrt | student),
#'   estoptions = list(EstM = 1, mcmcMeth = list(lclo = 1)),
#'   data = tutorial))
#' 
#' }
#' 
"tutorial"