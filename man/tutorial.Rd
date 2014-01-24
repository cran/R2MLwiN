\docType{data}
\name{tutorial}
\alias{tutorial}
\title{Exam results for six inner London Education Authorities}
\format{A data frame with 4059 observations on the following 10 variables:
  \describe{
    \item{\code{school}}{Numeric school identifier.}
    \item{\code{student}}{Numeric student identifier.}
    \item{\code{normexam}}{Students' exam score at age 16, normalised to have approximately a standard Normal distribution.}
    \item{\code{cons}}{A column of ones. If included as an explanatory variable in a regression model (e.g. in MLwiN), its coefficient is the intercept.}
    \item{\code{standlrt}}{Students' score at age 11 on the London Reading Test (LRT), standardised using Z-scores.}
    \item{\code{sex}}{Sex of pupil; levels are \code{boy}, \code{girl}.}
    \item{\code{schgend}}{Schools' gender; levels correspond to mixed school (\code{mixedsch}), boys' school (\code{boysch}), and girls' school (\code{girlsch}).}
    \item{\code{avslrt}}{Average LRT score in school.}
    \item{\code{schav}}{Average LRT score in school, coded into 3 categories: \code{low} = bottom 25\%, \code{mid} = middle 50\%, \code{high} = top 25\%.}
    \item{\code{vrband}}{Students' score in test of verbal reasoning at age 11, coded into 3 categories: \code{vb1} = top 25\%, \code{vb2} = middle 50\%, \code{vb3} = bottom 25\%.}
  }}
\source{
  Goldstein, H., Rasbash, J., Yang, M., Woodhouse, G., Pan, H., Nuttall, D., Thomas, S. (1993) A multilevel analysis of school examination results. \emph{Oxford Review of Education}, \bold{19}, 425--433.
  
  Rasbash, J., Browne, W. J., Healy, M., Cameron, B., Charlton, C. M. J. (2013) MLwiN v2.27. University of Bristol: Centre for Multilevel Modelling.
}

\description{
  A subset of data from a much larger dataset of examination results from six inner London Education Authorities (school boards).}
\details{
  The \code{tutorial} dataset is one of the sample datasets provided with the multilevel-modelling software package MLwiN (Rasbash et al., 2013), and is a subset of data from a much larger dataset of examination results from six inner London Education Authorities (school boards). The original analysis (Goldstein et al., 1993) sought to establish whether some secondary schools had better student exam performance at 16 than others, after taking account of variations in the characteristics of students when they started secondary school; i.e., the analysis investigated the extent to which schools `added value' (with regard to exam performance), and then examined what factors might be associated with any such differences.}

\seealso{
See \code{mlmRev} package for an alternative format of the same dataset.
}

\examples{
\dontrun{
# NB: hange path as appropriate
MLwiN <- "C:/Program Files (x86)/MLwiN v2.29/"
data(tutorial)

# Fit 2-level variance components model, using IGLS (default estimation method)
F1 <- "normexam ~ (0|cons) + (2|cons) + (1|cons)"
ID <- c("school", "student")
(VarCompModel <- runMLwiN(Formula = F1, levID = ID, indata = tutorial, MLwiNPath = MLwiN))
# print variance partition coefficient (VPC)
print(VPC <- VarCompModel["RP"][["RP2_var_cons"]] / (VarCompModel["RP"][["RP1_var_cons"]] + 
VarCompModel["RP"][["RP2_var_cons"]]))

# Fit same model using MCMC
(VarCompMCMC <- runMLwiN(Formula = F1, levID = ID, indata = tutorial, 
estoptions = list(EstM = 1), MLwiNPath = MLwiN))
# return diagnostics for VPC
VPC_MCMC <- VarCompMCMC["chains"][,"RP2_var_cons"] / (VarCompMCMC["chains"][,"RP1_var_cons"] + 
VarCompMCMC["chains"][,"RP2_var_cons"])
sixway(VPC_MCMC, name = "VPC")

# Adding predictor, allowing its coefficient to vary across groups (i.e. random slopes)
F2 <- "normexam ~ (0|cons + standlrt) + (2|cons + standlrt) + (1|cons)"
(standlrtRS_MCMC <- runMLwiN(
Formula = F2, levID = ID, indata = tutorial,
estoptions = list(EstM = 1), MLwiNPath = MLwiN))

# Example modelling complex level 1 variance
F3 <- "normexam ~ (0|cons + standlrt) + (2|cons + standlrt) + (1|cons + standlrt)"
standlrtC1V_MCMC <- runMLwiN (
Formula = F3, levID = ID, indata = tutorial,
estoptions = (list(EstM = 1,
#fit log of precision at level 1 as a function of predictors
mcmcMeth = list(lclo = 1))), 
MLwiNPath = MLwiN)
}
}
\keyword{datasets}

