\docType{data}
\name{jspmix1}
\alias{jspmix1}
\title{Dataset of pupils' test scores, a subset of the Junior School Project.}

\format{A data frame with 1119 observations on the following 8 variables:
  \describe{
    \item{\code{school}}{School identifying code.}
    \item{\code{id}}{Pupil identifying code.}
    \item{\code{sex}}{Sex of pupil; levels are \code{female} and \code{male}.}
    \item{\code{fluent}}{Fluency in English indicator, where \code{0} = beginner, \code{1} = intermediate, \code{2} = fully fluent; measured in Year 1.}
    \item{\code{ravens}}{Test score, out of 40; measured in Year 1.}
    \item{\code{english}}{Pupils' English test score, out of 100; measured in Year 3.}
    \item{\code{behaviour}}{Pupils' behaviour score, where \code{lowerquarter} = pupil rated in bottom 25\%, and \code{upper} otherwise; measured in Year 3.}
    \item{\code{cons}}{A column of ones. If included as an explanatory variable in a regression model (e.g. in MLwiN), its coefficient is the intercept.}
}}
\source{
Browne, W. J. (2012) \emph{MCMC Estimation in MLwiN Version 2.26.} University of Bristol: Centre for Multilevel Modelling.

Mortimore, P., Sammons, P., Stoll, L., Lewis, D., Ecob, R. (1988) \emph{School Matters}. Wells: Open Books.

Rasbash, J., Browne, W. J., Healy, M., Cameron, B., Charlton, C. M. J. (2013) MLwiN v2.27. University of Bristol: Centre for Multilevel Modelling.
}

\description{
  An educational dataset of pupils' test scores, a subset of the Junior School Project (Mortimore et al., 1988).}
\details{
  A subset of the Junior School Project (Mortimore et al., 1988), the \code{jspmix1} dataset is one of the sample datasets provided with the multilevel-modelling software package MLwiN (Rasbash et al., 2013), and is used in Browne (2012) as an example of modelling mixed responses. It consists of test scores for 1119 pupils across 47 schools. Note that the \code{behaviour} variable originally had three categories, and the middle 50\% and top 25\% have been combined to produce a binary variable.)}

\examples{
\dontrun{
# NB: change path as appropriate
MLwiN <- "C:/Program Files (x86)/MLwiN v2.27/"
data(jspmix1)
# behaviour coded 0/1
jspmix1$behaviour <- as.numeric(jspmix1$behaviour) - 1

# fit multilevel mixed response model, modelling effect of sex
# and ravens on both responses, and fluent on english response only
F1 = "c(english, probit(behaviour, cons)) ~ (0s|cons + sex + ravens) 
+ (0c|fluent{1, 0}) + (2s|cons) + (1s|cons.english)"
ID = c("school", "id")
(MixedRespMCMC <- runMLwiN(Formula = F1, levID = ID, 
D = c("Mixed", "Normal", "Binomial"), indata = jspmix1, 
estoptions = list(EstM = 1, mcmcMeth = list(fixM = 1, residM = 1, Lev1VarM = 1)),
MLwiNPath = MLwiN))
}
}
\keyword{datasets}

