\docType{data}
\name{bang1}
\alias{bang1}
\title{Sub-sample from the 1989 Bangladesh Fertility Survey}
\format{A data frame with 1934 observations on the following 11 variables:
  \describe{
    \item{\code{woman}}{Identifying code for each woman (level 1 unit).}
    \item{\code{district}}{Identifying code for each district (level 2 unit).}
    \item{\code{use}}{Contraceptive use status at time of survey; levels are \code{Not using} and \code{Using}.}
    \item{\code{lc}}{Number of living children at time of survey; levels correspond to none (\code{nokids}), one (\code{onekid}), two (\code{twokids}), or three or more children (\code{threeplus}).}
    \item{\code{age}}{Age of woman at time of survey (in years), centred on sample mean of 30 years.}
    \item{\code{urban}}{Type of region of residence; levels are \code{Rural} and \code{Urban}.}
    \item{\code{educ}}{Woman's level of education (\code{1} = None, \code{2} = Lower primary, \code{3} = Upper primary, \code{4} = Secondary and above.}
    \item{\code{hindu}}{Woman's religion; levels are \code{Muslim} and \code{Hindu}.}
    \item{\code{d_illit}}{Proportion of women in district who are literate.}
    \item{\code{d_pray}}{Proportion of Muslim women in district who pray every day (a measure of religiosity).}    
    \item{\code{cons}}{A column of ones. If included as an explanatory variable in a regression model (e.g. in MLwiN), its coefficient is the intercept.}
}}
\source{
Amin, S., Diamond, I., Steele, F. (1997) Contraception and religiosity in Bangladesh. In: G. W. Jones, J. C. Caldwell, R. M. Douglas, R. M. D'Souza (eds) \emph{The Continuing Demographic Transition}, 268--289. Oxford: Oxford University Press.

Browne, W. J. (2012) \emph{MCMC Estimation in MLwiN Version 2.26.} University of Bristol: Centre for Multilevel Modelling.

Huq, N. M., Cleland, J. (1990) \emph{Bangladesh fertility survey, 1989.} Dhaka: National Institute of Population Research and Training (NIPORT).

Rasbash, J., Browne, W. J., Healy, M., Cameron, B., Charlton, C. M. J. (2013) MLwiN v2.27. University of Bristol: Centre for Multilevel Modelling.
}

\description{
  A subset of data from the 1989 Bangladesh Fertility Survey, consisting of 1934 women across 60 districts.}
\details{
  The \code{bang1} dataset is one of the sample datasets provided with the multilevel-modelling software package MLwiN (Rasbash et al., 2013), and is a subset of data from the 1989 Bangladesh Fertility Survey (Huq and Cleland, 1990) used by Browne (2012) as an example when fitting logistic models for binary and binomial responses. The full sample was analysed in Amin et al. (1997).}

\seealso{
See \code{mlmRev} package for an alternative format of the same dataset, with fewer variables.
}

\examples{
\dontrun{
# NB: change path as appropriate
MLwiN <- "C:/Program Files (x86)/MLwiN v2.29/"
data(bang1)
bang1$use <- as.numeric(bang1$use) - 1
bang1$urban <- as.numeric(bang1$urban) - 1

# Fit 2-level random coefficient logistic model, using MCMC
# cons (constant of ones) as denominator specifies
# Bernoulli distribution (0/1 response)
F1 = "logit(use, cons) ~ (0|cons + age + lc[nokids] + urban) + (2|cons + urban)"
ID = c("district", "woman")
binomialMCMC <- runMLwiN(Formula = F1, levID = ID, D = "Binomial", 
indata = bang1, estoptions = list(EstM = 1), MLwiNPath = MLwiN)
}
}
\keyword{datasets}

