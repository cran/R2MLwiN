\docType{data}
\name{wage1}
\alias{wage1}
\title{Simulated dataset of office workers' salay and other employment details.}

\format{A data frame with 3022 observations on the following 21 variables:
  \describe{
    \item{\code{id}}{Unique office worker identifying code.}
    \item{\code{company}}{Identifying code for company worked for over the last 12 months.}
    \item{\code{company2}}{If worked for >1 company over the last 12 months, identifying code for second company.}
    \item{\code{company3}}{If worked for >2 company over the last 12 months, identifying code for third company.}
    \item{\code{company4}}{If worked for >3 company over the last 12 months, identifying code for fourth company.}
    \item{\code{age}}{Age of worker.}
    \item{\code{parttime}}{Part or full-time, with levels \code{Fulltime} and \code{Parttime}.}
    \item{\code{sex}}{Sex of worker, with levels \code{male} and \code{female}.}
    \item{\code{cons}}{A column of ones. If included as an explanatory variable in a regression model (e.g. in MLwiN), its coefficient is the intercept.}
    \item{\code{earnings}}{Workers' earnings over the last financial year.}
    \item{\code{logearn}}{Workers' (natural) log-transformed earnings over the last financial year.}
    \item{\code{numjobs}}{The number of companies worked for over the last 12 months.)}
    \item{\code{weight1}}{Proportion of time worked for employer listed in \code{company}.}
    \item{\code{weight2}}{Proportion of time worked for employer listed in \code{company2}.}
    \item{\code{weight3}}{Proportion of time worked for employer listed in \code{company3}.}
    \item{\code{weight4}}{Proportion of time worked for employer listed in \code{company4}.}
    \item{\code{ew1}}{Alternative (equal) weighting for \code{company} (1/\code{numjobs}).}
    \item{\code{ew2}}{Alternative (equal) weighting for \code{company2} (if numjobs >1 then 1/\code{numjobs}, else 0).}
    \item{\code{ew3}}{Alternative (equal) weighting for \code{company3} (if numjobs >2 then 1/\code{numjobs}, else 0).}
    \item{\code{ew4}}{Alternative (equal) weighting for \code{company4} (if numjobs >3 then 1/\code{numjobs}, else 0).}
    \item{\code{age_40}}{Age of worker, centered on 40 years.}    
}}
\source{
Browne, W. J. (2012) \emph{MCMC Estimation in MLwiN Version 2.26.} University of Bristol: Centre for Multilevel Modelling.

Rasbash, J., Browne, W. J., Healy, M., Cameron, B., Charlton, C. M. J. (2013) MLwiN v2.27. University of Bristol: Centre for Multilevel Modelling.
}

\description{
  A simulated dataset of office workers' salary (and associated information) in which workers exhibit multiple membership of companies worked for over past year.}
\details{
  The simulated \code{wage1} dataset is one of the sample datasets provided with the multilevel modelling software package MLwiN (Rasbash et al., 2013). It consists of salary and associated information for office workers, and is used by Browne (2012) as an example of modelling a multiple membership structure. The dataset exhibits multiple membership in that workers are clustered across the companies employing them over the past year, but some have worked for more than one company during that time.)}

\examples{
\dontrun{
# NB: change path as appropriate
MLwiN <- "C:/Program Files (x86)/MLwiN v2.29/"
data(wage1)
# fit age_40 and numjobs as fixed effects, with random effects for companies, 
# in multiple membership model
F1 = "logearn ~ (0|cons + age_40 + numjobs) + (1|cons) + (2|cons)"
ID = c("company", "id")
OurMultiMemb <- list("class" = 2, "N1" = 4, "weight" = "weight1", "id" = NA)
(MMembModel <- runMLwiN(Formula = F1, levID = ID, indata = wage1, 
estoptions = list(EstM = 1, xclass = OurMultiMemb, notation = "class"), MLwiNPath = MLwiN))
}
}
\keyword{datasets}

