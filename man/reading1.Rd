\docType{data}
\name{reading1}
\alias{reading1}
\title{Students' reading attainment in inner London infant schools.}

\format{A data frame with 407 observations on the following 13 variables:
  \describe{
    \item{\code{id}}{Unique pupil identifying code.}
    \item{\code{age1}}{Age at occasion 1.}
    \item{\code{read1}}{Reading score at occasion 1.}
    \item{\code{age2}}{Age at occasion 2.}
    \item{\code{read2}}{Reading score at occasion 2.}
    \item{\code{age3}}{Age at occasion 3.}
    \item{\code{read3}}{Reading score at occasion 3.}
    \item{\code{age4}}{Age at occasion 4.}
    \item{\code{read4}}{Reading score at occasion 4.}
    \item{\code{age5}}{Age at occasion 5.}
    \item{\code{read5}}{Reading score at occasion 5.}
    \item{\code{age6}}{Age at occasion 6.}
    \item{\code{read6}}{Reading score at occasion 6.}

}}
\source{
Rasbash, J., Charlton, C., Browne, W.J., Healy, M. and Cameron, B. (2009) \emph{MLwiN Version 2.1.} Centre for Multilevel Modelling, University of Bristol.
Rasbash, J., Steele, F., Browne, W.J., Goldstein, H. (2012) \emph{A User's Guide to MLwiN v2.26}. University of Bristol: Centre for Multilevel Modelling.
Tizard, B., Blatchford, P., Burke, J. & Farquhar, C. (1988). Young children at school in the inner city. Hove, Sussex: Lawrence Erlbaum.
}

\description{
  Reading score data for 407 pupils across 6 occasions.}
\details{
  The \code{reading1} dataset is one of the sample datasets provided with the multilevel-modelling software package MLwiN (Rasbash et al., 2009), and was analysed in Tizard et al. (1988); see also Rasbash et al. (2012) for further details.}

\examples{
\dontrun{
# from demo(UserGuide13)

data(reading1, package = "R2MLwiN")
summary(reading1)

reading1[reading1 == -10] <- NA

summary(reading1)

reading <- reshape(reading1, idvar = "student", timevar = "id",
                   varying = c("read1", "age1", "read2", "age2", "read3", "age3",
                   "read4", "age4", "read5", "age5", "read6", "age6"),
                   sep = "", direction = "long")

reading <- reading[c("student", "id", "age", "read")]
reading <- reading[order(reading$student, reading$id), ]

colnames(reading) <- c("student", "occasion", "age", "reading")
rownames(reading) <- NULL

summary(reading)

head(reading, 5)

tab <- aggregate(reading ~ occasion, reading,
                 function(x) c(N = length(x), mean = mean(x), sd = sd(x)))
tab <- rbind(tab, c(NA, NA))
tab$reading[7, ] <- c(length(na.omit(reading$reading)),
                      mean(na.omit(reading$reading)),
                      sd(na.omit(reading$reading)))
rownames(tab)[7] <- "Total"
tab

tab <- aggregate(age ~ occasion, reading,
                 function(x) c(N = length(x), mean = mean(x), sd = sd(x)))
tab <- rbind(tab, c(NA, NA))
tab$age[7, ] <- c(length(na.omit(reading$age)),
                  mean(na.omit(reading$age)),
                  sd(na.omit(reading$age)))
rownames(tab)[7] <- "Total"
tab

(mymodel1 <- runMLwiN(reading ~ 1 + (1 | student) + (1 | occasion),
                      data = reading))
}
}
\keyword{datasets}

