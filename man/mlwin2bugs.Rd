\name{mlwin2bugs}
\alias{mlwin2bugs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
This function captures output files from MLwiN for estimation in WinBUGS/OpenBUGS.
}
\description{
This function allows R to call WinBUGS using the output files from MLwiN. This function uses functionalities in the \code{\link[rbugs]{rbugs}} package.
}
\usage{
mlwin2bugs(D, levID, datafile, initfile, modelfile, bugEst, fact, addmore, 
n.chains, n.iter, n.burnin, n.thin, debug = T, bugs, 
bugsWorkingDir = tempdir(), OpenBugs = F, cleanBugsWorkingDir = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{D}{
  A vector specifying the type of distribution used in the model.
}
  \item{levID}{
A character (vector) specifies the level ID(s).
}
  \item{datafile}{
A file name where the WinBUGS data file will be saved in .txt format.
}
  \item{initfile}{
A file name where the WinBUGS initial values will be saved in .txt format.
}
  \item{modelfile}{
A file name where the WinBUGS model will be saved in .txt format.
}
  \item{bugEst}{
A file name where the estimates from BUGS will be stored in .txt format.
}
  \item{fact}{
A list of objects used to specify factor analysis. See Value below.
}
\item{addmore}{
A vector of strings specifying additional coefficients to be monitored.
}
  \item{n.chains}{
The number of chains to be monitored. This should match up with the number of sets of initial values given.
}
  \item{n.iter}{
The number of iterations for each chain
}
  \item{n.burnin}{
The length of burn-in for each chain
}
  \item{n.thin}{
Thinning rate
}
  \item{debug}{
A logical value indicating whether (\code{TRUE}) or not (\code{FALSE}) to close the BUGS window after completion of the  model run
}
  \item{bugs}{
The full name (including the path) of the BUGS executable
}
  \item{bugsWorkingDir}{
A directory where all the intermediate files are to be stored.
}
  \item{OpenBugs}{
If \code{TRUE}, OpenBUGS is used, if \code{FALSE} (the default) WinBUGS is used.
}
  \item{cleanBugsWorkingDir}{
if \code{TRUE}, the generated files will be removed from the \code{bugsWorkingDir}.
}
}

\value{
A list of objects to specify factor analysis, as used in the argument \code{fact}:
\item{nfact}{Specifies the number of factors}
\item{lev.fact}{Specifies the level/classification for the random part of the factor for each factor.}
\item{nfactor}{Specifies the number of correlated factors}
\item{factor}{A vector specifying the correlated factors}
\tabular{ll}{
(1)\tab  the first factor number; \cr
(2)\tab  the second factor number; \cr
(3)\tab  the starting value for the covariance and\cr
(4)\tab  an indicator of whether this covariance is constrained (\code{1}) or not (\code{0}).\cr
}
\item{loading}{A matrix specifying the starting values for the factor loadings and the starting value of the factor variance. Each row corresponds to a factor.}
\item{constr}{A matrix specifying indicators of whether the factor loadings and the factor variance are constrained (\code{1}) or not (\code{0}).}
}

\author{
Zhang, Z., Charlton, C.M.J., Parker, R.M.A., Leckie, G.B., and Browne, W.J. (2012) Centre for Multilevel Modelling, University of Bristol.
}
\note{
This function is derived from \code{\link[rbugs]{rbugs}} (written by Jun Yan and Marcos Prates).
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{runMLwiN}},\code{\link[rbugs]{rbugs}}
}
