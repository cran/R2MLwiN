\name{sixway}
\alias{sixway}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draws a sixway plot of MCMC diagnostics.
}
\description{
This function produces a variety of diagnostic plots and statistics for MCMC chains.
}
\usage{
sixway(chain, name=NULL, acf.maxlag=100, pacf.maxlag=10, thinning=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{chain}{
A numeric vector storing the MCMC chain for a chosen parameter.
}
  \item{name}{
The parameter name. If \code{name=NULL}, the parameter name is "x" by default.
}
  \item{acf.maxlag}{
Maximum lag at which to calculate the auto-correlation function. \code{acf.maxlag=100} by default.
}
  \item{pacf.maxlag}{
Maximum lag at which to calculate the partial auto-correlation function. \code{pacf.maxlag=10} by default.
}
  \item{thinning}{
An integer specifying the frequency with which successive values in the Markov chain are stored. By default \code{thinning=1}.
}
}

\value{
A variety of plots and statistics are displayed in an R graphic window, including the following:
\item{trace plot}{the plotted trajectory of an MCMC chain for a model parameter (all the chain's iterations all plotted);}
\item{kernel density plot}{kernel density estimates are computed using \code{\link{density}};}
\item{autocorrelation function}{the function \code{\link{acf}} is used to compute and plot estimates of the autocorrelation function;}
\item{partial autocorrelation function}{the function \code{\link{pacf}} computes and plots estimates of the partial autocorrelation function;}
\item{Monte Carlo standard error}{the estimated Monte Carlo standard error (\code{MCSE}) of the posterior estimate of the mean is plotted against the number of iterations;}
\item{accuracy diagnostics}{the box contains two contrasting accuracy diagnostics. The Raftery-Lewis diagnostic (\code{\link[coda]{raftery.diag}}) is a diagnostic based on a particular quantile of the distribution. The diagnostic Nhat is used to estimate the length of Markov chain required to estimate a particular quantile (e.g. the 2.5\% and 97.5\% quantiles) to a given accuracy. The Brooks-Draper diagnostic (\code{BD}) is a diagnostic based on the mean of the distribution; it is used to estimate the length of Markov chain required to produce a mean estimate to k(=2) significant figures with a given accuracy;}
\item{summary statistics}{this box provides summary statistics including the posterior mean, sd, mode, quantiles and the effective sample size (ESS) of the chain.}
}

\author{
Zhang, Z., Charlton, C.M.J., Parker, R.M.A., Leckie, G.B., and Browne, W.J. (2012) Centre for Multilevel Modelling, University of Bristol.
}

\seealso{
\code{\link{density}},\code{\link{acf}},\code{\link{pacf}},\code{\link[coda]{raftery.diag}},\code{\link[coda]{effectiveSize}}
}
\examples{
\dontrun{
library(R2MLwiN)
## Modify the following paths as appropriate.
## MLwiN folder
mlwin = "C:/Program Files (x86)/MLwiN v2.26/"
## MLwiN sample worksheet folder
wspath=paste(mlwin,"/samples/",sep="")

## Example: Normal
formula="normexam~(0|cons+standlrt)+(2|cons+standlrt)+(1|cons)"
levID=c('school','student')
estoptions= list(EstM=1,resi.store.levs=2)
wsfile=paste(wspath,"tutorial.ws",sep="")
inputfile=paste(tempdir(),"/tutorial.dta",sep="")
ws2foreign(wsfile, foreignfile=inputfile, MLwiNPath=mlwin)
library(foreign);indata =read.dta(inputfile)
mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions,MLwiNPath=mlwin)

chain=mymodel["chains"][,"FP_standlrt"]
sixway(chain,"beta_1")
}
}
