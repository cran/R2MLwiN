% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BD.R
\name{BD}
\alias{BD}
\title{Calculates Brooks-Draper diagnostic}
\usage{
BD(est, var, rho, k = 2, alpha = 0.05)
}
\arguments{
\item{est}{Numeric scalar for the mean of the distribution}

\item{var}{Numeric scalar for the variance of the distribution}

\item{rho}{The first lag (i.e. after zero) of the auto-correlation function (ACF) diagnostic}

\item{k}{Integer scalar corresponding to the number of significant figures (defaults to \code{2})}

\item{alpha}{Numeric scalar indicating the desired accuracy (defaults to \code{0.05})}
}
\value{
The Brooks-Draper diagnostic statistic is returned.
}
\description{
An internal function, for use in \code{\link{sixway}}, which calculates the
Brooks-Draper diagnostic, based on an unpublished paper by David Draper. It
estimates the length of a Markov chain required to produce a mean estimate
to k significant figures with a given accuracy (alpha). See Browne (2012)
for further details.
}
\references{
Browne, W.J. (2012) MCMC Estimation in MLwiN, v2.26.
Centre for Multilevel Modelling, University of Bristol.
}
\seealso{
\code{\link{sixway}}
}
\author{
Zhang, Z., Charlton, C.M.J., Parker, R.M.A., Leckie, G., and Browne,
W.J. (2016) Centre for Multilevel Modelling, University of Bristol.
}
