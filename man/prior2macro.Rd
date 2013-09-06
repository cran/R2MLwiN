\name{prior2macro}
\alias{prior2macro}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Translates informative prior information into a concise MLwiN macro.
}
\description{
An R list object containing informative prior information for a multilevel model is translated into a concise vector object to be used in an MLwiN macro.
}
\usage{
prior2macro(prior, formula, levID, D, indata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{prior}{
An R list object containing prior information for a multilevel model. See Value below.
}
  \item{formula}{
A character string specifying the model formula. See \code{\link{Formula.translate}}.
}
  \item{levID}{
A character (vector) specifying the level ID(s).
}
  \item{D}{
A character string/vector specifying the distribution of the current model.
}
  \item{indata}{
A data.frame object containing the variables to be modelled.
}
}

\value{
\item{prior}{Contains the prior information for a multilevel model. Also see the examples in \code{demo(Chapter06)}.}
\item{fixe}{For the fixed parameters, if proper normal priors are used for some parameters, a list of vectors of length two is provided, each of which specifies the mean and the standard deviation. If not given, default ('flat' or 'diffuse') priors are used for the parameters.}
\item{fixe.common}{For multivariate normal, multinomial and mixed response models, if common coefficients are added, use \code{fixe.common} rather than \code{fixe}.}
\item{fixe.sep}{If the common coefficients are added, use \code{fixe.sep} for the separate coefficients.}
\item{rp1}{A list object specifying the Wishart or gamma prior for the covariance matrix or scalar variance at level 1. Consists of: (1) \code{estimate} -- an estimate for the true value of the inverse of the covariance matrix; (2) \code{size} -- the number of rows in the covariance matrix. Note that this is a weakly-informative prior and the default prior is used if missing.}
\item{rp2}{A list object specifying the Wishart or gamma prior for the covariance matrix or scalar variance at level 2. Consists of: (1) \code{estimate} -- an estimate for the true value of the inverse of the covariance matrix; (2) \code{size} -- the number of rows in the covariance matrix. Note that this is a weakly-informative prior and the default prior is used if missing.}
\item{......}{}

A long vector is returned in the format of MLwiN macro language. This includes all the specified prior parameters.
}
\references{
MCMC estimation in MLwiN Version 2.25. Browne, W.J. (2012) Centre for Multilevel Modelling, University of Bristol.
}
\author{
Zhang, Z., Charlton, C.M.J., Parker, R.M.A., Leckie, G., and Browne, W.J. (2012) Centre for Multilevel Modelling, University of Bristol.
}


\seealso{
\code{\link{runMLwiN}}
}

