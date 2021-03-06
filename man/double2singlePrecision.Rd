% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/double2singlePrecision.R
\name{double2singlePrecision}
\alias{double2singlePrecision}
\title{Converts numerical values from double precision to single precision.}
\usage{
double2singlePrecision(x)
}
\arguments{
\item{x}{A \code{\link[base]{data.frame}} object, matrix or vector to be converted. Column(s)
of these objects will be ignored during conversion if they are not
numeric.}
}
\value{
An object of numerical values in single precision will be returned.
}
\description{
This function converts numeric column(s) of a data frame object, matrix or
vector from double precision to single precision, e.g. to avoid a warning
from MLwiN which currently only stores data in single precision.
}
\author{
Zhang, Z., Charlton, C.M.J., Parker, R.M.A., Leckie, G., and Browne,
W.J. (2016) Centre for Multilevel Modelling, University of Bristol.
}
