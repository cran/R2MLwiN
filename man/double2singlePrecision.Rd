\name{double2singlePrecision}
\alias{double2singlePrecision}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converts numerical values from double precision to single precision.
}
\description{
This function converts some column(s) of a data frame object, matrix or vector from double precision to single precision. 
}
\usage{
double2singlePrecision(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A data frame object, matrix or vector to be converted. Some column(s) of these objects will be ignored during conversion if they are not numeric (or double precision).
}
}

\value{
An object of numerical values in single precision will be returned.
}

\author{
Zhang, Z., Charlton, C.M.J., Parker, R.M.A., Leckie, G.B., and Browne, W.J. (2012) Centre for Multilevel Modelling, University of Bristol.
}
