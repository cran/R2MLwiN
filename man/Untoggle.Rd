\name{Untoggle}
\alias{Untoggle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converts a categorical variable into several separate binary variables
}
\description{
This function converts a vector (factor) of categorical character strings (integers) into several separate vectors of binary indicators.
}
\usage{
Untoggle(categrv, name = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{categrv}{
A vector (factor) of categorical character strings (integers).
}
  \item{name}{
A character string specifying a name of the categorical variable. Note that this option is only used when the categorical labels are integers.
}
}

\author{
Zhang, Z., Charlton, C.M.J., Parker, R.M.A., Leckie, G., and Browne, W.J. (2012) Centre for Multilevel Modelling, University of Bristol.
}

\examples{
\dontrun{
library(R2MLwiN)
## Modify the following paths as appropriate.
## MLwiN folder
mlwin = "C:/Program Files (x86)/MLwiN v2.29/"
## MLwiN sample worksheet folder 
wspath=paste(mlwin,"/samples/",sep="")
wsfile=paste(wspath,"tutorial.ws",sep="")
inputfile=paste(tempdir(),"/tutorial.dta",sep="")
ws2foreign(wsfile, foreignfile=inputfile, MLwiNPath=mlwin)
library(foreign);indata =read.dta(inputfile)
names(indata)
indata=cbind(indata,Untoggle(indata[["school"]],"school"))
names(indata)
}
}

