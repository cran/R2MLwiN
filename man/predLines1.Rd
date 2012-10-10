\name{predLines1}
\alias{predLines1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draws predicted lines using model estimates
}
\description{
This function draws predicted lines against an explanatory variable for selected groups at a higher level.
}
\usage{
predLines1(indata, FP, resi, xname, lev = 2, levID, selected = NULL, Legend=T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{indata}{
A data.frame object containing the data modelled.
}
  \item{FP}{
A vector of the fixed part estimates.
}
  \item{resi}{
The name of the file where the estimates of residuals are saved.
}
  \item{xname}{
A name of an explanatory variable.
}
  \item{lev}{
A digit indicating the level (of the multilevel model) at which to plot.
}
  \item{levID}{
A character (vector) specifying the level ID(s).
}
  \item{selected}{
A vector representing a group of selected members at a level. If \code{selected=NULL}, then all members at the level are included.
}
  \item{Legend}{
A logical value indicating whether a legend is to be added.
}
}

\author{
Zhang, Z., Charlton, C.M.J., Parker, R.M.A., Leckie, G.B., and Browne, W.J. (2012) Centre for Multilevel Modelling, University of Bristol.
}

\seealso{
\code{\link{predLines2}}
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
estoptions= list(EstM=0,resi.store=TRUE)
wsfile=paste(wspath,"tutorial.ws",sep="")
inputfile=paste(tempdir(),"/tutorial.dta",sep="")
ws2foreign(wsfile, foreignfile=inputfile, MLwiNPath=mlwin)
library(foreign);indata =read.dta(inputfile)
mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions,MLwiNPath=mlwin)

resi=mymodel["residual"]
FP=mymodel["FP"]
predLines1(indata,FP,resi=resi,xname="standlrt", lev=2, levID, selected=c(30,44,53,59))
}
}
