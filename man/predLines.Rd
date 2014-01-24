\name{predLines}
\alias{predLines}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draws predicted lines using a fitted model object
}
\description{
This function draws predicted lines against an explanatory variable for selected groups at a higher (>=2) level.
}
\usage{
predLines(object, indata, xname, lev=2, selected=NULL, probs=c(.025,.975), 
legend=TRUE, legend.space="top", legend.ncol=4, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Either "mlwinfitIGLS" or "mlwinfitMCMC" class object.
}
  \item{indata}{
An object (converts to data.frame) containing the data modelled.
}
  \item{xname}{
A name of an explanatory variable.
}
  \item{lev}{
A digit indicating the level (of the multilevel model) at which to plot.
}
  \item{selected}{
A vector specifying groups to selectively plot at the level specified in \code{lev}. If \code{selected=NULL}, then all groups at that level are 
included.
}
  \item{probs}{
A numeric vector of probabilities with values in \code{(0,1)} used to calculate the lower and upper quantiles from which the error bars are 
plotted. Currently, this is only available for a "mlwinfitIGLS" object.
}
  \item{legend}{
A logical value indicating whether a legend is to be added.
}
  \item{legend.space}{
A character string specifies one of the four sides, which can be one of "top", "bottom", "left" and "right". 
Default, \code{legend.space="top"}.
}
  \item{legend.ncol}{
An integer specifies a number of columns, possibly divided into blocks, each containing some rows. Default, \code{legend.ncol=2}.
}
  \item{...}{
Other arguments to be pased to \code{\link[lattice]{xyplot}}.
}
}

\author{
Zhang, Z., Charlton, C.M.J., Parker, R.M.A., Leckie, G., and Browne, W.J. (2012) Centre for Multilevel Modelling, University of Bristol.
}

\seealso{
\code{\link{predCurves}}
}


\examples{
\dontrun{
library(R2MLwiN)
## Modify the following paths as appropriate.
## MLwiN folder
mlwin = "C:/Program Files (x86)/MLwiN v2.29/"
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

predLines(mymodel, indata, xname="standlrt", lev = 2,
selected =c(30,44,53,59), probs=c(.025,.975))
}
}
