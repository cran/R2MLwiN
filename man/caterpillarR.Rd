\name{caterpillarR}
\alias{caterpillarR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draws caterpillar plots of the residuals at a particular level of a multilevel model.
}
\description{
Uses \code{qqmath} in the `lattice' package to draw Quantile-Quantile plots of the residuals at a particular level of a multilevel model against a theoretical distribution.
}
\usage{
## S3 method for class `numeric'
caterpillarR(resi, lev = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resi}{
Either a character string indicating where the residuals are stored or a data.frame object containing the residuals.
}
  \item{lev}{
A digit name indicating which level of a multilevel model residuals are to be plotted for.
}
}

\value{
A caterpillar plot of the residuals at the requested level of the random part of a multilevel model.
}

\author{
Zhang, Z., Charlton, C.M.J., Parker, R.M.A., Leckie, G., and Browne, W.J. (2012) Centre for Multilevel Modelling, University of Bristol.
}


\seealso{
\code{\link[lattice]{qqmath}}
}
\examples{
\dontrun{
library(R2MLwiN)
## Modify the following paths as appropriate.
## MLwiN folder
mlwin = "C:/Program Files (x86)/MLwiN v2.27/"
## MLwiN sample worksheet folder
wspath=paste(mlwin,"/samples/",sep="")

## Example: Normal
formula="normexam~(0|cons)+(2|cons)+(1|cons)"
levID=c('school','student')
estoptions= list(resi.store=TRUE)
wsfile=paste(wspath,"tutorial.ws",sep="")
inputfile=paste(tempdir(),"/tutorial.dta",sep="")
ws2foreign(wsfile, foreignfile=inputfile, MLwiNPath=mlwin)
library(foreign);indata =read.dta(inputfile)
mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions,MLwiNPath=mlwin)

## Caterpillar plot
caterpillarR(mymodel["residual"], lev=2)
}
}

