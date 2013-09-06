\name{trajectories}
\alias{trajectories}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots MCMC chain trajectories
}
\description{
This function draws trajectories of MCMC chains.
}
\usage{
trajectories(chains, Range = c(1, 5000), selected = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{chains}{
A data frame of parameter chains from MLwiN.
}
  \item{Range}{
An integer vector of length two specifying the first and last iterations of the chains.
}
  \item{selected}{
A character vector specifying the selected chains to be plotted.
}
}

\author{
Zhang, Z., Charlton, C.M.J., Parker, R.M.A., Leckie, G., and Browne, W.J. (2012) Centre for Multilevel Modelling, University of Bristol.
}

\seealso{
\code{\link{sixway}}
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
formula="normexam~(0|cons+standlrt)+(2|cons+standlrt)+(1|cons)"
levID=c('school','student')
estoptions= list(EstM=1)
wsfile=paste(wspath,"tutorial.ws",sep="")
inputfile=paste(tempdir(),"/tutorial.dta",sep="")
ws2foreign(wsfile, foreignfile=inputfile, MLwiNPath=mlwin)
library(foreign);indata =read.dta(inputfile)
mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin)

trajectories(mymodel["chains"],Range=c(4501,5000))
}
}
