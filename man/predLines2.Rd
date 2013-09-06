\name{predLines2}
\alias{predLines2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draws predicted lines using chains from a model estimated using MCMC
}
\description{
This function draws predicted lines against an explanatory variable for selected groups at a higher level. The median and quantiles are calculated for each group.
}
\usage{
predLines2(indata, chains, resi.chains, xname, lev = 2, levID,
selected = NULL, probs=c(.025,.975), Legend=T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{indata}{
An object (converts to data.frame) containing the data modelled.
}
  \item{chains}{
The parameter chains from MLwiN.
}
  \item{resi.chains}{
The residual chains from MLwiN.
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
A vector specifying groups to selectively plot at the level specified in \code{lev}. If \code{selected=NULL}, then all groups at that level are included.
}
  \item{probs}{
A numeric vector of probabilities with values in \code{(0,1)} used to calculate the lower and upper quantiles from which the error bars are plotted.
}
  \item{Legend}{
A logical value indicating whether a legend is to be added.
}
}

\author{
Zhang, Z., Charlton, C.M.J., Parker, R.M.A., Leckie, G., and Browne, W.J. (2012) Centre for Multilevel Modelling, University of Bristol.
}

\seealso{
\code{\link{predLines1}}
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
estoptions= list(EstM=1,resi.store.levs=2)
wsfile=paste(wspath,"tutorial.ws",sep="")
inputfile=paste(tempdir(),"/tutorial.dta",sep="")
ws2foreign(wsfile, foreignfile=inputfile, MLwiNPath=mlwin)
library(foreign);indata =read.dta(inputfile)
mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions,MLwiNPath=mlwin)

resi.chains=mymodel["resi.chains"]
chains=mymodel["chains"]
predLines2(indata, chains, resi.chains, xname="standlrt", lev = 2, levID,
selected =c(30,44,53,59), probs=c(.025,.975))
}
}
