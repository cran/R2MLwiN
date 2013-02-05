\name{caterpillar}
\alias{caterpillar}
\title{
Draws a caterpillar plot (in MLwiN style).
}
\description{
This function uses the means (or medians), upper-quantiles and lower-quantiles for all the groups to draw a caterpillar plot.
}
\usage{
caterpillar(y, x, qtlow, qtup, xlab = "", ylab = "",
xlim = NULL, ylim = NULL, main = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
A numerical vector of means (or medians) used as the \code{y} coordinates for the plot.
}
  \item{x}{
A numerical vector specifying the group labels as \code{x} coordinates that correspond to the \code{y} coordinates.
}
  \item{qtlow}{
A numerical vector of lower-quantiles to be used to plot error bars.
}
  \item{qtup}{
A numerical vector of upper-quantiles to be used to plot error bars.
}
  \item{xlab}{
A label for the \code{x} axis. This is empty by default.
}
  \item{ylab}{
A label for the \code{y} axis. This is empty by default.
}
  \item{xlim}{
  The \code{x} limits \code{(x1, x2)} of the plot. Note that \code{x1 > x2} is allowed and leads to a `reversed axis'. The default value, \code{NULL}, indicates that the range of the finite values to be plotted should be used.
}
  \item{ylim}{
  The y limits of the plot.
}
  \item{main}{
  A main title for the plot, see also \code{\link{title}}.
}
}

\author{
Zhang, Z., Charlton, C.M.J., Parker, R.M.A., Leckie, G.B., and Browne, W.J. (2012) Centre for Multilevel Modelling, University of Bristol.
}


\seealso{
\code{\link{caterpillarR}}
}
\examples{

\dontrun{
library(R2MLwiN)
## Modify the following paths as appropriate.
## MLwiN folder
mlwin = "C:/Program Files (x86)/MLwiN v2.26/"
## MLwiN sample worksheet folder
wspath=paste(mlwin,"/samples/",sep="")

## Example: tutorial
formula="normexam~(0|cons+standlrt)+(2|cons)+(1|cons)"
levID=c('school','student')
wsfile=paste(wspath,"tutorial.ws",sep="")
inputfile=paste(tempdir(),"/tutorial.dta",sep="")
ws2foreign(wsfile, foreignfile=inputfile, MLwiNPath=mlwin)
library(foreign);indata =read.dta(inputfile)
estoptions= list(EstM=1,resi.store=TRUE,resi.store.levs=2,mcmcMeth=list(iterations=5001))
mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions,MLwiNPath=mlwin)

lencateg = length(unique(indata[["school"]]))
resi.chain2 = mymodel["resi.chains"][[1]]
resi.chain2 = matrix(resi.chain2, nrow =lencateg)

## For each iteration, rank the schools
u0rank = apply(resi.chain2,2,rank)
## For each school, calculate the mean rank...
u0rankmn = apply(u0rank, 1,mean)
u0ranklo = apply(u0rank, 1, function(x) quantile(x,.025))
u0rankmd = apply(u0rank, 1,median)
u0rankhi = apply(u0rank, 1, function(x) quantile(x,.975))
rankno = order(u0rankmn)

caterpillar(y=u0rankmn[rankno], x=1:65, qtlow=u0ranklo[rankno], qtup=u0rankhi[rankno],
xlab="School", ylab="Rank")
}
}
