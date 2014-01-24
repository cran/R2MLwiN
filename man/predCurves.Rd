\name{predCurves}
\alias{predCurves}
\title{
Draws predicted curves (lines) using estimates from the fixed part of a fitted model.
}
\description{
This function draws predicted curves (lines) against an explanatory variable for each category of a categorical variable.
}
\usage{
predCurves(object, indata, xname, group = NULL, legend = T, 
legend.space="top", legend.ncol=2, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Either "mlwinfitIGLS" or "mlwinfitMCMC" class object.
}
  \item{indata}{
A data.frame object containing the data.
}
  \item{xname}{
A name of an explanatory variable.
}
  \item{group}{
A character name or a factor (or vector) specifying a categorical variable. \code{group=NULL} by default.
}
  \item{legend}{
A logical value indicating whether a legend for \code{group} is to be added.
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
\code{\link{predLines}}
}
\examples{

\dontrun{
library(R2MLwiN)
## Modify the following paths as appropriate.
## MLwiN folder
mlwin = "C:/Program Files (x86)/MLwiN v2.29/"
## MLwiN sample worksheet folder
wsfile=paste(mlwin,"/samples/alevchem.ws",sep="")
## the tutorial.dta will be save under the temporary folder
inputfile=paste(tempdir(),"/alevchem.dta",sep="")
ws2foreign(wsfile, foreignfile=inputfile, MLwiNPath=mlwin)
library(foreign);indata =read.dta(inputfile)
indata["gcseav"]=double2singlePrecision(indata["gcse-tot"]/indata["gcse-no"]-6)
indata["gcse^2"]=double2singlePrecision(indata["gcseav"]^2)
indata["gcse^3"]=double2singlePrecision(indata["gcseav"]^3)

## Example: A-level Chemistry
formula="a-point ~ (0|cons+gcseav+gcse^2+gcse^3+gender)+(1|cons )"
levID='pupil'
estoptions= list(EstM=1)
## Fit the model
mymodel=runMLwiN(formula, levID, D='Normal', indata, estoptions, MLwiNPath=mlwin)

predCurves(mymodel, indata,xname="gcseav", group="gender")
}
}

