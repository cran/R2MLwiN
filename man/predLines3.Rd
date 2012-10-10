\name{predLines3}
\alias{predLines3}
\title{
Draws predicted curves (lines) using estimates from the fixed part of a model.
}
\description{
This function draws predicted curves (lines) against an explanatory variable for each group of a categorical variable.
}
\usage{
predLines3(indata, FP, xname, group = NULL, Legend = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{indata}{
A data.frame object containing the data.
}
  \item{FP}{
A vector of the fixed part estimates.
}
  \item{xname}{
A name of an explanatory variable.
}
  \item{group}{
A character name or a factor (or vector) specifying a categorical variable. \code{group=NULL} by default.
}
  \item{Legend}{
A logical value indicating whether a legend for \code{group} is to be added.
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
mymodel=runMLwiN(formula, levID, D='Normal', indata, estoptions,MLwiNPath=mlwin)

FP=mymodel["FP"]
predLines3(indata,FP,xname="gcseav", group="gender")
}
}

