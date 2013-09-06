\name{ws2foreign}
\alias{ws2foreign}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converts data from one format to another within MLwiN
}
\description{
This function converts data from one format to another within MLwiN, via MLwiN macro language. The \code{foreign} package allows R to read data files for most of these formats.
}
\usage{
ws2foreign(wsfile, foreignfile, MLwiNPath, x64=FALSE)
}
\arguments{
\item{wsfile}{A file name specifying the data file (with a specific extension) to be converted.}
\item{foreignfile}{A file name specifying the data file (with a specific extension) after conversion.}
\item{MLwiNPath}{A path to the MLwiN folder. By default, \code{MLwiNPath} = "C:/Program Files (x86)/MLwiN v2.27/".}
\item{x64}{A logical value indicating whether the 64 bit version of MLwiN is used. If \code{FALSE} (by default), the 32 bit version is called.}
}
\details{
MLwiN supports conversion between MLwiN (*.wsz, *.ws), Minitab (*.mtw), SAS (*.xpt), SPSS (*.sav), and Stata (*.dta) files.
}
\value{
The converted data file (with a specific extension) will be saved in a given folder.
}

\author{
Zhang, Z., Charlton, C.M.J., Parker, R.M.A., Leckie, G., and Browne, W.J. (2012) Centre for Multilevel Modelling, University of Bristol.
}

\seealso{
\cite{\link[foreign]{read.dta}}
}
\examples{
\dontrun{
## Modify the following paths as appropriate.
## MLwiN folder
mlwin = "C:/Program Files (x86)/MLwiN v2.27/"
## MLwiN sample worksheet folder
wsfile=paste(mlwin,"/samples/tutorial.ws",sep="")
## the tutorial.dta will be save under the temporary folder
inputfile=paste(tempdir(),"/tutorial.dta",sep="")
ws2foreign(wsfile, foreignfile=inputfile, MLwiNPath=mlwin)
library(foreign)
indata =read.dta(inputfile)
}
}
