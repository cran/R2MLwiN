\name{mlwinfitIGLS-class}
\docType{class}
\alias{mlwinfitIGLS-class}
\alias{mlwinfitIGLS-class}
\alias{[,mlwinfitIGLS-method}
\alias{[<-,mlwinfitIGLS-method}
\alias{[[,mlwinfitIGLS-method}
\alias{[[<-,mlwinfitIGLS-method}
\alias{print,mlwinfitIGLS-method}
\alias{summary,mlwinfitIGLS-method}
\alias{update,mlwinfitIGLS-method}
\title{S4 class: mlwinfitIGLS}
\description{
This S4 class object is used to save the outputs from the fitted multilevel model using IGLS.
}

\section{An instance of the Class}{
  An instance is created by calling function \code{\link{runMLwiN}}.
}


\section{Slots}{
  \describe{
    \item{Nobs}{Computes the number of complete observations.}
    \item{DataLength}{Total number of cases.}
    \item{D}{A vector specifying the type of distribution to be modelled, which can include \code{"Normal"}, \code{"Binomial"} \code{"Poisson"}, \code{"Multinomial"}, \code{"Multivariate Normal"}, or \code{"Mixed"}.}
    \item{Formula}{A formula object (or a character string) specifying a multilevel model.}
    \item{levID}{  A character string (vector) of the specified level ID(s).}
    \item{estIGLS}{Captures the parameter estimates from MLwiN using the IGLS algorithm. <_FP_b column> has the fixed part estimates and its variances and covariances are stored in <_FP_v column>; <_RP_b column> has the random part estimates and its variances and covariances are stored in <_RP_v column>; The likelihood statistic is stored in <_Stats column>.}
    \item{FP }{Displays the fixed part estimates.}
    \item{RP }{Displays the random part estimates.}
    \item{FP.cov}{Displays a covariance matrix of the fixed part estimates.}
    \item{RP.cov}{Displays a covariance matrix of the random part estimates.}
    \item{call}{The matched call.}
    \item{elapsed.time}{Calculates the CPU time used for fitting the model.}
    \item{LIKE}{The deviance statistic (-2*log(like)).}
    \item{residual}{If \code{resi.store} is \code{TRUE}, then the residual estimates at all levels are returned.}

  }
}

\section{Methods}{
  \describe{
    \item{summary}{\code{signature(object = "mlwinfitIGLS")}: summarizes the outputs of the fitted model including z-statistic, p-values, confidence intervals, etc.}
\item{print}{summarizes the outputs of the fitted model including z-statistic, p-values, confident intervals, etc.}
\item{show}{summarizes the outputs of the fitted model including z-statistic, p-values, confident intervals, etc.}
\item{update}{see \code{\link[stats]{update}} on how to update fitted models. Note that "." syntax does not work for the \code{R2MLwiN} model\code{Formula}; 
a full formula object has to be specified to replace the old formula object.}
    \item{"\code{[}"}{\code{signature(object = "mlwinfitIGLS")}:
      this method gets a slot of an instance of the S4 class.}
    \item{"\code{[[}"}{\code{signature(object = "mlwinfitIGLS")}:
      the same method as  "\code{[}". }
    \item{"\code{[<-}"}{\code{signature(object = "mlwinfitIGLS")}:
      this method replaces a slot of an instance by a different value of the same kind of object. }
    \item{"\code{[[<-}"}{\code{signature(object = "mlwinfitIGLS")}:
      the same method as  "\code{[<-}". }
}
}

\seealso{
  \code{\link{runMLwiN}}
}
\examples{\dontrun{
library(R2MLwiN)
## Modify the following paths as appropriate.
## MLwiN folder
mlwin ="C:/Program Files (x86)/MLwiN v2.27/"
## MLwiN sample worksheet folder
wspath=paste(mlwin,"/samples/",sep="")

## MLwiN sample worksheet: tutorial dataset
wsfile=paste(wspath,"tutorial.ws",sep="");inputfile=paste(tempdir(),"/tutorial.dta",sep="")
ws2foreign(wsfile, foreignfile=inputfile, MLwiNPath=mlwin)
library(foreign);indata =read.dta(inputfile)

## Define the model
formula="normexam~(0|cons+standlrt)+(2|cons+standlrt)+(1|cons)"
levID=c('school','student')
## Choose option(s) for inference
estoptions= list(EstM=0)
## Fit the model
mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin)

##summary method
summary(mymodel)

##get method
mymodel["LIKE"]
}}
