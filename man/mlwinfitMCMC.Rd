\name{mlwinfitMCMC-class}
\docType{class}
\alias{mlwinfitMCMC-class}
\alias{mlwinfitMCMC-class}
\alias{[,mlwinfitMCMC-method}
\alias{[<-,mlwinfitMCMC-method}
\alias{[[,mlwinfitMCMC-method}
\alias{[[<-,mlwinfitMCMC-method}
\alias{print,mlwinfitMCMC-method}
\alias{summary,mlwinfitMCMC-method}
\alias{update,mlwinfitMCMC-method}
\title{S4 class: mlwinfitMCMC}
\description{
This S4 class object is used to save the outputs from the fitted multilevel model using MCMC.
}

\section{An instance of the Class}{
  An instance is created by calling function \code{\link{runMLwiN}}.
}


\section{Slots}{
  \describe{
    \item{Nobs}{Computes the number of complete observations.}
    \item{DataLength}{Total number of cases.}
    \item{burnin}{An integer specifying length of the burn-in.}
    \item{iterations}{An integer specifying the number of iterations after burn-in.}
    \item{D}{A vector specifying the type of distribution to be modelled, which can include \code{"Normal"}, \code{"Binomial"} \code{"Poisson"}, \code{"Multinomial"}, \code{"Multivariate Normal"}, or \code{"Mixed"}.}
    \item{Formula}{A formula object (or a character string) specifying a multilevel model.}
    \item{levID}{  A character string (vector) of the specified level ID(s).}
    \item{merr}{A vector which sets-up measurement errors on predictor variables.}
    \item{fact}{A list of objects specified for factor analysis, including \code{nfact}, \code{lev.fact}, \code{nfactor}, \code{factor}, \code{loading} and \code{constr}.}
    \item{xclass}{A list of objects specified for cross-classified and/or multiple membership models, including \code{class}, \code{N1}, \code{weight}, \code{id} and \code{car}. }
   \item{estMCMC}{Captures the parameter estimates from MLwiN using the MCMC algorithm. <_FP_b column> has the fixed part estimates and its variances and covariances are stored in <_FP_v column>; <_RP_b column> has the random part estimates and its variances and covariances are stored in <_RP_v column>; DIC and likelihood measures are stored in <_Stats column>.}
   \item{chains}{Captures the MCMC chains from MLwiN for all parameters.}
    \item{FP }{Displays the fixed part estimates.}
    \item{RP }{Displays the random part estimates.}
    \item{FP.cov}{Displays a covariance matrix of the fixed part estimates.}
    \item{RP.cov}{Displays a covariance matrix of the random part estimates.}
    \item{elapsed.time}{Calculates the CPU time used for fitting the model.}
    \item{call}{The matched call.}
    \item{LIKE}{The deviance statistic (-2*log(like)).}
    \item{MIdata}{If \code{dami} is not empty, then the complete response variable \code{y} are returned.}
    \item{BDIC}{Bayesian Deviance Information Criterion (DIC)}
    \item{fact.loadings}{If \code{fact} is not empty, then the factor loadings are returned.}
    \item{fact.cov}{If \code{fact} is not empty, then factor covariances are returned.}
    \item{fact.chains}{If \code{fact} is not empty, then the factor chains are returned.}
    \item{residual}{If \code{resi.store} is \code{TRUE}, then the residual estimates at all levels are returned.}
    \item{resi.chains}{If \code{resi.store.levs} is not empty, then the residual chains at these levels are returned.}
  }
}

\section{Methods}{
  \describe{
    \item{summary}{\code{signature(object = "mlwinfitMCMC")}: summarizes the outputs of the fitted model including z-statistic and associated p-values (or Bayesian one-sided p-values), credible intervals, etc.
    }
\item{print}{summarizes the outputs of the fitted model including z-statistic and associated p-values (or Bayesian one-sided p-values), credible intervals, etc.}
\item{show}{summarizes the outputs of the fitted model including z-statistic and associated p-values (or Bayesian one-sided p-values), credible intervals, etc.}
\item{update}{see \code{\link[stats]{update}} on how to update fitted models. Note that "." syntax does not work for the \code{R2MLwiN} model\code{Formula}; 
a full formula object has to be specified to replace the old formula object.}
    \item{"\code{[}"}{\code{signature(object = "mlwinfitMCMC")}:
      this method gets a slot of an instance of the S4 class. }
    \item{"\code{[[}"}{\code{signature(object = "mlwinfitMCMC")}:
      the same method as  "\code{[}". }
    \item{"\code{[<-}"}{\code{signature(object = "mlwinfitMCMC")}:
      this method replaces a slot of an instance by a different value of the same kind of object. }
    \item{"\code{[[<-}"}{\code{signature(object = "mlwinfitMCMC")}:
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
mlwin ="C:/Program Files (x86)/MLwiN v2.29/"
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
estoptions= list(EstM=1)
## Fit the model
mymodel=runMLwiN(formula, levID, D="Normal", indata, estoptions, MLwiNPath=mlwin)

##summary method
summary(mymodel)

##get method
mymodel["BDIC"]
}}
