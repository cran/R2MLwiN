\name{runMLwiN}
\alias{runMLwiN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calls MLwiN from R
}
\description{
This function executes MLwiN and then brings results back to R. A summary of the model will be printed, and a list of output objects will be saved in the work directory.
}
\usage{
runMLwiN(Formula, levID, D = "Normal", indata,
estoptions = list(EstM = 0), BUGO = NULL,
MLwiNPath = "C:/Program Files (x86)/MLwiN v2.29/", workdir=tempdir())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Formula}{
A formula object (or a character string) specifying the model formula. See \code{\link{Formula.translate}} for details.
}
  \item{levID}{
A character (vector) specifying the level ID(s).
}
  \item{D}{
A character string/vector specifying the distribution to be modelled.
}
  \item{indata}{
A data.frame object containing the data to be modelled.
}
  \item{estoptions}{
A list of options used for estimating the model. See Value below.
}
  \item{BUGO}{
A vector specifying BUGS options. If non-null, then WinBUGS/OpenBUGS, in conjunction with MLwiN, are used for modelling. see Value below.
}
  \item{MLwiNPath}{
A path to the MLwiN folder. By default, \code{MLwiNPath} = "C:/Program Files (x86)/MLwiN v2.29/".
}

  \item{workdir}{
A path to the folder where the output files are to be saved. If the folder specified does not exist, a new folder of that name is created; \code{workdir=tempdir()} by default.
}

}

\value{
The argument \code{estoptions} is a list which can contain the following options used for estimating the model:
\item{resi.store}{A logical value indicating whether residuals are to be stored or not.}
\item{resioptions}{
A string vector to specify the various residual options. The \code{"variances"} option calculates the posterior variances instead of the posterior standard errors; the \code{"standardised"}, \code{"leverage"}, \code{"influence"} and \code{"deletion"} options calculate standardised, leverage, influence and deletion residuals respectively; the \code{"sampling"} option calculates the sampling variance covariance matrix for the residuals; the \code{"norecode"} option prevents residuals with values exceedingly close or
equal to zero from being recoded to missing; the reflate option returns unshrunken residuals. Note that the default option is resioptions=c("variance","sampling"); "variance" cannot be used together with the other options to calculate standardised, leverage, influence and deletion residuals.
}
\item{resi.store.levs}{An integer vector indicating the levels at which the residual chains are to be stored.}
\item{debugmode}{A logical value determining whether MLwiN is run in the background or not. The default value is \code{FALSE}: i.e., MLwiN is run in the background. If \code{TRUE} MLwiN remains open after the model has run, and must be closed for the output to be returned to R. This option currently works for the 32bit MLwiN only.}
\item{x64}{
A logical value indicating whether the 64 bit version of MLwiN is used. If \code{FALSE} (by default), the 32 bit version is called.}
  \item{clean.files}{
If \code{TRUE}, the generated files will be removed from the \code{workdir}.
}
\item{show.file}{A logical value indicating whether the output files (e.g. macro file) are shown on the screen.}
\item{clre}{A matrix used to estimate some, but not all, of the variances and covariances for a set of coefficients at a particular level. Remove from the random part at level <first row> the covariance matrix element(s) defined by the pair(s) of rows <second row> <third row>. Each row corresponds to a removed entry of the covariance matrix.}
\item{notation}{Specifies the model subscript notation to be used in the MLwiN equations window. \code{"class"} means no multiple subscripts, whereas \code{"level"} has multiple subscripts.}
\item{mem.init}{A vector which sets and displays worksheet capacities for the current MLwiN session according to the value(s) specified. By default, the number of levels is \code{nlev}+1, worksheet size in thousands of cells is 6000, the number of columns is 2500, the number of explanatory variables is \code{num_vars}+10, and the number of group labels is 20. \code{nlev} is the number of levels specified by \code{levID}, and \code{num_vars} is approximately the number of explanatory variables calculated initially.}
\item{nonlinear}{ LINEarise mode N order M. \code{N=0} specifies marginal quasi-likelihood linearization (MQL), whilst \code{N=1} specifies penalised quasi-likelihood linearization (PQL); \code{M=1} specifies first order approximation, whilst \code{M=2} specifies second order approximation. \code{nonlinear=c(N=0,M=1)} by default.}
\item{Meth}{Specifies which maximum likelihood estimation method is to be used. If \code{Meth=0} estimation method is set to RIGLS. If \code{Meth=1} estimation method is set to IGLS (the default setting). If \code{Meth=2}, alternate between IGLS and RIGLS.}
\item{merr}{A vector which sets-up measurement errors on predictor variables. The first element N defines the number of variables that have measurement errors. Then, for each variable with measurement error, a pair of inputs is required: value Ma is the explanatory variable number for the Mth variable which has measurement error and value Mb is the variance of the measurement errors for the Mth variable.}
\item{fact}{A list of objects specified for factor analysis, including \code{nfact}, \code{lev.fact}, \code{nfactor}, \code{factor}, \code{loading} and \code{constr}. See \code{\link{MacroScript2}} for details.}
\item{weighting}{A list of objects specified for using weights in an analysis, including \code{levels}, \code{weights}, \code{mode}, \code{FSDE} and \code{RSDE}. See \code{\link{MacroScript1}} for details.}
\item{centring}{If not empty, centring is used for the selected explanatory variables. A list of objects, named by explanatory variables, specifies the ways to centre the variables. \code{list(rv1=1,...)} means that \code{rv1} is centring by its ground mean; \code{list(rv1=c(2,'group'),...)} means that \code{rv1} is centring by the mean of the specified group. \code{'group'} is a string specifying the group binary indicator, i.e., a vector of (0, 1);  \code{list(rv1=c(3,value),...)} means that \code{rv1} is centring by the specified numerical \code{value}. Here \code{rv1} is a string of the explanatory variable.
}
\item{xclass}{A list of objects specified for cross-classified and/or multiple membership models, including \code{class}, \code{N1}, \code{weight}, \code{id} and \code{car}. See \code{\link{MacroScript2}} for details.}
\item{mcmcMeth}{A list of objects specifying advanced MCMC methodology and prior options, including the following:\cr
\code{iterations}, \code{burnin}, \code{thinning}, \code{seed}, \code{priorParam}, \code{scale}, \code{refresh}, \code{fixM}, \code{residM}, \code{Lev1VarM}, \code{OtherVarM}, \code{adaption}, \code{priorcode}, \code{startval}, \code{rate}, \code{tol}, \code{lclo} and \code{nopause}. See \code{\link{MacroScript2}} for details.}
\item{mcmcOptions}{A list of objects specifying MCMC options, including the following:\cr
\code{orth}, \code{hcen}, \code{smcm}, \code{smvn}, \code{paex} and \code{mcco}. See \code{\link{MacroScript2}} for details.}

The argument \code{BUGO} is a vector specifying BUGS options as follows:
\item{version}{This indicates the version of WinBUGS where \code{3} = version 1.3 and \code{4} = version 1.4.}
\item{n.chains}{This specifies the number of chains used in the BUGS algorithm.}
\item{debug}{This determines whether BUGS stays open following completion of the model run; \code{debug=F} by default.}
\item{seed}{This sets the random number generator in BUGS.}
\item{bugs}{This specifies the path of the BUGS executable.}
\item{OpenBugs}{If \code{OpenBugs=TRUE}, OpenBUGS is used. Otherwise, WinBUGS is used.}

If \code{BUGO} is non-NULL then the output is an \code{\link{mcmc.list}} object.

If the IGLS algorithm is used, (i.e., \code{EstM=0}), then the outputs are
\item{estIGLS}{Captures the parameter estimates from MLwiN using the IGLS algorithm. <_FP_b column> has the fixed part estimates and its variances and covariances are stored in <_FP_v column>; <_RP_b column> has the random part estimates and its variances and covariances are stored in <_RP_v column>; The likelihood statistic is stored in <_Stats column>.}
\item{FP }{Displays the fixed part estimates.}
\item{RP }{Displays the random part estimates.}
\item{FP.cov}{Displays a covariance matrix of the fixed part estimates.}
\item{RP.cov}{Displays a covariance matrix of the random part estimates.}
\item{call}{The matched call.}
\item{LIKE}{The likelihood statistic (-2*log(like))}
\item{residual}{If \code{resi.store} is \code{TRUE}, then the residual estimates at all levels are returned.}

If the MCMC algorithm is used, (i.e., \code{EstM=1}), then the outputs are
\item{estMCMC}{Captures the parameter estimates from MLwiN using the MCMC algorithm. <_FP_b column> has the fixed part estimates and its variances and covariances are stored in <_FP_v column>; <_RP_b column> has the random part estimates and its variances and covariances are stored in <_RP_v column>; DIC and likelihood measures are stored in <_Stats column>.}
\item{chains}{Captures the MCMC chains from MLwiN for all parameters.}
\item{FP }{Displays the fixed part estimates.}
\item{RP }{Displays the random part estimates.}
\item{FP.cov}{Displays a covariance matrix of the fixed part estimates.}
\item{RP.cov}{Displays a covariance matrix of the random part estimates.}
\item{call}{The matched call.}
\item{LIKE}{The likelihood statistic (-2*log(like))}
\item{BDIC}{Bayesian Deviance Information Criterion (DIC)}
\item{fact.loadings}{If \code{fact} is not empty, then the factor loadings are returned.}
\item{fact.cov}{If \code{fact} is not empty, then factor covariances are returned.}
\item{residual}{If \code{resi.store} is \code{TRUE}, then the residual estimates at all levels are returned.}
\item{resi.chains}{If \code{resi.store.levs} is not empty, then the residual chains at these levels are returned.}
\item{chains.bugs}{If \code{BUGO} is used, then the output chains from WinBUGS/OpenBUGS are returned.}

}
\note{
Note that (1) if \code{EstM=1}, the IGLS algorithm is preprocessed to obtain the starting values of all the parameters for the MCMC algorithm; (2) from Version 2.26, a new mlnscript.exe is called for both x64 and i386 if MLwiN is running in the background (i.e. debugmode = FALSE); the debug mode is currently not available for the 64 bit MLwiN.
}
\author{
Zhang, Z., Charlton, C.M.J., Parker, R.M.A., Leckie, G., and Browne, W.J. (2012) Centre for Multilevel Modelling, University of Bristol.
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Formula.translate}},\code{\link{MacroScript1}},\code{\link{MacroScript2}}
}
\examples{

## The R2MLwiN package includes scripts to replicate all the analyses in
## Browne, W.J. (2009) MCMC estimation in MLwiN Version 2.13.
## Version 2.25 is available online; download from the following link:
## http://www.bristol.ac.uk/cmm/software/mlwin/download/mcmc-print.pdf
## Centre for Multilevel Modelling, University of Bristol

#Contents
#01 Introduction to MCMC Estimation and Bayesian Modelling
#02 Single Level Normal Response Modelling
#03 Variance Components Models
#04 Other Features of Variance Components Models
#05 Prior Distributions, Starting Values and Random Number Seeds
#06 Random Slopes Regression Models
#07 Using the WinBUGS Interface in MLwiN
#08 Running a Simulation Study in MLwiN
#09 Modelling Complex Variance at Level 1 / Heteroscedasticity
#10 Modelling Binary Responses
#11 Poisson Response Modelling
#12 Unordered Categorical Responses
#13 Ordered Categorical Responses
#14 Adjusting for Measurement Errors in Predictor Variables
#15 Cross Classified Models
#16 Multiple Membership Models
#17 Modelling Spatial Data
#18 Multivariate Normal Response Models and Missing Data
#19 Mixed Response Models and Correlated Residuals
#20 Multilevel Factor Analysis Modelling
#21 Using Structured MCMC
#22 Using the Structured MVN framework for models
#23 Using Orthogonal fixed effect vectors
#24 Parameter expansion
#25 Hierarchical Centring

\dontrun{
## Take Chapter03 as an example
## To find the location of a demo for Chapter03
file.show(system.file("demo", "Chapter03.R", package="R2MLwiN"))

## MLwiN folder
mlwin = "C:/Program Files (x86)/MLwiN v2.29/"

## To run the demo for Chapter03
demo(Chapter03)
}
}
