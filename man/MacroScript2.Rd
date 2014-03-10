\name{MacroScript2}
\alias{MacroScript2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Writes MLwiN macros to fit models using Markov chain Monte Carlo (MCMC) methods
}
\description{
MacroScript2 is an internal function which creates an MLwiN macro file to fit models using MCMC.
}
\usage{
MacroScript2(indata, dtafile, resp,
levID, expl, rp,
D, nonlinear, categ,
notation, nonfp, clre,
smat, Meth, merr, seed,
iterations, burnin, scale,
thinning, priorParam, refresh,
fixM, residM, Lev1VarM,
OtherVarM, adaption, priorcode,
rate, tol, lclo,
mcmcOptions, fact,
xclass = NULL, BUGO = NULL,
mem.init, optimat,nopause,
modelfile = modelfile,
initfile = initfile,
datafile = datafile,
macrofile = macrofile,
IGLSfile = IGLSfile,
MCMCfile = MCMCfile,
chainfile = chainfile,
MIfile = MIfile,
resifile = resifile,
resi.store = resi.store,
resioptions=resioptions,
resichains = resichains,
FACTchainfile = FACTchainfile,
resi.store.levs = resi.store.levs,
debugmode = debugmode,
startval = startval,
dami = dami)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{indata}{
A data.frame object containing the data to be modelled.
}
  \item{dtafile}{
  The file name of the dataset to be imported into MLwiN, which is in Stata format (i.e. with extension .dta).
}
  \item{resp}{
  A character string (vector) of response variable(s).
}
  \item{levID}{
  A character string (vector) of the specified level ID(s). The ID(s) should be sorted in the descending order of levels (e.g. \code{levID=c('level2','level1')} where \code{'level2'} is the higher level).
}
  \item{expl}{
  A character string (vector) of explanatory (predictor) variable(s).
}
  \item{rp}{
  A character string (vector) of random part of random variable(s).
}
  \item{D}{
  A vector specifying the type of distribution to be modelled, which can include \code{"Normal"}, \code{"Binomial"} \code{"Poisson"}, \code{"Unordered Multinomial"},  \code{"Ordered Multinomial"}, \code{"Multivariate Normal"}, or \code{"Mixed"}.
}
  \item{nonlinear}{
  LINEarise mode N order M. \code{N=0} specifies marginal quasi-likelihood linearization (MQL), whilst \code{N=1} specifies penalised quasi-likelihood linearization (PQL); \code{M=1} specifies first order approximation, whilst \code{M=2} specifies second order approximation. \code{nonlinear=c(N=0,M=1)} by default.
}
  \item{categ}{
  Specifies categorical variable(s) as a matrix. Each column corresponds to a categorical variable; the first row specifies the name(s) of variable(s);
the second row specifies the name(s) of reference group(s), \code{NA}(s) if no reference group; the third row states the number of categories for each variable.
}
  \item{notation}{
  Specifies the model subscript notation to be used in the MLwiN equations window. \code{"class"} means no multiple subscripts, whereas \code{"level"} has multiple subscripts.
}
  \item{nonfp}{
  Removes the fixed part of random variable(s). \code{NA} if no variable is removed.
}
  \item{clre}{
A matrix used to estimate some, but not all, of the variances and covariances for a set of coefficients at a particular level. Remove from the random part at level <first row> the covariance matrix element(s) defined by the pair(s) of rows <second row> <third row>. Each row corresponds to a removed entry of the covariance matrix.
}
  \item{smat}{An integer vector of length 2 specifying whether the covariance matrix at a particular level is diagonal. The first digit is the level indicator, whilst the second digit is a binary indicator where \code{1} indicates a diagonal covariance matrix and \code{0} indicates the full covariance matrix.
}
  \item{Meth}{
Specifies which maximum likelihood estimation method to be used. If \code{Meth=0} estimation method is set to RIGLS. If \code{Meth=1} estimation method is set to IGLS (the default setting). If \code{Meth} is absent, alternate between IGLS and RIGLS.
}
  \item{merr}{
A vector which sets-up measurement errors on predictor variables. The first element \code{N} defines the number of variables that have measurement errors. Then, for each variable with measurement error, a pair of inputs is required: value Ma is the explanatory variable number for the Mth variable which has measurement error and value Mb is the variance of the measurement errors for the Mth variable.

}
  \item{seed}{
An integer specifying the random seed in MLwiN. By default, \code{seed=1}.
}
  \item{iterations}{
An integer specifying the number of iterations after burn-in.
}
  \item{burnin}{
An integer specifying length of the burn-in.
}
  \item{scale}{
An integer specifying the scale factor.
}
  \item{thinning}{
An integer specifying the frequency with which successive values in the Markov chain are stored. By default \code{thinning=1}.
}
  \item{priorParam}{
A vector specifying the informative priors used. Also see \code{\link{prior2macro}}.
}
  \item{refresh}{
An integer specifying how frequently the parameter estimates are refreshed on the screen during iterations. By default \code{refresh=50}.
}
  \item{fixM}{
Specifies the fixed effect method: \code{1} for Gibbs Sampling, \code{2} for univariate MH Sampling and \code{3} for multivariate MH Sampling.
}
  \item{residM}{
Specifies the residual method: \code{1} for Gibbs Sampling, \code{2} for univariate MH Sampling and \code{3} for multivariate MH Sampling.
}
  \item{Lev1VarM}{
Specifies the level 1 variance method: \code{1} for Gibbs Sampling, \code{2} for univariate MH Sampling and \code{3} for multivariate MH Sampling.
}
  \item{OtherVarM}{
Specifies the variance method for other levels: \code{1} for Gibbs Sampling, \code{2} for univariate MH Sampling and \code{3} for multivariate MH Sampling.
}
  \item{adaption}{
\code{adaption=1} indicates adaptation is to be used; \code{0} otherwise.
}
  \item{priorcode}{
An integer indicating which default priors are to be used for the variance parameters. This parameter takes the value \code{1} for Gamma priors or \code{0} for Uniform on the variance scale priors. See the section on "Priors" in the MLwiN help system for more details on the meaning of these default priors.
}
  \item{rate}{
An integer specifying the acceptance rate (as a percentage); this command is ignored if \code{adaptation=0}.
}
  \item{tol}{
An integer specifying tolerance.
}
  \item{lclo}{
This command toggles on/off the possible forms of complex level 1 variation when using MCMC. By default (\code{lclo=0}) we express the level 1 variation as a function of the predictors.
If this is toggled (\code{lclo=1}) we express the log of the level 1 precision (1/variance) as a function of the predictors.
}
  \item{mcmcOptions}{
A list of other MCMC options used. See Value below.
}
  \item{fact}{
A list of objects specified for factor analysis. See Value below.
}
  \item{xclass}{
A list of objects specified for cross-classified and/or multiple membership models. See Value below.
}
  \item{BUGO}{
If the first entry of the vector is \code{TRUE}, the current model is outputted in BUGS code. \code{version=4} specifies WinBUGS 1.4 format, whilst \code{version=3} corresponds to WinBUGS 1.3 format. \code{n.chains} specifies the number of chains to be used in WinBUGS/OpenBUGS, \code{n.chains=1} by default. \code{debug} specifies whether BUGS is left open once the model has run, or not (\code{debug=F} by default). \code{seed} sets the random number generator in BUGS. \code{bugs} specifies the location of the WinBUGS/OpenBUGS executable. If\code{OpenBugs=TRUE} OpenBUGS is used, if \code{FALSE} (the default) WinBUGS is used.
}
  \item{mem.init}{
A vector which sets and displays worksheet capacities for the current MLwiN session according to the value(s) specified. By default, the number of levels is \code{nlev}+1; worksheet size in thousands of cells is 6000; the number of columns is 2500; the number of explanatory variables is \code{num_vars}+10; the number of group labels is 20. \code{nlev} is the number of levels specified by \code{levID}, and \code{num_vars} is approximately the number of explanatory variables calculated initially.
}
  \item{optimat}{
This option instructs MLwiN to limit the maximum matrix size that can be allocated by the (R)IGLS algorithm. Specify \code{optimat=TRUE} if MLwiN gives the following error message "Overflow allocating smatrix".  This error message arises if one more higher-level units is extremely large (contains more than 800 lower-level units).  In this situation runmlwin's default behaviour is to instruct MLwiN to allocate a larger matrix size to the (R)IGLS algorithm than is currently possible. Specifying \code{optimat=TRUE} caps the maximum matrix size at 800 lower-level units, circumventing the MLwiN error message, and allowing most MLwiN functionality.  
}
  \item{nopause}{
A logical value specifying whether the estimates are to be updated on screen or not, in MLwiN. Default \code{nopause=FALSE}, i.e. the screen is updated.
}
  \item{modelfile}{
A file name where the WinBUGS model will be saved in .txt format.
}
  \item{initfile}{
A file name where the WinBUGS initial values will be saved in .txt format.
}
  \item{datafile}{
A file name where the WinBUGS data will be saved in .txt format.
}
  \item{macrofile}{
A file name where the MLwiN macro file will be saved. The default location is in the temporary folder.
}
  \item{IGLSfile}{
A file name where the IGLS estimates will be saved. The default location is in the temporary folder.
}
  \item{MCMCfile}{
A file name where the MCMC estimates will be saved. The default location is in the temporary folder.
}
  \item{chainfile}{
A file name where the MCMC chains will be saved. The default location is in the temporary folder.
}
  \item{MIfile}{
A file name where the missing values will be saved. The default location is in the temporary folder.
}
  \item{resifile}{
A file name where the residual estimates will be saved. The default location is in the temporary folder.
}
  \item{resi.store}{
A logical value to indicate if residuals are to be stored (\code{TRUE}) or not (\code{FALSE}).
}
   \item{resioptions}{
A string vector to specify the various residual options. The \code{"variances"} option calculates the posterior variances instead of the posterior standard errors; the \code{"standardised"}, \code{"leverage"}, \code{"influence"} and \code{"deletion"} options calculate standardised, leverage, influence and deletion residuals respectively; the \code{"sampling"} option calculates the sampling variance covariance matrix for the residuals; the \code{"norecode"} option prevents residuals with values exceedingly close or
equal to zero from being recoded to missing; the reflate option returns unshrunken residuals. Note that the default option is resioptions=c("variance","sampling"); "variance" cannot be used together with the other options to calculate standardised, leverage, influence and deletion residuals.
}
\item{resichains}{
A file name where the residual chains will be saved. The default location is in the temporary folder.
}
\item{FACTchainfile}{
A file name where the factor chains will be saved. The default location is in the temporary folder.
}
\item{resi.store.levs}{
An integer vector indicating the levels at which the residual chains are to be stored.
}
  \item{debugmode}{
A logical value determining whether MLwiN is run in the background or not. The default value is \code{FALSE}: i.e. MLwiN is run in the background. If \code{TRUE} MLwiN remains open after the model has run, allowing the user to interact with MLwiN directly (but note that output will not be returned to R until MLwiN is closed).
}

\item{startval}{
A list of numeric vectors specifying the starting values when using MCMC. \code{FP.b} corresponds to the estimates for the fixed part; \code{FP.v} specifies the variance/covariance estimates for the fixed part; \code{RP.b} specifies the variance estimates for the random part; \code{RP.v} corresponds to the variance/covariance matrix of the variance estimates for the random part. \code{startval=NULL} by default: i.e. the estimates obtained from IGLS are used as the starting values for MCMC.
}
\item{dami}{This command outputs a complete (i.e. including non-missing responses) response variable y. If \code{dami=c(0,<iter1>,<iter2>,...)} then the response variables returned will be the value of y at the iterations quoted (as integers \code{<iter1>,<iter2>}, etc.); these can be used for multiple imputation. If \code{dami=1} the value of y will be the mean estimate from the iterations produced. \code{dami=2} is as for \code{dami=1} but with the standard errors of the estimate additionally being stored. \code{dami=NULL} by default.
}
}

\value{
A list of other MCMC options as used in the argument \code{mcmcOptions}:
\item{orth}{If \code{orth=1}, orthogonal fixed effect vectors are used; zero otherwise.}
\item{hcen}{An integer specifying the level where we use hierarchical centering.}
\item{smcm}{If \code{smcm=1}, structured MCMC is used; zero otherwise.}
\item{smvn}{If \code{smvn=1}, the structured MVN framework is used; zero otherwise.}
\item{paex}{This gives a vector of length two. If the second digit is \code{1}, parameter expansion is used at level <the first digit>; zero otherwise.}
\item{mcco}{This command allows the user to have constrained settings for the lowest level variance matrix in a multivariate Normal model. The default value for \code{mcco}, \code{0}, is used for a full covariance matrix. Four other settings are currently available:\cr
\tabular{ll}{
(\code{1}) \tab all correlations equal and all variances equal;\cr
(\code{2}) \tab an AR1 structure with all variances equal;\cr
(\code{3})\tab  all correlations equal but independent variances;\cr
(\code{4}) \tab an AR1 structure with independent variances.\cr
}
}

A list of objects specified for cross-classified and/or multiple membership models, as used in the argument \code{xclass}:
\item{class}{An integer (vector) of the specified class(es).}
\item{N1}{This defines a multiple membership across \code{N1} units at level \code{class}. \code{N1}>1 if there is multiple membership.
}
\item{weight}{If there is multiple membership then the column number \code{weight}, which is the length of the dataset, will contain the first set of weights for the multiple membership.
Note that there should be \code{N1} weight columns and they should be sequential in the worksheet starting from \code{weight}.}
\item{id}{If the response is multivariate then the column number \code{id} must be input and this contains the first set of identifiers for the classification. Note that for a p-variate model each lowest level unit contains p records and the identifiers (sequence numbers) for each response variate need to be extracted into \code{id} and following columns. There should be \code{N1} of these identifier columns and they should be sequential starting from \code{id} in the multivariate case.
}
\item{car}{\code{car=TRUE} indicates the spatial CAR model; \code{FALSE} otherwise. \code{car=FALSE} if ignored.}

A list of objects specified for factor analysis, as used in the argument \code{fact}:
\item{nfact}{Specifies the number of factors}
\item{lev.fact}{Specifies the level/classification for the random part of the factor for each factor.}
\item{nfactcor}{Specifies the number of correlated factors}
\item{factcor}{A vector specifying the correlated factors}
\tabular{ll}{
(1)\tab  the first factor number; \cr
(2)\tab  the second factor number; \cr
 (3)\tab  the starting value for the covariance and\cr
(4)\tab  an indicator of whether this covariance is constrained (\code{1}) or not (\code{0}).\cr
}
\item{loading}{A matrix specifying the starting values for the factor loadings and the starting value of the factor variance. Each row corresponds to a factor.}
\item{constr}{A matrix specifying indicators of whether the factor loadings and the factor variance are constrained (\code{1}) or not (\code{0}).}

The MLwiN macro file is created in the temporary directory (\code{tempdir()}) and will be displayed on screen if \code{show.file=TRUE}.
}

\note{
Note that for \code{FixM}, \code{residM}, \code{Lev1VarM} and \code{OtherVarM}, not all combinations of methods are available for all sets of parameters and all models.

}

\author{
Zhang, Z., Charlton, C.M.J., Parker, R.M.A., Leckie, G., and Browne, W.J. (2012) Centre for Multilevel Modelling, University of Bristol.
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[R2MLwiN]{MacroScript1}, \link{tempdir}}
}


