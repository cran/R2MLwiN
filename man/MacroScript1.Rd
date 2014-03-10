\name{MacroScript1}
\alias{MacroScript1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Writes MLwiN macros to fit models using the iterative generalized least squares (IGLS) algorithm
%%  ~~function to do ... ~~
}
\description{
MacroScript1 is an internal function which creates an MLwiN macro file to fit a multilevel model using IGLS.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MacroScript1(indata, dtafile, resp,
levID, expl, rp, D = "Normal",
nonlinear = c(0, 1), categ = NULL,
notation = NULL, nonfp = NA,
clre, smat, Meth = 1,
BUGO = NULL, mem.init = "default",
optimat=F, weighting = NULL,
modelfile = modelfile,
initfile = initfile, datafile = datafile,
macrofile = macrofile, IGLSfile = IGLSfile,
resifile = resifile, resi.store = resi.store,
resioptions = resioptions, debugmode = debugmode)
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
  Removes the fixed part of random variable(s). \code{NA} if no variable to be removed.
}
  \item{clre}{
A matrix used to estimate some, but not all, of the variances and covariances for a set of coefficients at a particular level. Removes from the random part at level <first row> the covariance matrix element(s) defined by the pair(s) of rows <second row> <third row>. Each row corresponds to a removed entry of the covariance matrix.
}
  \item{smat}{An integer vector of length 2 specifying whether the covariance matrix at a particular level is diagonal. The first digit is the level indicator, whilst the second digit is a binary indicator where \code{1} indicates a diagonal covariance matrix and \code{0} indicates the full covariance matrix.
}
  \item{Meth}{
Specifies which maximum likelihood estimation method to be used. If \code{Meth=0} estimation method is set to RIGLS. If \code{Meth=1} estimation method is set to IGLS (the default setting). If \code{Meth} is absent, alternate between IGLS and RIGLS.
}
  \item{BUGO}{
If the first entry of the vector is \code{TRUE}, the current model is outputted in BUGS code. \code{version=4} specifies WinBUGS 1.4 format, whilst \code{version=3} corresponds to WinBUGS 1.3 format. \code{n.chains} specifies the number of chains to be used in WinBUGS/OpenBUGS, \code{n.chains=1} by default. \code{debug} specifies whether BUGS is left open once the model has run, or not (\code{debug=F} by default). \code{seed} sets the random number generator in BUGS. \code{bugs} specifies the location of the WinBUGS/OpenBUGS executable. If\code{OpenBugs=TRUE} OpenBUGS is used, if \code{FALSE} (the default) WinBUGS is used.
}
  \item{optimat}{
This option instructs MLwiN to limit the maximum matrix size that can be allocated by the (R)IGLS algorithm. Specify \code{optimat=TRUE} if MLwiN gives the following error message "Overflow allocating smatrix".  This error message arises if one more higher-level units is extremely large (contains more than 800 lower-level units).  In this situation runmlwin's default behaviour is to instruct MLwiN to allocate a larger matrix size to the (R)IGLS algorithm than is currently possible. Specifying \code{optimat=TRUE} caps the maximum matrix size at 800 lower-level units, circumventing the MLwiN error message, and allowing most MLwiN functionality.  
}
  \item{mem.init}{
A vector which sets and displays worksheet capacities for the current MLwiN session according to the value(s) specified. By default, the number of levels is \code{nlev}+1; worksheet size in thousands of cells is 6000; the number of columns is 2500; the number of explanatory variables is \code{num_vars}+10; the number of group labels is 20. \code{nlev} is the number of levels specified by \code{levID}, and \code{num_vars} is approximately the number of explanatory variables calculated initially.
}
  \item{weighting}{
A list of objects specified for using weights in an analysis, including \code{levels}, \code{weights}, \code{mode}, \code{FSDE} and \code{RSDE}. By default, \code{weighting} is \code{NULL}, i.e., equal weights for all units at all levels. If not \code{NULL}, \code{weights} is a string vector of names specifying the weights for all corresponding \code{levels} that is an integer vector of the same length of \code{weights}. \code{mode=0}: Weighting off (equal weights); \code{mode=1}: the specified raw weights are used at all levels; \code{mode=2}: the standardised weights are used at all levels. \code{FSDE} and \code{RSDE} set the type of standard error computation for fixed and random parameters. If \code{FSDE} and/or \code{RSDE} are/is ignored, \code{FSDE=2} and/or \code{RSDE=2} by default. If \code{FSDE=2} (or \code{RSDE=2}), compute robust or `sandwich' standard errors based on raw residuals. If \code{FSDE=0} (or \code{RSDE=0}), use standard, uncorrected, IGLS or RIGLS computation. Note that weights cannot be used with multivariate models.
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
A file name where the parameter estimates will be saved. The default location is in the temporary folder.
}
  \item{resifile}{
A file name where the residuals will be saved. The default location is in the temporary folder.
}
  \item{resi.store}{
A logical value to indicate if the residuals are to be stored (\code{TRUE}) or not (\code{FALSE}).
}
   \item{resioptions}{
A string vector to specify the various residual options. The \code{"variances"} option calculates the posterior variances instead of the posterior standard errors; the \code{"standardised"}, \code{"leverage"}, \code{"influence"} and \code{"deletion"} options calculate standardised, leverage, influence and deletion residuals respectively; the \code{"sampling"} option calculates the sampling variance covariance matrix for the residuals; the \code{"norecode"} option prevents residuals with values exceedingly close or
equal to zero from being recoded to missing; the reflate option returns unshrunken residuals. Note that the default option is resioptions=c("variance","sampling"); "variance" cannot be used together with the other options to calculate standardised, leverage, influence and deletion residuals.
}
  \item{debugmode}{
A logical value determining whether MLwiN is run in the background or not. The default value is \code{FALSE}: i.e. MLwiN is run in the background. If \code{TRUE} MLwiN remains open after the model has run, allowing the user to interact with MLwiN directly (but note that output will not be returned to R until MLwiN is closed).
}

}
%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The MLwiN macro file is created in the temporary directory (\code{tempdir()}) and will be displayed on screen if \code{show.file=TRUE}..
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
Zhang, Z., Charlton, C.M.J., Parker, R.M.A., Leckie, G., and Browne, W.J. (2012) Centre for Multilevel Modelling, University of Bristol.
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[R2MLwiN]{MacroScript2},\link{tempdir}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
