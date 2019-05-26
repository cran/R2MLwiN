#' This function captures output files from MLwiN for estimation in
#' WinBUGS/OpenBUGS.
#' 
#' This function allows R to call WinBUGS using the output files from MLwiN.
#' This function uses functionalities in the \code{\link[R2WinBUGS]{R2WinBUGS}}
#' package.
#' 
#' @param D A vector specifying the type of distribution used in the model.
#' @param levID A character (vector) specifying the level ID(s).
#' @param datafile A file name where the BUGS data file will be saved in
#' .txt format.
#' @param initfiles A list of file names where the BUGS initial values will
#' be saved in .txt format.
#' @param modelfile A file name where the BUGS model will be saved in .txt
#' format.
#' @param bugEst A file name where the estimates from BUGS will be stored in
#' .txt format.
#' @param fact A list of objects used to specify factor analysis. See `Details'
#' below.
#' @param addmore A vector of strings specifying additional coefficients to be
#' monitored.
#' @param n.chains The number of chains to be monitored.
#' @param n.iter The number of iterations for each chain
#' @param n.burnin The length of burn-in for each chain
#' @param n.thin Thinning rate
#' @param debug A logical value indicating whether (\code{TRUE}) or not
#' (\code{FALSE}; the default) to close the BUGS window after completion of the
#' model run
#' @param bugs The full name (including the path) of the BUGS executable
#' @param bugsWorkingDir A directory where all the intermediate files are to be
#' stored; defaults to \code{tempdir()}.
#' @param OpenBugs If \code{TRUE}, OpenBUGS is used, if \code{FALSE} (the
#' default) WinBUGS is used.
#' @param cleanBugsWorkingDir If \code{TRUE}, the generated files will be
#' removed from the \code{bugsWorkingDir}; defaults to \code{FALSE}.
#' @param seed An integer specifying the random seed.
#'
#' @details
#' A list of objects to specify factor analysis, as used in the
#' argument \code{fact}:
#' \itemize{
#' \item \code{nfact}: specifies the number of factors;
#' \item \code{lev.fact}: Specifies the level/classification for the random part of
#' the factor for each factor;
#' \item \code{nfactcor}: specifies the number of
#' correlated factors;
#' \item \code{factcor}: a vector specifying the correlated
#' factors: the first element corresponds to the first factor number, the
#' second to the second factor number, the third element corresponds to the
#' starting value for the covariance and the fourth element to whether this
#' covariance is constrained
#' (\code{1}) or not (\code{0}). If more than one pair of factors is correlated,
#' then repeat this sequence for each pair.
#' \item \code{loading}: a matrix specifying the
#' starting values for the factor loadings and the starting value of the factor
#' variance. Each row corresponds to a factor.
#' \item \code{constr}: a matrix
#' specifying indicators of whether the factor loadings and the factor variance
#' are constrained (\code{1}) or not (\code{0}).
#' }
#'
#' @return Returns an \code{\link[coda]{mcmc}} object.
#'
#' @author Zhang, Z., Charlton, C.M.J., Parker, R.M.A., Leckie, G., and Browne,
#' W.J. (2016) Centre for Multilevel Modelling, University of Bristol.
#'
#' @seealso \code{\link{runMLwiN}},\code{\link[R2WinBUGS]{bugs}}
#' @export
mlwin2bugs <- function(D,levID, datafile, initfiles, modelfile, bugEst, fact, addmore, n.chains, n.iter, n.burnin, n.thin, debug=FALSE, bugs,
                       bugsWorkingDir=tempdir(), OpenBugs = FALSE, cleanBugsWorkingDir = FALSE, seed = NULL){

  
  nlev= length(levID)
  if(nlev==1){
    parameters=c("beta","va0","sigma")
  }else{
    if (D[1]=="Multivariate Normal"||D[1]=="Multinomial"||D[1]=="Mixed"){
      ux=sapply(3:nlev, function(x) paste("u",x,sep=""))
      sigma2=sapply(2:nlev, function(x) paste("sigma2.u",x,sep=""))
      if (!is.null(fact)){
        nfact=fact$nfact
        loadname=NULL
        sigma2.fact.name=factname=rep(0,nfact)
        for (i in 1:nfact){
          loadname=c(loadname,sapply(1:(ncol(fact$loading)-1), function(x) paste("load",i,".",x,sep="")))
          factname[i]=paste("fact",i,sep="")
          sigma2.fact.name[i]=paste("sigma2.fact",i,sep="")
        }
        parameters=c("beta",ux, sigma2, loadname,factname,sigma2.fact.name)
      }else{
        parameters=c("beta",ux, sigma2)
      }
    }else{
      if(D[1]=="t"){
        ux=sapply(2:nlev, function(x) paste("u",x,sep=""))
        sigma2=sapply(2:nlev, function(x) paste("sigma2.u",x,sep=""))
        parameters=c("beta",ux, sigma2, "va0", "sigma","sigma2","df")
      }else{
        ux=sapply(2:nlev, function(x) paste("u",x,sep=""))
        sigma2=sapply(2:nlev, function(x) paste("sigma2.u",x,sep=""))
        parameters=c("beta",ux, sigma2, "va0", "sigma","sigma2")
      }
    }
  }
  if (!is.null(addmore)) parameters=c(parameters,addmore)

  program <- "OpenBUGS"
  if (!OpenBugs) {
      program <- "WinBUGS"
  }
  chain.bugs <- R2WinBUGS::bugs(data = datafile, inits = initfiles,
                             parameters.to.save = parameters, model.file = modelfile, 
                             n.chains = n.chains, n.iter = n.iter, n.burnin = n.burnin, n.thin = n.thin,
                             debug = debug, DIC = TRUE, codaPkg = FALSE,
                             program = program, working.directory = bugsWorkingDir, clearWD = cleanBugsWorkingDir,
                             bugs.seed = seed)
  chains.bugs.mcmc <- as.mcmc.list(chain.bugs)

  chains.bugs.mcmc
}
