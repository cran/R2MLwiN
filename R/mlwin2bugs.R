mlwin2bugs <-
function(D,levID, datafile, initfile, modelfile, bugEst, fact, addmore, n.chains, n.iter, n.burnin, n.thin, debug=T, bugs,
    bugsWorkingDir=tempdir(), OpenBugs = F, cleanBugsWorkingDir = FALSE){

    PACKages<-as.character(as.data.frame(installed.packages())$Package)
    packs.req= "rbugs"
    test<-( packs.req %in% PACKages)
    if (!all(test))
	       install.packages(packs.req[!test],repos="http://cran.r-project.org")
    require(rbugs)
    rbugs2=function (data.file, inits.files, paramSet, model, bugEst, fact, n.chains = 1, n.iter = 2000,
        n.burnin = floor(n.iter/2), n.thin = max(1, floor(n.chains *
            (n.iter - n.burnin)/1000)), dic = FALSE, debug = FALSE,
        bugs = system("which OpenBUGS", TRUE), bugsWorkingDir, OpenBugs = TRUE,
        cleanBugsWorkingDir = FALSE, genFilesOnly = FALSE, verbose = FALSE,
        seed = NULL)
    {
        Windows = FALSE
        os.type <- .Platform$OS.type
        if (length(bugsWorkingDir) == 0)
            stop("It is required to specify the bugsWorkingDir")
        if (os.type == "windows") {
            if (!file.exists(bugs))
                stop(paste("BUGS executable", bugs, "does not exists."))
            Windows = TRUE
        }
        else if (os.type == "unix") {
            if (length(bugs) == 0)
                bugs <- system("which OpenBUGS", TRUE)
            if (length(bugs) == 0)
                stop(paste("BUGS executable", bugs, "does not exists."))
        }
        else warning("This function has not been tested on mac-os.")
        #bugsWorkingDir <- filePathAsAbsolute(bugsWorkingDir)
        if (is.null(bugsWorkingDir)) {
            bugsWorkingDir <- tempfile("bugsWorkingDir")
            if (!file.exists(bugsWorkingDir))
                dir.create(bugsWorkingDir)
            on.exit(if (cleanBugsWorkingDir) unlink(bugsWorkingDir,
                TRUE))
        }
        workingDir <- bugsWorkingDir
        #workingDir <- filePathAsAbsolute(workingDir)
        if (!file.exists(model)) stop("Model file doesn't exits.")
        model.file <- file.path(workingDir, "model.txt")
        file.copy(model, model.file, overwrite = TRUE)


        script.file <- paste(workingDir, "script.txt", sep = "/")
        genBugsScript(paramSet, n.chains, n.iter, n.burnin, n.thin,
            dic, model.file, data.file, inits.files, bugsWorkingDir,
            script.file, debug, OpenBugs, Windows, seed)
            
        trLbr <- function(unix) {
          lines <- readLines(unix)
          #newlines <- sub('$', '\r', lines)
          #writeLines(newlines, unix)
          writeLines(lines, unix, sep="\r\n")
        }
        if (OpenBugs) {
            trLbr(model.file)
            trLbr(data.file)
            for (i in inits.files) trLbr(i)
        }
        if (genFilesOnly) {
            cat("Files are generated in", workingDir, "\n")
            return(TRUE)
        }
        runBugs(bugs, script.file, n.chains, workingDir, OpenBugs,
            Windows, verbose)
        all <- getBugsOutput(n.chains, workingDir, OpenBugs)  ##Modified by Marcos
        all$n.iter = n.iter                                   ##Modified by Marcos
        all$n.burnin = n.burnin                               ##Modified by Marcos
        all$n.thin = n.thin                                   ##Modified by Marcos
 
         #### functions to get the output
        getCodaFileNames <- function(n.chains, workingDir, OpenBugs) {
          CODA <- if (OpenBugs) "codaCODA" else "coda"
          INDEX <- if (OpenBugs) "index" else "Index"
          CHAIN <- if (OpenBugs) "chain" else NULL
          coda  <- file.path(workingDir, CODA)
          codaFiles <- paste(coda, CHAIN, 1:n.chains, ".txt", sep="")
          codaIndexFile <- paste(coda, INDEX, ".txt", sep="")
          list(codaFiles=codaFiles, codaIndexFile=codaIndexFile)
        }       
        if(cleanBugsWorkingDir) { ##Modified by Marcos
          fnames <- getCodaFileNames(n.chains, workingDir, OpenBugs) ##Modified by Marcos
          coda.files <- c(fnames$codaIndexFile, fnames$codaFiles) ##Modified by Marcos
          log.file <- file.path(workingDir, "log.txt") ##Modified by Marcos
          file.remove(c(model.file, log.file, data.file, ##Modified by Marcos
                        inits.files, script.file, coda.files)) ##Modified by Marcos
        }
       
        all
    }
    environment(rbugs2)<-environment(rbugs)

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
    chains.bugs=rbugs2(data.file = datafile, inits.files=initfile,
    paramSet=parameters, model=modelfile, bugEst=bugEst, n.chains = n.chains, n.iter = n.iter, n.burnin=n.burnin, n.thin=n.thin, debug=T, bugs=bugs,
    bugsWorkingDir=bugsWorkingDir, OpenBugs=OpenBugs, cleanBugsWorkingDir=cleanBugsWorkingDir)
    chains.bugs.mcmc=rbugs2coda(chains.bugs,burnin=1,n.thin)
    #assign("chains.bugs.mcmc",chains.bugs.mcmc,envir = .GlobalEnv)
    chains.bugs.mcmc
}
