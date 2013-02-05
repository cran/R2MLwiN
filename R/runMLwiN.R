runMLwiN <-
function(Formula, levID, D="Normal", indata, estoptions=list(EstM=0), BUGO=NULL, MLwiNPath="C:/Program Files (x86)/MLwiN v2.26/",workdir=tempdir()) {

    PACKages<-as.character(as.data.frame(installed.packages())$Package)
    packs.req= c("foreign","rbugs","coda")
    test<-( packs.req %in% PACKages)
    if (!all(test))
	       install.packages(packs.req[!test],repos="http://cran.r-project.org")
    require(foreign); require(rbugs); require(coda)

    EstM=estoptions$EstM
    if (is.null(EstM)) EstM=0

    show.file=estoptions$show.file
    if (is.null(show.file)) show.file = F

    resi.store=estoptions$resi.store
    if (is.null(resi.store)) resi.store=F

    resioptions=estoptions$resioptions
    if (is.null(resioptions)) resioptions = c("variance","sampling") #c("standardised","deletion","leverage","influnce","sampling")
    if ("variance"%in%resioptions&&("standardised"%in%resioptions||"deletion"%in%resioptions||"leverage"%in%resioptions)){
        stop("variance will not be calculated together with standardiased or deletion or leverage. Please remove variance in resioptions, and then standard error will be calculated instead.")
    }
    resi.store.levs=estoptions$resi.store.levs

    weighting = estoptions$weighting

    centring = estoptions$centring


    if (!is.null(centring)){
        for (p in names(centring)){
            if(as.integer(centring[[p]][1])==1){
                indata[[p]]=indata[[p]]-mean(indata[[p]])
            }
            if(as.integer(centring[[p]][1])==2){
                indata[[p]]=indata[[p]]-mean(indata[[p]][as.logical(indata[[centring[[p]][2]]])])
            }
            if(as.integer(centring[[p]][1])==3){
                indata[[p]]=indata[[p]]-as.integer(centring[[p]][2])
            }
        }
    }

    debugmode=estoptions$debugmode
    if(is.null(debugmode)) debugmode=F

    x64=estoptions$x64
    if(is.null(x64)) x64=F

    clean.files=estoptions$clean.files
    if (is.null(clean.files)) clean.files=T

    if (!file.access(workdir)==0) dir.create(workdir)

    dtafile = gsub("\\", "/", tempfile("dtafile_",tmpdir =workdir, fileext=".dta"), fixed=TRUE)
    macrofile =gsub("\\", "/", tempfile("macrofile_",tmpdir =workdir, fileext=".txt"), fixed=TRUE)
    IGLSfile =gsub("\\", "/", tempfile("IGLSfile_",tmpdir =workdir, fileext=".dta"), fixed=TRUE)
    if (EstM==1) MCMCfile =gsub("\\", "/", tempfile("MCMCfile_",tmpdir =workdir, fileext=".dta"), fixed=TRUE)
    if (EstM==1) chainfile =gsub("\\", "/", tempfile("chainfile_",tmpdir =workdir, fileext=".dta"), fixed=TRUE)
    if (!is.null(BUGO))  bugofile=gsub("\\", "/", tempfile("bugofile_",tmpdir =workdir, fileext=".txt"), fixed=TRUE) else bugofile=NULL
    if (!is.null(BUGO))  modelfile=gsub("\\", "/", tempfile("modelfile_",tmpdir =workdir, fileext=".txt"), fixed=TRUE) else modelfile=NULL
    if (!is.null(BUGO))  initfile=gsub("\\", "/", tempfile("initfile_",tmpdir =workdir,fileext=".txt"), fixed=TRUE) else initfile=NULL
    if (!is.null(BUGO))  datafile=gsub("\\", "/", tempfile("datafile_",tmpdir =workdir,fileext=".txt"), fixed=TRUE) else datafile=NULL
    if (!is.null(BUGO))  scriptfile=gsub("\\", "/", tempfile("scriptfile_",tmpdir =workdir,fileext=".txt"), fixed=TRUE)
    if (!is.null(BUGO))  bugEst=gsub("\\", "/", tempfile("bugEst_",tmpdir =workdir, fileext=".txt"), fixed=TRUE)
    if (resi.store) resifile=gsub("\\", "/", tempfile("resifile_",tmpdir =workdir,fileext=".dta"), fixed=TRUE)
    if (!is.null(resi.store.levs)) resichains=gsub("\\", "/", tempfile("resichains_",tmpdir =workdir,fileext=".dta"), fixed=TRUE)

    write.dta(indata, dtafile)


    invars=Formula.translate(Formula,levID, D,indata)
    resp=invars$resp
    expl=invars$expl
    D =invars$D
    rp=invars$rp
    nonfp=invars$nonfp
    if(is.null(nonfp)) {
        if(is.list(expl)){
            nonfp=list("nonfp.sep"=NA,"nonfp.common"=NA)
        }else{
            nonfp=NA
        }
    }
    categ=invars$categ
    if(is.null(categ)){
        categ=NULL
    }else{
        if (is.null(rownames(categ))){
            rownames(categ)=c("var","ref","ncateg")
        }
    }


    if (D[1]=='Multinomial'||D[1]=='Multivariate Normal'||D[1]=='Mixed'){
        levID=c(levID,NA)
    }

    clre=estoptions$clre
    smat=estoptions$smat

    notation=estoptions$notation
    if(is.null(notation)) notation="level"


    mem.init=estoptions$mem.init
    if(is.null(mem.init)) mem.init="default"

    nonlinear=estoptions$nonlinear

    if (is.null(nonlinear)) nonlinear=c(0,1)
    Meth=estoptions$Meth
    if(is.null(Meth)) Meth=1

    fact =estoptions$fact
    xclass=estoptions$xclass

    align2right=function(titlename,ele){
        #for printing the table on the screen
        all.ele=c(titlename,ele)
        len.all.ele=nchar(all.ele)
        max.len.ele=max(len.all.ele)
        for (j in 1:length(all.ele)){
            if (len.all.ele[j]<max.len.ele){
                len.diff=max.len.ele-len.all.ele[j]
                all.ele[j]=paste(paste(rep(" ",len.diff),collapse=""),all.ele[j],sep="")
            }
        }

        all.ele
    }

    align2left=function(titlename,ele){
        #for printing the table on the screen
        all.ele=c(titlename,ele)
        len.all.ele=nchar(all.ele)
        max.len.ele=max(len.all.ele)
        for (j in 1:length(all.ele)){
            if (len.all.ele[j]<max.len.ele){
                len.diff=max.len.ele-len.all.ele[j]
                all.ele[j]=paste(all.ele[j],paste(rep(" ",len.diff),collapse=""),sep="")
            }
        }

        all.ele
    }

    signifstar = function(pval){
        starstr="Error"
        if (pval>=0&&pval<=1){
            if(pval<0.001){
                starstr='***'
            }
            if(pval>=0.001&&pval<0.01){
                starstr='** '
            }
            if(pval>=0.01&&pval<0.05){
                starstr='*  '
            }
            if(pval>=0.05&&pval<0.1){
                starstr='.  '
            }
            if(pval>=0.1){
                starstr='   '
            }
        }
        starstr
    }
    if (EstM==0){
        MacroScript1(indata, dtafile,resp, levID, expl, rp, D, nonlinear, categ,notation, nonfp, clre,smat,Meth,
        BUGO,mem.init, weighting, bugofile=bugofile,modelfile=modelfile,initfile=initfile,datafile=datafile,macrofile=macrofile,IGLSfile=IGLSfile,resifile=resifile,resi.store=resi.store,resioptions=resioptions,debugmode=debugmode)
        iterations=estoptions$mcmcMeth$iterations
        if(is.null(iterations)) iterations=5000
        burnin=estoptions$mcmcMeth$burnin
        if(is.null(burnin)) burnin=500
        thinning=estoptions$mcmcMeth$thinning
        if(is.null(thinning)) thinning=1
        WD <- getwd()
        if (x64){
            MLwiNPath1=paste(MLwiNPath,'/x64/',sep='')
        }else{
            MLwiNPath1=paste(MLwiNPath,'/i386/',sep='')
        }
        if (file.access(MLwiNPath1)==0) setwd(MLwiNPath1) else setwd(MLwiNPath)
        if (debugmode){
            if (file.access("mlwin.exe", mode=1)==0){
                cmd=paste("mlwin.exe /run", macrofile)
            }else{
                if (file.access("mlnscript.exe", mode=1)==0){
                    cmd=paste("mlnscript.exe /run", macrofile)
                }
            }
        }else{
            if (file.access("mlnscript.exe", mode=1)==0){
                cmd=paste("mlnscript.exe /run", macrofile)
            }else{
                cmd=paste("mlwin.exe /nogui /run", macrofile)
            }
        }
        time1=proc.time()
        shell(cmd)
        time2=proc.time()-time1
        setwd(WD)
        estIGLS <-read.dta(IGLSfile)

        FP=as.vector(na.omit(estIGLS[,1]))
        names(FP)=FP.names
        RP=as.vector(na.omit(estIGLS[,3]))
        levID0=levID
        if (is.na(levID0[length(levID0)])){
            tmp.RP.names=gsub("RP","",RP.names)
            for (i in 1:length(RP.names)){
                tmpstrlist=unlist(strsplit(tmp.RP.names[i],"\\_"))
                tmpno=as.integer(tmpstrlist[1])-1
                RP.names[i]=paste("RP",tmpno,"_",paste(tmpstrlist[-1],collapse="_"),sep="")
            }
        }
        names(RP)=RP.names
        LIKE=na.omit(estIGLS[,dim(estIGLS)[2]])[1]
        if(LIKE==1) LIKE=NA
        cat("\n")
        cat(paste(rep("-",50),collapse="*"),"\n")
        cat("MLwiN multilevel model",paste("(",D[1],")",sep=""),"\n")
        cat("Estimation algorithm:  IGLS        Elapsed time :",paste(round(time2[3],2),"s",sep=""), "\n")
        cat("Number of obs: ",length(indata[[1]]),"\n")
        cat(paste("Deviance statistic: ", round(LIKE,1)),"\n")
        cat(paste(rep("-",50),collapse="-"),"\n")
        cat("The model formula:\n")
        cat(gsub("[[:space:]]","",Formula),"\n")
        levID.display=""
        if (is.na(levID0[length(levID0)])){
            levID0=levID0[-length(levID0)]
        }
        for (i in 1:length(levID0)){
            levID.display=paste(levID.display,"Level ",length(levID0)+1-i,": ",levID0[i],"     ",sep="")
        }
        cat(levID.display,"\n")
        cat(paste(rep("-",50),collapse="-"),"\n")


        estIGLS2=na.omit(estIGLS[,2])
        FP.cov=matrix(NA,length(FP),length(FP))
        k=1
        for (i in 1:length(FP)){
            for (j in 1:i){
                FP.cov[i,j]=estIGLS2[k]
                k=k+1
            }
        }
        colnames(FP.cov)=rownames(FP.cov)=FP.names
        estIGLS4=na.omit(estIGLS[,4])
        RP.cov=matrix(NA,length(RP),length(RP))
        k=1
        for (i in 1:length(RP)){
            for (j in 1:i){
                RP.cov[i,j]=estIGLS4[k]
                k=k+1
            }
        }
        colnames(RP.cov)=rownames(RP.cov)=RP.names

        cat("The fixed part estimates: ","\n")
        FP.print=rbind(FP,sqrt(diag(FP.cov)))
        z.score=FP.print[1,]/FP.print[2,]
        p.value=2 * pnorm(abs(z.score), lower.tail = FALSE)
        strstar=as.vector(sapply(p.value,signifstar))
        qt025=FP.print[1,]-qnorm(.975)*FP.print[2,]
        qt975=FP.print[1,]+qnorm(.975)*FP.print[2,]
        FP.print=rbind(FP.print,z.score,p.value,qt025,qt975)
        FP.names2=gsub("FP+\\_","",FP.names)

        printcol0=align2left("        ",FP.names2)
        printcol1=align2right("Coef.",format(round(FP.print[1,],5),nsmall = 5))
        printcol2=align2right("Std. Err.",format(round(FP.print[2,],5),nsmall = 5))
        printcol3=align2right("z",format(round(FP.print[3,],2),nsmall = 2))
        printcol4=align2right("Pr(>|z|)",formatC(FP.print[4,]))
        printcol4b=align2right("   ",strstar)
        printcol5=align2right("[95% Conf.",format(round(FP.print[5,],5),nsmall = 5))
        printcol6=align2right("Interval]",format(round(FP.print[6,],5),nsmall = 5))
        for (i in 1:(ncol(FP.print)+1)){
            cat(printcol0[i]," ",printcol1[i]," ",printcol2[i]," ",printcol3[i]," ",printcol4[i]," ",printcol4b[i]," ",printcol5[i]," ",printcol6[i],"\n")
        }
        cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 ","\n")
        nlev=length(levID)
        if (is.na(levID[length(levID)])){
            mlwinlev=(nlev-1):1
            levID2=levID0
        }else{
            mlwinlev=nlev:1
            levID2=levID
        }

        RP.print=rbind(RP,sqrt(diag(RP.cov)))
        qt025=RP.print[1,]-qnorm(.975)*RP.print[2,]
        qt975=RP.print[1,]+qnorm(.975)*RP.print[2,]
        RP.print=rbind(RP.print,qt025,qt975)
        for (i in 1:length(mlwinlev)){
                RPx.pos=grep(paste("RP",mlwinlev[i],sep=""),RP.names)
                if (length(RPx.pos)!=0){
                    cat(paste(rep("-",50),collapse="-"),"\n")
                    RPx.names=gsub(paste("RP+",mlwinlev[i],"+\\_",sep=""),"",RP.names[RPx.pos])
                    RPx = as.matrix(RP.print[,RPx.pos],nrow=4)
                    printcol0=align2left("        ",RPx.names)
                    printcol1=align2right("Coef.",format(round(RPx[1,],5),nsmall = 5))
                    printcol2=align2right("Std. Err.",format(round(RPx[2,],5),nsmall = 5))
                    printcol5=align2right("[95% Conf.",format(round(RPx[3,],5),nsmall = 5))
                    printcol6=align2right("Interval]",format(round(RPx[4,],5),nsmall = 5))
                    cat("The random part estimates at the",levID2[i],"level:","\n")
                    for (i in 1:(ncol(RPx)+1)){
                        cat(printcol0[i]," ",printcol1[i]," ",printcol2[i]," ",printcol5[i]," ",printcol6[i],"\n")
                    }
                }
        }
        cat(paste(rep("-",50),collapse="*"),"\n")

    }

    # MCMC algorithm (using the starting values obtain from IGLS algorithm)
    if (EstM==1){
        mcmcMeth=estoptions$mcmcMeth
        startval=mcmcMeth$startval
        seed=mcmcMeth$seed
        if(is.null(seed)) seed=1
        iterations=mcmcMeth$iterations
        if(is.null(iterations)) iterations=5000
        burnin=mcmcMeth$burnin
        if(is.null(burnin)) burnin=500
        thinning=mcmcMeth$thinning
        if(is.null(thinning)) thinning=1
        priorParam=mcmcMeth$priorParam
        if(is.null(priorParam)) priorParam="default"
        scale =mcmcMeth$scale
        if(is.null(scale)) scale=5.8
        refresh=mcmcMeth$refresh
        if(is.null(refresh)) refresh=50
        fixM=mcmcMeth$fixM
        if(is.null(fixM)) {
            if(D[1]=="Poisson"||D[1]=="Multinomial"||D[1]=="Binomial"||D[1]=="Mixed"){
                fixM=2
            }else{
                fixM=1
            }
        }
        residM=mcmcMeth$residM
        if(is.null(residM)){
            if(D[1]=="Poisson"||D[1]=="Multinomial"||D[1]=="Binomial"||D[1]=="Mixed"){
                residM=2
            }else{
                residM=1
            }
        }
        Lev1VarM=mcmcMeth$Lev1VarM
        if(is.null(Lev1VarM)){
            if(D[1]=="Poisson"||D[1]=="Multinomial"||D[1]=="Binomial"||D[1]=="Mixed"){
                Lev1VarM=2
            }else{
                Lev1VarM=1
            }
        }
        OtherVarM=mcmcMeth$OtherVarM
        if(is.null(OtherVarM)) OtherVarM=1
        adaption=mcmcMeth$adaption
        if(is.null(adaption)) adaption=1
        priorcode=mcmcMeth$priorcode
        if (is.null(priorcode)) priorcode=1
        rate=mcmcMeth$rate
        if (is.null(rate)) rate=50
        tol=mcmcMeth$tol
        if (is.null(tol)) tol=10
        lclo=mcmcMeth$lclo
        if (is.null(lclo)) lclo =0
        nopause=mcmcMeth$nopause
        if (is.null(nopause)) nopause=F
        dami=mcmcMeth$dami
        if (!is.null(dami)){
            esamplefile=gsub("\\", "/", tempfile("esamplefile_",tmpdir =workdir, fileext=".dta"), fixed=TRUE)
        }else{
            dami=esamplefile=NULL
        }
        if (!is.null(fact)){
            FACTchainfile=gsub("\\", "/", tempfile("factchainfile_",tmpdir =workdir,fileext=".dta"), fixed=TRUE)
        }else{
            FACTchainfile=NULL
        }
        mcmcOptions=estoptions$mcmcOptions
        if (D[1]=='Multivariate Normal'){
            if(!is.null(mcmcOptions)) {
                mcmcOptions2=list(orth=0,hcen=0, smcm=0,smvn=0, paex=c(2,0),mcco=0)
                for (ii in names(mcmcOptions)) mcmcOptions2[[ii]]=mcmcOptions[[ii]]
                mcmcOptions=mcmcOptions2
            }else{
                mcmcOptions=list(orth=0,hcen=0, smcm=0,smvn=0, paex=c(2,0),mcco=0)
            }
        }else{
            if(!is.null(mcmcOptions)) {
                mcmcOptions2=list(orth=0,hcen=0, smcm=0,smvn=0, paex=c(2,0))
                for (ii in names(mcmcOptions)) mcmcOptions2[[ii]]=mcmcOptions[[ii]]
                mcmcOptions=mcmcOptions2
            }else{
                mcmcOptions=list(orth=0,hcen=0, smcm=0,smvn=0, paex=c(2,0))
            }
        }
        merr=estoptions$merr



        MacroScript2(indata, dtafile,resp, levID, expl, rp, D,nonlinear, categ,notation,nonfp,clre,smat,Meth,merr,seed,iterations,burnin,scale,thinning,priorParam,refresh,fixM,residM,Lev1VarM, OtherVarM,adaption,priorcode,rate, tol,lclo,mcmcOptions,fact,xclass,BUGO,mem.init,
        nopause,bugofile=bugofile,modelfile=modelfile,initfile=initfile,datafile=datafile,macrofile=macrofile,IGLSfile=IGLSfile,MCMCfile=MCMCfile,
        chainfile=chainfile,esamplefile=esamplefile,resifile=resifile,resi.store=resi.store,resioptions=resioptions,resichains=resichains,FACTchainfile=FACTchainfile,resi.store.levs=resi.store.levs,debugmode=debugmode,startval=startval, dami=dami)
        WD <- getwd()
        if (x64){
            MLwiNPath1=paste(MLwiNPath,'/x64/',sep='')
        }else{
            MLwiNPath1=paste(MLwiNPath,'/i386/',sep='')
        }
        if (file.access(MLwiNPath1)==0) setwd(MLwiNPath1) else setwd(MLwiNPath)
        if (debugmode){
            if (file.access("mlwin.exe", mode=1)==0){
                cmd=paste("mlwin.exe /run", macrofile)
            }else{
                if (file.access("mlnscript.exe", mode=1)==0){
                    cmd=paste("mlnscript.exe /run", macrofile)
                }
            }
        }else{
            if (file.access("mlnscript.exe", mode=1)==0){
                cmd=paste("mlnscript.exe /run", macrofile)
            }else{
                cmd=paste("mlwin.exe /nogui /run", macrofile)
            }
        }
        cat("MLwiN is running, please wait......\n")
        time1=proc.time()
        shell(cmd)
        time2=proc.time()-time1
        setwd(WD)

        estMCMC <-read.dta(MCMCfile)
        chains <- read.dta(chainfile)

        chain.names=names(chains)
        FP.names=chain.names[grep('FP',chain.names)]
        RP.names=chain.names[grep('RP',chain.names)]
        FP=as.vector(na.omit(estMCMC[,1]))
        names(FP)=FP.names
        RP=as.vector(na.omit(estMCMC[,3]))
        levID0=levID
        if (is.na(levID0[length(levID0)])){
            tmp.RP.names=gsub("RP","",RP.names)
            for (i in 1:length(RP.names)){
                tmpstrlist=unlist(strsplit(tmp.RP.names[i],"\\_"))
                tmpno=as.integer(tmpstrlist[1])-1
                RP.names[i]=paste("RP",tmpno,"_",paste(tmpstrlist[-1],collapse="_"),sep="")
            }
            chain.names[grep('RP',chain.names)] =RP.names
            names(chains)=chain.names
        }
        names(RP)=RP.names
        Chains=mcmc(data=chains[,-c(1,2)])
        ESS=effectiveSize(Chains)
        cat("\n")
        cat(paste(rep("-",50),collapse="*"),"\n")
        cat("MLwiN multilevel model",paste("(",D[1],")",sep=""),"\n")
        if (is.null(xclass)){
            cat("Estimation algorithm:  MCMC      Elapsed time :",paste(round(time2[3],2),"s",sep=""), "\n")
        }else{
            cat("Estimation algorithm:  MCMC      Cross-classified              Elapsed time :",paste(round(time2[3],2),"s",sep=""), "\n")
        }
        cat("Number of obs: ",length(indata[[1]]),"            Number of iter.:",iterations,"            Burn-in:", burnin , "\n")


        if (!(D[1]=="Mixed")&&is.null(merr)&&is.null(fact)){
            BDIC=na.omit(estMCMC[,dim(estMCMC)[2]])[c(3,4,2,1)]
            BDIC.names=c("Dbar", "D(thetabar)",  "pD", "DIC")
            names(BDIC)=BDIC.names
            cat("Bayesian Deviance Information Criterion (DIC)\n")
            cat("Dbar      D(thetabar)    pD      DIC\n")
            cat(formatC(BDIC,format="f",digits=3,width=-10),"\n")
        }else{
            LIKE=na.omit(estMCMC[,dim(estMCMC)[2]])[1]
            if(LIKE==1) LIKE=NA
            cat(paste("Deviance statistic: ", round(LIKE,1)),"\n")
        }
        cat(paste(rep("-",50),collapse="-"),"\n")
        cat("The model formula:\n")
        cat(gsub("[[:space:]]","",Formula),"\n")
        levID.display=""
        if (is.na(levID0[length(levID0)])){
            levID0=levID0[-length(levID0)]
        }
        for (i in 1:length(levID0)){
            levID.display=paste(levID.display,"Level ",length(levID0)+1-i,": ",levID0[i],"     ",sep="")
        }
        cat(levID.display,"\n")
        cat(paste(rep("-",50),collapse="-"),"\n")
        if (!is.null(fact)){
            loadings=na.omit(estMCMC[,5])
            load.names=rep(NA,length(loadings))
            k=1
            for (i in 1:fact$nfact){
                for (j in resp){
                    load.names[k]=paste("load",i,"_",j,sep="")
                    k=k+1
                }
            }
            loadings.sd=na.omit(estMCMC[,6])
            qt025=loadings-qnorm(.975)*loadings.sd
            qt975=loadings+qnorm(.975)*loadings.sd
            loads=rbind(loadings,loadings.sd,qt025,qt975)
            colnames(loads)=load.names

            for (j in 1:fact$nfact){
                cat("The estimates of factor",j,"loadings:\n")
                loadx.names=load.names[grep(paste("load+",j,"+\\_",sep=""),load.names)]
                loadx=loads[,loadx.names]
                printcol0=align2left("        ",loadx.names)
                printcol1=align2right("Coef.",format(round(loadx[1,],5),nsmall = 5))
                printcol2=align2right("Std. Err.",format(round(loadx[2,],5),nsmall = 5))
                printcol3=align2right("[95% Conf.",format(round(loadx[3,],5),nsmall = 5))
                printcol4=align2right("Interval]",format(round(loadx[4,],5),nsmall = 5))
                for (i in 1:(ncol(loadx)+1)){
                    cat(printcol0[i]," ",printcol1[i]," ",printcol2[i]," ",printcol3[i]," ",printcol4[i],"\n")
                }
                cat(paste(rep("-",50),collapse="-"),"\n")
            }

            fact.cov=fact.cov.names=na.omit(estMCMC[,7])
            fact.cov.sd=na.omit(estMCMC[,8])
            k=1
            for (i in 1:fact$nfact){
                for(j in 1:i){
                    if(i==j){
                        fact.cov.names[k]=paste("var_fact",i,sep="")
                    }else{
                        fact.cov.names[k]=paste("cov_fact",i,"_fact",j,sep="")
                    }
                    k=k+1
                }
            }
            qt025=fact.cov-qnorm(.975)*fact.cov.sd
            qt975=fact.cov+qnorm(.975)*fact.cov.sd
            fcov=rbind(fact.cov,fact.cov.sd,qt025,qt975)
            colnames(fcov)=fact.cov.names

            cat("The estimates of factor covariances:\n")
            printcol0=align2left("        ",fact.cov.names)
            printcol1=align2right("Coef.",format(round(fcov[1,],5),nsmall = 5))
            printcol2=align2right("Std. Err.",format(round(fcov[2,],5),nsmall = 5))
            printcol3=align2right("[95% Conf.",format(round(fcov[3,],5),nsmall = 5))
            printcol4=align2right("Interval]",format(round(fcov[4,],5),nsmall = 5))
            for (i in 1:(ncol(fcov)+1)){
                cat(printcol0[i]," ",printcol1[i]," ",printcol2[i]," ",printcol3[i]," ",printcol4[i],"\n")
            }
            cat(paste(rep("-",50),collapse="-"),"\n")

        }


        estMCMC2=na.omit(estMCMC[,2])
        FP.cov=matrix(NA,length(FP),length(FP))
        k=1
        for (i in 1:length(FP)){
            for (j in 1:i){
                FP.cov[i,j]=estMCMC2[k]
                k=k+1
            }
        }
        colnames(FP.cov)=rownames(FP.cov)=FP.names
        estMCMC4=na.omit(estMCMC[,4])
        RP.cov=matrix(NA,length(RP),length(RP))
        k=1
        for (i in 1:length(RP)){
            for (j in 1:i){
                RP.cov[i,j]=estMCMC4[k]
                k=k+1
            }
        }
        colnames(RP.cov)=rownames(RP.cov)=RP.names

        cat("The fixed part estimates: ","\n")
        FP.print=rbind(FP,sqrt(diag(FP.cov)))
        if (sum(grepl("bcons",colnames(Chains)))>0){
            bcons.pos=grep("bcons",colnames(Chains))
            Chains[1,bcons.pos]=Chains[1,bcons.pos]-0.001
        }

        t.stats=apply(Chains,2,function(x) mean(x)/sd(x))
        p.values=2*pnorm(abs(t.stats),lower.tail =F)
        strstar=as.vector(sapply(p.values,signifstar))
        t.stat=NULL
        for (i in FP.names)  t.stat=c(t.stat, t.stats[[i]])
        p.value=NULL
        for (i in FP.names)  p.value=c(p.value, p.values[[i]])
        qt025=NULL
        for (i in FP.names)  qt025=c(qt025, quantile(Chains[,i],.025))
        qt975=NULL
        for (i in FP.names)  qt975=c(qt975, quantile(Chains[,i],.975))
        FP.print=rbind(FP.print,t.stat,p.value,qt025,qt975,ESS[FP.names])
        FP.names2=gsub("FP+\\_","",FP.names)

        printcol0=align2left("        ",FP.names2)
        printcol1=align2right("Coef.",format(round(FP.print[1,],5),nsmall = 5))
        printcol2=align2right("Std. Err.",format(round(FP.print[2,],5),nsmall = 5))
        printcol3=align2right("z",format(round(FP.print[3,],2),nsmall = 2))
        printcol4=align2right("Pr(>|z|)",formatC(FP.print[4,]))
        printcol4b=align2right("   ",strstar)
        printcol5=align2right("[95% Conf.",format(round(FP.print[5,],5),nsmall = 5))
        printcol6=align2right("Interval]",format(round(FP.print[6,],5),nsmall = 5))
        printcol7=align2right("ESS",format(round(FP.print[7,]),nsmall=0))
        for (i in 1:(ncol(FP.print)+1)){
            cat(printcol0[i]," ",printcol1[i]," ",printcol2[i]," ",printcol3[i]," ",printcol4[i]," ",printcol4b[i]," ",printcol5[i]," ",printcol6[i]," ",printcol7[i],"\n")
        }
        cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 ","\n")

        nlev=length(levID)
        if (is.na(levID[length(levID)])){
            mlwinlev=(nlev-1):1
            levID2=levID0
        }else{
            mlwinlev=nlev:1
            levID2=levID
        }

        RP.print=rbind(RP,sqrt(diag(RP.cov)))
        qt025=NULL
        for (i in RP.names)  qt025=c(qt025, quantile(Chains[,i],.025))
        qt975=NULL
        for (i in RP.names)  qt975=c(qt975, quantile(Chains[,i],.975))
        RP.print=rbind(RP.print,qt025,qt975,ESS[RP.names])
        for (i in 1:length(mlwinlev)){
                RPx.pos=grep(paste("RP",mlwinlev[i],sep=""),RP.names)
                if (length(RPx.pos)!=0){
                    cat(paste(rep("-",50),collapse="-"),"\n")
                    RPx.names=gsub(paste("RP+",mlwinlev[i],"+\\_",sep=""),"",RP.names[RPx.pos])
                    RPx = as.matrix(RP.print[,RPx.pos],nrow=4)
                    printcol0=align2left("        ",RPx.names)
                    printcol1=align2right("Coef.",format(round(RPx[1,],5),nsmall = 5))
                    printcol2=align2right("Std. Err.",format(round(RPx[2,],5),nsmall = 5))
                    printcol5=align2right("[95% Conf.",format(round(RPx[3,],5),nsmall = 5))
                    printcol6=align2right("Interval]",format(round(RPx[4,],5),nsmall = 5))
                    printcol7=align2right("ESS",format(round(RPx[5,]),nsmall = 0))
                    cat("The random part estimates at the",levID2[i],"level:","\n")
                    for (i in 1:(ncol(RPx)+1)){
                        cat(printcol0[i]," ",printcol1[i]," ",printcol2[i]," ",printcol5[i]," ",printcol6[i]," ",printcol7[i],"\n")
                    }
                }
        }
        cat(paste(rep("-",50),collapse="*"),"\n")
    }

    if (show.file) file.show(macrofile)
    if ((!is.null(BUGO))&&!(D[1]=="Mixed")){
        if (show.file) file.show(modelfile)
        n.iter=iterations+burnin
        addmore=NULL
        if(EstM==1){
            if(!is.null(xclass)&&length(xclass)==5){
	if(sum(xclass[[5]])>0){
	       addmore=c(addmore,"carmean")
	}
            }	
            if(is.matrix(mcmcOptions$paex)){
                if(sum(mcmcOptions$paex[,2])>0){
                    vx=sapply(2:nlev, function(x) paste("v",x,sep=""))
                    sigma2v=sapply(2:nlev, function(x) paste("sigma2.v",x,sep=""))
                    addmore=c(addmore,vx,sigma2v)
                }
            }else{
                if(is.vector(mcmcOptions$paex)&&length(mcmcOptions$paex)==2){
                    if(mcmcOptions$paex[2]>0){
                        vx=sapply(2:nlev, function(x) paste("v",x,sep=""))
                        sigma2v=sapply(2:nlev, function(x) paste("sigma2.v",x,sep=""))
                        addmore=c(addmore, vx,sigma2v)
                    }
                }
            }
        }
        #if(show.file) file.show(bugEst)
        chains.bugs.mcmc=mlwin2bugs(D,levID, datafile, initfile, modelfile, bugEst, fact, addmore, n.chains = as.numeric(BUGO[2]), n.iter = n.iter, n.burnin=burnin, n.thin=thinning, debug=T, bugs=BUGO[3],
        bugsWorkingDir=workdir, OpenBugs = as.logical(BUGO[4]), cleanBugsWorkingDir=clean.files)
        time2=proc.time()-time1
    }else{
        if (D[1]=="Mixed"&&(!is.null(BUGO)))  warning("The Mixed response model is currently not implemented in WinBUGS/OpenBUGS.")
    }

    if (EstM==0){


        outIGLS=new("mlwinfitIGLS")
        outIGLS["Nobs"]=nrow(indata)
        outIGLS["D"]=D
        outIGLS["Formula"]=Formula
        outIGLS["levID"]=levID
        outIGLS["estIGLS"]=estIGLS
        outIGLS["FP"]=FP
        outIGLS["RP"]=RP
        outIGLS["FP.cov"]=FP.cov
        outIGLS["RP.cov"]=RP.cov
        outIGLS["LIKE"]=LIKE
        outIGLS["elapsed.time"]=time2[3]
        if ((!is.null(BUGO))&&!(D[1]=="Mixed")){
            outIGLS["chains.bugs"]=chains.bugs.mcmc
        }

        if (resi.store){
            resiraw=read.dta(resifile)
            residelpos=grep("^[c]?[[:digit:]]+$", names(resiraw))
            if(length(residelpos)==0){
                outIGLS["residual"]=resiraw
            }else{
                resisavename=names(resiraw)[-residelpos]
                outIGLS["residual"]=resiraw[resisavename]
            }
        }

    }
    if (EstM==1){



        outMCMC=new("mlwinfitMCMC")

        outMCMC["Nobs"]=nrow(indata)
        outMCMC["burnin"]=burnin
        outMCMC["iterations"]=iterations
        outMCMC["D"]=D
        outMCMC["Formula"]=Formula
        outMCMC["levID"]=levID
        outMCMC["merr"]=merr
        outMCMC["fact"]=fact
        outMCMC["xclass"]=xclass
        outMCMC["estMCMC"]=estMCMC
        outMCMC["FP"]=FP
        outMCMC["RP"]=RP
        outMCMC["FP.cov"]=FP.cov
        outMCMC["RP.cov"]=RP.cov
        outMCMC["chains"]=chains
        outMCMC["elapsed.time"]=time2[3]
        if (!(D[1]=="Mixed")&&is.null(merr)&&is.null(fact)){
            outMCMC["BDIC"]=BDIC
        }else{
            outMCMC["LIKE"]=LIKE
        }
        if ((!is.null(BUGO))&&!(D[1]=="Mixed")){
            outMCMC["chains.bugs"]=chains.bugs.mcmc
        }
        if (!is.null(fact)){
            outMCMC["fact.loadings"]=loadings
            outMCMC["fact.cov"]=fact.cov
            outMCMC["fact.chains"]=read.dta(FACTchainfile)
        }
        if (!is.null(resi.store.levs)){
            outMCMC["resi.chains"]=read.dta(resichains)
        }
        if (!is.null(dami)){
            outMCMC["esample"]=read.dta(esamplefile)
        }

        if (resi.store){
            resiraw=read.dta(resifile)
            residelpos=grep("^[c]?[[:digit:]]+$", names(resiraw))
            if(length(residelpos)==0){
                outMCMC["residual"]=resiraw
            }else{
                resisavename=names(resiraw)[-residelpos]
                outMCMC["residual"]=resiraw[resisavename]
            }
        }


    }

    if (clean.files){
        file.remove(dtafile)
        file.remove(macrofile)
        file.remove(IGLSfile)
        if (EstM==1) file.remove(MCMCfile)
        if (EstM==1) file.remove(chainfile)
        if (!is.null(BUGO))  file.remove(bugofile)
        if (!is.null(BUGO))  file.remove(modelfile)
        if (resi.store) file.remove(resifile)
        if (EstM==1){
            if (!is.null(resi.store.levs)) file.remove(resichains)
            if (!is.null(fact)) file.remove(FACTchainfile)
            if (!is.null(dami)) file.remove(esamplefile)
        }
    }

    if (EstM==0){
        outIGLS
    }else {
        if (EstM==1) outMCMC
    }
}
