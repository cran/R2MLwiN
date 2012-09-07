MacroScript2 <-
function(indata,dtafile,resp, levID, expl, rp, D,nonlinear, categ,notation,nonfp,clre,smat,Meth,merr,seed,iterations,burnin,scale,thinning,priorParam,refresh,fixM,residM,Lev1VarM, OtherVarM,adaption,priorcode,rate, tol,lclo,mcmcOptions,fact,xclass=NULL,BUGO=NULL,mem.init,
     nopause,bugofile=bugofile,modelfile=modelfile,initfile=initfile,datafile=datafile,macrofile=macrofile,IGLSfile=IGLSfile,MCMCfile=MCMCfile,
     chainfile=chainfile,esamplefile=esamplefile,resifile=resifile,resi.store=resi.store,resichains=resichains,FACTchainfile=FACTchainfile,resi.store.levs=resi.store.levs,debugmode=debugmode, startval=startval, dami=dami){


    nlev=length(levID)

    nrp=length(rp)
    if (nrp>0){
        rp.names=names(rp)
        if (D[1]=='Multinomial'||D[1]=='Multivariate Normal'||D[1]=='Mixed'){
            for (i in 1:nrp){
                temp=rp.names[i]
                rp.names[i]=paste("rp",as.integer(sub("rp","",temp))+1,sep="")
            }
        }
        names(rp)=rp.names
    }
    num.expl.init=function(p,nonfp,categ){
        num_vars=0
        if (is.na(nonfp[1])){
                if (is.null(categ)|| sum(p==categ["var",])==0){
                    num_vars=num_vars+1
                }else{
                    if (is.na(categ["ref",which(p==categ["var",])])){
                        num_vars=num_vars+ as.numeric(categ["ncateg",which(p==categ["var",])])
                    }else{
                        num_vars=num_vars+ as.numeric(categ["ncateg",which(p==categ["var",])])-1
                    }
                }
            }else{
                if (sum(p==nonfp)==0){
                    if (is.null(categ)|| sum(p==categ["var",])==0){
                        num_vars=num_vars+1
                    }else{
                        if (is.na(categ["ref",which(p==categ["var",])])){
                            num_vars=num_vars+ as.numeric(categ["ncateg",which(p==categ["var",])])
                        }else{
                            num_vars=num_vars+ as.numeric(categ["ncateg",which(p==categ["var",])])-1
                        }
                    }
                }
        }
        num_vars

    }

    if(is.list(expl)){
        sep.coeff=expl$sep.coeff
        if (is.list(nonfp)){
            nonfp.sep=nonfp$nonfp.sep
            nonfp.common=nonfp$nonfp.common
        }
        num_vars=sum(sapply(sep.coeff, function(x) num.expl.init(x,nonfp.sep,categ)))

        if (D[1]=='Multinomial'){
            nresp=length(levels(indata[,resp]))-1
            ## it is true if adding each expl variables separately
            num_vars=num_vars *nresp
        }
        if (D[1]=='Multivariate Normal'){
            nresp=length(resp)
            num_vars=num_vars *nresp
        }


        common.coeff=expl$common.coeff
        num_vars=num_vars+sum(sapply(common.coeff, function(x) num.expl.init(x,nonfp$nonfp.common,categ)))

        common.coeff.id=expl$common.coeff.id

    }else{
        num_vars=sum(sapply(expl, function(x) num.expl.init(x,nonfp,categ)))
        if (D[1]=='Multinomial'){
            nresp=length(levels(indata[,resp]))-1
            num_vars=num_vars *nresp
        }
        if (D[1]=='Multivariate Normal'){
            nresp=length(resp)
            num_vars=num_vars *nresp
        }
    }


    if(nrp>0){
        for(ii in 1:nrp){
            rpx=rp[[rp.names[ii]]]
            nrpx=length(rpx)
            if (nrpx==1) num_vars=num_vars+1
            if (nrpx>=2) num_vars=num_vars+ nrpx*(nrpx-1)/2+nrpx
        }
    }



    ## Write into macro file
    wrt =function(x) write(x,macrofile, append = T)




    cat(file=macrofile)
    wrt("ECHO    0")
    wrt("NOTE    *****************************************************************")
    wrt("NOTE      MLwiN macro created by rtomlwin command")
    wrt("NOTE    *****************************************************************")
    wrt("")

    wrt("NOTE    Initialise MLwiN storage")
    if(mem.init[1]=="default"){
        wrt(paste("INIT    ",nlev+1," 6000 2500 ",num_vars+10," 20",sep=""))
    }else{
        wrt(paste("INIT    ",mem.init[1],mem.init[2],mem.init[3],mem.init[4],mem.init[5]))
    }


    wrt("MONI    0")
    wrt("NOTE    Import the R data into MLwiN")
    wrt(paste("RSTA    '",dtafile,"'",sep=""))

    if (notation=='class'){
        wrt("INDE 1")
    }
    if (!(D[1]=="Multinomial"||D[1]=="Multivariate Normal"||D[1]=="Mixed")){
        wrt("NOTE   Specify the response variable")
        for (p in resp) wrt(paste("RESP    '",p,"'",sep=""))
        wrt("")
    }


    if (D[1] == 'Binomial') {
        wrt("NOTE   Specify the level identifier(s)")
        for (ii in 1:nlev){
            aa=nlev:1
            if(!is.na(levID[ii])) wrt(paste("IDEN ",aa[ii],"    '",levID[ii],"'",sep=""))

        }
        wrt("")

        wrt('RDISt 1 0')
        wrt(paste("DOFFs 1 '",D[3],"'",sep=""))
        wrt('SET b10 0')
        if (D[2] == 'logit') {wrt('SET b13 0');DD2=0}
        if (D[2] == 'probit') {wrt('SET b13 1');DD2=1}
        if (D[2] == 'cloglog') {wrt('SET b13 2');DD2=2}

        interpos=grep("\\:",expl)
        if (length(interpos)==0){
            for (p in expl){
                if (is.null(categ)){
                    wrt(paste("ADDT    '",p,"'",sep=""))
                }else{
                    if (sum(p==categ["var",])!=0) {
                        if(is.na(categ["ref",which(p==categ["var",])])){
                            wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                        }else{
                            wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
                        }
                    }else{
                        wrt(paste("ADDT    '",p,"'",sep=""))
                    }
                }
            }
        }else{
            exply=expl[interpos]
            explx=expl[-interpos]
            if (length(explx)>0){
                for (p in explx){
                    if (is.null(categ)){
                        wrt(paste("ADDT    '",p,"'",sep=""))
                    }else{
                        if (sum(p==categ["var",])!=0) {
                            if(is.na(categ["ref",which(p==categ["var",])])){
                                wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                            }else{
                                wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
                            }
                        }else{
                            wrt(paste("ADDT    '",p,"'",sep=""))
                        }
                    }
                }
            }
            for (i in 1:length(exply)){
                TT=""
                interx=unlist(strsplit(exply[i],"\\:"))
                for (j in 1:length(interx)){
                    TT=paste(TT,"'",interx[j],"' ",sep="")
                }
                wrt(paste("ADDT    ",TT,sep=""))
            }

        }
        wrt("")
    }


    if(D[1]=='Mixed'){
        nresp=length(resp)
        for(ii in 1:nresp) wrt(paste("MVAR 1   '", resp[ii],"'",sep=""))
        wrt("NOTE   Specify the level identifier(s)")
        for (ii in 1:nlev){
            aa=nlev:2
            if(!is.na(levID[ii])) wrt(paste("IDEN ",aa[ii],"    '",levID[ii],"'",sep=""))
        }
        wrt("IDEN 1 'resp_indicator'")
        jj=1
        for(ii in 2:length(D)){
            if(D[[ii]][1]=="Binomial"){
                wrt(paste("RDISt ",jj," 0",sep=""))
                wrt(paste("DOFFs ",jj," '",D[[ii]][3],"'",sep=""))
                if (D[[ii]][2] == 'logit') {wrt('SET b13 0');DD2=0}
                if (D[[ii]][2] == 'probit') {wrt('SET b13 1');DD2=1}
                if (D[[ii]][2] == 'cloglog') {wrt('SET b13 2');DD2=2}
            }
            if(D[[ii]][1]=="Poisson"){
                wrt(paste("RDISt ",jj," 1",sep=""))
                wrt('SET b10 1')
                wrt('SET b13 3');DD2=3
                DD2=3
                if (as.logical(D[[jj]][2])) {
                    #wrt(paste("CALC '",D[[jj]][3],"' = loge(","'", D[[jj]][3],"')",sep=""))
                    wrt(paste("DOFFs 1 '",D[[jj]][3],"'",sep=""))
                }
            }
            jj=jj+1
        }
        wrt("")
        if(is.list(expl)){
            interpos1=grep("\\:",sep.coeff)
            if (length(interpos1)==0){
                 for (x in 1:length(sep.coeff)){
                     p=sep.coeff[x]
                     if (is.null(categ)){
                         wrt(paste("ADDT    '",p,"'",sep=""))
                     }else{
                        if (sum(p==categ["var",])!=0) {
                            if(is.na(categ["ref",which(p==categ["var",])])){
                                wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                            }else{
                                wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
                            }
                        }else{
                            wrt(paste("ADDT    '",p,"'",sep=""))
                        }
                     }
                 }
            }else{
                exply=sep.coeff[interpos1]
                explx=sep.coeff[-interpos1]
                if (length(explx)>0){
                    for (p in explx){
                        if (is.null(categ)){
                            wrt(paste("ADDT    '",p,"'",sep=""))
                        }else{
                            if (sum(p==categ["var",])!=0) {
                                if(is.na(categ["ref",which(p==categ["var",])])){
                                    wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                                }else{
                                    wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
                                }
                            }else{
                                wrt(paste("ADDT    '",p,"'",sep=""))
                            }
                        }
                    }
                }
                for (i in 1:length(exply)){
                    TT=""
                    interx=unlist(strsplit(exply[i],"\\:"))
                    for (j in 1:length(interx)){
                        TT=paste(TT,"'",interx[j],"' ",sep="")
                    }
                    wrt(paste("ADDT    ",TT,sep=""))
                }

            }
            interpos2=grep("\\:",common.coeff)
            if (length(interpos2)==0){
                for (y in 1:length(common.coeff)){
                     p=common.coeff[y]
                     len.common.id=length(common.coeff.id[y,])
                     tt="RPAT    "
                     aa=1:len.common.id
                     partname=aa[rep(1,len.common.id)==common.coeff.id[y,]]
                     bb=""
                     for (ii in aa) bb=paste(bb,partname[ii],sep="")
                     for (ii in 1:len.common.id) tt=paste(tt,common.coeff.id[y,ii])
                     wrt(tt)

                     p=common.coeff[y]
                     if (is.null(categ)){
                        wrt(paste("ADDT    '",p,"'",sep=""))
                     }else{
                        if (sum(p==categ["var",])!=0) {
                            if(is.na(categ["ref",which(p==categ["var",])])){
                                wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                            }else{
                                wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
                            }
                        }else{
                            wrt(paste("ADDT    '",p,"'",sep=""))
                        }
                     }
                }
            }else{
                for (y in 1:length(common.coeff)){
                    p=common.coeff[y]
                    len.common.id=length(common.coeff.id[y,])
                    tt="RPAT    "
                    aa=1:len.common.id
                    partname=aa[rep(1,len.common.id)==common.coeff.id[y,]]
                    bb=""
                    for (ii in aa) bb=paste(bb,partname[ii],sep="")
                    for (ii in 1:len.common.id) tt=paste(tt,common.coeff.id[y,ii])
                    wrt(tt)
                    p=common.coeff[y]
                    if (!(y%in%interpos2)){
                        if (is.null(categ)){
                            wrt(paste("ADDT    '",p,"'",sep=""))
                        }else{
                            if (sum(p==categ["var",])!=0) {
                                if(is.na(categ["ref",which(p==categ["var",])])){
                                    wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                                }else{
                                    wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
                                }
                            }else{
                                wrt(paste("ADDT    '",p,"'",sep=""))
                            }
                        }
                    }else{
                        TT=""
                        interx=unlist(strsplit(p,"\\:"))
                        for (j in 1:length(interx)){
                            TT=paste(TT,"'",interx[j],"' ",sep="")
                        }
                        wrt(paste("ADDT    ",TT,sep=""))
                    }
                    wrt("RPAT")
                    common.coeff[y]=paste(p,".",bb,sep="")
             }
          }

        }else{
                interpos=grep("\\:",expl)
                if (length(interpos)==0){
                    for (p in expl){
                        if (is.null(categ)){
                            wrt(paste("ADDT    '",p,"'",sep=""))
                        }else{
                            if (sum(p==categ["var",])!=0) {
                                if(is.na(categ["ref",which(p==categ["var",])])){
                                    wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                                }else{
                                    wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
                                }
                            }else{
                                wrt(paste("ADDT    '",p,"'",sep=""))
                            }
                        }
                    }
                }else{
                    exply=expl[interpos]
                    explx=expl[-interpos]
                    if (length(explx)>0){
                        for (p in explx){
                            if (is.null(categ)){
                                wrt(paste("ADDT    '",p,"'",sep=""))
                            }else{
                                if (sum(p==categ["var",])!=0) {
                                    if(is.na(categ["ref",which(p==categ["var",])])){
                                        wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                                    }else{
                                        wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
                                    }
                                }else{
                                    wrt(paste("ADDT    '",p,"'",sep=""))
                                }
                            }
                        }
                    }
                    for (i in 1:length(exply)){
                        TT=""
                        interx=unlist(strsplit(exply[i],"\\:"))
                        for (j in 1:length(interx)){
                            TT=paste(TT,"'",interx[j],"' ",sep="")
                        }
                        wrt(paste("ADDT    ",TT,sep=""))
                    }

                }
         }
         wrt("")

    }

     if (D[1]=='Multivariate Normal'){
        nresp=length(resp)
        for (ii in 1:nresp) wrt(paste("MVAR 1   '", resp[ii],"'",sep=""))
        wrt("NOTE   Specify the level identifier(s)")
        for (ii in 1:nlev){
            aa=nlev:2
            if(!is.na(levID[ii])) wrt(paste("IDEN ",aa[ii],"    '",levID[ii],"'",sep=""))
        }
        wrt("IDEN 1 'resp_indicator'")
        wrt('SET b13 0')
        wrt("")
        if(is.list(expl)){
            interpos1=grep("\\:",sep.coeff)
            if (length(interpos1)==0){
                 for (x in 1:length(sep.coeff)){
                     p=sep.coeff[x]
                     if (is.null(categ)){
                         wrt(paste("ADDT    '",p,"'",sep=""))
                     }else{
                        if (sum(p==categ["var",])!=0) {
                            if(is.na(categ["ref",which(p==categ["var",])])){
                                wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                            }else{
                                wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
                            }
                        }else{
                            wrt(paste("ADDT    '",p,"'",sep=""))
                        }
                     }
                 }
            }else{
                exply=sep.coeff[interpos1]
                explx=sep.coeff[-interpos1]
                if (length(explx)>0){
                    for (p in explx){
                        if (is.null(categ)){
                            wrt(paste("ADDT    '",p,"'",sep=""))
                        }else{
                            if (sum(p==categ["var",])!=0) {
                                if(is.na(categ["ref",which(p==categ["var",])])){
                                    wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                                }else{
                                    wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
                                }
                            }else{
                                wrt(paste("ADDT    '",p,"'",sep=""))
                            }
                        }
                    }
                }
                for (i in 1:length(exply)){
                    TT=""
                    interx=unlist(strsplit(exply[i],"\\:"))
                    for (j in 1:length(interx)){
                        TT=paste(TT,"'",interx[j],"' ",sep="")
                    }
                    wrt(paste("ADDT    ",TT,sep=""))
                }

            }
            interpos2=grep("\\:",common.coeff)
            if (length(interpos2)==0){
                for (y in 1:length(common.coeff)){
                     p=common.coeff[y]
                     len.common.id=length(common.coeff.id[y,])
                     tt="RPAT    "
                     aa=1:len.common.id
                     partname=aa[rep(1,len.common.id)==common.coeff.id[y,]]
                     bb=""
                     for (ii in aa) bb=paste(bb,partname[ii],sep="")
                     for (ii in 1:len.common.id) tt=paste(tt,common.coeff.id[y,ii])
                     wrt(tt)

                     p=common.coeff[y]
                     if (is.null(categ)){
                        wrt(paste("ADDT    '",p,"'",sep=""))
                     }else{
                        if (sum(p==categ["var",])!=0) {
                            if(is.na(categ["ref",which(p==categ["var",])])){
                                wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                            }else{
                                wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
                            }
                        }else{
                            wrt(paste("ADDT    '",p,"'",sep=""))
                        }
                     }
                }
            }else{
                for (y in 1:length(common.coeff)){
                    p=common.coeff[y]
                    len.common.id=length(common.coeff.id[y,])
                    tt="RPAT    "
                    aa=1:len.common.id
                    partname=aa[rep(1,len.common.id)==common.coeff.id[y,]]
                    bb=""
                    for (ii in aa) bb=paste(bb,partname[ii],sep="")
                    for (ii in 1:len.common.id) tt=paste(tt,common.coeff.id[y,ii])
                    wrt(tt)
                    p=common.coeff[y]
                    if (!(y%in%interpos2)){
                        if (is.null(categ)){
                            wrt(paste("ADDT    '",p,"'",sep=""))
                        }else{
                            if (sum(p==categ["var",])!=0) {
                                if(is.na(categ["ref",which(p==categ["var",])])){
                                    wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                                }else{
                                    wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
                                }
                            }else{
                                wrt(paste("ADDT    '",p,"'",sep=""))
                            }
                        }
                    }else{
                        TT=""
                        interx=unlist(strsplit(p,"\\:"))
                        for (j in 1:length(interx)){
                            TT=paste(TT,"'",interx[j],"' ",sep="")
                        }
                        wrt(paste("ADDT    ",TT,sep=""))
                    }
                    wrt("RPAT")
                    common.coeff[y]=paste(p,".",bb,sep="")
             }
          }

        }else{
                interpos=grep("\\:",expl)
                if (length(interpos)==0){
                    for (p in expl){
                        if (is.null(categ)){
                            wrt(paste("ADDT    '",p,"'",sep=""))
                        }else{
                            if (sum(p==categ["var",])!=0) {
                                if(is.na(categ["ref",which(p==categ["var",])])){
                                    wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                                }else{
                                    wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
                                }
                            }else{
                                wrt(paste("ADDT    '",p,"'",sep=""))
                            }
                        }
                    }
                }else{
                    exply=expl[interpos]
                    explx=expl[-interpos]
                    if (length(explx)>0){
                        for (p in explx){
                            if (is.null(categ)){
                                wrt(paste("ADDT    '",p,"'",sep=""))
                            }else{
                                if (sum(p==categ["var",])!=0) {
                                    if(is.na(categ["ref",which(p==categ["var",])])){
                                        wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                                    }else{
                                        wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
                                    }
                                }else{
                                    wrt(paste("ADDT    '",p,"'",sep=""))
                                }
                            }
                        }
                    }
                    for (i in 1:length(exply)){
                        TT=""
                        interx=unlist(strsplit(exply[i],"\\:"))
                        for (j in 1:length(interx)){
                            TT=paste(TT,"'",interx[j],"' ",sep="")
                        }
                        wrt(paste("ADDT    ",TT,sep=""))
                    }

                }
         }
         wrt("")
    }



    if (D[1]== 'Multinomial'){

        wrt("NAME c180 'resp' c181 'resp_indicator'")
        wrt(paste('MNOM ',as.numeric(D[4]), " '",resp,"' ",  "c180 c181 ",as.numeric(D[5]),sep=""))
        wrt("RESP   'resp'")
        wrt("NOTE   Specify the level identifier(s)")
        for (ii in 1:c(nlev-1)){
            aa=nlev:1
            if(!is.na(levID[ii])) wrt(paste("IDEN ",aa[ii],"    '",levID[ii],"'",sep=""))
        }
        wrt("IDEN 1 'resp_indicator'")
        wrt("")

        if (as.numeric(D[4])==0) wrt('RDISt 1 4') else wrt('RDISt 1 5')
        wrt(paste("DOFFs 1 '",D[3],"'",sep=""))
        if (D[2] == 'logit') {wrt('SET b13 0');DD2=0}
        if (D[2] == 'probit') {wrt('SET b13 1');DD2=1}
        if (D[2] == 'cloglog') {wrt('SET b13 2');DD2=2}

        wrt("")
        if(is.list(expl)){
            interpos1=grep("\\:",sep.coeff)
            if (length(interpos1)==0){
                 for (x in 1:length(sep.coeff)){
                     p=sep.coeff[x]
                     if (is.null(categ)){
                         wrt(paste("ADDT    '",p,"'",sep=""))
                     }else{
                            if (sum(p==categ["var",])!=0) {
                                if(is.na(categ["ref",which(p==categ["var",])])){
                                    wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                                }else{
                                    wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
                                }
                            }else{
                                wrt(paste("ADDT    '",p,"'",sep=""))
                            }
                     }
                 }
            }else{
                exply=sep.coeff[interpos1]
                explx=sep.coeff[-interpos1]
                if (length(explx)>0){
                    for (p in explx){
                        if (is.null(categ)){
                            wrt(paste("ADDT    '",p,"'",sep=""))
                        }else{
                            if (sum(p==categ["var",])!=0) {
                                if(is.na(categ["ref",which(p==categ["var",])])){
                                    wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                                }else{
                                    wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
                                }
                            }else{
                                wrt(paste("ADDT    '",p,"'",sep=""))
                            }
                        }
                    }
                }
                for (i in 1:length(exply)){
                    TT=""
                    interx=unlist(strsplit(exply[i],"\\:"))
                    for (j in 1:length(interx)){
                        TT=paste(TT,"'",interx[j],"' ",sep="")
                    }
                    wrt(paste("ADDT    ",TT,sep=""))
                }

            }
            interpos2=grep("\\:",common.coeff)
            if (length(interpos2)==0){
                for (y in 1:length(common.coeff)){
                     p=common.coeff[y]
                     len.common.id=length(common.coeff.id[y,])
                     tt="RPAT    "
                     aa=1:len.common.id
                     partname=aa[rep(1,len.common.id)==common.coeff.id[y,]]
                     bb=""
                     for (ii in aa) bb=paste(bb,partname[ii],sep="")
                     for (ii in 1:len.common.id) tt=paste(tt,common.coeff.id[y,ii])
                     wrt(tt)

                     p=common.coeff[y]
                     if (is.null(categ)){
                        wrt(paste("ADDT    '",p,"'",sep=""))
                     }else{
                         if (sum(p==categ["var",])!=0) {
                             if(is.na(categ["ref",which(p==categ["var",])])){
                                 wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                             }else{
                                 wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
                             }
                         }else{
                             wrt(paste("ADDT    '",p,"'",sep=""))
                         }
                     }
                }
            }else{
                for (y in 1:length(common.coeff)){
                    p=common.coeff[y]
                    len.common.id=length(common.coeff.id[y,])
                    tt="RPAT    "
                    aa=1:len.common.id
                    partname=aa[rep(1,len.common.id)==common.coeff.id[y,]]
                    bb=""
                    for (ii in aa) bb=paste(bb,partname[ii],sep="")
                    for (ii in 1:len.common.id) tt=paste(tt,common.coeff.id[y,ii])
                    wrt(tt)
                    p=common.coeff[y]
                    if (!(y%in%interpos2)){
                        if (is.null(categ)){
                            wrt(paste("ADDT    '",p,"'",sep=""))
                        }else{
                            if (sum(p==categ["var",])!=0) {
                                if(is.na(categ["ref",which(p==categ["var",])])){
                                    wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                                }else{
                                    wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
                                }
                            }else{
                                wrt(paste("ADDT    '",p,"'",sep=""))
                            }
                        }
                    }else{
                        TT=""
                        interx=unlist(strsplit(p,"\\:"))
                        for (j in 1:length(interx)){
                            TT=paste(TT,"'",interx[j],"' ",sep="")
                        }
                        wrt(paste("ADDT    ",TT,sep=""))
                    }
                    wrt("RPAT")
                    common.coeff[y]=paste(p,".",bb,sep="")
             }
          }

        }else{
                interpos=grep("\\:",expl)
                if (length(interpos)==0){
                    for (p in expl){
                        if (is.null(categ)){
                            wrt(paste("ADDT    '",p,"'",sep=""))
                        }else{
                            if (sum(p==categ["var",])!=0) {
                                if(is.na(categ["ref",which(p==categ["var",])])){
                                    wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                                }else{
                                    wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
                                }
                            }else{
                                wrt(paste("ADDT    '",p,"'",sep=""))
                            }
                        }
                    }
                }else{
                    exply=expl[interpos]
                    explx=expl[-interpos]
                    if (length(explx)>0){
                        for (p in explx){
                            if (is.null(categ)){
                                wrt(paste("ADDT    '",p,"'",sep=""))
                            }else{
                                if (sum(p==categ["var",])!=0) {
                                    if(is.na(categ["ref",which(p==categ["var",])])){
                                        wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                                    }else{
                                        wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
                                    }
                                }else{
                                    wrt(paste("ADDT    '",p,"'",sep=""))
                                }
                            }
                        }
                    }
                    for (i in 1:length(exply)){
                        TT=""
                        interx=unlist(strsplit(exply[i],"\\:"))
                        for (j in 1:length(interx)){
                            TT=paste(TT,"'",interx[j],"' ",sep="")
                        }
                        wrt(paste("ADDT    ",TT,sep=""))
                    }

                }
         }
         wrt("")
    }

    if (D[1] == 'Normal') {
        wrt("NOTE   Specify the level identifier(s)")
        for (ii in 1:nlev){
            aa=nlev:1
            if(!is.na(levID[ii])) wrt(paste("IDEN ",aa[ii],"    '",levID[ii],"'",sep=""))
        }
        wrt("")

        wrt("NOTE   Specify covariate(s) used anywhere in the model")
        interpos=grep("\\:",expl)
        if (length(interpos)==0){
            for (p in expl){
                if (is.null(categ)){
                    wrt(paste("ADDT    '",p,"'",sep=""))
                }else{
                    if (sum(p==categ["var",])!=0) {
                        if(is.na(categ["ref",which(p==categ["var",])])){
                            wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                        }else{
                            wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
                        }
                    }else{
                        wrt(paste("ADDT    '",p,"'",sep=""))
                    }
                }
            }
        }else{
            exply=expl[interpos]
            explx=expl[-interpos]
            if (length(explx)>0){
                for (p in explx){
                    if (is.null(categ)){
                        wrt(paste("ADDT    '",p,"'",sep=""))
                    }else{
                        if (sum(p==categ["var",])!=0) {
                            if(is.na(categ["ref",which(p==categ["var",])])){
                                wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                            }else{
                                wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
                            }
                        }else{
                            wrt(paste("ADDT    '",p,"'",sep=""))
                        }
                    }
                }
            }
            for (i in 1:length(exply)){
                TT=""
                interx=unlist(strsplit(exply[i],"\\:"))
                for (j in 1:length(interx)){
                    TT=paste(TT,"'",interx[j],"' ",sep="")
                }
                wrt(paste("ADDT    ",TT,sep=""))
            }

        }
        wrt("")
    }
    if (D[1] == 'Poisson') {
        wrt("NOTE   Specify the level identifier(s)")
        for (ii in 1:nlev){
            aa=nlev:1
            if(!is.na(levID[ii])) wrt(paste("IDEN ",aa[ii],"    '",levID[ii],"'",sep=""))
        }
        wrt("")

        wrt('RDISt 1 1')
        wrt('SET b10 1')
        wrt('SET b13 3')
        DD2=3
        if (as.logical(D[2])) {
            #wrt(paste("CALC '",D[3],"' = loge(","'", D[3],"')",sep=""))
            wrt(paste("DOFFs 1 '",D[3],"'",sep=""))
        }
        interpos=grep("\\:",expl)
        if (length(interpos)==0){
            for (p in expl){
                if (is.null(categ)){
                    wrt(paste("ADDT    '",p,"'",sep=""))
                }else{
                    if (sum(p==categ["var",])!=0) {
                        if(is.na(categ["ref",which(p==categ["var",])])){
                            wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                        }else{
                            wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
                        }
                    }else{
                        wrt(paste("ADDT    '",p,"'",sep=""))
                    }
                }
            }
        }else{
            exply=expl[interpos]
            explx=expl[-interpos]
            if (length(explx)>0){
                for (p in explx){
                    if (is.null(categ)){
                        wrt(paste("ADDT    '",p,"'",sep=""))
                    }else{
                        if (sum(p==categ["var",])!=0) {
                            if(is.na(categ["ref",which(p==categ["var",])])){
                                wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                            }else{
                                wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
                            }
                        }else{
                            wrt(paste("ADDT    '",p,"'",sep=""))
                        }
                    }
                }
            }
            for (i in 1:length(exply)){
                TT=""
                interx=unlist(strsplit(exply[i],"\\:"))
                for (j in 1:length(interx)){
                    TT=paste(TT,"'",interx[j],"' ",sep="")
                }
                wrt(paste("ADDT    ",TT,sep=""))
            }

        }
        wrt("")
    }


    if (D[1] == 'Negbinom') {
        wrt("NOTE   Specify the level identifier(s)")
        for (ii in 1:nlev){
            aa=nlev:1
            if(!is.na(levID[ii])) wrt(paste("IDEN ",aa[ii],"    '",levID[ii],"'",sep=""))
        }
        wrt("")

        wrt('RDISt 1 2')
        wrt('SET b10 1')
        wrt('SET b13 3')
        DD2=3
        if (as.logical(D[2])) {
            #wrt(paste("CALC '",D[3],"' = loge(","'", D[3],"')",sep=""))
            wrt(paste("DOFFs 1 '",D[3],"'",sep=""))
        }
        interpos=grep("\\:",expl)
        if (length(interpos)==0){
            for (p in expl){
                if (is.null(categ)){
                    wrt(paste("ADDT    '",p,"'",sep=""))
                }else{
                     if (sum(p==categ["var",])!=0) {
                         if(is.na(categ["ref",which(p==categ["var",])])){
                             wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                         }else{
                             wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
                         }
                     }else{
                         wrt(paste("ADDT    '",p,"'",sep=""))
                     }
                }
            }
        }else{
            exply=expl[interpos]
            explx=expl[-interpos]
            if (length(explx)>0){
                for (p in explx){
                    if (is.null(categ)){
                        wrt(paste("ADDT    '",p,"'",sep=""))
                    }else{
                        if (sum(p==categ["var",])!=0) {
                            if(is.na(categ["ref",which(p==categ["var",])])){
                                wrt(paste("ADDT    '",p,"' ", -10000000,sep=""))
                            }else{
                                wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
                            }
                        }else{
                            wrt(paste("ADDT    '",p,"'",sep=""))
                        }
                    }
                }
            }
            for (i in 1:length(exply)){
                TT=""
                interx=unlist(strsplit(exply[i],"\\:"))
                for (j in 1:length(interx)){
                    TT=paste(TT,"'",interx[j],"' ",sep="")
                }
                wrt(paste("ADDT    ",TT,sep=""))
            }

        }
        wrt("")
    }




    if (is.list(nonfp)){
        wrt("NOTE Turn off the fixed part of the explotary varible(s)")
        nonfp.sep=nonfp$nonfp.sep
        nonfp.common=nonfp$nonfp.common
        if (!is.na(nonfp.sep[1])){
            interpos=grep("\\:",nonfp.sep)
            if (length(interpos)==0){
                for (p in nonfp.sep) wrt(paste("FPAR 0  '",p,"'",sep=""))
            }else{
                for (i in 1:length(nonfp.sep)){
                    if (i %in% interpos){
                        wrt(paste("FPAR 0  '",gsub("\\:","\\.",nonfp.sep[i]),"'",sep=""))
                    }else{
                        wrt(paste("FPAR 0  '",nonfp.sep[i],"'",sep=""))
                    }
                }
            }
        }
        if (!is.na(nonfp.common[1])){
            interpos=grep("\\:",nonfp.common)
            if (length(interpos)==0){
                for (p in nonfp.common) wrt(paste("FPAR 0  '",p,"'",sep=""))
            }else{
                for (i in 1:length(nonfp.common)){
                    if (i %in% interpos){
                        wrt(paste("FPAR 0  '",gsub("\\:","\\.",nonfp.common[i]),"'",sep=""))
                    }else{
                        wrt(paste("FPAR 0  '",nonfp.common[i],"'",sep=""))
                    }
                }
            }
        }
    }else{
        if (!is.na(nonfp[1])){
            wrt("NOTE Turn off the fixed part of the explotary varible(s)")
            for (p in nonfp) wrt(paste("FPAR 0  '",gsub("\\:","\\.",p),"'",sep=""))
        }
    }

    wrt("")
    wrt("NOTE   Specify random part covariate(s)")
    if (nrp>0){
        for (ii in 1:nrp){
            for (p in rp[[ii]]) wrt(paste("SETV  ",as.numeric(sub("rp","",rp.names[ii])),"   '",gsub("\\:","\\.",p),"'",sep=""))
        }
    }
    if (!is.null(clre)){
        nclre=ncol(clre)
        for (ii in 1:nclre){
            wrt(paste("CLRE  ",as.numeric(clre[1,ii])," '",gsub("\\:","\\.",clre[2,ii]),"' '",gsub("\\:","\\.",clre[3,ii]),"'",sep=""))
        }
    }

    if (!is.null(smat)){
        wrt(paste("SMAT ", smat[1], smat[2]))
    }
    if (!is.null(fact)){
        if (D[1]=="Multinomial"||D[1]=="Multivariate Normal"||D[1]=="Mixed"){
            fact$lev.fact=fact$lev.fact+1
        }
        for (i in 1:fact$nfact){
            wrt(paste("SMAT ",fact$lev.fact[i],0))
        }
    }

    nexpl=length(expl)
    wrt("")

    wrt("NOTE   Set estimation method")
    if (Meth!=2){
        wrt(paste("METH", Meth))
    }
    wrt(paste("LINE ",nonlinear[1],nonlinear[2]))
    wrt("")

    if (!is.null(fact)){
        TT=NULL
        for (i in 1:fact$nfact){
            TT=c(TT, fact$lev.fact[i], matrix(rbind(fact$loading[i,],fact$constr[i,]),nrow=1))
        }
        if (fact$nfactcor>0) TT=c(TT,fact$factcor)
        FACT=as.vector(c(length(resp),fact$nfact, fact$nfactcor,TT))
        rm(TT)
        TT=""
        for (i in 1:length(FACT)){
            TT=paste(TT,FACT[i])
        }
        wrt(paste("FACT ",TT))
        wrt("SMFA 1 c300")
        wrt("SMFA 2 c301")

    }

    if (D[1]=="Normal"){
        wrt("PREF   0")
        wrt("POST   0")
    }

    wrt("NOTE   Fit the model")
    wrt("STAR")
    wrt("BATC 1")
    wrt("NEXT")
    wrt("MONI 1")
    wrt("ITNU 0 b21")
    wrt("CONV b22")
    wrt("")

    if (debugmode){
    wrt("NOTE   Open the equations window")
    wrt("WSET 15 1")
    wrt("EXPA 3")
    wrt("ESTM 2")
    }
    wrt("NOTE    *****************************************************************")
    wrt("")

    wrt("NOTE    *****************************************************************")
    wrt("NOTE       Export the model results to R")
    wrt("NOTE    *****************************************************************")
    wrt("NAME   c1300 '_Stats'")
    wrt("LIKE   b100")
    wrt("EDIT 3 c1300 b100")
    wrt("EDIT 7 c1300 b21")
    wrt("EDIT 8 c1300 b22")
    wrt("NAME   c1098 '_FP_b'")
    wrt("NAME   c1099 '_FP_v'")
    wrt("NAME   c1096 '_RP_b'")
    wrt("NAME   c1097 '_RP_v'")
    wrt("NAME   c1094 '_esample'")
    wrt("SUM '_esample' b1")
    wrt("EDIT 9 c1300 b1")
    wrt(paste("PSTA '",IGLSfile, "' ","'_FP_b' ","'_FP_v' ", "'_RP_b' ", "'_RP_v' ", "'_Stats'",sep=""))


    wrt("NOTE    *****************************************************************")
    wrt("NOTE Set estimation method to MCMC")
    wrt("NOTE    *****************************************************************")
    wrt("EMODe  3")

    if (!is.null(startval)){
        if (!is.null(startval$FP.b)){
            wrt(paste("JOIN ",paste(startval$FP.b, collapse=" "), " '_FP_b'",sep=""))
        }
        if (!is.null(startval$FP.v)){
            wrt(paste("JOIN ",paste(startval$FP.v, collapse=" "), " '_FP_v'",sep=""))
        }
        if (!is.null(startval$RP.b)){
            wrt(paste("JOIN ",paste(startval$RP.b, collapse=" "), " '_RP_b'",sep=""))
        }
        if (!is.null(startval$RP.v)){
            wrt(paste("JOIN ",paste(startval$RP.v, collapse=" "), " '_RP_v'",sep=""))
        }
    }

    if (!is.null(resi.store.levs)){
        tcell=350
        for (i in 1:length(resi.store.levs)){
            wrt(paste("SMRE ", resi.store.levs[i], " c",tcell+i,sep=""))
            wrt(paste("NAME ", " c",tcell+i, " 'resi_lev",resi.store.levs[i],"'",sep=""))
        }

    }
    if (!is.null(merr)){
        nmerr=as.numeric(merr[1])
        tt=paste("MERR  ",nmerr)
        j=1
        for (ii in 1:nmerr){
            tt=paste(tt," '",merr[j+1],"' ",as.numeric(merr[j+2]),sep="")
            j=j+2
        }
        wrt(tt)
    }
    wrt(paste("ORTH",mcmcOptions$orth))
    if (mcmcOptions$hcen==0){
        wrt(paste("HCEN",mcmcOptions$hcen))
    }else{
        wrt(paste("HCEN",1, mcmcOptions$hcen))
    }
    wrt(paste("SMCM",mcmcOptions$smcm))
    wrt(paste("SMVN",mcmcOptions$smvn))
    if(is.matrix(mcmcOptions$paex)){
        apply(mcmcOptions$paex,1,function(x) wrt(paste("PAEX",x[1],x[2])))
    }else{
         wrt(paste("PAEX",mcmcOptions$paex[1],mcmcOptions$paex[2]))
    }

    if (D[1]=="Multivariate Normal") wrt(paste("MCCO ",mcmcOptions$mcco))
    if (!is.null(xclass)){
        for(ii in 1:length(as.numeric(xclass[[1]]))){
            if (as.numeric(xclass[[2]][ii])==1){
                wrt(paste("MULM",as.numeric(xclass[[1]][ii])," 1"))
                wrt(paste("CARP",as.numeric(xclass[[1]][ii])," 0"))
            }
            if (xclass[[2]][ii]>1){
                if (length(xclass)==4){
                    carflag=F
                }else{
                    carflag=xclass[[5]][ii]
                }
                if (is.na(xclass[[4]][ii])){
                    wrt(paste("MULM ",as.numeric(xclass[[1]][ii])," ", as.numeric(xclass[[2]][ii])," '",xclass[[3]][ii],"'",sep=""))
        			if (carflag){
                        wrt(paste("CARP",as.numeric(xclass[[1]][ii])," 1"))
        			}else{
        				wrt(paste("CARP",as.numeric(xclass[[1]][ii])," 0"))
        			}
                }else{
                    wrt(paste("MULM ",as.numeric(xclass[[1]][ii])," ", as.numeric(xclass[[2]][ii])," '",xclass[[3]][ii],"'"," '",xclass[[4]][ii],"'",sep=""))
        			if (carflag){
                        wrt(paste("CARP",as.numeric(xclass[[1]][ii])," 1"))
        			}else{
        				wrt(paste("CARP",as.numeric(xclass[[1]][ii])," 0"))
        			}
                }
            }
        }
        wrt("XCLA 1")
    }

    wrt("")

    wrt("NOTE Set MCMC seed")
    wrt(paste("MCRS ",seed,sep=""))
    wrt("")

    wrt("NOTE Set prior distribution parameters")
    if (priorParam[1]!="default"){
        wrt("PRIOR  c1092")
        lenpp=length(priorParam)
        tempt=" "
        for (i in 1:lenpp)  tempt=paste(tempt,priorParam[i])
        wrt(paste("JOIN",tempt," c1092",sep=""))
        wrt("")
    }else{
        #wrt("GAMP   0.001 0.001")
        wrt("")
    }

    if (D[1]=="Normal"){
        wrt("PREF   0")
        wrt("POST   0")
    }

    wrt("MISR   0")
    if (D[1]=="Multinomial"&& as.numeric(D[4])==0){
        tempcell= 998
        len.rpx=2
        wrt(paste("NOTE Calculate MCMC starting values for level ",2," residuals",sep=""))
        wrt(paste("RLEV   ",2,sep=""))
        wrt("RFUN")
        wrt("RCOV   2")
        tempcol=(tempcell+len.rpx):tempcell
        tempvec=tempvec2=NULL
        for (i in 1:(len.rpx+1)) tempvec=paste(tempvec, paste("c", tempcol[i],sep=""))
        for (i in 1:len.rpx) tempvec2=paste(tempvec2, paste("c", tempcol[i],sep=""))
        wrt(paste("ROUT   ",tempvec,sep=""))
        wrt("RESI")
        wrt(paste("JOIN   c",997, tempvec2," c",997,sep=""))
        wrt(paste("JOIN   c",996," c", tempcol[len.rpx+1]," c",996,sep=""))
        wrt(paste("ERAS   ",tempvec,sep=""))
    }
    if (nrp>0){
        tempcell= 998
        for (j in nrp:1){
            if (as.numeric(sub("rp","",rp.names[j]))!=1){
                if (as.numeric(sub("rp","",rp.names[j]))==2){
                    rpx=rp[[j]]
                    len.rpx=length(rpx)
                    if (D[1]=="Mixed"){
                        rpx=c(rpx,"bcons")
                        len.rpx=length(rpx)
                    }
                    wrt(paste("NOTE Calculate MCMC starting values for level ",as.numeric(sub("rp","",rp.names[j]))," residuals",sep=""))
                    wrt(paste("RLEV   ",as.numeric(sub("rp","",rp.names[j])),sep=""))
                    wrt("RFUN")
                    wrt("RCOV   2")
                    tempcol=(tempcell+len.rpx):tempcell
                    tempvec=tempvec2=NULL
                    for (i in 1:(len.rpx+1)) tempvec=paste(tempvec, paste("c", tempcol[i],sep=""))
                    for (i in 1:len.rpx) tempvec2=paste(tempvec2, paste("c", tempcol[i],sep=""))
                    wrt(paste("ROUT   ",tempvec,sep=""))
                    wrt("RESI")
                    wrt(paste("JOIN   c",997, tempvec2," c",997,sep=""))
                    wrt(paste("JOIN   c",996," c", tempcol[len.rpx+1]," c",996,sep=""))
                    wrt(paste("ERAS   ",tempvec,sep=""))
                }else{

                        rpx=rp[[j]]
                        len.rpx=length(rp[[j]])
                        wrt(paste("NOTE Calculate MCMC starting values for level ",as.numeric(sub("rp","",rp.names[j]))," residuals",sep=""))
                        wrt(paste("RLEV   ",as.numeric(sub("rp","",rp.names[j])),sep=""))
                        wrt("RFUN")
                        wrt("RCOV   2")
                        tempcol=(tempcell+len.rpx):tempcell
                        tempvec=tempvec2=NULL
                        for (i in 1:(len.rpx+1)) tempvec=paste(tempvec, paste("c", tempcol[i],sep=""))
                        for (i in 1:len.rpx) tempvec2=paste(tempvec2, paste("c", tempcol[i],sep=""))
                        wrt(paste("ROUT   ",tempvec,sep=""))
                        wrt("RESI")
                        wrt(paste("JOIN   c",997, tempvec2," c",997,sep=""))
                        wrt(paste("JOIN   c",996," c", tempcol[len.rpx+1]," c",996,sep=""))
                        wrt(paste("ERAS   ",tempvec,sep=""))
                }
            }
        }
    }
    wrt("MISR   1")

    if(D[1]=="Normal") DD=1
    if(D[1]=="Binomial") DD=2
    if(D[1]=="Mixed") DD=5
    if(D[1]=="Poisson") DD=3
    if(D[1]=="Multivariate Normal") DD=4
    if(D[1]=="Multinomial") {if (as.numeric(D[4])==0) DD=6 else DD=7; wrt("CLRV 2")}


    wrt("NOTE   fit the model in MCMC")
    wrt(paste("MTOT   ",iterations,sep=""))
    wrt(paste("LCLO   ",lclo,sep=""))
    if (!is.null(xclass)&&length(xclass)>=4){
        wrt(paste("MCMC   0 ", burnin," ",adaption," ",scale," ",rate," ", tol," ",fixM," ",residM," ", Lev1VarM, " ", OtherVarM," ",priorcode," ",DD,sep=""))
    }else{
        if (priorParam[1]!="default"){
            wrt(paste("MCMC   0 ", burnin," ",adaption," ",scale," ",rate," ", tol," c997 c996 c1092 ",fixM," ",residM," ", Lev1VarM, " ", OtherVarM," ",priorcode," ",DD,sep=""))
        }else{
            wrt(paste("MCMC   0 ", burnin," ",adaption," ",scale," ",rate," ", tol," c997 c996 ",fixM," ",residM," ", Lev1VarM, " ", OtherVarM," ",priorcode," ",DD,sep=""))
        }
    }
    wrt("ERAS  c1090 c1091 c997 c996")
    wrt("")
    if (!is.null(dami)&&dami[1]==0&&length(dami)>1){
        ndami=length(dami)
        tempmvcell=300
        mvnames=rep(NA,ndami-1)
        for (i in 2:ndami){
            wrt(paste("MCMC 1 ",dami[i]-dami[i-1]," ",thinning," c1090 c1091 c1003 c1004 1 ",DD,sep="") )
            wrt("PUPN c1003 c1004")
            wrt("AVER c1091 b99 b100")
            wrt(paste("DAMI 0 c",tempmvcell,sep=""))
            mvnames[i-1]=paste("'_est_",dami[i],"'",sep="")
            wrt(paste("NAME c",tempmvcell," '_est_",dami[i],"'",sep=""))
            wrt("PAUS 1")
            tempmvcell=tempmvcell+1
        }
        if (dami[ndami]<iterations){
            wrt(paste("MCMC 1 ",iterations-dami[ndami]," ",thinning," c1090 c1091 c1003 c1004 1 ",DD,sep="") )
            wrt("PUPN c1003 c1004")
            wrt("AVER c1091 b99 b100")
            wrt("PAUS 1")
        }
        wrt("CALC   '_esample'=abso('_esample'-1)")
        wrt(paste("PSTA '",esamplefile, "' ",paste(mvnames,collapse=" ")," '_esample'",sep=""))
        wrt(paste("ERAS  ",paste(mvnames,collapse=" "),sep=""))
    }else{
        if (debugmode&&(!nopause)){
            for (i in 1:floor(iterations/refresh)){
                wrt(paste("MCMC 1 ",refresh," ",thinning," c1090 c1091 c1003 c1004 1 ",DD,sep="") )
                wrt("PUPN c1003 c1004")
                wrt("AVER c1091 b99 b100")
                wrt("PAUS 1")
            }
            is.wholenumber <-function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
            if(!is.wholenumber(iterations/refresh)){
                wrt(paste("MCMC 1 ",iterations%%refresh," ", thinning," c1090 c1091 c1003 c1004 1 ",DD,sep="") )
                wrt("PUPN c1003 c1004")
                wrt("AVER c1091 b99 b100")
                wrt("PAUS 1")
            }
        }else{
            wrt(paste("MCMC 1 ",iterations," ",thinning," c1090 c1091 c1003 c1004 1 ",DD,sep="") )
            wrt("PUPN c1003 c1004")
            wrt("AVER c1091 b99 b100")
        }
    }


    wrt("NOTE    *****************************************************************")
    wrt("NOTE       Export the model results to R")
    wrt("NOTE    *****************************************************************")
    if (!(D[1]=="Mixed")&&is.null(merr)&&is.null(fact)){
        wrt("BDIC b1 b2 b3 b4")
        wrt("EDIT 3 c1300 b1")
        wrt("EDIT 4 c1300 b2")
        wrt("EDIT 5 c1300 b3")
        wrt("EDIT 6 c1300 b4")
    }
    wrt("EDIT 7 c1300 b21")
    wrt("EDIT 8 c1300 b22")
    wrt("NAME   c1098 '_FP_b'")
    wrt("NAME   c1099 '_FP_v'")
    wrt("NAME   c1096 '_RP_b'")
    wrt("NAME   c1097 '_RP_v'")
    wrt("NAME   c1094 '_esample'")
    wrt("SUM '_esample' b1")
    wrt("EDIT 9 c1300 b1")
    if(is.null(fact)){
        wrt(paste("PSTA '",MCMCfile, "' ","'_FP_b' ","'_FP_v' ", "'_RP_b' ", "'_RP_v' ","'_Stats'",sep=""))
    }else{
        wrt("DAFA c302 c303")
        wrt("DAFL c304 c305")
        wrt("DAFV c306 c307")
        wrt("NAME c300 '_FACT_load_b_chain'")
        wrt("NAME c301 '_FACT_load_v_chain'")
        wrt("NAME c302 '_FACT_value_b'")
        wrt("NAME c303 '_FACT_value_v'")
        wrt("NAME   c304  '_FACT_load_b'")
        wrt("NAME   c305  '_FACT_load_v'")
        wrt("NAME   c306  '_FACT_var_b'")
        wrt("NAME   c307  '_FACT_var_v'")
        wrt(paste("PSTA '",FACTchainfile,"' ","'_FACT_load_b_chain' ","'_FACT_load_v_chain' ","'_FACT_value_b' ","'_FACT_value_v' ",sep=""))
        wrt(paste("PSTA '",MCMCfile, "' ","'_FP_b' ","'_FP_v' ", "'_RP_b' ", "'_RP_v' ","'_FACT_load_b' ","'_FACT_load_v' ","'_FACT_var_b' ", "'_FACT_var_v' ","'_Stats'",sep=""))
        wrt("ERAS c300 c301")
    }
    wrt("ERAS '_Stats'")
    wrt("")

    if (!is.null(dami)&&length(dami)==1){
        wrt("NOTE generate example if there a missing values")
        wrt("SWIT b1")
        wrt("CASE 0:")
        wrt("LEAVE")
        wrt("CASE:")
        wrt("CALC   '_esample'=abso('_esample'-1)")
        if (dami==0){
            wrt("DAMI 0 c300")
            wrt("NAME c300 '_est'")
            wrt(paste("PSTA '",esamplefile, "' '_est' '_esample' ",sep=""))
            wrt("ERAS  c300")
        }
        if (dami==1){
            wrt("DAMI 1 c300")
            wrt("NAME c300 '_est'")
            wrt(paste("PSTA '",esamplefile, "' '_est' '_esample' ",sep=""))
            wrt("ERAS  c300")
        }
        if (dami==2){
            wrt("DAMI 2 c300 c301")
            wrt("NAME c300 '_est'")
            wrt("NAME c301 '_SDs'")
            wrt(paste("PSTA '",esamplefile, "' '_est' '_SDs' '_esample' ",sep=""))
            wrt("ERAS  c300 c301")
        }
        wrt("ENDS")
        wrt("")
    }

    wrt("NOTE export parameter chain")
    wrt("NAME   c1091 'deviance'")
    wrt("NAME   c1090 'mcmcchains'")

    num_beg=cellnum=1103

    if (D[1]=='Multinomial'){
        nresp=length(levels(indata[,resp]))-1
        resp.names=levels(indata[,resp])[-as.numeric(D[5])]


        if (is.list(expl)){
            nonfp.s=nonfp.sep
            for (i in 1:length(resp.names)){
                if (D['mode']==0){
                    nonfp.s=gsub(paste(".",resp.names[i],sep=""),"", nonfp.s)
                }
                if (D['mode']==1){
                    nonfp.s=gsub(paste(".(>=",resp.names[i],")",sep=""),"", nonfp.s)
                }
            }
            nonfp.s=unique(nonfp.s)
            for (p in sep.coeff){
                if (is.na(nonfp.sep[1])||sum(p==nonfp.s)==0){
                    if (is.null(categ)|| sum(p==categ["var",])==0){
                            for (j in 1:nresp){
                                wrt(paste("NAME c",cellnum," 'FP_",chartr(".","_",p),"_",resp.names[j],"'",sep=""))
                                wrt(paste("DESC c",cellnum," 'FP:",chartr(".","_",p),"_",resp.names[j],"'",sep=""))
                                cellnum=cellnum+1
                            }
                    }else{
                        if (is.na(categ["ref",which(p==categ["var",])])){
                                categ.names=levels(indata[[p]])
                                for (j in 1:nresp){
                                    for (i in 1:as.numeric(categ["ncateg",which(p==categ["var",])])){
                                        wrt(paste("NAME c",cellnum," 'FP_",chartr(".","_",categ.names[i]),"_",resp.names[j],"'",sep=""))
                                        wrt(paste("DESC c",cellnum," 'FP:",chartr(".","_",categ.names[i]),"_",resp.names[j],"'",sep=""))
                                        cellnum=cellnum+1
                                    }
                                    #cellnum=cellnum+ as.numeric(categ["ncateg",which(p==categ["var",])])
                                }
                        }else{
                                categ.names=levels(indata[[p]])
                                refx=categ["ref",which(p==categ["var",])]
                                categ.names=categ.names[-which(refx==categ.names)]
                                for (j in 1:nresp){
                                    for (i in 1:(as.numeric(categ["ncateg",which(p==categ["var",])])-1)){
                                        wrt(paste("NAME c",cellnum," 'FP_",chartr(".","_",categ.names[i]),"_",resp.names[j],"'",sep=""))
                                        wrt(paste("DESC c",cellnum," 'FP:",chartr(".","_",categ.names[i]),"_",resp.names[j],"'",sep=""))
                                        cellnum=cellnum+1
                                    }
                                    #cellnum=cellnum+ as.numeric(categ["ncateg",which(p==categ["var",])])-1
                                }
                        }
                    }
                }
            }
            for (p in common.coeff){
                nonfp.c=nonfp.common
                for (i in 1:length(nonfp.c)){
                    nonfp.c[i]=gsub("\\.*[[:digit:]]","",nonfp.c[i])
                }
                if (is.na(nonfp.common[1])||sum(p==nonfp.c)==0){
                    if (is.null(categ)|| sum(p==categ["var",])==0){
                        wrt(paste("NAME c",cellnum," 'FP_",chartr(".","_",p),"'",sep=""))
                        wrt(paste("DESC c",cellnum," 'FP:",chartr(".","_",p),"'",sep=""))
                        cellnum=cellnum+1
                    }else{
                       if (is.na(categ["ref",which(p==categ["var",])])){
                          categ.names=levels(indata[[p]])
                          for (i in 1:as.numeric(categ["ncateg",which(p==categ["var",])])){
                              wrt(paste("NAME c",cellnum," 'FP_",chartr(".","_",categ.names[i]),"'",sep=""))
                              wrt(paste("DESC c",cellnum," 'FP:",chartr(".","_",categ.names[i]),"'",sep=""))
                              cellnum=cellnum+1
                          }
                       }else{
                          categ.names=levels(indata[[p]])
                          refx=categ["ref",which(p==categ["var",])]
                          categ.names=categ.names[-which(refx==categ.names)]
                          for (i in 1:(as.numeric(categ["ncateg",which(p==categ["var",])])-1)){
                              wrt(paste("NAME c",cellnum," 'FP_",chartr(".","_",categ.names[i]),"'",sep=""))
                              wrt(paste("DESC c",cellnum," 'FP:",chartr(".","_",categ.names[i]),"'",sep=""))
                              cellnum=cellnum+1
                          }
                       }
                    }
                }
            }
        }else{
            nonfp.s=nonfp
            for (i in 1:length(resp.names)){
                if (D['mode']==0){
                    nonfp.s=gsub(paste(".",resp.names[i],sep=""),"", nonfp.s)
                }
                if (D['mode']==1){
                    nonfp.s=gsub(paste(".(>=",resp.names[i],")",sep=""),"", nonfp.s)
                }
            }
            nonfp.s=unique(nonfp.s)
            expla=expl
            for (p in expla){
                    if (is.na(nonfp[1])||sum(p==nonfp.s)==0){
                        if (is.null(categ)|| sum(p==categ["var",])==0){
                            for (j in 1:nresp){
                                wrt(paste("NAME c",cellnum," 'FP_",chartr(".","_",p),"_",resp.names[j],"'",sep=""))
                                wrt(paste("DESC c",cellnum," 'FP:",chartr(".","_",p),"_",resp.names[j],"'",sep=""))
                                cellnum=cellnum+1
                            }

                        }else{
                            if (is.na(categ["ref",which(p==categ["var",])])){
                                    categ.names=levels(indata[[p]])
                                    for (j in 1:nresp){
                                        for (i in 1:as.numeric(categ["ncateg",which(p==categ["var",])])){
                                            wrt(paste("NAME c",cellnum," 'FP_",chartr(".","_",categ.names[i]),"_",resp.names[j],"'",sep=""))
                                            wrt(paste("DESC c",cellnum," 'FP:",chartr(".","_",categ.names[i]),"_",resp.names[j],"'",sep=""))
                                            cellnum=cellnum+1
                                        }
                                    }
                                }else{
                                    categ.names=levels(indata[[p]])
                                    refx=categ["ref",which(p==categ["var",])]
                                    categ.names=categ.names[-which(refx==categ.names)]
                                    for (j in 1:nresp){
                                        for (i in 1:(as.numeric(categ["ncateg",which(p==categ["var",])])-1)){
                                            wrt(paste("NAME c",cellnum," 'FP_",chartr(".","_",categ.names[i]),"_",resp.names[j],"'",sep=""))
                                            wrt(paste("DESC c",cellnum," 'FP:",chartr(".","_",categ.names[i]),"_",resp.names[j],"'",sep=""))
                                            cellnum=cellnum+1
                                        }
                                    }

                                }

                        }
                    }
            }
        }

    }else{
        if (D[1]=="Multivariate Normal"||D[1]=="Mixed"){
            nresp=length(resp)

            if (is.list(expl)){
                nonfp.s=nonfp.sep
                for (i in 1:length(resp)){
                    nonfp.s=gsub(paste(".",resp[i],sep=""),"", nonfp.s)
                }
                nonfp.s=unique(nonfp.s)
                for (p in sep.coeff){
                    if (is.na(nonfp.sep[1])||sum(p==nonfp.s)==0){
                        if (is.null(categ)|| sum(p==categ["var",])==0){
                                for (j in 1:nresp){
                                    wrt(paste("NAME c",cellnum," 'FP_",chartr(".","_",p),"_",resp[j],"'",sep=""))
                                    wrt(paste("DESC c",cellnum," 'FP:",chartr(".","_",p),"_",resp[j],"'",sep=""))
                                    cellnum=cellnum+1
                                }
                        }else{
                            if (is.na(categ["ref",which(p==categ["var",])])){
                                    categ.names=levels(indata[[p]])
                                    for (j in 1:nresp){
                                        for (i in 1:as.numeric(categ["ncateg",which(p==categ["var",])])){
                                            wrt(paste("NAME c",cellnum," 'FP_",chartr(".","_",categ.names[i]),"_",resp.names[j],"'",sep=""))
                                            wrt(paste("DESC c",cellnum," 'FP:",chartr(".","_",categ.names[i]),"_",resp.names[j],"'",sep=""))
                                            cellnum=cellnum+1
                                        }
                                        #cellnum=cellnum+ as.numeric(categ["ncateg",which(p==categ["var",])])
                                    }
                            }else{
                                    categ.names=levels(indata[[p]])
                                    refx=categ["ref",which(p==categ["var",])]
                                    categ.names=categ.names[-which(refx==categ.names)]
                                    for (j in 1:nresp){
                                        for (i in 1:(as.numeric(categ["ncateg",which(p==categ["var",])])-1)){
                                            wrt(paste("NAME c",cellnum," 'FP_",chartr(".","_",categ.names[i]),"_",resp.names[j],"'",sep=""))
                                            wrt(paste("DESC c",cellnum," 'FP:",chartr(".","_",categ.names[i]),"_",resp.names[j],"'",sep=""))
                                            cellnum=cellnum+1
                                        }
                                        #cellnum=cellnum+ as.numeric(categ["ncateg",which(p==categ["var",])])-1
                                    }
                            }
                        }
                    }
                }
                for (p in common.coeff){
                    nonfp.c=nonfp.common
                    for (i in 1:length(nonfp.c)){
                        nonfp.c[i]=gsub("\\.*[[:digit:]]","",nonfp.c[i])
                    }
                    if (is.na(nonfp.common[1])||sum(p==nonfp.c)==0){
                        if (is.null(categ)|| sum(p==categ["var",])==0){
                            wrt(paste("NAME c",cellnum," 'FP_",chartr(".","_",p),"'",sep=""))
                            wrt(paste("DESC c",cellnum," 'FP:",chartr(".","_",p),"'",sep=""))
                            cellnum=cellnum+1
                        }else{
                           if (is.na(categ["ref",which(p==categ["var",])])){
                              categ.names=levels(indata[[p]])
                              for (i in 1:as.numeric(categ["ncateg",which(p==categ["var",])])){
                                  wrt(paste("NAME c",cellnum," 'FP_",chartr(".","_",categ.names[i]),"'",sep=""))
                                  wrt(paste("DESC c",cellnum," 'FP:",chartr(".","_",categ.names[i]),"'",sep=""))
                                  cellnum=cellnum+1
                              }
                           }else{
                              categ.names=levels(indata[[p]])
                              refx=categ["ref",which(p==categ["var",])]
                              categ.names=categ.names[-which(refx==categ.names)]
                              for (i in 1:(as.numeric(categ["ncateg",which(p==categ["var",])])-1)){
                                  wrt(paste("NAME c",cellnum," 'FP_",chartr(".","_",categ.names[i]),"'",sep=""))
                                  wrt(paste("DESC c",cellnum," 'FP:",chartr(".","_",categ.names[i]),"'",sep=""))
                                  cellnum=cellnum+1
                              }
                           }
                        }
                    }
                }
            }else{
                nonfp.s=nonfp
                for (i in 1:length(resp)){
                    nonfp.s=gsub(paste(".",resp[i],sep=""),"", nonfp.s)
                }
                nonfp.s=unique(nonfp.s)
                expla=expl
                for (p in expla){
                        if (is.na(nonfp[1])||sum(p==nonfp.s)==0){
                            if (is.null(categ)|| sum(p==categ["var",])==0){
                                for (j in 1:nresp){
                                    wrt(paste("NAME c",cellnum," 'FP_",chartr(".","_",p),"_",resp[j],"'",sep=""))
                                    wrt(paste("DESC c",cellnum," 'FP:",chartr(".","_",p),"_",resp[j],"'",sep=""))
                                    cellnum=cellnum+1
                                }

                            }else{
                                if (is.na(categ["ref",which(p==categ["var",])])){
                                    categ.names=levels(indata[[p]])
                                    for (j in 1:nresp){
                                        for (i in 1:as.numeric(categ["ncateg",which(p==categ["var",])])){
                                            wrt(paste("NAME c",cellnum," 'FP_",chartr(".","_",categ.names[i]),"_",resp.names[j],"'",sep=""))
                                            wrt(paste("DESC c",cellnum," 'FP:",chartr(".","_",categ.names[i]),"_",resp.names[j],"'",sep=""))
                                            cellnum=cellnum+1
                                        }
                                    }

                                }else{
                                    categ.names=levels(indata[[p]])
                                    refx=categ["ref",which(p==categ["var",])]
                                    categ.names=categ.names[-which(refx==categ.names)]
                                    for (j in 1:nresp){
                                        for (i in 1:(as.numeric(categ["ncateg",which(p==categ["var",])])-1)){
                                            wrt(paste("NAME c",cellnum," 'FP_",chartr(".","_",categ.names[i]),"_",resp.names[j],"'",sep=""))
                                            wrt(paste("DESC c",cellnum," 'FP:",chartr(".","_",categ.names[i]),"_",resp.names[j],"'",sep=""))
                                            cellnum=cellnum+1
                                        }
                                    }

                                }

                            }
                        }
                }
            }

        }else{
                expla=expl
                #if (!is.null(inter)) expla=expl1 else expla=expl
                for (p in expla){
                        if (is.na(nonfp[1])||sum(p==nonfp)==0){
                            if (is.null(categ)|| sum(p==categ["var",])==0){
                                wrt(paste("NAME c",cellnum," 'FP_",p,"'",sep=""))
                                wrt(paste("DESC c",cellnum," 'FP:",p,"'",sep=""))
                                cellnum=cellnum+1
                            }else{
                                if (is.na(categ["ref",which(p==categ["var",])])){
                                      categ.names=levels(indata[[p]])
                                      for (i in 1:as.numeric(categ["ncateg",which(p==categ["var",])])){
                                          wrt(paste("NAME c",cellnum," 'FP_",chartr(".","_",categ.names[i]),"'",sep=""))
                                          wrt(paste("DESC c",cellnum," 'FP:",chartr(".","_",categ.names[i]),"'",sep=""))
                                          cellnum=cellnum+1
                                      }
                                }else{
                                      categ.names=levels(indata[[p]])
                                      refx=categ["ref",which(p==categ["var",])]
                                      categ.names=categ.names[-which(refx==categ.names)]
                                      for (i in 1:(as.numeric(categ["ncateg",which(p==categ["var",])])-1)){
                                          wrt(paste("NAME c",cellnum," 'FP_",chartr(".","_",categ.names[i]),"'",sep=""))
                                          wrt(paste("DESC c",cellnum," 'FP:",chartr(".","_",categ.names[i]),"'",sep=""))
                                          cellnum=cellnum+1
                                      }
                                }
                            }
                        }
                }
            }
    }

    wrt.resid=function(rpx, resid.lev, cellnum){
        nrpx=length(rpx)
        for (j in 1: nrpx){
            for (i in 1:j){
                if (i==j){
                    wrt(paste("NAME c",cellnum," 'RP",resid.lev,"_var_",chartr(".", "_", rpx[i]),"'",sep=""))
                    wrt(paste("DESC c",cellnum," 'RP",resid.lev,":var(",chartr(".", "_", rpx[i]),")'",sep=""))
                    cellnum=cellnum+1
                }else{
                    wrt(paste("NAME c",cellnum," 'RP",resid.lev,"_cov_",chartr(".", "_", rpx[i]),"_",chartr(".", "_", rpx[j]),"'",sep=""))
                    wrt(paste("DESC c",cellnum," 'RP",resid.lev,":cov(",chartr(".", "_", rpx[i]),",",chartr(".", "_", rpx[j]),")'",sep=""))
                    cellnum=cellnum+1
                }
            }
        }
        cellnum
    }

    wrt.resid2=function(rpx, resid.lev, cellnum, clre){
        nrpx=length(rpx)
        nclre=ncol(clre)
        k=1
        for (j in 1: nrpx){
            for (i in 1:j){
                if (i==j){
                    if (resid.lev==as.numeric(clre[1,k])&&rpx[i]==clre[2,k]&&rpx[i]==clre[3,k]){
                        k=k+1
                    }else{
                        wrt(paste("NAME c",cellnum," 'RP",resid.lev,"_var_",chartr(".", "_", rpx[i]),"'",sep=""))
                        wrt(paste("DESC c",cellnum," 'RP",resid.lev,":var(",chartr(".", "_", rpx[i]),")'",sep=""))
                        cellnum=cellnum+1
                    }
                }else{
                    if (resid.lev==as.numeric(clre[1,k])&&rpx[i]==clre[2,k]&&rpx[j]==clre[3,k]){
                        k=k+1
                    }else{
                        wrt(paste("NAME c",cellnum," 'RP",resid.lev,"_cov_",chartr(".", "_", rpx[i]),"_",chartr(".", "_", rpx[j]),"'",sep=""))
                        wrt(paste("DESC c",cellnum," 'RP",resid.lev,":cov(",chartr(".", "_", rpx[i]),",",chartr(".", "_", rpx[j]),")'",sep=""))
                        cellnum=cellnum+1
                    }
                }
            }
        }
        cellnum
    }

    wrt.resid3=function(rpx, resid.lev, cellnum){
        nrpx=length(rpx)
        for (j in 1: nrpx){
            for (i in 1:j){
                if (i==j){
                    wrt(paste("NAME c",cellnum," 'RP",resid.lev,"_var_",chartr(".", "_", rpx[i]),"'",sep=""))
                    wrt(paste("DESC c",cellnum," 'RP",resid.lev,":var(",chartr(".", "_", rpx[i]),")'",sep=""))
                    cellnum=cellnum+1
                }
            }
        }
        cellnum
    }

    if (D[1]=='Mixed'&& nrp>0){
            if(sum(as.numeric(sub("rp","",rp.names))==2)>0){
                rp[["rp2"]]=c("bcons",rp[["rp2"]])
            }else{
                rp.names=c(rp.names,"rp2")
                rp=c(rp,rp2=c("bcons"))
            }
    }
    if (nrp>0){
        for (ii in 1:nrp){
            if(!is.null(fact)){
                cellnum=wrt.resid3(rp[[ii]],as.numeric(sub("rp","",rp.names[ii])),cellnum)
            }else{
                if (is.null(clre)){
                    cellnum=wrt.resid(rp[[ii]],as.numeric(sub("rp","",rp.names[ii])),cellnum)
                }else{
                    cellnum=wrt.resid2(rp[[ii]],as.numeric(sub("rp","",rp.names[ii])),cellnum, clre)
                }
            }
        }
    }
    # Add in extra parameters ect.
    if (D[1]=='Binomial'||D[1]=='Poisson'||(D[1]=='Multinomial'&&as.numeric(D["mode"])==0)){
        wrt(paste("NAME c",cellnum," 'RP1_bcons_1'",sep=""))
        wrt(paste("DESC c",cellnum," 'RP1:bcons_1'",sep=""))
        cellnum=cellnum+1
    }

    lencell=cellnum-num_beg
    num_end =cellnum-1

    wrt(paste("CODE ",iterations, lencell, 1, "c1300"))
    wrt("CALC c1300 = c1300 * 1")
    wrt("NAME   c1300 'itnum'")
    wrt(paste("CODE ",lencell, 1,iterations, "c1301"))
    wrt("NAME   c1301 'parnum'")
    wrt("NAME   c1302 'iteration'")
    wrt("DESC   c1302 '\\Iteration'")
    tempnum=num_beg:num_end
    tempstr=NULL
    for (i in tempnum) tempstr=paste(tempstr, paste("c",i,sep=""))

    wrt(paste("UNVE ",lencell,"'parnum' 'itnum' 'mcmcchains' 'iteration'",tempstr))
    if(D[1]=='Multinomial'&&as.numeric(D["mode"])==1){
        wrt(paste("PUT ",iterations, 1, paste("c",cellnum,sep="")))
        wrt(paste("NAME c",cellnum," 'RP1_bcons_1'",sep=""))
        wrt(paste("DESC c",cellnum," 'RP1:bcons_1'",sep=""))
        tempstr=paste(tempstr, paste("c",cellnum,sep=""))
        cellnum=cellnum+1
    }
    wrt(paste("PSTA '",chainfile, "' ","'iteration' 'deviance'",tempstr,sep=""))
    wrt(paste("ERAS 'itnum' 'parnum' 'iteration'",tempstr,sep=""))

    if (resi.store&&nrp>0){
        tempcell=998
        tr.all=NULL
        for (j in nrp:1){
            rpx=rp[[j]]
            len.rpx=length(rp[[j]])
            wrt(paste("NOTE Calculate MCMC starting values for level ",as.numeric(sub("rp","",rp.names[j]))," residuals",sep=""))
            levtt=as.numeric(sub("rp","",rp.names[j]))
            wrt(paste("RLEV   ",levtt,sep=""))
            wrt("RFUN")
            wrt("RCOV   2")
            tempcol=(tempcell+len.rpx):tempcell
            tr=paste("c",tempcol,sep="")
            for (k in 1:len.rpx){
                wrt(paste("NAME ",tr[k],paste("'lev_",levtt,"_resi_est_",rpx[k],"'",sep="")))
            }
            wrt(paste("NAME ",tr[len.rpx+1],paste("'lev_",levtt,"_resi_cov","'",sep="")))
            wrt(paste("ROUT ", paste(tr,collapse=" ")))
            wrt("MISR 0")
            wrt("RESI")
            wrt("MISR 1")
            tr.all=c(tr.all,tr)
            tempcell=tempcell+len.rpx+1
        }
        wrt(paste("PSTA '",resifile, "' ", paste(tr.all,collapse=" "),sep=""))
        wrt(paste("ERAS   ", paste(tr.all,collapse=" "),sep=""))

        if (!is.null(resi.store.levs)){
            resiname=rep(NA,length(resi.store.levs))
            for (i in 1:length(resi.store.levs)){
                resiname[i]=paste("'resi_lev",resi.store.levs[i],"'",sep="")
            }
            wrt(paste("PSTA '",resichains,"' ",paste(resiname,collapse=" "),sep=""))
        }
    }
    if ((!is.null(BUGO))&&!(D[1]=="Mixed")&&nrp>0){
        if(D[1]=="Normal") DD=1
        if(D[1]=="Binomial") DD=2
        if(D[1]=="Poisson") DD=3
        if(D[1]=='Multivariate Normal') DD=4
        if(D[1]=="Multinomial") {if (as.numeric(D[4])==0) DD=6 else DD=7}

        tempcell= 998
        for (j in nrp:1){
            rpx=rp[[j]]
            len.rpx=length(rp[[j]])
            wrt(paste("NOTE Calculate MCMC starting values for level ",as.numeric(sub("rp","",rp.names[j]))," residuals",sep=""))
            wrt(paste("RLEV   ",as.numeric(sub("rp","",rp.names[j])),sep=""))
            wrt("RFUN")
            wrt("RCOV   2")
            tempcol=(tempcell+len.rpx):tempcell
            tempvec=tempvec2=NULL
            for (i in 1:(len.rpx+1)) tempvec=paste(tempvec, paste("c", tempcol[i],sep=""))
            for (i in 1:len.rpx) tempvec2=paste(tempvec2, paste("c", tempcol[i],sep=""))
            wrt(paste("ROUT   ",tempvec,sep=""))
            wrt("MISR 0")
            wrt("RESI")
            wrt("MISR 1")
            wrt(paste("JOIN   c",997, tempvec2," c",997,sep=""))
            wrt(paste("JOIN   c",996," c", tempcol[len.rpx+1]," c",996,sep=""))
            wrt(paste("ERAS   ",tempvec,sep=""))
        }

        version=as.numeric(BUGO[1])
        if(D[1]=='Normal'||D[1]=='Multivariate Normal') DD2=0
        if (!is.null(bugofile)) wrt(paste("BUGO ",version," ",DD," ",DD2," c997 ", "'",bugofile,"'",sep=""))
        wrt(paste("BUGO 6 ",DD," ",DD2, " c997 ","'",modelfile,"' ","'",initfile,"' ","'",datafile,"'",sep=""))
        wrt("ERAS   c997 c996")
    }
    #wrt(paste("STOR ",paste(tempfile("worksheet_"),".dta",sep="")))
    if (!debugmode) wrt("EXIT")
}
