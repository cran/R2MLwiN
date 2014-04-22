MacroScript1 <-
function(indata,dtafile,resp, levID, expl, rp, D='Normal', nonlinear=c(0,1), categ=NULL,notation=NULL, nonfp=NA, clre,smat, Meth=1,
BUGO=NULL,mem.init="default", optimat=F, weighting=NULL,modelfile=modelfile,initfile=initfile,datafile=datafile,
macrofile=macrofile,IGLSfile=IGLSfile,resifile=resifile,resi.store=resi.store,resioptions=resioptions,debugmode=debugmode){

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


    if (nrp>0){
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
        wrt(paste("INIT    ",nlev+1," 6000 2500 ",num_vars+10," 20", sep=""))
    }else{
        wrt(paste("INIT    ",mem.init[1],mem.init[2],mem.init[3],mem.init[4],mem.init[5]))
    }

    wrt("NOTE     Limit the maximum matrix size")
    if (optimat){
        wrt("OPTS   1")
    }else{
        wrt("OPTS   0")
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
                            wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                            #wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
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
                                wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                                #wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
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
            expl <- c(explx, exply)
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
                                wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                                #wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
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
                                    wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                                    #wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
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
                sep.coeff <- c(explx, exply)
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
                                wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                                #wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
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
                                    wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                                    #wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
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
                                    wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                                    #wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
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
                                        wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                                        #wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
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
                    expl <- c(explx, exply)
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
                                wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                                #wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
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
                                    wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                                    #wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
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
                sep.coeff <- c(explx, exply)
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
                                wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                                #wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
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
                                    wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                                    #wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
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
                                    wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                                    #wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
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
                                        wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                                        #wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
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
                    expl <- c(explx, exply)
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
                                    wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                                    #wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
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
                                    wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                                    #wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
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
                sep.coeff <- c(explx, exply)
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
                                wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                                #wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
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
                                    wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                                    #wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
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
                                    wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                                    #wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
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
                                        wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                                        #wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
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
                    expl <- c(explx, exply)
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
                            wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                            #wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
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
                                wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                                #wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
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
            expl <- c(explx, exply)
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
                            wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                            #wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
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
                                wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                                #wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
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
            expl <- c(explx, exply)
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
                            wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                            #wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
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
                                wrt(paste("ADDT    '", p, "' ", which(levels(indata[,p])==categ["ref", which(p == categ["var", ])]), sep = ""))
                                #wrt(paste("ADDT    '",p,"' '", categ["ref",which(p==categ["var",])],"'",sep=""))
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
            expl <- c(explx, exply)
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

    nexpl=length(expl)
    wrt("")

    wrt("NOTE   Set estimation method")
    if (Meth!=2){
        wrt(paste("METH",Meth))
    }
    wrt(paste("LINE ",nonlinear[1],nonlinear[2]))
    wrt("")
    if (!is.null(weighting)){
        if (is.null(weighting$FSDE)) weighting$FSDE=2
        if (is.null(weighting$RSDE)) weighting$RSDE=2
        if(length(weighting$levels)==length(weighting$weights)){
            for (i in 1:length(weighting$weights)){
                if (!is.na(weighting$weights[i])){
                    wrt(paste("NOTE   Specify sampling weights at level", weighting$levels[i]))
                    wrt(paste("WEIG ", weighting$levels[i]," ", 1, " '",weighting$weights[i],"'",sep=""))
                    wrt("")
                }else{
                    wrt(paste("NOTE   Specify equal weights at level", weighting$levels[i]))
                    wrt(paste("WEIG ", weighting$levels[i]," ", 1,sep=""))
                    wrt("")
                }
                if (as.integer(weighting$mode)==2){
                    wrt("NOTE   Standardised weighting")
                    wrt(paste("WEIG ", weighting$levels[i]," ", 2, " c",1700+as.integer(weighting$levels[i]),sep=""))
                    wrt("WEIG 2")
                    wrt("")
                }
                if (as.integer(weighting$mode)==1){
                    wrt("NOTE   Raw weighting")
                    wrt(paste("WEIG ", weighting$levels[i]," ", 2, " c",1700+as.integer(weighting$levels[i]),sep=""))
                    wrt("WEIG 1")
                    wrt("")
                }
            }
            if (as.integer(weighting$mode)>0){
                wrt("NOTE   Create the standardised weights")
                wrt("WEIG")
                wrt("")
                if ( weighting$FSDE==2){
                    wrt("NOTE   Turn on sandwich estimators for the fixed part parameter standard errors")
                    wrt("FSDE 2")
                    wrt("")
                }else{
                    wrt("FSDE 0")
                    wrt("")
                }
                if ( weighting$RSDE==2){
                    wrt("NOTE   Turn on sandwich estimators for the random part parameter standard errors")
                    wrt("RSDE 2")
                    wrt("")
                }else{
                    wrt("RSDE 0")
                    wrt("")
                }
            }else{
                wrt("NOTE   Create the equal weights")
                wrt("WEIG")
                wrt("WEIG   0")
                wrt("")
            }
        }else{
            stop("The length of levels does not match with the length of weights.")
        }
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
    if (D[1]=="Normal" || D[1]=="Multivariate Normal"){
    wrt("LIKE   b100")
    }
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


    calcresiduals = function(level, rpx, resioptions, tempcell =998, mcmc=T, clre=clre){

        wrt("")
        if (!("norecode"%in%resioptions)){
            wrt("MISR 0")
        }

        tempcell = tempcell
        len.rpx = length(rpx)
        if (level == 2 & D[[1]][1]=="Mixed"){
            for (i in 2:length(D)){
                if (D[[i]][1]=="Binomial" | D[[i]][1]=="Poisson"){
                    len.rpx = len.rpx + 1
                }
            }
        }
        residual_estimates=NULL
        for (k in 1:len.rpx){
            tr=paste("c",tempcell,sep="")
            residual_estimates = c(residual_estimates,tr)
            wrt(paste("NAME ",tr, paste("'lev_",level,"_resi_est_",rpx[k],"'",sep="")))
            wrt(paste("DESC ",tr, paste("'residual estimates'",sep="")))
            tempcell =1 +tempcell
        }

        if ("variance" %in% resioptions){
            residual_var=NULL
            for (k in 1:len.rpx){
                tr=paste("c",tempcell,sep="")
                residual_var = c(residual_var,tr)
                wrt(paste("NAME ",tr, paste("'lev_",level,"_resi_variance_",rpx[k],"'",sep="")))
                wrt(paste("DESC ",tr, paste("'residual variance'",sep="")))
                tempcell =1 +tempcell
            }
        }else{
            residual_se=NULL
            for (k in 1:len.rpx){
                        tr=paste("c",tempcell,sep="")
                        residual_se = c(residual_se,tr)
                        wrt(paste("NAME ",tr, paste("'lev_",level,"_resi_se_",rpx[k],"'",sep="")))
                        wrt(paste("DESC ",tr, paste("'residual standard error'",sep="")))
                        tempcell =1 +tempcell
            }

        }
        wrt("RFUN")
        if ("variance" %in% resioptions){
            wrt(paste(c("ROUT ", residual_estimates,residual_var), collapse=" "))
        }else{
            wrt(paste(c("ROUT ", residual_estimates, residual_se), collapse=" "))
        }
        wrt("")

        wrt(paste("RLEV   ",level,sep=""))
        wrt("RCOV   1")

        if ("standardised"%in%resioptions||"deletion"%in%resioptions||"leverage"%in%resioptions){
            std_residual_estimates=NULL
            for (k in 1:len.rpx){
                tr=paste("c",tempcell,sep="")
                std_residual_estimates=c(std_residual_estimates,tr)
                wrt(paste("NAME ",tr, paste("'lev_",level,"_std_resi_est_",rpx[k],"'",sep="")))
                wrt(paste("DESC ",tr, paste("'std standardised residual'",sep="")))
                tempcell =1 +tempcell
            }

            wrt("RTYP   0")
            if (mcmc){
                wrt("MCRE")
            }else{
                wrt("RESI")
            }
            ccount =1
            if (!("variance" %in% resioptions)){
                for (cc in std_residual_estimates){
                    wrt(paste("CALC ", cc, "=",residual_estimates[ccount],"/sqrt(", residual_se[ccount],")",sep=""))
                    ccount =1 +ccount
                }
            }

        }

        #NOTE leverage depends on residuals with rtype 0
        if ("leverage"%in%resioptions||"influence"%in%resioptions){
            #influence requires leverage to be calculated
            residual_leverage=NULL
            for (k in 1:len.rpx){
                tr=paste("c",tempcell,sep="")
                residual_leverage = c(residual_leverage,tr)
                wrt(paste("NAME ",tr, paste("'lev_",level,"_resi_leverage_",rpx[k],"'",sep="")))
                wrt(paste("DESC ",tr, paste("'leverage residual'",sep="")))
                tempcell =1 +tempcell
            }

            ccount =1
            for (cc in residual_leverage){
                resi_sdx = residual_se[ccount]
                wrt(paste("OMEGa ",level,rpx[ccount], paste("c",tempcell,sep="")))# retrieve variance for corresponding random parameter
                wrt(paste("PICK 1 ",paste("c",tempcell,sep=""), "b50"))
                wrt(paste("ERASe ",paste("c",tempcell,sep="")))
                wrt(paste("CALC ", cc, "=1-sqrt(",resi_sdx,")/sqrt(b50)"))
                ccount =1 +ccount
            }

            if (!("standardised"%in%resioptions)&&!("deletion"%in%resioptions)){
                wrt(paste(c("ERASe ",std_residual_estimates),collapse=" "))
            }


        }

        wrt("RTYP   1")# Compute comparative variances
        if (mcmc){
            wrt("MCRE")
        }else{
            wrt("RESI")
        }

        if (!("variance"%in%resioptions)){
            for (cc in residual_se){
                wrt(paste("CALC ",cc, "=sqrt(",cc,")",sep=""))# Convert the variances to standard errors
            }
        }

        if ("deletion"%in%resioptions||"influence"%in%resioptions){
            residual_deletion=NULL
            for (k in 1:len.rpx){
                tr=paste("c",tempcell,sep="")
                residual_deletion = c(residual_deletion,tr)
                wrt(paste("NAME ",tr, paste("'lev_",level,"_resi_deletion_",rpx[k],"'",sep="")))
                wrt(paste("DESC ",tr, paste("'deletion residual'",sep="")))
                tempcell =1 +tempcell
            }


            wrt(paste("NOBS ",level,"b31 b32"))
            ccount =1
            for (cc in residual_deletion){
                stdres =std_residual_estimates[ccount]
                wrt(paste("CALC   ", cc, "=", stdres,"/ sqrt((b31 - 1 -", stdres,"^2)/(b31 - 2))",sep=""))
                ccount = 1+ ccount
            }

            if (!("standardised"%in%resioptions)&&!("leverage"%in%resioptions)){
                wrt(paste(c("ERASe ",std_residual_estimates),collapse=" "))
            }
        }

        if ("influence"%in%resioptions){
            residual_influence = NULL
            for (k in 1:len.rpx){
                tr=paste("c",tempcell,sep="")
                residual_influence = c(residual_influence,tr)
                wrt(paste("NAME ",tr, paste("'lev_",level,"_resi_influence_",rpx[k],"'",sep="")))
                wrt(paste("DESC ",tr, paste("'influence residual'",sep="")))
                tempcell =1 +tempcell
            }

            ccount =1
            for (cc in residual_influence){
                res_del=residual_deletion[ccount]
                res_lev=residual_leverage[ccount]
                wrt(paste("SUM    ", res_lev, " b50",sep=""))
                wrt(paste("CALC   ", cc, "=",res_lev,"/b50",sep=""))
                wrt(paste("CALC   ", cc, "=sqrt(",cc,"/(1-",cc,"))*abso(",res_del,")",sep=""))
                ccount =ccount+1
            }

            if (!("deletion"%in%resioptions)){
                wrt(paste(c("ERASE  ", residual_deletion),collapse=" "))
            }
            if(!("leverage"%in%resioptions)){
                wrt(paste(c("ERASE  ", residual_leverage),collapse=" "))
            }
        }

        if ("sampling"%in%resioptions){
            varcols = NULL
            numcombs = 0
            k=1
            for (i in 1:len.rpx){
                for (j in 1:i){
                    tempflag =1
                    if (i==j){
                        ## removel some variance residuals
                        if (!is.null(clre)){
                            if (!(level==as.numeric(clre[1,k])&&rpx[i]==clre[2,k]&&rpx[i]==clre[3,k])){
                                tr=paste("c",tempcell,sep="")
                                varcols = c(varcols,tr)
                                wrt(paste("NAME ",tr, paste("'lev_",level,"_resi_var_",rpx[i],"'",sep="")))
                                wrt(paste("DESC ",tr, paste("'sampling variance'",sep="")))
                            }else{
                                tempflag =0
                                if (k<ncol(clre)) k = k+1
                            }
                        }else{
                            tr=paste("c",tempcell,sep="")
                            varcols = c(varcols,tr)
                            wrt(paste("NAME ",tr, paste("'lev_",level,"_resi_var_",rpx[i],"'",sep="")))
                            wrt(paste("DESC ",tr, paste("'sampling variance'",sep="")))
                        }
                    }else{
                        if (!is.null(clre)){
                            if (!(level==as.numeric(clre[1,k])&&rpx[i]==clre[2,k]&&rpx[j]==clre[3,k])||
                            !(level==as.numeric(clre[1,k])&&rpx[j]==clre[2,k]&&rpx[i]==clre[3,k])){
                                tr=paste("c",tempcell,sep="")
                                varcols = c(varcols,tr)
                                wrt(paste("NAME ",tr, paste("'lev_",level,"_resi_cov_",rpx[i],"_",rpx[j],"'",sep="")))
                                wrt(paste("DESC ",tr, paste("'sampling covariance'",sep="")))
                            }else{
                                tempflag =0
                                if (k<ncol(clre)) k=k+1
                            }
                        }else{
                            tr=paste("c",tempcell,sep="")
                            varcols = c(varcols,tr)
                            wrt(paste("NAME ",tr, paste("'lev_",level,"_resi_cov_",rpx[i],"_",rpx[j],"'",sep="")))
                            wrt(paste("DESC ",tr, paste("'sampling covariance'",sep="")))
                        }
                    }
                    if (tempflag ==1){
                        tempcell =1 +tempcell
                        numcombs =1 +numcombs
                    }
                }
            }

            std_residual_sampling = NULL
            ii=1
            for (k in 1:len.rpx){
                tr=paste("c",tempcell,sep="")
                std_residual_sampling = c(std_residual_sampling,tr)
                tempcell =1 +tempcell
            }

            tempcol1 = paste("c",tempcell+1,sep="")
            tempcol4 = paste("c",tempcell,sep="")
            wrt(paste("NAME ",tempcol4, paste("'lev_",level,"_resi_cov'",sep="")))
            wrt(paste("DESC ",tempcol4, paste("'sampling var(cov)'",sep="")))
            tempcell=tempcell+1

            wrt("RFUN")
            wrt(paste(c("ROUT   ",std_residual_sampling, tempcol4), collapse=" "))
            wrt("RCOV 2")
            wrt("RESI")
            wrt("")

            #NOTE: This is square rooted, as the residual covariances are sometimes negative
            wrt(paste("NOBS ",level, " b31 b32",sep=""))
            wrt(paste("CODE ",numcombs, " 1 b31 ", tempcol1,sep=""))
            wrt(paste(c("SPLIt ",tempcol4, tempcol1, varcols), collapse=" "))
            wrt(paste(c("ERAS ",tempcol1, tempcol4), collapse=" "))
            wrt(paste(c("ERAS ", std_residual_sampling), collapse=" "))
        }

        wrt("")
        wrt(paste("NOBS ", level, " b30 b31",sep=""))
        wrt(paste("GENE 1 b30 1",paste("c",tempcell,sep="")))
        wrt(paste("NAME ",paste("c",tempcell,sep=""), " 'lev_",level,"_residualid'",sep=""))
        if (!("norecode"%in%resioptions)){
            wrt("MISR 1")
        }
        wrt("")
        tempcell

    }


    if (resi.store&& nrp>0){
        for (j in nrp:1){
            rpx=rp[[j]]
            len.rpx=length(rp[[j]])
            wrt(paste("NOTE Calculate level ",as.numeric(sub("rp","",rp.names[j]))," residuals",sep=""))
            levtt=as.numeric(sub("rp","",rp.names[j]))
            if (j==nrp){
                tempcell=calcresiduals(levtt, rpx, resioptions, tempcell =998, mcmc=F, clre=clre)
            }else{
                tempcell=calcresiduals(levtt, rpx, resioptions, tempcell =tempcell+1, mcmc=F, clre=clre)
            }
        }
        wrt(paste("PSTA '",resifile, "' c998-",paste("c",tempcell,sep=""),sep=""))
        wrt(paste("ERAS c988-",paste("c",tempcell,sep=""),sep=""))
    }

    FP=NULL

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
                                FP=c(FP, paste("FP_",chartr(".","_",p),"_",resp.names[j],sep=""))
                            }
                    }else{
                        if (is.na(categ["ref",which(p==categ["var",])])){
                                categ.names=levels(indata[[p]])
                                for (j in 1:nresp){
                                    for (i in 1:as.numeric(categ["ncateg",which(p==categ["var",])])){
                                        FP=c(FP,  paste("FP_",chartr(".","_",categ.names[i]),"_",resp.names[j],sep=""))
                                    }
                                }
                        }else{
                                categ.names=levels(indata[[p]])
                                refx=categ["ref",which(p==categ["var",])]
                                categ.names=categ.names[-which(refx==categ.names)]
                                for (j in 1:nresp){
                                    for (i in 1:(as.numeric(categ["ncateg",which(p==categ["var",])])-1)){
                                        FP=c(FP, paste("FP_",chartr(".","_",categ.names[i]),"_",resp.names[j],sep=""))
                                    }
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
                        FP=c(FP, paste("FP_",chartr(".","_",p),sep=""))
                    }else{
                       if (is.na(categ["ref",which(p==categ["var",])])){
                          categ.names=levels(indata[[p]])
                          for (i in 1:as.numeric(categ["ncateg",which(p==categ["var",])])){
                              FP=c(FP, paste("FP_",chartr(".","_",categ.names[i]),sep=""))
                          }
                       }else{
                          categ.names=levels(indata[[p]])
                          refx=categ["ref",which(p==categ["var",])]
                          categ.names=categ.names[-which(refx==categ.names)]
                          for (i in 1:(as.numeric(categ["ncateg",which(p==categ["var",])])-1)){
                               FP=c(FP, paste("FP_",chartr(".","_",categ.names[i]),sep=""))
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
                                FP=c(FP, paste("FP_",chartr(".","_",p),"_",resp.names[j],sep=""))
                            }

                        }else{
                            if (is.na(categ["ref",which(p==categ["var",])])){
                                categ.names=levels(indata[[p]])
                                for (j in 1:nresp){
                                    for (i in 1:as.numeric(categ["ncateg",which(p==categ["var",])])){
                                        FP=c(FP,  paste("FP_",chartr(".","_",categ.names[i]),"_",resp.names[j],sep=""))
                                    }
                                }

                            }else{
                                categ.names=levels(indata[[p]])
                                refx=categ["ref",which(p==categ["var",])]
                                categ.names=categ.names[-which(refx==categ.names)]
                                for (j in 1:nresp){
                                    for (i in 1:(as.numeric(categ["ncateg",which(p==categ["var",])])-1)){
                                        FP=c(FP, paste("FP_",chartr(".","_",categ.names[i]),"_",resp.names[j],sep=""))
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
                                    FP=c(FP, paste("FP_",chartr(".","_",p),"_",resp[j],sep=""))
                                }
                        }else{
                            if (is.na(categ["ref",which(p==categ["var",])])){
                                categ.names=levels(indata[[p]])
                                for (j in 1:nresp){
                                    for (i in 1:as.numeric(categ["ncateg",which(p==categ["var",])])){
                                        FP=c(FP,  paste("FP_",chartr(".","_",categ.names[i]),"_",resp.names[j],sep=""))
                                    }
                                }

                            }else{
                                categ.names=levels(indata[[p]])
                                refx=categ["ref",which(p==categ["var",])]
                                categ.names=categ.names[-which(refx==categ.names)]
                                for (j in 1:nresp){
                                    for (i in 1:(as.numeric(categ["ncateg",which(p==categ["var",])])-1)){
                                        FP=c(FP, paste("FP_",chartr(".","_",categ.names[i]),"_",resp.names[j],sep=""))
                                    }
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
                            FP=c(FP, paste("FP_",chartr(".","_",p),sep=""))
                        }else{
                           if (is.na(categ["ref",which(p==categ["var",])])){
                                  categ.names=levels(indata[[p]])
                                  for (i in 1:as.numeric(categ["ncateg",which(p==categ["var",])])){
                                      FP=c(FP, paste("FP_",chartr(".","_",categ.names[i]),sep=""))
                                  }
                           }else{
                                  categ.names=levels(indata[[p]])
                                  refx=categ["ref",which(p==categ["var",])]
                                  categ.names=categ.names[-which(refx==categ.names)]
                                  for (i in 1:(as.numeric(categ["ncateg",which(p==categ["var",])])-1)){
                                       FP=c(FP, paste("FP_",chartr(".","_",categ.names[i]),sep=""))
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
                                    FP=c(FP, paste("FP_",chartr(".","_",p),"_",resp[j],sep=""))
                                }

                            }else{
                                if (is.na(categ["ref",which(p==categ["var",])])){
                                        categ.names=levels(indata[[p]])
                                        for (j in 1:nresp){
                                            for (i in 1:as.numeric(categ["ncateg",which(p==categ["var",])])){
                                                FP=c(FP,  paste("FP_",chartr(".","_",categ.names[i]),"_",resp.names[j],sep=""))
                                            }
                                        }
                                }else{
                                        categ.names=levels(indata[[p]])
                                        refx=categ["ref",which(p==categ["var",])]
                                        categ.names=categ.names[-which(refx==categ.names)]
                                        for (j in 1:nresp){
                                            for (i in 1:(as.numeric(categ["ncateg",which(p==categ["var",])])-1)){
                                                FP=c(FP, paste("FP_",chartr(".","_",categ.names[i]),"_",resp.names[j],sep=""))
                                            }
                                        }
                                }

                            }
                        }
                }
            }

        }else{
                expla=expl
                for (p in expla){
                        if (is.na(nonfp[1])||sum(p==nonfp)==0){
                            if (is.null(categ)|| sum(p==categ["var",])==0){
                                FP=c(FP, paste("FP_",chartr(".","_",p),sep=""))
                            }else{
                                if (is.na(categ["ref",which(p==categ["var",])])){
                                      categ.names=levels(indata[[p]])
                                      for (i in 1:as.numeric(categ["ncateg",which(p==categ["var",])])){
                                          FP=c(FP, paste("FP_",chartr(".","_",categ.names[i]),sep=""))
                                      }
                                }else{
                                      categ.names=levels(indata[[p]])
                                      refx=categ["ref",which(p==categ["var",])]
                                      categ.names=categ.names[-which(refx==categ.names)]
                                      for (i in 1:(as.numeric(categ["ncateg",which(p==categ["var",])])-1)){
                                           FP=c(FP, paste("FP_",chartr(".","_",categ.names[i]),sep=""))
                                      }
                                }
                            }
                        }
                }
            }
    }

    resid.names=function(rpx, resid.lev,RP){
        nrpx=length(rpx)
        for (j in 1: nrpx){
            for (i in 1:j){
                if (i==j){
                    RP=c(RP, paste("RP",resid.lev,"_var_",chartr(".", "_", rpx[i]),sep=""))
                }else{
                    RP=c(RP, paste("RP",resid.lev,"_cov_",chartr(".", "_", rpx[i]),"_",chartr(".", "_", rpx[j]),sep=""))
                }
            }
        }
        RP
    }

    resid2.names=function(rpx, resid.lev, clre,RP){
        nrpx=length(rpx)
        nclre=ncol(clre)
        k=1
        for (j in 1: nrpx){
            for (i in 1:j){
                if (i==j){
                    if (resid.lev==as.numeric(clre[1,k])&&rpx[i]==clre[2,k]&&rpx[i]==clre[3,k]){
                        if (k<ncol(clre)) k=k+1
                    }else{
                        RP=c(RP, paste("RP",resid.lev,"_var_",chartr(".", "_", rpx[i]),sep=""))
                    }
                }else{
                    if ((resid.lev==as.numeric(clre[1,k])&&rpx[i]==clre[2,k]&&rpx[j]==clre[3,k])||
                    (resid.lev==as.numeric(clre[1,k])&&rpx[j]==clre[2,k]&&rpx[i]==clre[3,k])){
                        if (k<ncol(clre)) k=k+1
                    }else{
                        RP=c(RP, paste("RP",resid.lev,"_cov_",chartr(".", "_", rpx[i]),"_",chartr(".", "_", rpx[j]),sep=""))
                    }
                }
            }
        }
        RP
    }



    RP =NULL
    if (nrp>0){
        for (ii in 1:nrp){
            if (is.null(clre)){
                RP=resid.names(rp[[ii]],as.numeric(sub("rp","",rp.names[ii])),RP)
            }else{
                 RP=resid2.names(rp[[ii]],as.numeric(sub("rp","",rp.names[ii])), clre,RP)
            }
        }
    }




    # Add in extra parameters ect.
#     if (D[1]=='Binomial'||D[1]=='Poisson'){
#         RP=c(RP, "RP1_bcons_1")
#     }
#     if (D[1]=='Multinomial'){
#         if (D["mode"]==0){
#             RP=c(RP, "RP2_-P/P","RP1_bcons_1")
#         }
#         if (D["mode"]==1){
#             RP=c(RP, "RP2_P*")
#         }
#     }
    assign("FP.names",FP,envir = parent.frame())#environment(runMLwiN))
    assign("RP.names",RP,envir = parent.frame())#environment(runMLwiN))


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

        version=as.numeric(BUGO["version"])
        if(D[1]=='Normal'||D[1]=='Multivariate Normal') DD2=0
        #if (!is.null(bugofile)) wrt(paste("BUGO ",version," ",DD," ",DD2," c997 ", "'",bugofile,"'",sep=""))
        wrt(paste("BUGO 6 ",DD," ",DD2, " c997 ","'",modelfile,"' ","'",initfile,"' ","'",datafile,"'",sep=""))
        wrt("ERAS   c997 c996")
    }
    #wrt(paste("STOR ",paste(tempfile("worksheet_"),".dta",sep="")))
    if (!debugmode) wrt("EXIT")
}
