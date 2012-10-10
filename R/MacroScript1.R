MacroScript1 <-
function(indata,dtafile,resp, levID, expl, rp, D='Normal', nonlinear=c(0,1), categ=NULL,notation=NULL, nonfp=NA, clre,smat, Meth=1,
BUGO=NULL,mem.init="default",weighting=NULL,bugofile=bugofile,modelfile=modelfile,initfile=initfile,datafile=datafile,macrofile=macrofile,IGLSfile=IGLSfile,resifile=resifile,resi.store=resi.store,debugmode=debugmode){

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

    if (resi.store&& nrp>0){
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
            wrt("MISR   0")
            wrt("RESI")
            wrt("MISR   1")
            tr.all=c(tr.all,tr)
            tempcell=tempcell+len.rpx+1
        }
        wrt(paste("PSTA '",resifile, "' ", paste(tr.all,collapse=" "),sep=""))
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
                        k=k+1
                    }else{
                        RP=c(RP, paste("RP",resid.lev,"_var_",chartr(".", "_", rpx[i]),sep=""))
                    }
                }else{
                    if (resid.lev==as.numeric(clre[1,k])&&rpx[i]==clre[2,k]&&rpx[j]==clre[3,k]){
                        k=k+1
                    }else{
                        RP=c(RP, paste("RP",resid.lev,"_cov_",chartr(".", "_", rpx[i]),"_",chartr(".", "_", rpx[j]),sep=""))
                    }
                }
            }
        }
        RP
    }


    if (D[1]=='Mixed'){
            if(sum(as.numeric(sub("rp","",rp.names))==2)>0){
                rp[["rp2"]]=c("bcons",rp[["rp2"]])
            }else{
                rp.names=c(rp.names,"rp2")
                rp=c(rp,rp2=c("bcons"))
            }
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
    if (D[1]=='Binomial'||D[1]=='Poisson'){
        RP=c(RP, "RP1_bcons_1")
    }
    if (D[1]=='Multinomial'){
        if (D["mode"]==0){
            RP=c(RP, "RP2_-P/P","RP1_bcons_1")
        }
        if (D["mode"]==1){
            RP=c(RP, "RP2_P*")
        }
    }


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

        version=as.numeric(BUGO[1])
        if(D[1]=='Normal'||D[1]=='Multivariate Normal') DD2=0
        if (!is.null(bugofile)) wrt(paste("BUGO ",version," ",DD," ",DD2," c997 ", "'",bugofile,"'",sep=""))
        wrt(paste("BUGO 6 ",DD," ",DD2, " c997 ","'",modelfile,"' ","'",initfile,"' ","'",datafile,"'",sep=""))
        wrt("ERAS   c997 c996")
    }
    #wrt(paste("STOR ",paste(tempfile("worksheet_"),".dta",sep="")))
    if (!debugmode) wrt("EXIT")
}
