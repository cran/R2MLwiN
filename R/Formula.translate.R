Formula.translate <-
function(Formula,levID, D='Normal',indata){

    regmatches <- function (x, m, invert = FALSE)
    {
        ##This function is available in the base package from R.2.14.0
        ##Includes this here for backwards compatiblility
        if (length(x) != length(m))
            stop(gettextf("%s and %s must have the same length",
                sQuote("x"), sQuote("m")), domain = NA)
        ili <- is.list(m)
        useBytes <- if (ili)
            any(unlist(lapply(m, attr, "useBytes")))
        else any(attr(m, "useBytes"))
        if (useBytes) {
            asc <- iconv(x, "latin1", "ASCII")
            ind <- is.na(asc) | (asc != x)
            if (any(ind))
                Encoding(x[ind]) <- "bytes"
        }
        if (!ili && !invert) {
            so <- m[ind <- (!is.na(m) & (m > -1L))]
            eo <- so + attr(m, "match.length")[ind] - 1L
            return(substring(x[ind], so, eo))
        }
        y <- if (invert) {
            Map(function(u, so, ml) {
                if ((n <- length(so)) == 1L) {
                    if (is.na(so))
                      return(character())
                    else if (so == -1L)
                      return(u)
                }
                beg <- if (n > 1L) {
                    eo <- so + ml - 1L
                    if (any(eo[-n] >= so[-1L]))
                      stop(gettextf("need non-overlapping matches for %s",
                        sQuote("invert = TRUE")), domain = NA)
                    c(1L, eo + 1L)
                }
                else {
                    c(1L, so + ml)
                }
                end <- c(so - 1L, nchar(u))
                substring(u, beg, end)
            }, x, m, if (ili)
                lapply(m, attr, "match.length")
            else attr(m, "match.length"), USE.NAMES = FALSE)
        }
        else {
            Map(function(u, so, ml) {
                if (length(so) == 1L) {
                    if (is.na(so) || (so == -1L))
                      return(character())
                }
                substring(u, so, so + ml - 1L)
            }, x, m, lapply(m, attr, "match.length"), USE.NAMES = FALSE)
        }
        names(y) <- names(x)
        y
    }


    if (D[1]=='Ordered Multinomial'||D[1]=='Unordered Multinomial'||D[1]=='Multivariate Normal'||D[1]=='Mixed'){

        nlev=length(levID)
        Formula=gsub('[[:space:]]','',Formula)
        cc=c(0:nlev)
        cflag=0
        if(sum(grepl("\\(+[[:digit:]]+[[:alpha:]]+\\|",Formula))>0) cflag=1
        if(cflag==0){
            for (i in cc){
                Formula=sub(paste(i,"\\|",sep=""),paste(i,"s\\|",sep=""),Formula)
            }
        }

        Formula=strsplit(Formula,"~")[[1]]
        resp=Formula[1]
        if (D[1]=='Multivariate Normal'){
            resp=sub("c\\(","",resp)
            resp=sub("\\)","",resp)
            resp=strsplit(resp,",")[[1]]
        }
        if (D[1]=="Unordered Multinomial"){
            D=rep(NA,5)
            resp=sub("log\\(","",resp)
            resp=sub("\\)","",resp)
            resp=strsplit(resp,",")[[1]]
            D[1]='Unordered Multinomial'
            names(D)[1]="distr"
            D[2]='logit'
            names(D)[2]="link"
            D[3]=resp[2]
            names(D)[3]="offset"
            D[4]=0
            names(D)[4]="mode"
            D[5]=which(resp[3]==levels(indata[[resp[1]]]))
            names(D)[5]="ref.cat"
            resp=resp[1]
        }
        if (D[1]=="Ordered Multinomial"){
            D=rep(NA,5)
            if ((grepl("logit",resp))){
                D[2]="logit"
                resp=sub("logit\\(","",resp)
                resp=sub("\\)","",resp)
            }

            if ((grepl("probit",resp))){
                D[2]="probit"
                resp=sub("probit\\(","",resp)
                resp=sub("\\)","",resp)
            }
            if ((grepl("cloglog",resp))){
                D[2]="cloglog"
                resp=sub("cloglog\\(","",resp)
                resp=sub("\\)","",resp)
            }
            resp=strsplit(resp,",")[[1]]
            D[1]='Ordered Multinomial'
            names(D)[1]="distr"
            names(D)[2]="link"
            D[3]=resp[2]
            names(D)[3]="offset"
            D[4]=1
            names(D)[4]="mode"
            D[5]=which(resp[3]==levels(indata[[resp[1]]]))
            names(D)[5]="ref.cat"
            resp=resp[1]
        }
        if (D[1]=="Mixed") D=as.list(D)
        if (D[[1]]=="Mixed"){
            resp=sub("c\\(","",resp)
            resp=sub("\\)","",resp)
            resp=strsplit(resp,",")[[1]]
            lenD=length(D)-1
            resp2=rep(NA,lenD)
            j=1
            for (i in 1:length(resp)){
                if(!(grepl('\\(',resp[i]))&&!(grepl('\\)',resp[i]))){
                    resp2[j]=resp[i]
                    j=j+1
                }else{
                    if(grepl('\\(',resp[i])){
                        ts=paste(resp[i],",",sep="")
                    }
                    if(grepl('\\)',resp[i])){
                        ts=paste(ts,resp[i],sep="")
                        resp2[j]=ts
                        j=j+1
                    }
                }

            }
            resp=resp2

            for (i in 1:length(resp)){
                respx=resp[i]
                if (D[[i+1]]=="Normal"){
                    resp[i]=respx
                }
                if (D[[i+1]]=="Binomial"){
                    D[[i+1]]=rep(NA,3)
                    if ((grepl("logit",respx))){
                        D[[i+1]][1]="Binomial"
                        D[[i+1]][2]="logit"
                        respx=sub("logit\\(","",respx)
                        respx=sub("\\)","",respx)
                    }
                    if ((grepl("logit",respx))){
                        D[[i+1]][1]="Binomial"
                        D[[i+1]][2]="logit"
                        respx=sub("logit\\(","",respx)
                        respx=sub("\\)","",respx)
                    }
                    if ((grepl("probit",respx))){
                        D[[i+1]][1]="Binomial"
                        D[[i+1]][2]="probit"
                        respx=sub("probit\\(","",respx)
                        respx=sub("\\)","",respx)
                    }
                    if ((grepl("cloglog",respx))){
                        D[[i+1]][1]="Binomial"
                        D[[i+1]][2]="cloglog"
                        respx=sub("cloglog\\(","",respx)
                        respx=sub("\\)","",respx)
                    }
                    respx=strsplit(respx,",")[[1]]
                    D[[i+1]][3]=respx[2]
                    resp[i]=respx[1]
                }
                if (D[[i+1]]=='Poisson'|| D[[i+1]]=='Negbinom'){
                        respx=sub("log\\(","",respx)
                        respx=sub("\\)","",respx)
                        respx=strsplit(respx,",")[[1]]
                        Distr=D[[i+1]]
                        D[[i+1]]=rep(NA,3)
                        if (length(respx)==2){
                            D[[i+1]][1]=Distr
                            D[[i+1]][2]=T
                            D[[i+1]][3]=respx[2]
                        }else{
                            D[[i+1]][1]=Distr
                            D[[i+1]][2]=F
                        }
                        resp[i]=respx[1]
                }
            }
        }
        left=Formula[2]
        left=unlist(strsplit(left,"\\+\\("))
        left=unlist(strsplit(left,"\\("))
        left=unlist(strsplit(left,"\\)"))
        nleft=length(left)

        categ=NULL
    	leftsplit <- strsplit(left, "(\\+)|(\\|)")
	    categstr <- unique(unlist(sapply(leftsplit, function(x){unlist(regmatches(x, gregexpr("([[:alnum:]]*(\\_|\\-|\\^|\\&)*[[:alnum:]])+(\\[+\\]|\\[+[[:print:]]+\\])", x)))})))

        Rversion = R.Version()
        if ((as.numeric(Rversion$major) >= 2) && (as.numeric(Rversion$minor) >= 14)) {
			left = sapply(regmatches(left, gregexpr("\\[[^]]*\\]", left), invert = TRUE),function(x)paste(x, collapse=""))
        }
        else {
            left = gsub("(\\[+\\]|\\[+[[:print:]]+\\])", "", left)
        }
        ncategstr=length(categstr)
        if (ncategstr>0){
            categ=matrix(,nrow=3,ncol=ncategstr)
            rownames(categ)=c("var","ref","ncateg")
            for (i in 1:ncategstr){
                cvx=unlist(strsplit(categstr[i],"\\["))
                categ[1,i]=cvx[1]
                cvy=sub("\\]","",cvx[2])
                if (cvy=="") cvy=NA
                categ[2,i]=cvy
                categ[3,i]=length(levels(indata[[categ[1,i]]]))
            }
        }


        fixs.no=grep("0s+\\|",left)
        fixs=left[fixs.no]
        fixc.no=grep("0c+\\|",left)
        fixc=left[fixc.no]
        if(length(fixc)!=0) {
            fixc=unlist(strsplit(fixc,"\\|"))
            fixc=unlist(strsplit(fixc[2],"\\+"))
            cidmat=matrix(,nrow=length(fixc),ncol=2)
            for (i in 1:length(fixc)){
                if(length(grep("\\{",fixc[i]))==0){
                    cidmat[i,1]=fixc[i]
                }else{
                    tt=unlist(strsplit(fixc[i],"\\{"))
                    cidmat[i,1]=tt[1]
                    cidmat[i,2]=sub("\\}","",tt[2])
                }
            }
            fixc= cidmat[,1]

        }else{
            cidmat=NULL
        }
        if(length(fixs)!=0) {
            fixs=unlist(strsplit(fixs,"\\|"))
            fixs=unlist(strsplit(fixs[2],"\\+"))
        }
        if(D[1]=='Ordered Multinomial'||D[1]=='Unordered Multinomial') nlev=nlev-1
        rands.no=rep(NA,nlev)
        randc.no=rep(NA,nlev)
        if(D[1]=='Ordered Multinomial'||D[1]=='Unordered Multinomial'){
            effect.lev=(nlev+1):2
        }else{
            effect.lev=nlev:1
        }
        for (i in 1:(nlev)){
            t1=grep(paste(effect.lev[i],"s+\\|",sep=""),left)
            if(length(t1)!=0) rands.no[i]=t1
            t2=grep(paste(effect.lev[i],"c+\\|",sep=""),left)
            if(length(t2)!=0) randc.no[i]=t2
        }
        randS=left[rands.no]
        randC=left[randc.no]
        rands=randc=list()

        if (nlev>0){
            for (i in 1:(nlev)){
                if(length(randS[i])!=0) {
                    rands[[i]]=unlist(strsplit(randS[[i]],"\\|"))
                    rands[[i]]=unlist(strsplit(rands[[i]][2],"\\+"))
                }
                if(length(randC[i])!=0) {
                    randc[[i]]=unlist(strsplit(randC[[i]],"\\|"))
                    randc[[i]]=unlist(strsplit(randc[[i]][2],"\\+"))
                    if (D[1]=='Ordered Multinomial'||D[1]=='Unordered Multinomial'){
                        nresp=length(unique(indata[[resp]]))
                    }else{
                        nresp=length(resp)
                    }
                    for(j in 1:length(randc[[i]])){
                        if(!is.na(randc[[i]][1])){
                            if(length(grep("\\{",randc[[i]][j]))==0){
                                cidmat=rbind(cidmat,c(randc[[i]][j],NA))
                            }else{
                                randcc=unlist(strsplit(randc[[i]][j],"\\{"))
                                randc[[i]][j]=randcc[1]
                                tempid=sub("\\}","",randcc[2])
                                #cidmat=rbind(cidmat,c(randc[[i]][j],gsub(",","",tempid)))
                                cidmat=rbind(cidmat,c(randc[[i]][j],tempid))
                            }
                        }
                    }
                }

            }
            randS=unique(na.omit(unlist(rands)))
            for (i in 1:length(randS)){
                if (sum(grepl("\\.",randS[i]))>0){
                    ttemp=unlist(strsplit(randS[i],"\\."))
                    ttemp=paste(ttemp[-length(ttemp)],collapse="")
                    if(ttemp%in%randS){
                        randS=randS[-i]
                    }
                }
            }
            if (length(fixs)==0){
                temps=randS
                nonfps=NULL
                if (D[1]=='Ordered Multinomial'||D[1]=='Unordered Multinomial'){
                    names.resp=levels(indata[[resp]])
                    names.resp=names.resp[-as.numeric(D["ref.cat"])]
                    refcatint=as.numeric(D["ref.cat"])
                    if (refcatint==1) usign=">" else usign="<"
                }else{
                    names.resp=resp
                }
                for (i in 1:length(temps)){
                    if(D[1]=='Ordered Multinomial'){
                        if(sum(grepl("\\.",temps[i]))==0){
                            nonfps=c(nonfps,sapply(names.resp,function(x) paste(temps[i],".(",usign,"=",x,")",sep="")))
                        }else{
                            ttemp=unlist(strsplit(temps[i],"\\."))
                            ttemp=ttemp[length(ttemp)]
                            if (ttemp%in%sapply(names.resp,function(x) paste(".(",usign,"=",x,")",sep=""))){
                                nonfps=c(nonfps,temps[i])
                            }else{
                                nonfps=c(nonfps,sapply(names.resp,function(x) paste(temps[i],".(",usign,"=",x,")",sep="")))
                            }
                        }
                    }else{
                        if(sum(grepl("\\.",temps[i]))==0){
                            nonfps=c(nonfps,sapply(names.resp,function(x) paste(temps[i],x,sep=".")))
                        }else{
                            ttemp=unlist(strsplit(temps[i],"\\."))
                            ttemp=ttemp[length(ttemp)]
                            if (ttemp%in%names.resp){
                                nonfps=c(nonfps,temps[i])
                            }else{
                                nonfps=c(nonfps,sapply(names.resp,function(x) paste(temps[i],x,sep=".")))
                            }
                        }
                    }
                }
                fixs=temps
            }else{
                temps=randS
                nonfps=NULL
                for(i in 1:length(temps)){
                    if(sum(grepl("\\.",temps[i]))>0){
                        ttemp0=unlist(strsplit(temps[i],"\\."))
                        ttemp0=paste(ttemp0[-length(ttemp0)],collapse="")
                        temps[i]=ttemp0
                    }
                }
                temps=randS[!(temps %in% fixs)]
                if (length(temps)!=0){
                    nonfps=NULL
                    if (D[1]=='Ordered Multinomial'||D[1]=='Unordered Multinomial'){
                        names.resp=levels(indata[[resp]])
                        names.resp=names.resp[-as.numeric(D["ref.cat"])]
                        refcatint=as.numeric(D["ref.cat"])
                        if (refcatint==1) usign=">" else usign="<"
                    }else{
                        names.resp=resp
                    }
                    for (i in 1:length(temps)){
                        if(D[1]=='Ordered Multinomial'){
                            if(sum(grepl("\\.",temps[i]))==0){
                                nonfps=c(nonfps,sapply(names.resp,function(x) paste(temps[i],".(",usign,"=",x,")",sep="")))
                            }else{
                                ttemp=unlist(strsplit(temps[i],"\\."))
                                ttemp=ttemp[length(ttemp)]
                                if (ttemp%in%sapply(names.resp,function(x) paste(".(",usign,"=",x,")",sep=""))){
                                    nonfps=c(nonfps,temps[i])
                                }else{
                                    nonfps=c(nonfps,sapply(names.resp,function(x) paste(temps[i],".(",usign,"=",x,")",sep="")))
                                }
                            }
                        }else{
                            if(sum(grepl("\\.",temps[i]))==0){
                                nonfps=c(nonfps,sapply(names.resp,function(x) paste(temps[i],x,sep=".")))
                            }else{
                                ttemp=unlist(strsplit(temps[i],"\\."))
                                ttemp=ttemp[length(ttemp)]
                                if (ttemp%in%names.resp){
                                    nonfps=c(nonfps,temps[i])
                                }else{
                                    nonfps=c(nonfps,sapply(names.resp,function(x) paste(temps[i],x,sep=".")))
                                }
                            }
                        }
                    }
                    fixs=c(fixs,temps)
                }else{
                    nonfps=character(0)
                }
            }


            randC=na.omit(unlist(randc))
            randCC=rep(NA,length(randC))

            if (length(randC)!=0){

                na.pos=which(is.na(cidmat[,2]))
                non.na.pos=which(!is.na(cidmat[,2]))
                for (i in na.pos){
                    if(cidmat[i,1]%in%cidmat[non.na.pos,1]){
                        cidmat[i,]=c(NA,NA)
                    }else{
                        tempid=1:nresp
                        if(D[1]=='Ordered Multinomial'||D[1]=='Unordered Multinomial'){
                            tempid=tempid[-as.numeric(D["ref.cat"])]
                        }
                        cidmat[i,2]=paste(tempid,collapse=",")

                    }
                }
                if(sum(is.na(cidmat[,1]))>0) cidmat=cidmat[-which(is.na(cidmat[,1])),]
                common.coeff=unique(paste(cidmat[,1],cidmat[,2],sep="@"))
                lencom=length(common.coeff)
                tt.id=unlist(strsplit(common.coeff,"\\@"))[(1:lencom)*2]
                tt.names=unlist(strsplit(common.coeff,"\\@"))[(1:lencom)*2-1]
                common.coeff=sub("\\@","\\.",common.coeff)
                if (D[1]=='Ordered Multinomial'||D[1]=='Unordered Multinomial') lencol=nresp-1 else lencol=nresp
                ccid.mat=matrix(0,nrow=length(tt.names),ncol=lencol)
                rownames(ccid.mat)=tt.names


                for (i in 1:length(tt.id)){
                    if (D[1]=='Ordered Multinomial'||D[1]=='Unordered Multinomial') refcatint=as.numeric(D["ref.cat"])
                    nonrefcatpos=as.numeric(unlist(strsplit(tt.id[i],",")))
                    if (D[1]=='Ordered Multinomial'||D[1]=='Unordered Multinomial') nonrefcatpos[which(nonrefcatpos>refcatint)]=nonrefcatpos[which(nonrefcatpos>refcatint)]-1
                    ccid.mat[i,nonrefcatpos]=1
                }

                randC=unique(na.omit(unlist(randc)))
                randCC=rep(NA,length(randC))
                for (i in 1:length(randC)){
                    randCC[i]= grep(randC[i],common.coeff)
                }
                randCC=common.coeff[randCC]
                randCC=gsub(",","",randCC)
                if (length(fixc)==0){
                    nonfpc= randCC
                }else{
                    nonfpc=randCC[!(randC%in%fixc)]
                }
                fixc=tt.names
            }else{
                ccid.mat=NULL
                nonfpc=NULL
            }
            lenfixc=length(fixc)

            if (lenfixc!=0&&length(randC)==0){
                if (D[1]=='Ordered Multinomial'||D[1]=='Unordered Multinomial') lencol=nresp-1 else lencol=nresp
                ccid.mat=matrix(0,nrow=lenfixc,ncol=lencol)
                rownames(ccid.mat)=fixc
                if (D[1]=='Ordered Multinomial'||D[1]=='Unordered Multinomial') refcatint=as.numeric(D["ref.cat"])

                for (i in 1:lenfixc){
                    if(is.na(cidmat[i,2])){
                        tempid=1:nresp
                        if(D[1]=='Ordered Multinomial'||D[1]=='Unordered Multinomial'){
                            tempid=tempid[-as.numeric(D["ref.cat"])]
                        }
                        cidmat[i,2]=paste(tempid,collapse=",")
                    }

                    nonrefcatpos=as.numeric(unlist(strsplit(cidmat[i,2],",")))
                    if (D[1]=='Ordered Multinomial'||D[1]=='Unordered Multinomial') nonrefcatpos[which(nonrefcatpos>refcatint)]=nonrefcatpos[which(nonrefcatpos>refcatint)]-1
                    ccid.mat[i,nonrefcatpos]=1
                }
            }


            rp=list()
            rp.names=NULL
            for (i in 1:length(rands)){
                if (!is.na(rands[[i]][1])){
                    rptemp=NULL
                    for (j in 1:length(rands[[i]])){
                        rp.name=paste("rp",effect.lev[i],sep="")
                        rp.names=c(rp.names,rp.name)
                        if (D[1]=='Ordered Multinomial'||D[1]=='Unordered Multinomial'){
                            names.resp=levels(indata[[resp]])
                            names.resp=names.resp[-as.numeric(D["ref.cat"])]
                            if (D[1]=='Ordered Multinomial'||D[1]=='Unordered Multinomial') refcatint=as.numeric(D["ref.cat"])
                        }else{
                            names.resp=resp
                        }
                        if(D[1]=='Ordered Multinomial'){
                                if (refcatint==1) usign=">" else usign="<"
                                if(sum(grepl("\\.",rands[[i]][j]))==0){
                                    rptemp=c(rptemp,sapply(names.resp,function(x) paste(rands[[i]][j],".(",usign,"=",x,")",sep="")))
                                }else{
                                    ttemp=unlist(strsplit(rands[[i]][j],"\\."))
                                    ttemp=ttemp[length(ttemp)]
                                    if (ttemp%in%sapply(names.resp,function(x) paste(".(",usign,"=",x,")",sep=""))){
                                       rptemp=c(rptemp,rands[[i]][j])
                                    }else{
                                        rptemp=c(rptemp,sapply(names.resp,function(x) paste(rands[[i]][j],".(",usign,"=",x,")",sep="")))
                                    }
                                }
                        }else{
                                if(sum(grepl("\\.",rands[[i]][j]))==0){
                                    rptemp=c(rptemp,sapply(names.resp,function(x) paste(rands[[i]][j],x,sep=".")))
                                }else{
                                    ttemp=unlist(strsplit(rands[[i]][j],"\\."))
                                    ttemp=ttemp[length(ttemp)]
                                    if (ttemp%in%names.resp){
                                        rptemp=c(rptemp,rands[[i]][j])
                                    }else{
                                        rptemp=c(rptemp,sapply(names.resp,function(x) paste(rands[[i]][j],x,sep=".")))
                                    }
                                }
                        }
                    }
                    rp[[rp.name]]=rptemp
                }
            }

            for (i in 1:length(randc)){
                if (!is.na(randc[[i]][1])){
                    rp.name=paste("rp",effect.lev[i],sep="")
                    if(!(rp.name %in% rp.names)){
                        rp.names=c(rp.names,rp.name)
                    }
                    rptemp=NULL
                    for (j in 1:length(randc[[i]])){
                        randcid=cidmat[grep(randc[[i]][j],cidmat[,1])[1],2]
                        rptemp=c(rptemp, paste(randc[[i]][j],gsub(",","",randcid),sep="."))
                    }
                    if(is.null(rp[[rp.name]])){
                        rp[[rp.name]]=rptemp
                    }else{
                        rptt=rp[[rp.name]]
                        rptemp=c(rptt,rptemp)
                        rp[[rp.name]]=rptemp
                    }
                }
            }

        }else{
            lenfixc=length(fixc)
            if (D[1]=='Ordered Multinomial'||D[1]=='Unordered Multinomial'){
                 nresp=length(unique(indata[[resp]]))
            }else{
                 nresp=length(resp)
            }
            if (lenfixc!=0){
                if (D[1]=='Ordered Multinomial'||D[1]=='Unordered Multinomial') lencol=nresp-1 else lencol=nresp
                ccid.mat=matrix(0,nrow=lenfixc,ncol=lencol)
                rownames(ccid.mat)=fixc
                if (D[1]=='Ordered Multinomial'||D[1]=='Unordered Multinomial') refcatint=as.numeric(D["ref.cat"])

                for (i in 1:lenfixc){
                    if(is.na(cidmat[i,2])){
                        tempid=1:nresp
                        if(D[1]=='Ordered Multinomial'||D[1]=='Unordered Multinomial'){
                            tempid=tempid[-as.numeric(D["ref.cat"])]
                        }
                        cidmat[i,2]=paste(tempid,collapse=",")
                    }

                    nonrefcatpos=as.numeric(unlist(strsplit(cidmat[i,2],",")))
                    if (D[1]=='Ordered Multinomial'||D[1]=='Unordered Multinomial') nonrefcatpos[which(nonrefcatpos>refcatint)]=nonrefcatpos[which(nonrefcatpos>refcatint)]-1
                    ccid.mat[i,nonrefcatpos]=1
                }
            }else{
                ccid.mat=NULL
            }
            rp=nonfps=nonfps=nonfpc=NULL
        }


        if(D[1]=='Ordered Multinomial'||D[1]=='Unordered Multinomial') D[1]='Multinomial'
        invars <-new.env()
        if(length(randC)==0&&length(fixc)==0){
            invars$resp=resp
            invars$expl=fixs
            if (length(rp)!=0) invars$rp=rp
            if (length(nonfps)!=0) invars$nonfp=nonfps
            if (!is.null(categ)) invars$categ=categ
            invars$D=D
        }else{
            invars$resp=resp
            if (length(fixs)!=0) invars$expl$sep.coeff=fixs else invars$expl$sep.coeff=NA
            if (length(fixc)!=0) invars$expl$common.coeff=fixc else invars$expl$common.coeff=NA
            if (length(rp)!=0) invars$rp=rp
            if (length(ccid.mat)!=0) invars$expl$common.coeff.id=ccid.mat
            if (length(nonfps)!=0) invars$nonfp$nonfp.sep=nonfps else invars$nonfp$nonfp.sep=NA
            if (length(nonfpc)!=0) invars$nonfp$nonfp.common=nonfpc else invars$nonfp$nonfp.common=NA
            if (!is.null(categ)) invars$categ=categ
            invars$D=D

        }
        invars=as.list(invars)

    }
    if (D[1]=='Binomial'){
        D=rep(NA,3)
        names(D)=c("Distr","link","offset")
        D[1]='Binomial'
        nlev=length(levID)
        Formula=gsub('[[:space:]]','',Formula)
        Formula=strsplit(Formula,"~")[[1]]
        resp=Formula[1]
        if ((grepl("logit",resp))){
            D[2]="logit"
            resp=sub("logit\\(","",resp)
            resp=sub("\\)","",resp)
        }
        if ((grepl("logit",resp))){
            D[2]="logit"
            resp=sub("logit\\(","",resp)
            resp=sub("\\)","",resp)
        }
        if ((grepl("probit",resp))){
            D[2]="probit"
            resp=sub("probit\\(","",resp)
            resp=sub("\\)","",resp)
        }
        if ((grepl("cloglog",resp))){
            D[2]="cloglog"
            resp=sub("cloglog\\(","",resp)
            resp=sub("\\)","",resp)
        }

        resp=strsplit(resp,",")[[1]]
        D[3]=resp[2]
        resp=resp[-2]

        left=Formula[2]
        left=unlist(strsplit(left,"\\+\\("))
        left=unlist(strsplit(left,"\\("))
        left=unlist(strsplit(left,"\\)"))
        nleft=length(left)

        categ=NULL
    	leftsplit <- strsplit(left, "(\\+)|(\\|)")
	    categstr <- unique(unlist(sapply(leftsplit, function(x){unlist(regmatches(x, gregexpr("([[:alnum:]]*(\\_|\\-|\\^|\\&)*[[:alnum:]])+(\\[+\\]|\\[+[[:print:]]+\\])", x)))})))

        Rversion = R.Version()
        if ((as.numeric(Rversion$major) >= 2) && (as.numeric(Rversion$minor) >= 14)) {
			left = sapply(regmatches(left, gregexpr("\\[[^]]*\\]", left), invert = TRUE),function(x)paste(x, collapse=""))
        }
        else {
            left = gsub("(\\[+\\]|\\[+[[:print:]]+\\])", "", left)
        }
        ncategstr=length(categstr)
        if (ncategstr>0){
            categ=matrix(,nrow=3,ncol=ncategstr)
            rownames(categ)=c("var","ref","ncateg")
            for (i in 1:ncategstr){
                cvx=unlist(strsplit(categstr[i],"\\["))
                categ[1,i]=cvx[1]
                cvy=sub("\\]","",cvx[2])
                if (cvy=="") cvy=NA
                categ[2,i]=cvy
                categ[3,i]=length(levels(indata[[categ[1,i]]]))
            }
        }

        fixs.no=grep("0+\\|",left)
        fixs=left[fixs.no]
        if(length(fixs)!=0) {
            fixs=unlist(strsplit(fixs,"\\|"))
            fixs=unlist(strsplit(fixs[2],"\\+"))
        }
        if(nlev>1){
            effect.lev=nlev:2
            rands.no=rep(NA,nlev-1)
            for (i in 1:(nlev-1)){
                t1=grep(paste(effect.lev[i],"+\\|",sep=""),left)
                if(length(t1)!=0) rands.no[i]=t1

            }
            randS=left[rands.no]
            rands=list()

            for (i in 1:(nlev-1)){
                if(length(randS[i])!=0) {
                    rands[[i]]=unlist(strsplit(randS[[i]],"\\|"))
                    rands[[i]]=unlist(strsplit(rands[[i]][2],"\\+"))
                }
            }
            randS=unique(na.omit(unlist(rands)))
            if (length(fixs)==0){
                nonfps=randS
                fixs=randS
            }else{
                temps=randS[!(randS %in% fixs)]
                if (length(temps)!=0){
                    nonfps=temps
                    fixs=c(fixs,temps)
                }else{
                    nonfps=character(0)
                }
            }

            rp=list()
            rp.names=NULL
            for (i in 1:length(rands)){
                if (!is.na(rands[[i]][1])){
                    rp.name=paste("rp",effect.lev[i],sep="")
                    rp.names=c(rp.names,rp.name)
                    rptemp=NULL

                    for (j in 1:length(rands[[i]])){
                        rptemp=c(rptemp,rands[[i]][j])
                    }
                    rp[[rp.name]]=rptemp
                }
            }
        }else{
          rp=nonfps=NULL
        }

        invars <-new.env()
        invars$resp=resp
        invars$expl=fixs
        if (length(rp)!=0) invars$rp=rp
        if (length(nonfps)!=0) invars$nonfp=nonfps
        invars$D=D
        if (!is.null(categ)) invars$categ=categ
        invars=as.list(invars)

    }

    if (D[1]=='Poisson'|| D[1]=='Negbinom'){
        nlev=length(levID)
        Formula=gsub('[[:space:]]','',Formula)
        Formula=strsplit(Formula,"~")[[1]]
        resp=Formula[1]
        resp=sub("log\\(","",resp)
        resp=sub("\\)","",resp)
        resp=strsplit(resp,",")[[1]]
        DD=D[1]

        if (length(resp)==2){
            D=rep(NA,3)
            D[1]=DD
            D[2]=T
            D[3]=resp[2]
            resp=resp[-2]
        }else{
            D=rep(NA,2)
            D[1]=DD
            D[2]=F
        }

        left=Formula[2]
        left=unlist(strsplit(left,"\\+\\("))
        left=unlist(strsplit(left,"\\("))
        left=unlist(strsplit(left,"\\)"))
        nleft=length(left)

        categ=NULL
    	leftsplit <- strsplit(left, "(\\+)|(\\|)")
	    categstr <- unique(unlist(sapply(leftsplit, function(x){unlist(regmatches(x, gregexpr("([[:alnum:]]*(\\_|\\-|\\^|\\&)*[[:alnum:]])+(\\[+\\]|\\[+[[:print:]]+\\])", x)))})))
        Rversion = R.Version()
        if ((as.numeric(Rversion$major) >= 2) && (as.numeric(Rversion$minor) >= 14)) {
			left = sapply(regmatches(left, gregexpr("\\[[^]]*\\]", left), invert = TRUE),function(x)paste(x, collapse=""))
        }
        else {
            left = gsub("(\\[+\\]|\\[+[[:print:]]+\\])", "", left)
        }
        ncategstr=length(categstr)
        if (ncategstr>0){
            categ=matrix(,nrow=3,ncol=ncategstr)
            rownames(categ)=c("var","ref","ncateg")
            for (i in 1:ncategstr){
                cvx=unlist(strsplit(categstr[i],"\\["))
                categ[1,i]=cvx[1]
                cvy=sub("\\]","",cvx[2])
                if (cvy=="") cvy=NA
                categ[2,i]=cvy
                categ[3,i]=length(levels(indata[[categ[1,i]]]))
            }
        }

        fixs.no=grep("0+\\|",left)
        fixs=left[fixs.no]
        if(length(fixs)!=0) {
            fixs=unlist(strsplit(fixs,"\\|"))
            fixs=unlist(strsplit(fixs[2],"\\+"))
        }
        if(nlev>1){
            effect.lev=nlev:2
            rands.no=rep(NA,nlev-1)
            for (i in 1:(nlev-1)){
                t1=grep(paste(effect.lev[i],"+\\|",sep=""),left)
                if(length(t1)!=0) rands.no[i]=t1

            }
            randS=left[rands.no]
            rands=list()

            for (i in 1:(nlev-1)){
                if(length(randS[i])!=0) {
                    rands[[i]]=unlist(strsplit(randS[[i]],"\\|"))
                    rands[[i]]=unlist(strsplit(rands[[i]][2],"\\+"))
                }
            }
            randS=unique(na.omit(unlist(rands)))
            if (length(fixs)==0){
                nonfps=randS
                fixs=randS
            }else{
                temps=randS[!(randS %in% fixs)]
                if (length(temps)!=0){
                    nonfps=temps
                    fixs=c(fixs,temps)
                }else{
                    nonfps=character(0)
                }
            }

            rp=list()
            rp.names=NULL
            for (i in 1:length(rands)){
                if (!is.na(rands[[i]][1])){
                    rp.name=paste("rp",effect.lev[i],sep="")
                    rp.names=c(rp.names,rp.name)
                    rptemp=NULL

                    for (j in 1:length(rands[[i]])){
                        rptemp=c(rptemp,rands[[i]][j])
                    }
                    rp[[rp.name]]=rptemp
                }
            }
        }else{
            rp=nonfps=NULL
        }

        invars <-new.env()
        invars$resp=resp
        invars$expl=fixs
        if (length(rp)!=0) invars$rp=rp
        if (length(nonfps)!=0) invars$nonfp=nonfps
        invars$D=D
        if (!is.null(categ)) invars$categ=categ
        invars=as.list(invars)
    }

    if (D[1]=='Normal'){
        nlev=length(levID)
        Formula=gsub('[[:space:]]','',Formula)
        Formula=strsplit(Formula,"~")[[1]]
        resp=Formula[1]


        left=Formula[2]
        left=unlist(strsplit(left,"\\+\\("))
        left=unlist(strsplit(left,"\\("))
        left=unlist(strsplit(left,"\\)"))
        nleft=length(left)

        categ=NULL
    	leftsplit <- strsplit(left, "(\\+)|(\\|)")
	    categstr <- unique(unlist(sapply(leftsplit, function(x){unlist(regmatches(x, gregexpr("([[:alnum:]]*(\\_|\\-|\\^|\\&)*[[:alnum:]])+(\\[+\\]|\\[+[[:print:]]+\\])", x)))})))
        Rversion = R.Version()
        if ((as.numeric(Rversion$major) >= 2) && (as.numeric(Rversion$minor) >= 14)) {
			left = sapply(regmatches(left, gregexpr("\\[[^]]*\\]", left), invert = TRUE),function(x)paste(x, collapse=""))
        }
        else {
            left = gsub("(\\[+\\]|\\[+[[:print:]]+\\])", "", left)
        }
        ncategstr=length(categstr)
        if (ncategstr>0){
            categ=matrix(,nrow=3,ncol=ncategstr)
            rownames(categ)=c("var","ref","ncateg")
            for (i in 1:ncategstr){
                cvx=unlist(strsplit(categstr[i],"\\["))
                categ[1,i]=cvx[1]
                cvy=sub("\\]","",cvx[2])
                if (cvy=="") cvy=NA
                categ[2,i]=cvy
                categ[3,i]=length(levels(indata[[categ[1,i]]]))
            }
        }

        fixs.no=grep("0+\\|",left)
        fixs=left[fixs.no]
        if(length(fixs)!=0) {
            fixs=unlist(strsplit(fixs,"\\|"))
            fixs=unlist(strsplit(fixs[2],"\\+"))
        }
        effect.lev=nlev:1
        rands.no=rep(NA,nlev)
        for (i in 1:nlev){
            t1=grep(paste(effect.lev[i],"+\\|",sep=""),left)
            if(length(t1)!=0) rands.no[i]=t1

        }
        randS=left[rands.no]
        rands=list()

        for (i in 1:nlev){
            if(length(randS[i])!=0) {
                rands[[i]]=unlist(strsplit(randS[[i]],"\\|"))
                rands[[i]]=unlist(strsplit(rands[[i]][2],"\\+"))
            }
        }
        randS=unique(na.omit(unlist(rands)))
        if (length(fixs)==0){
            nonfps=randS
            fixs=randS
        }else{
            temps=randS[!(randS %in% fixs)]
            if (length(temps)!=0){
                nonfps=temps
                fixs=c(fixs,temps)
            }else{
                nonfps=character(0)
            }
        }

        rp=list()
        rp.names=NULL
        for (i in 1:length(rands)){
            if (!is.na(rands[[i]][1])){
                rp.name=paste("rp",effect.lev[i],sep="")
                rp.names=c(rp.names,rp.name)
                rptemp=NULL

                for (j in 1:length(rands[[i]])){
                    rptemp=c(rptemp,rands[[i]][j])
                }
                rp[[rp.name]]=rptemp
            }
        }



        invars <-new.env()
        invars$resp=resp
        invars$expl=fixs
        if (length(rp)!=0) invars$rp=rp
        if (length(nonfps)!=0) invars$nonfp=nonfps
        invars$D=D
        if (!is.null(categ)) invars$categ=categ
        invars=as.list(invars)
    }
    return(invars)
}
