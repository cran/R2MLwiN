prior2macro <-
function(prior,formula,levID,D, indata){
## translation from prior information to MLwiN macro

    #fixed effect
    nlev=length(levID)
    formula=gsub('[[:space:]]','',formula)
    cc=c(0:nlev)

    cflag=0

    if(sum(grepl("\\(+[[:digit:]]+[[:alpha:]]+\\|",formula))>0) cflag=1

    formula=unlist(strsplit(formula,"~"))

    resp=formula[1]
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
                    if ((grepl("logit",respx))){
                        respx=sub("logit\\(","",respx)
                        respx=sub("\\)","",respx)
                    }
                    if ((grepl("logit",respx))){
                        respx=sub("logit\\(","",respx)
                        respx=sub("\\)","",respx)
                    }
                    if ((grepl("probit",respx))){
                        respx=sub("probit\\(","",respx)
                        respx=sub("\\)","",respx)
                    }
                    if ((grepl("cloglog",respx))){
                        respx=sub("cloglog\\(","",respx)
                        respx=sub("\\)","",respx)
                    }
                    respx=strsplit(respx,",")[[1]]
                    resp[i]=respx[1]
                }
                if (D[[i+1]]=='Poisson'|| D[[i+1]]=='Negbinom'){
                        respx=sub("log\\(","",respx)
                        respx=sub("\\)","",respx)
                        respx=strsplit(respx,",")[[1]]
                        resp[i]=respx[1]
                }
            }
    }
    if (D[1]=='Ordered Multinomial'||D[1]=='Unordered Multinomial'){
        names.resp=levels(indata[[resp]])
        names.resp=names.resp[-as.numeric(D["ref.cat"])]
    }else{
        names.resp=resp
    }
    left=formula[2]
    left=unlist(strsplit(left,"\\+\\("))
    left=unlist(strsplit(left,"\\("))
    left=unlist(strsplit(left,"\\)"))

    categ=NULL
    categstr=unique(unlist(regmatches(left,gregexpr("[[:alnum:]]+\\[+[[:alnum:]]+\\]",left))))
    regmatches(left,gregexpr("\\[+[[:alnum:]]+\\]",left))<-""
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
    TT=NULL

    if(cflag==1){
        fixs.no=grep("0s+\\|",left)
        fixs=left[fixs.no]
        if(length(fixs)!=0) {
            fixs=unlist(strsplit(fixs,"\\|"))
            fixs=unlist(strsplit(fixs[2],"\\+"))
        }
        for (i in 1:length(fixs)){
            if (fixs[i]%in%categ[1,]){
                pos=which(fixs[i]==categ[1,])
                fixsa=fixs[1:i]
                fixsa=fixsa[-i]
                fixsb=fixs[i:length(fixs)]
                fixsb=fixsb[-1]
                if (is.na(categ[2,pos])){
                    fixs=c(fixsa,levels(indata[[categ[1,pos]]]),fixsb)
                }else{
                    refx=categ[2,pos]
                    categx=levels(indata[[categ[1,pos]]])
                    categx=categx[-which(refx==categx)]
                    fixs=c(fixsa,categx,fixsb)
                }
            }
        }
        fixS=NULL
        for (i in 1:length(names.resp)){
            fixS=c(fixS,paste(fixs,names.resp[i],sep="."))
        }
        fixs=fixS
        fixc.no=grep("0c+\\|",left)
        fixc=left[fixc.no]
        if(length(fixc)!=0) {
            fixc=unlist(strsplit(fixc,"\\|"))
            fixc=unlist(strsplit(fixc[2],"\\+"))
            fixcc=rep(NA,nrow=length(fixc))
            for (i in 1:length(fixc)){
                if(length(grep("\\{",fixc[i]))==0){
                    fixcc[i]=fixc[i]
                }else{
                    tt=unlist(strsplit(fixc[i],"\\{"))
                    fixcc=tt[1]
                }
            }
            fixc= fixcc
        }
        if (length(fixs)>0){
            fpps=prior$fixe.sep
            for (i in 1:length(fixs)){
                if (fixs[i]%in%names(fpps)){
                    tname=fixs[i]
                    TT=c(TT,1,fpps[[tname]][1],fpps[[tname]][2])
                }else{
                    TT=c(TT,0)
                }
            }
        }
        if (length(fixc)>0){
            fppc=prior$fixe.common
            for (i in 1:length(fixc)){
                if (fixc[i]%in%names(fppc)){
                    tname=fixc[i]
                    TT=c(TT,1,fpps[[tname]][1],fpps[[tname]][2])
                }else{
                    TT=c(TT,0)
                }
            }
        }
    }else{
        fixs.no=grep("0+\\|",left)
        fixs=left[fixs.no]
        if(length(fixs)!=0) {
            fixs=unlist(strsplit(fixs,"\\|"))
            fixs=unlist(strsplit(fixs[2],"\\+"))
        }
        for (i in 1:length(fixs)){
            if (fixs[i]%in%categ[1,]){
                pos=which(fixs[i]==categ[1,])
                fixsa=fixs[1:i]
                fixsa=fixsa[-i]
                fixsb=fixs[i:length(fixs)]
                fixsb=fixsb[-1]
                if (is.na(categ[2,pos])){
                    fixs=c(fixsa,levels(indata[[categ[1,pos]]]),fixsb)
                }else{
                    refx=categ[2,pos]
                    categx=levels(indata[[categ[1,pos]]])
                    categx=categx[-which(refx==categx)]
                    fixs=c(fixsa,categx,fixsb)
                }
            }
        }
        if (length(names.resp)>1){
            fixS=NULL
            for (i in 1:length(names.resp)){
                fixS=c(fixS,paste(fixs,names.resp[i],sep="."))
            }
            fixs=fixS
        }
        if (length(fixs)>0){
            fpps=prior$fixe
            for (i in 1:length(fixs)){
                if (fixs[i]%in%names(fpps)){
                    tname=fixs[i]
                    TT=c(TT,1,fpps[[tname]][1],fpps[[tname]][2])
                }else{
                    TT=c(TT,0)
                }
            }
        }
    }

    if (D[1]=="Normal"||D[1]=="Multivariate Normal"||D[1]=="Mixed"){
        efflev=nlev:1
    }else{
        if (nlev>1) efflev=nlev:2 else efflev=NULL
    }

    if (!is.null(efflev)){
        rp.names=paste("rp",efflev,sep="")
        for (i in 1:length(rp.names)){
            if (rp.names[i]%in%names(prior)){
                tname=rp.names[i]
                mat=prior[[tname]]$estimate
                mat[upper.tri(mat)]=mat[lower.tri(mat)]
                tt=c(as.vector(mat[upper.tri(mat, diag=T)]),prior[[tname]]$size)
                TT=c(TT,1,tt)
            }else{
                TT=c(TT,0)
            }
        }
    }
    if (!(D[1]=="Normal"||D[1]=="Multivariate Normal"||D[1]=="Mixed")) TT=c(TT,0)
    TT

}
