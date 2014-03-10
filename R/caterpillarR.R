caterpillarR <-
function(resi, lev=2){
    ##produce caterpillar plots for the random effects(level>=2)
    ##using qqmath() in the lme4 package
    ##Only work with full covariance specified

    PACKages<-as.character(as.data.frame(installed.packages())$Package)
    packs.req= "lme4"
    test<-( packs.req %in% PACKages)
    if (!all(test))
	       install.packages(packs.req[!test],repos="http://cran.r-project.org")
    require(lme4)
    if (is.character(resi)) myresi=read.dta(resi) else myresi=resi
    est.names=names(myresi)[grep(paste("lev_",lev,"_resi_est",sep=""),names(myresi))]
    if (length(est.names)==1){
        est=as.matrix(na.omit(myresi[[est.names]]),ncol=1)
        colnames(est)=sub("_resi_est","",est.names)
        var=na.omit(myresi[,grep(paste("lev_",lev,"_resi_(var|variance)_",sep=""),names(myresi))])
        d1=length(est)
        tt=array(,c(1,1,d1))
        tt[1,1,]=var
    }else{
        est=NULL
        for (i in 1:length(est.names)){
            est=cbind(est,na.omit(myresi[[est.names[i]]]))
        }
        colnames(est)=sub("_resi_est","",est.names)
        tempnames=sub(paste("lev_",lev,"_resi_est_",sep=""),"",est.names)
        d1=dim(est)[1]
        d2=dim(est)[2]
#        cov.vec=na.omit(myresi[,grep(paste("lev_",lev,"_resi_cov+$",sep=""),names(myresi))])
        m=(d2*(d2+1))/2
#        cov.lower=matrix(cov.vec,m,d1)
        cov.lower=matrix(NA,m,d1)
        ccount =1
        for (i in 1:length(est.names)){
            for (j in 1:i){
                if (i==j){
                    tmatch=grep(paste("lev_",lev,"_resi_(var|variance)_",tempnames[i],sep=""),names(myresi))
                    if (length(tmatch)!=0){
                        cov.lower[ccount,]=na.omit(myresi[,tmatch])
                    }else{
                        cov.lower[ccount,]=rep(0,d1)
                    }
                }else{
                    tmatch=grep(paste("lev_",lev,"_resi_cov_",tempnames[i],"_",tempnames[j],sep=""),names(myresi))
                    if (length(tmatch)!=0){
                        cov.lower[ccount,]=na.omit(myresi[,tmatch])
                    }else{
                        cov.lower[ccount,]=rep(0,d1)
                    }
                }
                ccount=ccount+1
            }
        }
        tt=array(,c(d2,d2,d1))
        for (x in 1:d1){
            tt[,,x][upper.tri(tt[,,x],diag=T)]=cov.lower[,x]
            tt[,,x][lower.tri(tt[,,x],diag=F)]=t(tt[,,x])[lower.tri(tt[,,x],diag=F)]
        }
    }

    rr=NULL
    rr$Subject=data.frame(est)
    attr(rr$Subject, "postVar")=tt
    class(rr)<- "ranef.mer"
    qqmath(rr)

}
