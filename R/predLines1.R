predLines1 <-
function(indata,FP,resi,xname, lev=2, levID, selected=NULL, Legend=T){
## This function is to draw predicted lines at higher levels (level>=2)
    categrv=as.factor(indata[[rev(levID)[lev]]])
    levels(categrv)=1:length(levels(categrv))
    categrv=as.integer(categrv)
    if (is.null(selected)){
        selected =unique(categrv)
    }

    if (is.character(resi)){
        PACKages<-as.character(as.data.frame(installed.packages())$Package)
        packs.req= "foreign"
        test<-( packs.req %in% PACKages)
        if (!all(test))
    	       install.packages(packs.req[!test],repos="http://cran.r-project.org")
        require(foreign)
        myresi=read.dta(resi)
    }else{
        myresi=resi
    }


    est.names=names(myresi)[grep(paste("lev_",lev,"_resi_est",sep=""),names(myresi))]
    if (length(est.names)==1){
        est0=na.omit(myresi[[est.names]])
        if (length(est0)==length(unique(categrv))){
            est=as.matrix(est0[categrv],ncol=1)
            colnames(est)=sub("_resi_est","",est.names)
        }else{
            stop("The number of groups do not match the number of residual estimates.")
        }
    }else{
        est0=NULL
        for (i in 1:length(est.names)){
            est0=cbind(est0,na.omit(myresi[[est.names[i]]]))
        }
        if (nrow(est0)==length(unique(categrv))){
            est=as.matrix(est0[categrv,])
            colnames(est)=sub("_resi_est","",est.names)
        }else{
            stop("The number of groups do not match the number of residual estimates.")
        }
    }


    rpx.names=sub(paste("lev_",lev,"_",sep=""),"",colnames(est))
    fp.names=sub("FP_","",names(FP))
    tval=0
    for (i in 1:length(fp.names)){
        if (is.factor(indata[[fp.names[i]]])){
            indata[[fp.names[i]]]=as.integer(indata[[fp.names[i]]])-1
        }
        tval=tval+as.numeric(indata[[fp.names[i]]])*FP[i]
    }
    for (i in 1:length(rpx.names)){
        if (is.factor(indata[[rpx.names[i]]])){
            indata[[rpx.names[i]]]=as.integer(indata[[rpx.names[i]]])-1
        }
        tval=tval+indata[[rpx.names[i]]]*est[,i]
    }


    pred.min=min(tval); pred.max=max(tval);pred.diff=pred.max-pred.min
    x=indata[[xname]]
    x.min=min(x); x.max=max(x);x.diff=x.max-x.min
    if(Legend) x.max=x.max+x.diff*.1

    plot(NA,xlim=c(x.min-x.diff*.05,x.max+x.diff*.05),ylim=c(pred.min-pred.diff*.05,pred.max+pred.diff*.05),ylab="ypred",xlab=xname)
    color=1
    for (i in selected){
        ypred=tval[which(categrv==i)]
        lines(sort(x[which(categrv==i)]),ypred[order(x[which(categrv==i)])],col=color)
        color=color+1
    }
    if(Legend) legend(x.max-x.diff*.1, pred.max, selected, col = 1:(color-1), lty=rep(1,color-1), cex = .8)
}
