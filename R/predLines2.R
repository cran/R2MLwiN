predLines2 <-
function(indata, chains, resi.chains, xname, lev=2, levID, selected=NULL, probs=c(.025,.975), Legend=T){

## This function is to draw predicted lines (medians, lower quantiles and upper quantiles) at higher levels (level>=2)

    categrv=indata[[rev(levID)[lev]]]
    if (is.null(selected)){
        selected =unique(categrv)
    }

    if (is.character(resi.chains)){
        PACKages<-as.character(as.data.frame(installed.packages())$Package)
        packs.req= "foreign"
        test<-( packs.req %in% PACKages)
        if (!all(test))
    	       install.packages(packs.req[!test],repos="http://cran.r-project.org")
        require(foreign)
        resi.chains=read.dta(resi.chains)
    }

    rpx.names=sub(paste("RP",lev,"_var_",sep=""),"",colnames(chains)[grep(paste("RP",lev,"_var_",sep=""),colnames(chains))])
    lenrpx=length(rpx.names)
    lencateg=length(unique(categrv))

    resi.chains=matrix(resi.chains[[grep(paste("resi_lev",lev,sep=""),names(resi.chains))]],nrow=lenrpx*lencateg)
    FP.pos=grep("FP_",names(chains))
    fp.names=sub("FP_","",names(chains)[FP.pos])
    tval=0
    for (i in 1:length(fp.names)){
        if (is.factor(indata[[fp.names[i]]])){
            indata[[fp.names[i]]]=as.integer(indata[[fp.names[i]]])-1
        }
        tval=tval+indata[[fp.names[i]]]%o%chains[[FP.pos[i]]]
    }


  	t2val=matrix(0,nrow(tval),ncol(tval))
  	for (j in 1:ncol(resi.chains)){
          for (i in 1:length(rpx.names)){
              loc=(1:lencateg)*lenrpx-(lenrpx-i)
              resix=resi.chains[loc,j]
              if (is.factor(indata[[rpx.names[i]]])){
                indata[[rpx.names[i]]]=as.integer(indata[[rpx.names[i]]])-1
              }
  	    	  t2val[,j]=t2val[,j]+indata[[rpx.names[i]]]*resix[categrv]

  	    }
  	}
  	tval=tval+t2val

    tval.med=apply(tval,1, median)
    tval.low=apply(tval,1,function(x) quantile(x,probs[1]))
    tval.up=apply(tval,1,function(x) quantile(x,probs[2]))
    pred.min=min(tval.low); pred.max=max(tval.up);pred.diff=pred.max-pred.min
    x=indata[[xname]]
    x.min=min(x); x.max=max(x);x.diff=x.max-x.min
    if(Legend) x.max=x.max+x.diff*.1

    plot(NA,xlim=c(x.min-x.diff*.05,x.max+x.diff*.05),ylim=c(pred.min-pred.diff*.05,pred.max+pred.diff*.05),ylab="ypred",xlab=xname)
    color=1
    for (i in selected){
        ypred=tval.med[which(categrv==i)]
        ypred.low=tval.low[which(categrv==i)]
        ypred.up=tval.up[which(categrv==i)]
        lines(sort(x[which(categrv==i)]),ypred[order(x[which(categrv==i)])],col=color)
        lines(sort(x[which(categrv==i)]),ypred.low[order(x[which(categrv==i)])],col=color,lty=3)
        lines(sort(x[which(categrv==i)]),ypred.up[order(x[which(categrv==i)])],col=color,lty=3)
        color=color+1
    }
    if(Legend) legend(x.max-x.diff*.1, pred.max, selected, col = 1:(color-1), lty=rep(1,color-1), cex = .8)
}
