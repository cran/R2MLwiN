predLines3 <-
function(indata,FP,xname, group=NULL, Legend=T){
## This function is to draw predicted lines using fixed part estimates

    if (!is.null(group)){
        if(is.character(group)) group=indata[[group]]
        if(!is.factor(group)) group=as.factor(group)
    }


    fp.names=sub("FP_","",names(FP))
    tval=0
    for (i in 1:length(fp.names)){
        if (is.factor(indata[[fp.names[i]]])){
            indata[[fp.names[i]]]=as.integer(indata[[fp.names[i]]])-1
        }
        tval=tval+as.numeric(indata[[fp.names[i]]])*FP[i]
    }

    pred.min=min(tval); pred.max=max(tval);pred.diff=pred.max-pred.min
    x=indata[[xname]]
    x.min=min(x); x.max=max(x);x.diff=x.max-x.min
    if(Legend) x.max=x.max+x.diff*.2

    if (!is.null(group)){
        levs=levels(group); nlev=length(levs)
        plot(NA,xlim=c(x.min-x.diff*.05,x.max+x.diff*.2),ylim=c(pred.min-pred.diff*.05,pred.max+pred.diff*.05),ylab="ypred",xlab=xname)
        color=1
        for(i in 1:nlev){
            ypred=tval[group==levs[i]]
            lines(sort(x[group==levs[i]]),ypred[order(x[group==levs[i]])],col=color)
            color=color+1
        }
        if(Legend) legend(x.max-x.diff*.1, pred.max, levs, col = 1:(color-1), lty=rep(1,color-1), cex = .8)
    }else{
        plot(x=sort(x),y=tval[order(x)],xlim=c(x.min-x.diff*.05,x.max+x.diff*.05),ylim=c(pred.min-pred.diff*.05,pred.max+pred.diff*.05),ylab="ypred",xlab=xname,type="l")
    }
}
