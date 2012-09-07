trajectories = function(chains,Range=c(1,5000),selected=NULL){
    #This function draws trajectories of the chains for each parameter estimate

    if (!is.data.frame(chains)) chains=as.data.frame(chains)

    if(is.null(selected)){
        cname=names(chains)
        if ("iteration" %in% cname){
            chains=chains[,-which(cname=="iteration")]
        }
        cname=names(chains)
        nchains=length(cname)
    }else{
        cname=selected
        if ("iteration" %in% cname) cname=cname[-which(cname=="iteration")]
        nchains=length(cname)
    }
    if(nchains==1) opar=par(mfrow=c(1,1))
    if(nchains==2) opar=par(mfrow=c(2,1))
    if(nchains==3) opar=par(mfrow=c(3,1))
    if(nchains==4) opar=par(mfrow=c(2,2))
    if(nchains>4) opar=par(mfrow=c(3,2))
    if(nchains>6) opar=par(mfrow=c(3,3))	


    nwindows=0
    interval=Range[1]:Range[2]
    for (i in 1:nchains){
        plot(interval,chains[interval,cname[i]],xlab="iteration",
        ylab=cname[i],type="l")
        nwindows=nwindows+1
        if ((nwindows%%9)==0) {windows(); opar=par(mfrow=c(3,3))}
    }
    on.exit(par(opar))

}
