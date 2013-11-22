        setClass(Class = "mlwinfitMCMC", representation = representation(Nobs="numeric",DataLength="numeric",burnin="numeric",iterations="numeric",
        D="ANY", Formula="ANY", levID="character", merr="ANY", fact="ANY", xclass="ANY", estMCMC ="data.frame",
        FP="numeric", RP="numeric", RP.cov="matrix", FP.cov="matrix", chains="ANY",
        elapsed.time="numeric", call="ANY",BDIC="numeric",LIKE="ANY",fact.loadings="numeric",
        fact.cov="numeric",fact.chains="ANY",MIdata="data.frame",residual="data.frame",resi.chains="ANY"))


        setMethod(
            f= "[",
            signature="mlwinfitMCMC",
            definition=function(x,i,j,drop){
                if(i=="Nobs"){return(x@Nobs)}else {}
                if(i=="DataLength"){return(x@DataLength)}else {}                
                if(i=="burnin"){return(x@burnin)}else {}
                if(i=="iterations"){return(x@iterations)}else {}
                if(i=="D"){return(x@D)}else {}
                if(i=="Formula"){return(x@Formula)}else {}
                if(i=="levID"){return(x@levID)}else {}
                if(i=="merr"){return(x@merr)}else {}
                if(i=="fact"){return(x@fact)}else {}
                if(i=="xclass"){return(x@xclass)}else {}
                if(i=="estMCMC"){return(x@estMCMC)}else {}
                if(i=="FP"){return(x@FP)}else {}
                if(i=="RP"){return(x@RP)}else {}
                if(i=="FP.cov"){return(x@FP.cov)}else {}
                if(i=="RP.cov"){return(x@RP.cov)}else {}
                if(i=="chains"){return(x@chains)}else {}
                if(i=="elapsed.time"){return(x@elapsed.time)}else {}
                if(i=="BDIC"){return(x@BDIC)}else {}
                if(i=="call"){return(x@call)}else {}
                if(i=="LIKE"){return(x@LIKE)}else {}
#                if(i=="chains.bugs"){return(x@chains.bugs)}else {}
                if(i=="fact.loadings"){return(x@fact.loadings)}else {}
                if(i=="fact.cov"){return(x@fact.cov)}else {}
                if(i=="fact.chains"){return(x@fact.chains)}else {}
                if(i=="MIdata"){return(x@MIdata)}else {}
                if(i=="residual"){return(x@residual)}else {}
                if(i=="resi.chains"){return(x@resi.chains)}else {}

            }
        )

        setReplaceMethod(
            f= "[",
            signature="mlwinfitMCMC",
            definition=function(x,i,j,value){
                if(i=="Nobs"){x@Nobs<-value}else {}
                if(i=="DataLength"){x@DataLength<-value}else {}                                           
                if(i=="burnin"){x@burnin<-value}else {}
                if(i=="iterations"){x@iterations<-value}else {}
                if(i=="D"){x@D<-value}else {}
                if(i=="Formula"){x@Formula<-value}else {}
                if(i=="levID"){x@levID<-value}else {}
                if(i=="merr"){x@merr<-value}else {}
                if(i=="fact"){x@fact<-value}else {}
                if(i=="xclass"){x@xclass<-value}else {}
                if(i=="estMCMC"){x@estMCMC<-value}else {}
                if(i=="FP"){x@FP<-value}else {}
                if(i=="RP"){x@RP<-value}else {}
                if(i=="FP.cov"){x@FP.cov<-value}else {}
                if(i=="RP.cov"){x@RP.cov<-value}else {}
                if(i=="chains"){x@chains<-value}else {}
                if(i=="elapsed.time"){x@elapsed.time<-value}else {}
                if(i=="BDIC"){x@BDIC<-value}else {}
                if(i=="call"){x@call<-value}else {}
                if(i=="LIKE"){x@LIKE<-value}else {}
#                if(i=="chains.bugs"){x@chains.bugs<-value}else {}
                if(i=="fact.loadings"){x@fact.loadings<-value}else {}
                if(i=="fact.cov"){x@fact.cov<-value}else {}
                if(i=="fact.chains"){x@fact.chains<-value}else {}
                if(i=="MIdata"){x@MIdata<-value}else {}
                if(i=="residual"){x@residual<-value}else {}
                if(i=="resi.chains"){x@resi.chains<-value}else {}
                validObject(x)
                return (x)
            }
        )

        setMethod(
            f= "[[",
            signature="mlwinfitMCMC",
            definition=function(x,i,j,drop){
                if(i=="Nobs"){return(x@Nobs)}else {}
                if(i=="DataLength"){return(x@DataLength)}else {}
                if(i=="burnin"){return(x@burnin)}else {}
                if(i=="iterations"){return(x@iterations)}else {}
                if(i=="D"){return(x@D)}else {}
                if(i=="Formula"){return(x@Formula)}else {}
                if(i=="levID"){return(x@levID)}else {}
                if(i=="merr"){return(x@merr)}else {}
                if(i=="fact"){return(x@fact)}else {}
                if(i=="xclass"){return(x@xclass)}else {}
                if(i=="estMCMC"){return(x@estMCMC)}else {}
                if(i=="FP"){return(x@FP)}else {}
                if(i=="RP"){return(x@RP)}else {}
                if(i=="FP.cov"){return(x@FP.cov)}else {}
                if(i=="RP.cov"){return(x@RP.cov)}else {}
                if(i=="chains"){return(x@chains)}else {}
                if(i=="elapsed.time"){return(x@elapsed.time)}else {}
                if(i=="BDIC"){return(x@BDIC)}else {}
                if(i=="call"){return(x@call)}else {}
                if(i=="LIKE"){return(x@LIKE)}else {}
#                if(i=="chains.bugs"){return(x@chains.bugs)}else {}
                if(i=="fact.loadings"){return(x@fact.loadings)}else {}
                if(i=="fact.cov"){return(x@fact.cov)}else {}
                if(i=="fact.chains"){return(x@fact.chains)}else {}
                if(i=="MIdata"){return(x@MIdata)}else {}
                if(i=="residual"){return(x@residual)}else {}
                if(i=="resi.chains"){return(x@resi.chains)}else {}

            }
        )

        setReplaceMethod(
            f= "[[",
            signature="mlwinfitMCMC",
            definition=function(x,i,j,value){
                if(i=="Nobs"){x@Nobs<-value}else {}
                if(i=="DataLength"){x@DataLength<-value}else {}
                if(i=="burnin"){x@burnin<-value}else {}
                if(i=="iterations"){x@iterations<-value}else {}
                if(i=="D"){x@D<-value}else {}
                if(i=="Formula"){x@Formula<-value}else {}
                if(i=="levID"){x@levID<-value}else {}
                if(i=="merr"){x@merr<-value}else {}
                if(i=="fact"){x@fact<-value}else {}
                if(i=="xclass"){x@xclass<-value}else {}
                if(i=="estMCMC"){x@estMCMC<-value}else {}
                if(i=="FP"){x@FP<-value}else {}
                if(i=="RP"){x@RP<-value}else {}
                if(i=="FP.cov"){x@FP.cov<-value}else {}
                if(i=="RP.cov"){x@RP.cov<-value}else {}
                if(i=="chains"){x@chains<-value}else {}
                if(i=="elapsed.time"){x@elapsed.time<-value}else {}
                if(i=="BDIC"){x@BDIC<-value}else {}
                if(i=="call"){x@call<-value}else {}
                if(i=="LIKE"){x@LIKE<-value}else {}
#                if(i=="chains.bugs"){x@chains.bugs<-value}else {}
                if(i=="fact.loadings"){x@fact.loadings<-value}else {}
                if(i=="fact.cov"){x@fact.cov<-value}else {}
                if(i=="fact.chains"){x@fact.chains<-value}else {}
                if(i=="MIdata"){x@MIdata<-value}else {}
                if(i=="residual"){x@residual<-value}else {}
                if(i=="resi.chains"){x@resi.chains<-value}else {}
                validObject(x)
                return (x)
            }
        )
        setMethod("summary",
            signature(object = "mlwinfitMCMC"),                  
            function (object,  ...)
            {
              object
            }
        )
        printMCMC <- function(x, digits = max(3, getOption("digits") - 2), signif.stars = getOption("show.signif.stars"), z.ratio = T,...)
            {

                object <- summary(x)
                align2right=function(titlename,ele){
                    #for printing the table on the screen
                    all.ele=c(titlename,ele)
                    len.all.ele=nchar(all.ele)
                    max.len.ele=max(len.all.ele)
                    for (j in 1:length(all.ele)){
                        if (len.all.ele[j]<max.len.ele){
                            len.diff=max.len.ele-len.all.ele[j]
                            all.ele[j]=paste(paste(rep(" ",len.diff),collapse=""),all.ele[j],sep="")
                        }
                    }

                    all.ele
                }

                align2left=function(titlename,ele){
                    #for printing the table on the screen
                    all.ele=c(titlename,ele)
                    len.all.ele=nchar(all.ele)
                    max.len.ele=max(len.all.ele)
                    for (j in 1:length(all.ele)){
                        if (len.all.ele[j]<max.len.ele){
                            len.diff=max.len.ele-len.all.ele[j]
                            all.ele[j]=paste(all.ele[j],paste(rep(" ",len.diff),collapse=""),sep="")
                        }
                    }

                    all.ele
                }

                signifstar = function(pval){
                    starstr="Error"
                    if (pval>=0&&pval<=1){
                        if(pval<0.001){
                            starstr='***'
                        }
                        if(pval>=0.001&&pval<0.01){
                            starstr='** '
                        }
                        if(pval>=0.01&&pval<0.05){
                            starstr='*  '
                        }
                        if(pval>=0.05&&pval<0.1){
                            starstr='.  '
                        }
                        if(pval>=0.1){
                            starstr='   '
                        }
                    }
                    starstr
                }

                FP.names=names(object@FP)
                RP.names=names(object@RP)
#                Chains=mcmc(data=object@chains[,-c(1,2)])
                ESS=effectiveSize(object@chains)
                levID0=object@levID
                cat("\n")
                cat(paste(rep("-",50),collapse="*"),"\n")
                cat("MLwiN multilevel model",paste("(",object@D[1],")",sep=""),"\n")
                if (is.null(object@xclass)){
                    cat("Estimation algorithm:  MCMC      Elapsed time :",paste(round(object@elapsed.time,2),"s",sep=""), "\n")
                }else{
                    cat("Estimation algorithm:  MCMC      Cross-classified              Elapsed time :",paste(round(object@elapsed.time,2),"s",sep=""), "\n")
                }
                cat("Number of obs: ",object@Nobs,"(from total",object@DataLength,")        Number of iter.:",object@iterations,"         Burn-in:", object@burnin , "\n")


                if (!(object@D[1]=="Mixed")&&is.null(object@merr)&&is.null(object@fact)){
                    object@BDIC=na.omit(object@estMCMC[,dim(object@estMCMC)[2]])[c(3,4,2,1)]
                    BDIC.names=c("Dbar", "D(thetabar)",  "pD", "DIC")
                    names(object@BDIC)=BDIC.names
                    cat("Bayesian Deviance Information Criterion (DIC)\n")
                    cat("Dbar      D(thetabar)    pD      DIC\n")
                    cat(formatC(object@BDIC,format="f",digits=3,width=-10),"\n")
                }else{
                    cat(paste("Deviance statistic: ", round(object@LIKE,1)),"\n")
                }


                cat(paste(rep("-",50),collapse="-"),"\n")
                cat("The model formula:\n")
                print(object@Formula)#cat(gsub("[[:space:]]","",object@Formula),"\n")
                levID.display=""
                if (is.na(levID0[length(levID0)])){
                    levID0=levID0[-length(levID0)]
                }
                for (i in 1:length(levID0)){
                    levID.display=paste(levID.display,"Level ",length(levID0)+1-i,": ",levID0[i],"     ",sep="")
                }
                cat(levID.display,"\n")
                cat(paste(rep("-",50),collapse="-"),"\n")

                if (!is.null(object@fact)&&object@D[1]=='Multivariate Normal'){
                    loadings=na.omit(object@estMCMC[,5])
                    load.names=rep(NA,length(loadings))

                    Formula1=strsplit(object@Formula,"~")[[1]]
                    resp=Formula1[1]
                    resp=sub("c\\(","",resp)
                    resp=sub("\\)","",resp)
                    resp=strsplit(resp,",")[[1]]
        		
                    k=1
                    for (i in 1:object@fact$nfact){
                        for (j in resp){
                            load.names[k]=paste("load",i,"_",j,sep="")
                            k=k+1
                        }
                    }
                    loadings.sd=na.omit(object@estMCMC[,6])
                    qt025=loadings-qnorm(.975)*loadings.sd
                    qt975=loadings+qnorm(.975)*loadings.sd
                    loads=rbind(loadings,loadings.sd,qt025,qt975)
                    colnames(loads)=load.names

                    for (j in 1:object@fact$nfact){
                        cat("The estimates of factor",j,"loadings:\n")
                        loadx.names=load.names[grep(paste("load+",j,"+\\_",sep=""),load.names)]
                        loadx=loads[,loadx.names]
                        printcol0=align2left("        ",loadx.names)
                        printcol1=align2right("Coef.",format(round(loadx[1,],digits),nsmall = digits))
                        printcol2=align2right("Std. Err.",format(round(loadx[2,],digits),nsmall = digits))
                        printcol3=align2right("[95% Conf.",format(round(loadx[3,],digits),nsmall = digits))
                        printcol4=align2right("Interval]",format(round(loadx[4,],digits),nsmall = digits))
                        for (i in 1:(ncol(loadx)+1)){
                            cat(printcol0[i]," ",printcol1[i]," ",printcol2[i]," ",printcol3[i]," ",printcol4[i],"\n")
                        }
                        cat(paste(rep("-",50),collapse="-"),"\n")
                    }

                    object@fact.cov=fact.cov.names=na.omit(object@estMCMC[,7])
                    fact.cov.sd=na.omit(object@estMCMC[,8])
                    k=1
                    for (i in 1:object@fact$nfact){
                        for(j in 1:i){
                            if(i==j){
                                fact.cov.names[k]=paste("var_fact",i,sep="")
                            }else{
                                fact.cov.names[k]=paste("cov_fact",i,"_fact",j,sep="")
                            }
                            k=k+1
                        }
                    }
                    qt025=object@fact.cov-qnorm(.975)*fact.cov.sd
                    qt975=object@fact.cov+qnorm(.975)*fact.cov.sd
                    fcov=rbind(object@fact.cov,fact.cov.sd,qt025,qt975)
                    colnames(fcov)=fact.cov.names

                    cat("The estimates of factor covariances:\n")
                    printcol0=align2left("        ",fact.cov.names)
                    printcol1=align2right("Coef.",format(round(fcov[1,],digits),nsmall = digits))
                    printcol2=align2right("Std. Err.",format(round(fcov[2,],digits),nsmall = digits))
                    printcol3=align2right("[95% Conf.",format(round(fcov[3,],digits),nsmall = digits))
                    printcol4=align2right("Interval]",format(round(fcov[4,],digits),nsmall = digits))
                    for (i in 1:(ncol(fcov)+1)){
                        cat(printcol0[i]," ",printcol1[i]," ",printcol2[i]," ",printcol3[i]," ",printcol4[i],"\n")
                    }
                    cat(paste(rep("-",50),collapse="-"),"\n")

                }

                cat("The fixed part estimates: ","\n")
                FP.print=rbind(object@FP,sqrt(diag(object@FP.cov)))
                if (sum(grepl("bcons",colnames(object@chains)))>0){
                    bcons.pos=grep("bcons",colnames(object@chains))
                    object@chains[1,bcons.pos]=object@chains[1,bcons.pos]-0.001
                }

                t.stats=apply(object@chains,2,function(x) mean(x)/sd(x))

                p.values=2*pnorm(abs(t.stats),lower.tail =F)
                t.stat=NULL
                for (i in FP.names)  t.stat=c(t.stat, t.stats[[i]])
                p.value=NULL                
                for (i in FP.names)  p.value=c(p.value, p.values[[i]])
                onesided.p.value=NULL
                for (i in FP.names){
                  x=object@chains[,i]
                  if(sign(mean(x))>0){
                    onesided.p.values=sum(x<0)/length(x)
                  }else{
                    onesided.p.values=sum(x>0)/length(x)
                  }
                  onesided.p.value=c(onesided.p.value, onesided.p.values)
                }
                strstar=as.vector(sapply(p.value,signifstar))
                qt025=NULL
                for (i in FP.names)  qt025=c(qt025, quantile(object@chains[,i],.025))
                qt975=NULL
                for (i in FP.names)  qt975=c(qt975, quantile(object@chains[,i],.975))
                FP.print=rbind(FP.print,t.stat,p.value,onesided.p.value,qt025,qt975,ESS[FP.names])
                FP.names2=gsub("FP+\\_","",FP.names)

                printcol0=align2left("        ",FP.names2)
                printcol1=align2right("Coef.",format(round(FP.print[1,],digits),nsmall = digits))
                printcol2=align2right("Std. Err.",format(round(FP.print[2,],digits),nsmall = digits))
                printcol3=align2right("z",format(round(FP.print[3,],2),nsmall = 2))
                printcol4=align2right("Pr(>|z|)",formatC(FP.print[4,]))
                printcol4b=align2right("   ",strstar)
                printcol5=align2right("pMCMC(1-sided)",formatC(FP.print[5,]))
                printcol6=align2right("[95% Cred.",format(round(FP.print[6,],digits),nsmall = digits))
                printcol7=align2right("Interval]",format(round(FP.print[7,],digits),nsmall = digits))
                printcol8=align2right("ESS",format(round(FP.print[8,]),nsmall=0))
                for (i in 1:(ncol(FP.print)+1)){
                    cat(printcol0[i]," ",printcol1[i]," ",printcol2[i]," ")
                    if (z.ratio){
                      cat(printcol3[i]," ",printcol4[i]," ")
                      if (signif.stars){
                        cat(printcol4b[i]," ")
                      }
                    }
                    else{
                      cat(printcol5[i]," ")
                    }
                    cat(printcol6[i]," ",printcol7[i]," ",printcol8[i],"\n")
                }
                if(signif.stars & z.ratio){
                  cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 ","\n")
                }

                nlev=length(object@levID)
                if (is.na(object@levID[length(object@levID)])){
                    mlwinlev=(nlev-1):1
                    levID2=levID0
                }else{
                    mlwinlev=nlev:1
                    levID2=object@levID
                }

                RP.print=rbind(object@RP,sqrt(diag(object@RP.cov)))
                qt025=NULL
                for (i in RP.names)  qt025=c(qt025, quantile(object@chains[,i],.025))
                qt975=NULL
                for (i in RP.names)  qt975=c(qt975, quantile(object@chains[,i],.975))
                RP.print=rbind(RP.print,qt025,qt975,ESS[RP.names])
                for (i in 1:length(mlwinlev)){
                        RPx.pos=grep(paste("RP",mlwinlev[i],sep=""),RP.names)
                        if (length(RPx.pos)!=0){
                            cat(paste(rep("-",50),collapse="-"),"\n")
                            RPx.names=gsub(paste("RP+",mlwinlev[i],"+\\_",sep=""),"",RP.names[RPx.pos])
                            RPx = as.matrix(RP.print[,RPx.pos],nrow=4)
                            printcol0=align2left("        ",RPx.names)
                            printcol1=align2right("Coef.",format(round(RPx[1,],digits),nsmall = digits))
                            printcol2=align2right("Std. Err.",format(round(RPx[2,],digits),nsmall = digits))
                            printcol5=align2right("[95% Cred.",format(round(RPx[3,],digits),nsmall = digits))
                            printcol6=align2right("Interval]",format(round(RPx[4,],digits),nsmall = digits))
                            printcol7=align2right("ESS",format(round(RPx[5,]),nsmall = 0))
                            cat("The random part estimates at the",levID2[i],"level:","\n")
                            for (i in 1:(ncol(RPx)+1)){
                                cat(printcol0[i]," ",printcol1[i]," ",printcol2[i]," ",printcol5[i]," ",printcol6[i]," ",printcol7[i],"\n")
                            }
                        }
                }
                cat(paste(rep("-",50),collapse="*"),"\n")

            }

        setMethod("print", "mlwinfitMCMC", printMCMC)
        setMethod("show",  "mlwinfitMCMC", function(object) printMCMC(object))
        setMethod("update", "mlwinfitMCMC", updateMLwiN)
