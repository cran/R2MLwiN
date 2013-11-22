      # An S4 class that stores the outputs of the fitted model.
      # @slot a contains an R object
      # @export
      setClass(Class = "mlwinfitIGLS", representation = representation(Nobs="numeric",DataLength="numeric",
        D="ANY", Formula="ANY", levID="character", estIGLS ="data.frame",
        FP="numeric", RP="numeric", RP.cov="matrix", FP.cov="matrix", LIKE="ANY",
        elapsed.time="numeric", call="ANY",residual="data.frame"))


        # extract parts of mlwinfitIGLS
        #
        # @name [
        # @aliases [,mlwinfitIGLS-method
        # @docType methods
        # @rdname extract-methods
        #
        setMethod(
            f= "[",
            signature="mlwinfitIGLS",
            definition=function(x,i,j,drop){
                if(i=="Nobs"){return(x@Nobs)}else {}
                if(i=="DataLength"){return(x@DataLength)}else {}
                if(i=="D"){return(x@D)}else {}
                if(i=="Formula"){return(x@Formula)}else {}
                if(i=="levID"){return(x@levID)}else {}
                if(i=="estIGLS"){return(x@estIGLS)}else {}
                if(i=="FP"){return(x@FP)}else {}
                if(i=="RP"){return(x@RP)}else {}
                if(i=="FP.cov"){return(x@FP.cov)}else {}
                if(i=="RP.cov"){return(x@RP.cov)}else {}
                if(i=="elapsed.time"){return(x@elapsed.time)}else {}
                if(i=="call"){return(x@call)}else {}
                if(i=="LIKE"){return(x@LIKE)}else {}
#                if(i=="chains.bugs"){return(x@chains.bugs)}else {}
                if(i=="residual"){return(x@residual)}else {}
            }
        )
        # extract parts of mlwinfitIGLS
        #
        # @name [
        # @aliases [,mlwinfitIGLS-method
        # @docType methods
        # @rdname extract-methods
        #
        setMethod(
            f= "[[",
            signature="mlwinfitIGLS",
            definition=function(x,i,j,drop){
                if(i=="Nobs"){return(x@Nobs)}else {}  
                if(i=="DataLength"){return(x@DataLength)}else {}
                if(i=="D"){return(x@D)}else {}
                if(i=="Formula"){return(x@Formula)}else {}
                if(i=="levID"){return(x@levID)}else {}
                if(i=="estIGLS"){return(x@estIGLS)}else {}
                if(i=="FP"){return(x@FP)}else {}
                if(i=="RP"){return(x@RP)}else {}
                if(i=="FP.cov"){return(x@FP.cov)}else {}
                if(i=="RP.cov"){return(x@RP.cov)}else {}
                if(i=="elapsed.time"){return(x@elapsed.time)}else {}
                if(i=="call"){return(x@call)}else {}
                if(i=="LIKE"){return(x@LIKE)}else {}
#                if(i=="chains.bugs"){return(x@chains.bugs)}else {}
                if(i=="residual"){return(x@residual)}else {}
            }
        )

        # replace names of mlwinfitIGLS
        #
        # @name [
        # @aliases [<-,mlwinfitIGLS-method
        # @docType methods
        # @rdname extract-methods
        setReplaceMethod(
            f= "[",
            signature="mlwinfitIGLS",
            definition=function(x,i,j,value){
                if(i=="Nobs"){x@Nobs<-value}else {}
                if(i=="DataLength"){x@DataLength<-value}else {}
                if(i=="D"){x@D<-value}else {}
                if(i=="Formula"){x@Formula<-value}else {}
                if(i=="levID"){x@levID<-value}else {}
                if(i=="estIGLS"){x@estIGLS<-value}else {}
                if(i=="FP"){x@FP<-value}else {}
                if(i=="RP"){x@RP<-value}else {}
                if(i=="FP.cov"){x@FP.cov<-value}else {}
                if(i=="RP.cov"){x@RP.cov<-value}else {}
                if(i=="elapsed.time"){x@elapsed.time<-value}else {}
                if(i=="call"){x@call<-value}else {}
                if(i=="LIKE"){x@LIKE<-value}else {}
#                if(i=="chains.bugs"){x@chains.bugs<-value}else {}
                if(i=="residual"){x@residual<-value}else {}
                validObject(x)
                return (x)
            }
        )
        # replace names of mlwinfitIGLS
        #
        # @name [
        # @aliases [<-,mlwinfitIGLS-method
        # @docType methods
        # @rdname extract-methods
        setReplaceMethod(
            f= "[[",
            signature="mlwinfitIGLS",
            definition=function(x,i,j,value){
                if(i=="Nobs"){x@Nobs<-value}else {}
                if(i=="DataLength"){x@DataLength<-value}else {}
                if(i=="D"){x@D<-value}else {}
                if(i=="Formula"){x@Formula<-value}else {}
                if(i=="levID"){x@levID<-value}else {}
                if(i=="estIGLS"){x@estIGLS<-value}else {}
                if(i=="FP"){x@FP<-value}else {}
                if(i=="RP"){x@RP<-value}else {}
                if(i=="FP.cov"){x@FP.cov<-value}else {}
                if(i=="RP.cov"){x@RP.cov<-value}else {}
                if(i=="elapsed.time"){x@elapsed.time<-value}else {}
                if(i=="call"){x@call<-value}else {}
                if(i=="LIKE"){x@LIKE<-value}else {}
#                if(i=="chains.bugs"){x@chains.bugs<-value}else {}
                if(i=="residual"){x@residual<-value}else {}
                validObject(x)
                return (x)
            }
        )
        setMethod("summary",
                  signature(object = "mlwinfitIGLS"),
                  function (object,  ...)
            {
                    object

            }
        )

      printIGLS <- function(x, digits = max(3, getOption("digits") - 2), signif.stars = getOption("show.signif.stars"),...)
      
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
                cat("\n")
                cat(paste(rep("-",50),collapse="*"),"\n")
                cat("MLwiN multilevel model",paste("(",object@D[1],")",sep=""),"\n")
                cat("Estimation algorithm:  IGLS        Elapsed time :",paste(round(object@elapsed.time,2),"s",sep=""), "\n")
                cat("Number of obs: ",object@Nobs,"(from total",object@DataLength,")\n")
                cat(paste("Deviance statistic: ", round(object@LIKE,1)),"\n")
                cat(paste(rep("-",50),collapse="-"),"\n")
                cat("The model formula:\n")
                print(object@Formula)#cat(gsub("[[:space:]]","",object@Formula),"\n")
                levID0=object@levID
                levID.display=""
                if (is.na(levID0[length(levID0)])){
                    levID0=levID0[-length(levID0)]
                }
                for (i in 1:length(levID0)){
                    levID.display=paste(levID.display,"Level ",length(levID0)+1-i,": ",levID0[i],"     ",sep="")
                }
                cat(levID.display,"\n")
                cat(paste(rep("-",50),collapse="-"),"\n")

                FP.names=names(object@FP);RP.names=names(object@RP)

                cat("The fixed part estimates: ","\n")
                FP.print=rbind(object@FP,sqrt(diag(object@FP.cov)))
                z.score=FP.print[1,]/FP.print[2,]
                p.value=2 * pnorm(abs(z.score), lower.tail = FALSE)
                strstar=as.vector(sapply(p.value,signifstar))
                qt025=FP.print[1,]-qnorm(.975)*FP.print[2,]
                qt975=FP.print[1,]+qnorm(.975)*FP.print[2,]
                FP.print=rbind(FP.print,z.score,p.value,qt025,qt975)
                FP.names2=gsub("FP+\\_","",FP.names)

                printcol0=align2left("        ",FP.names2)
                printcol1=align2right("Coef.",format(round(FP.print[1,],digits),nsmall = digits))
                printcol2=align2right("Std. Err.",format(round(FP.print[2,],digits),nsmall = digits))
                printcol3=align2right("z",format(round(FP.print[3,],2),nsmall = 2))
                printcol4=align2right("Pr(>|z|)",formatC(FP.print[4,]))
                printcol4b=align2right("   ",strstar)
                printcol5=align2right("[95% Conf.",format(round(FP.print[5,],digits),nsmall = digits))
                printcol6=align2right("Interval]",format(round(FP.print[6,],digits),nsmall = digits))
                for (i in 1:(ncol(FP.print)+1)){
                    cat(printcol0[i]," ",printcol1[i]," ",printcol2[i]," ",printcol3[i]," ",printcol4[i]," ",printcol4b[i]," ",printcol5[i]," ",printcol6[i],"\n")
                }
                if(signif.stars){
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
                qt025=RP.print[1,]-qnorm(.975)*RP.print[2,]
                qt975=RP.print[1,]+qnorm(.975)*RP.print[2,]
                RP.print=rbind(RP.print,qt025,qt975)
                for (i in 1:length(mlwinlev)){
                        RPx.pos=grep(paste("RP",mlwinlev[i],sep=""),RP.names)
                        if (length(RPx.pos)!=0){
                            cat(paste(rep("-",50),collapse="-"),"\n")
                            RPx.names=gsub(paste("RP+",mlwinlev[i],"+\\_",sep=""),"",RP.names[RPx.pos])
                            RPx = as.matrix(RP.print[,RPx.pos],nrow=4)
                            printcol0=align2left("        ",RPx.names)
                            printcol1=align2right("Coef.",format(round(RPx[1,],digits),nsmall = digits))
                            printcol2=align2right("Std. Err.",format(round(RPx[2,],digits),nsmall = digits))
#                            printcol5=align2right("[95% Conf.",format(round(RPx[3,],digits),nsmall = digits))
#                            printcol6=align2right("Interval]",format(round(RPx[4,],digits),nsmall = digits))
                            cat("The random part estimates at the",levID2[i],"level:","\n")
                            for (i in 1:(ncol(RPx)+1)){
 #                               cat(printcol0[i]," ",printcol1[i]," ",printcol2[i]," ",printcol5[i]," ",printcol6[i],"\n")
                              cat(printcol0[i]," ",printcol1[i]," ",printcol2[i],"\n")                            
                            }
                        }
                }
                cat(paste(rep("-",50),collapse="*"),"\n")

            }
      setMethod("print", "mlwinfitIGLS", printIGLS)
      setMethod("show",  "mlwinfitIGLS", function(object) printIGLS(object))


    updateMLwiN <- function (object, Formula., levID., estoptions., ..., 
    keep.order = TRUE, evaluate = TRUE)
    {
        my.update.formula <- function(old, new, keep.order = TRUE, 
            ...) {
            env <- environment(as.formula(old))
            tmp <- update.formula(as.formula(old), as.formula(new))
            out <- formula(terms.formula(tmp, simplify = TRUE, keep.order = keep.order))
            environment(out) <- env
            return(out)
        }
        if (is.null(newcall <- getCall(object)))
            stop("need an object with call component")
        extras <- match.call(expand.dots = FALSE)$...
        if (length(newcall$Formula)) 
            newcall$Formula <- eval(newcall$Formula) 
        if (!missing(Formula.)) 
            newcall$Formula <- my.update.formula(as.formula(newcall$Formula), 
                Formula., keep.order = keep.order)        
        if (!missing(levID.)) 
            newcall$levID <- {
                if (length(newcall$levID)) 
                    my.update.formula(as.formula(newcall$levID), 
                      levID., keep.order = keep.order)
                else levID.
            }
        if (!missing(estoptions.)) 
            newcall$estoptions <- {
                if (length(newcall$estoptions)) 
                    my.update.formula(as.formula(newcall$estoptions), estoptions., 
                      keep.order = keep.order)
                else estoptions.
            }      
        if (length(extras)) {
            existing <- !is.na(match(names(extras), names(newcall)))
            for (a in names(extras)[existing]) newcall[[a]] <- extras[[a]]
            if (any(!existing)) {
                newcall <- c(as.list(newcall), extras[!existing])
                newcall <- as.call(newcall)
            }
        }
        if (evaluate)
            eval(newcall, sys.parent())
        else newcall
    }
    setMethod("update", "mlwinfitIGLS", updateMLwiN)
