predCurves <-
function(object, indata,xname, group=NULL, legend=T, legend.space="top", legend.ncol=2, ...){
## This function is to draw predicted lines using fixed part estimates

  FP <- object["FP"]
  
  if (!is.null(group)){
    if(is.character(group)) group <- indata[[group]]
    if(!is.factor(group)) group <- as.factor(group)
  }

  fp.names <- sub("FP_","",names(FP))
  tval <- 0
  for (i in 1:length(fp.names)){
    if (is.factor(indata[[fp.names[i]]])){
      indata[[fp.names[i]]] <- as.integer(indata[[fp.names[i]]])-1
    }
    tval <- tval+as.numeric(indata[[fp.names[i]]])*FP[i]
  }

  pred.min <- min(tval)
  pred.max <- max(tval)
  x=indata[[xname]]
  x.min=min(x) 
  x.max=max(x)
  
  if (legend && length(group)){
    key=list(lines = Rows(trellis.par.get("superpose.line"),1:nlevels(group)),
    text=list(lab=levels(group)), space=legend.space, columns=legend.ncol)
  }else{
    key <- NULL
  }
     
  if (!is.null(group)){
    levs=levels(group); nlev=length(levs)
    trellis.obj <- xyplot(tval~x, 
        prepanel = function(x,y,...){list(xlim=c(x.min, x.max), ylim=c(pred.min,pred.max))},
        groups=group,
        panel= function(x,y, groups,...){  
          col <- Rows(trellis.par.get("superpose.line"),1:nlev)$col
          for (i in 1:nlev){
            ypred <- y[groups==levs[i]]
            panel.xyplot(x=sort(x[groups==levs[i]]),y=ypred[order(x[groups==levs[i]])], col=col[i], type="l", ...)
          }
        },key=key, ylab="ypred", xlab=xname, ...)
  }else{
    trellis.obj <- xyplot(tval~x, 
        prepanel = function(x,y,...){list(xlim=c(x.min, x.max), ylim=c(pred.min,pred.max))},
        panel= function(x,y,...){  
          panel.xyplot(x=sort(x),y=y[order(x)], type="l", ...)
        }, ylab="ypred", xlab=xname, ...)
  }
  print(trellis.obj)
  invisible(trellis.obj)  
}
