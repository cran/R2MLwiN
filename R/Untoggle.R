Untoggle = function(categrv,name=NULL){
    ## this function will untoggle categorical variable into a few separate binary variables
    vars = unique(categrv)
    N = length(vars)
    rvs=sapply(1:N, function(x) as.integer(categrv==vars[x]))
    if (is.factor(vars)){
        colnames(rvs)=levels(vars)
    }else{
        if (is.character(vars)){
            colnames(rvs)=vars
        }else{
            if(!is.null(name)&&is.numeric(vars)) colnames(rvs)=paste(name,vars,sep="_")
        }
    }
    rvs
}
