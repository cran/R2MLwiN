BD <-
function(est, var, rho, k, alpha){
    ## Based on an upublished paper by Divid Draper
    temp1=qnorm(1-alpha/2)
    est=abs(est)
    b=floor(log(est,10))
    temp2=(sqrt(var)/(10^(b-k+1)))
    answer=4*temp1^2*temp2^2*(1+rho)/(1-rho)
    nhat =ceiling(answer)
    nhat
}
