MCSE <-
function(chain, xmcse, ymcse, ll, ul, rho){
    ## Calculate Monte Carlo Standard Error
    if (ul<ll){
        temp=ll
        ll=ul
        ul=temp
    }
    if (ul-ll<0.0001){
        ul=20
        ll=0.5
    }
    ll=ll*length(chain)
    ul=ul*length(chain)

    mini=min(chain)
    maxi=max(chain)
    Sum=sum(chain)
    SumSq=sum(chain^2)
    vary=(SumSq-(Sum*Sum/length(chain)))/length(chain)
    mult = sqrt((1+rho)/(1-rho))
    mult = mult*sqrt(vary)

    Nxmcse=length(xmcse)
    temp2=0:(Nxmcse-1)
    xmcse=ll+(ul-ll)*temp2/(Nxmcse-1)
    ymcse=mult/sqrt(xmcse)

    cbind(xmcse,ymcse)

}
