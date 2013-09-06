# MCSE <-
# function(chain, xmcse, ymcse, ll, ul, rho){
#     ## Calculate Monte Carlo Standard Error
#     if (ul<ll){
#         temp=ll
#         ll=ul
#         ul=temp
#     }
#     if (ul-ll<0.0001){
#         ul=20
#         ll=0.5
#     }
#     ll=ll*length(chain)
#     ul=ul*length(chain)
# 
#     mini=min(chain)
#     maxi=max(chain)
#     Sum=sum(chain)
#     SumSq=sum(chain^2)
#     vary=(SumSq-(Sum*Sum/length(chain)))/length(chain)
#     mult = sqrt((1+rho)/(1-rho))
#     mult = mult*sqrt(vary)
# 
#     Nxmcse=length(xmcse)
#     temp2=0:(Nxmcse-1)
#     xmcse=ll+(ul-ll)*temp2/(Nxmcse-1)
#     ymcse=mult/sqrt(xmcse)
# 
#     cbind(xmcse,ymcse)
# 
# }

# NOTE:
# var is variance of chain
# rho is acf for first lag
# runlength is the length of unthinned chain
MCSE <- function(chain, rho, ll=0.5, ul=20) {
  chain_var <- var(chain)
  if (is.mcmc(chain)){
  runlength <- end(chain)-(start(chain)-1)
  }
  else{
  runlength <- length(chain)
  }
  if (ul < ll) {
    temp <- ll
    ll <- ul
    ul <- temp
  }
  if (ul - ll < 0.0001) {
    ul = 20
    ll = 0.5
  }
  ll <- ll * runlength
  ul <- ul * runlength
  mult <- (sqrt((1.0 + rho) / (1.0 - rho))) * sqrt(chain_var)
#  answer <- mult / sqrt(runlength)
  
  mcsepoints <- 1000
  mcse <- ll + ((ul - ll) * ((0:(mcsepoints - 1)) / mcsepoints))
  updates <- matrix(mult, mcsepoints, 1) / sqrt(mcse)
#  cat("Mean MCSE: ", answer)
  cbind(mcse, updates)
}