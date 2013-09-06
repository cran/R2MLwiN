BD <-
function(est, var, rho, k=2, alpha=0.05){
    # Based on an upublished paper by David Draper
   ceiling(4 * qnorm(1 - alpha / 2)^2 * (sqrt(var) / (10^(floor(log10(abs(est))) - k + 1)))^2 * (1 + rho)/(1 - rho))
}
