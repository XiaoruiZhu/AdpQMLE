#' The Log-likelihood function of GARCH(p,q) model. 
#'
#' @title Log-likelihood function fo GARCH(p,q) 
#'
#' @param series The time series that need to be fitted. 
#' @param p The order of GARCH terms \eqn{\sigma^2}
#' @param q The order of ARCH terms \eqn{\epsilon^2}
#'
#' @param 
#'
#' @return
#' @export
#'
#' @examples
LogL_GARCH_Norm <- function(series, p, q) {
  GARCH_Norm <- function(para){
    n <- length(series)
    sig2 <- numeric(n); # tem1 <- numeric(1); tem2 <- numeric(1)
    alpha <- para[1:(q+1)] # w is the alpha(1)
    beta <- para[(q+2):(q+1+p)]
    d <- max(q,p)
    sig2[1:(d)] <- alpha[1]/(1.0-sum(alpha[2:(q+1)])-sum(beta))
    for (t in (d+1):n){
      sig2[t] = sum(alpha * c(1, series[t - (1:q)]^2)) + sum(beta * sig2[t - (1:p)])
      # It is changed based on my NOTES in red GARCH(P,Q)  
    }
    if (c(alpha,beta) > rep(0,(q+p+1)) && (sum(alpha[2:(q+1)])+sum(beta)) < 1 && sig2>=0)  
    {
      return(sum(2*log(sqrt(sig2)) + series^2/(sig2)))
      # Attention! Here is a 2 times so that the log-likelihood function is consistent. 
    }
    else return(Inf)
  }
  GARCH_Norm
}

