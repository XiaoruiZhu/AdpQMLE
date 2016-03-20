#' The Log-likelihood function of GARCH model. Here is the GARCH(1,1). Need to be changed. 
#'
#' @param series The time series that need to be fitted. 
#'
#' @param 
#'
#' @return
#' @export
#'
#' @examples
LogL_GARCH_Norm <- function(series){
  GARCH_Norm <- function(para){
    n <- length(series)
    sig2 <- numeric(n); tem1 <- numeric(1); tem2 <- numeric(1)
    w <- para[1]
    alpha <- para[2]
    beta <- para[3]
    sig2[1] <- w/(1.0-alpha-beta)
    for (t in 2:n){
      tem2 <- beta * (tem2+tem1)
      tem1 <- series[t-1]^2
      sig2[t] <- (w/(1-beta) + alpha * (tem1+tem2)) 
      #Change this part based on my NOTES in red GARCH(P,Q)  
    }
    if (c(w,alpha,beta) > rep(0,3) && (alpha+beta) < 1 && sig2>=0)  
    {
      return(sum(2*log(sqrt(sig2)) + series^2/(sig2)))
      # Attention! Here is a 2 times so that the log-likelihood function is consistent. 
    }
    else return(Inf)
  }
  GARCH_Norm
}
