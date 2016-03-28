#' The Log-likelihood function of GARCH model. Here is the GARCH(1,1). Need to be changed. 
#'
#' @param series The time series that need to be fitted. 
#' @param df The specified degree of freedome of student's t innovation.
#' @param p The order of GARCH terms \eqn{\sigma^2}
#' @param q The order of ARCH terms \eqn{\epsilon^2}
#' #'
#' @param 
#'
#' @return
#' @export
#'
LogL_GARCH_t <- function(series, p, q, df){
  GARCH_e_t <- function(para){
    n <- length(series)
    sig2 <- numeric(n)
    # tem1=numeric(1);tem2=numeric(1)
    alpha <- para[1:(q+1)] # w is the alpha(1)
    beta <- para[(q+2):(q+1+p)]
    d <- max(q,p)
    sig2[1:(d)] <- alpha[1]/(1.0-sum(alpha[2:(q+1)])-sum(beta))
    for (t in (d+1):n){
#       tem2 <- beta*(tem2+tem1)
#       tem1 <- series[t-1]^2
      sig2[t] <-  sum(alpha * c(1, series[t - (1:q)]^2)) + sum(beta * sig2[t - (1:p)])
#       sig2[t] <- (w/(1-beta)+alpha*(tem1+tem2)) #这按照笔记红色的通项公式改写可计算GARCH(P,Q)
    }
    if (c(alpha,beta) > rep(0,(q+p+1)) && (sum(alpha[2:(q+1)])+sum(beta)) < 1 && sig2>=0)
    {
      tQMLE.e <- series / (sqrt(sig2))
      g <- (gamma((df + 1)/2) / (pi * df)^0.5 / gamma(df/2))*(1+tQMLE.e^2/(df))^(-(df+1)/2)
      return(sum(log(sqrt(sig2))-log(g)))
    }
    else return(Inf)
  }
  GARCH_e_t
}
