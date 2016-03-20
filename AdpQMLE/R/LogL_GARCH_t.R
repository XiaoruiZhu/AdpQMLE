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
LogL_GARCH_t <- function(series){
  tL=function(para){
    n=length(series)
    sig2=numeric(n);tem1=numeric(1);tem2=numeric(1)
    w=para[1]
    alpha=para[2]
    beta=para[3]
    sig2[1]=w/(1-alpha-beta)
    for (t in 2:n){
      tem2 <- beta*(tem2+tem1)
      tem1 <- series[t-1]^2
      sig2[t] <- (w/(1-beta)+alpha*(tem1+tem2)) #这按照笔记红色的通项公式改写可计算GARCH(P,Q)
    }
    if (c(w,alpha,beta) > rep(0,3) && (alpha+beta) < 1 && sig2>=rep(0,n))  
    {
      tQMLE.e <- seires/(sqrt(sig2))
      f=(gamma((df+1)/2)/(pi*df)^0.5/gamma(df/2))*(1+tQMLE.e^2/(df))^(-(df+1)/2)
      sum(log(sig2)-2*log(f))
    }
    else Inf
  }
  tL
}
