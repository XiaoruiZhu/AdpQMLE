#' Simple MLE based on normal residua assumption.
#' 
#'
#' @param x is time series data. It is need to be revise.
#'
#' @return is the estimated parameters of GARCH(1,1)
#' @export
#'
#' @examples
MyMLE <- function(x){
  ## normal distribution innovation likelihood
  lik <- function(x){
    TTL <- function(par){
      n <- length(x)
      sig2 <- numeric(n); tem1 <- numeric(1); tem2 <- numeric(1)
      w <- par[1]
      alpha <- par[2]
      beta <- par[3]
      sig2[1] <- w/(1-alpha-beta)
      for (t in 2:n){
        tem2=beta*(tem2+tem1)
        tem1=x[t-1]^2
        sig2[t]=(w/(1-beta)+alpha*(tem1+tem2)) 
        #Change this part based on my NOTES in red GARCH(P,Q)  
      }
      if (c(w,alpha,beta) > rep(0,3) && (alpha+beta) < 1 && sig2>=0)  
      {
        sum(log(sqrt(sig2))+x^2/(sig2))
      }
      else Inf
    }
    TTL
  }
  mle.N=nlminb(c(0.1,0.1,0.1),lik(x),lower=c(0,0,0),upper=c(Inf,1,1))
  #mle.N
  list(mle.N=mle.N$par)
}
