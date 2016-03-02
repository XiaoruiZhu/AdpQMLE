#' MytQMLE is a function used to estimate parameters of GARCH(1,1) with student's t innovation.
#'
#' @param x is the time series that was hypothesized as GARCH(1,1) model. 
#' @param dfest is specified degree of freedom of innovation. 
#'
#' @return
#' @export
#'
#' @examples
MytQMLE=function(x,dfest){
  # sig2 in the formula is the sig^2!!!!!!!!!!!!!!pay attention.
  # t分布新息的GARCH(1,1) likelihood function
  df=dfest
  likt<-function(x){
    tL=function(para){
      n=length(x)
      sig2=numeric(n);tem1=numeric(1);tem2=numeric(1)
      w=para[1]
      alpha=para[2]
      beta=para[3]
      sig2[1]=w/(1-alpha-beta)
      for (t in 2:n){
        tem2=beta*(tem2+tem1)
        tem1=x[t-1]^2
        sig2[t]=(w/(1-beta)+alpha*(tem1+tem2)) #这按照笔记红色的通项公式改写可计算GARCH(P,Q)
      }
      if (c(w,alpha,beta) > rep(0,3) && (alpha+beta) < 1 && sig2>=rep(0,n))  
      {
        tQMLE.e=x/(sqrt(sig2))
        f=(gamma((df+1)/2)/(pi*df)^0.5/gamma(df/2))*(1+tQMLE.e^2/(df))^(-(df+1)/2)
        sum(log(sig2)-2*log(f))
      }
      else Inf
    }
    tL
  }
  
  #plot(c(1:500),x,type='l')
  qmle.N=nlminb(c(0.01,0.01,0.01),likt(x),lower=c(0,0,0),upper=c(Inf,1,1))
  #mle.N
  list(qmle.N=qmle.N$par)
}
