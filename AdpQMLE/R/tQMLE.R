#' MytQMLE is a function used to estimate parameters of GARCH(1,1) with student's t innovation.
#'
#' @param x is the time series that was hypothesized as GARCH(1,1) model. 
#' @param dfest is specified degree of freedom of innovation. 
#'
#' @return
#' @export
#'
#' @examples
tQMLE=function(series, LogLFunc, dfest){
  # sig2 in the formula is the sig^2!!!!!!!!!!!!!!pay attention.
  # t分布新息的GARCH(1,1) likelihood function
  df=dfest
  
  #plot(c(1:500),x,type='l')
  tQMLE.N=nlminb(c(0.01,0.01,0.01),LogLFunc(seires),lower=c(0,0,0),upper=c(Inf,1,1))
  #mle.N
  list(tQMLE.N=tQMLE.N$par)
}
