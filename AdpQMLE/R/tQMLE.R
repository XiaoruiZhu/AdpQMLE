#' tQMLE is a function used to estimate parameters of GARCH(1,1) with student's t innovation.
#'
#' @param dfest is specified degree of freedom of innovation. 
#' @param series The orginal time series, which need to be fitted as GARCH(1,1)  
#' @param LogLFunc is the log likelihood function of GARCH(1,1) with student's t innovation. This input can be changed according to  
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
  tQMLE.N=nlminb(c(0.01,0.01,0.01),LogLFunc(series,df),lower=c(0,0,0),upper=c(Inf,1,1))
  #mle.N
  list(tQMLE.N=tQMLE.N$par)
}
