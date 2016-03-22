#' Simple Maximum Likelihood Estimation based on normal residual assumption.
#' 
#'
#' @param series is time series data. It is need to be revise.
#' @param LogLFunc 
#'
#' @return is the estimated parameters of GARCH(1,1)
#' @export
#'
#' @examples
MLE <- function(y, X, LogLFunc = c("LogL_GARCH_Norm", "LogL_Linear_Norm")){
  ## normal distribution innovation likelihood
  if (missing(X) || LogLFunc == "LogL_GARCH_Norm"){
    LogLFunc <- LogL_GARCH_Norm
    MLE.N <- nlminb(c(0.01,0.01,0.01), LogLFunc(y), lower=c(0,0,0), upper=c(Inf,1,1))
    list(MLE.N = MLE.N$par)
  } else if (!missing(X) && LogLFunc == "LogL_Linear_Norm") {
    LogLFunc <- LogL_Linear_Norm
    MLE.N <- nlminb(c(0.01,0.01), LogLFunc(X, y))
  }
}
