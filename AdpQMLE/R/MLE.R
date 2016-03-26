#' Simple Maximum Likelihood Estimation based on normal residual assumption.
#' 
#'
#' @param series is time series data. It is need to be revise.
#' @param LogLFunc 
#'
#' @return is the estimated parameters of GARCH(p,q)
#' @export
#'
#' @examples
MLE <- function(y, X, LogLFunc = c("LogL_GARCH_Norm", "LogL_Linear_Norm"), order = c(1,1)){
  ## normal distribution innovation likelihood
  if (missing(X) || LogLFunc == "LogL_GARCH_Norm"){
    LogLFunc <- LogL_GARCH_Norm
    q <- order[1]; p <- order[2]
    ini.para <- rep(0.01, p+q+1) 
    low.cons <- rep(0, p+q+1)
    up.cons <- c(Inf, rep(1, p+q))
    MLE.N <- nlminb(ini.para, LogLFunc(y, p, q), lower=low.cons, upper=up.cons)
    list(MLE.N = MLE.N$par)
  } else if (!missing(X) && LogLFunc == "LogL_Linear_Norm") {
    LogLFunc <- LogL_Linear_Norm
    MLE.N <- nlminb(c(0.01,0.01), LogLFunc(X, y))
  }
}
