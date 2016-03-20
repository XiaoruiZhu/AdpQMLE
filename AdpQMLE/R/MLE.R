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
MLE <- function(series, LogLFunc){
  ## normal distribution innovation likelihood
  MLE.N <- nlminb(c(0.01,0.01,0.01), LogLFunc(series), lower=c(0,0,0), upper=c(Inf,1,1))
  list(MLE.N = MLE.N$par)
}
