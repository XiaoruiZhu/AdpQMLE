#' tQMLE is a function that can be used to estimate parameters of GARCH(1,1) with student's t innovation by 
#' specified log-likelihood estimation as "LogL_GARCH_t" and "dfest"(degree of freedom). It also includes 
#' QMLE other
#'
#' @title Quasi-maximum likelihood estimation with student's t innovation
#' @param series The orginal time series, which need to be fitted as GARCH(1,1)  
#' @param LogLFunc is the log likelihood function of GARCH(1,1) with student's t innovation. 
#'        This input can be changed according to assumption. Default setting is normal distributed innovation. 
#' @param dfest is specified degree of freedom of innovation. 
#'
#' @return
#' @export
#'
#' @examples
#' # If the prespecified degree of freedom of student's t innovation is 4, then the tQML estimator will be:
#' # est <- tQMLE(series = y, LogLFunc = LogL_GARCH_t, dfest = 4)
tQMLE <- function(series, LogLFunc = c("LogL_GARCH_Norm", "LogL_GARCH_t"), dfest){
  # sig2 in the formula is the sig^2!!!!!!!!!!!!!!pay attention.
  # LogL_GARCH_t is the log-likelihood function of GARCH(1,1) model with t innovation
  if (missing(series)) {
    stop("No input data for 'series'!")
  }
  if (missing(LogLFunc) || (LogLFunc != "LogL_GARCH_t") || (missing(dfest))) {
    LogLFunc <- LogL_GARCH_Norm
    QMLE.N <- MLE(series = series, LogLFunc)$MLE.N
    list(QMLE.N = QMLE.N)
  } else
  {
    df <- dfest
    LogLFunc <- LogL_GARCH_t
    QMLE.t <- nlminb(c(0.01,0.01,0.01), LogLFunc(series,df), lower=c(0,0,0), upper=c(Inf,1,1))
    list(QMLE.t = QMLE.t$par)
  }
}
