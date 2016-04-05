#' tQMLE is a function that can be used to estimate parameters of GARCH(p,q) with student's t innovation by
#' specified log-likelihood estimation as "LogL_GARCH_t" and "dfest"(degree of freedom). It also includes
#' QMLE other
#'
#' @title Quasi-maximum likelihood estimation with student's t innovation
#'
#' @param series The orginal time series, which need to be fitted as GARCH(1,1)
#' @param LogLFunc is the log likelihood function of GARCH(p,q) with student's t innovation. The model is
#'        setted as \eqn{\sigma^{2}_{t|t-1}=\omega+\sum_{i=1}^{q}\alpha_{i}u^{2}_{t-i}+\sum_{j=1}^{p}\beta_{j}\sigma^{2}_{t-j}}
#'        This input can be changed according to assumption. Default setting is normal distributed innovation.
#' @param dfest is specified degree of freedom of innovation.
#' @param order \eqn{p}: order for GARCH terms, \eqn{q}: order for ARCH terms.
#' @param params.PIV vector/list of length 4 containing parameters m, nu, location, scale for Pearson 
#'         type IV distribution (in this order!).
#'
#' @return A list.
#' @export
#'
#' @examples
#' # If the prespecified degree of freedom of student's t innovation is 4, then the tQML estimator will be:
#' xx <- GARCH_Gene(alpha = c(0.1, 0.2, 0.3), beta = 0.3, n = 1000, rnd = "rt", df.t = 4)
#' y <- xx$x
#' plot(y, type = "l")
#' # Two bad estimation methods
#' library(tseries)
#' x.arch <- garch(y, order = c(2,1)) # Fit GARCH(2,1)
#' est1 <- MLE(y = y, LogLFunc = LogL_GARCH_Norm, order = c(2,1))
#' est1
#' # Better estimation methods with specified innovation.
#' est2 <- QMLE(series = y, LogLFunc = LogL_GARCH_t, dfest = 4)
#' est2
QMLE <- function(series, LogLFunc = c("LogL_GARCH_Norm", "LogL_GARCH_t", "LogL_GARCH_PIV"), order = c(1,1), dfest, params.PIV){
  # sig2 in the formula is the sig^2!!!!!!!!!!!!!!pay attention.
  if (missing(series)) {
    stop("No input data for 'series'!")
  }
  q <- order[1]; p <- order[2]
  if (!missing(LogLFunc)) 
    LogLFunc <- match.arg(LogLFunc)
  else LogLFunc <- "LogL_GARCH_Norm"
  
  if (LogLFunc == "LogL_GARCH_Norm") {
    # || (missing(dfest)) || (missing(params.PIV))) 
    LogLFunc <- LogL_GARCH_Norm
    QMLE.N <- MLE(y = series, LogLFunc(series), order = order)$MLE.N
    pred <- com.residue(alpha = QMLE.N[1:(q+1)], beta = QMLE.N[(q+2):(p+q+1)], series=series)
    print("Estimated as normal error!")
    list(QMLE.N = QMLE.N, sigma.sq = pred$sig.sq, e = pred$e)
  } else if ((LogLFunc == "LogL_GARCH_t") && (!missing(dfest)))
  {
    df <- dfest
    LogLFunc <- LogL_GARCH_t
    ini.para <- rep(0.01, p+q+1)
    low.cons <- rep(0, p+q+1)
    up.cons <- c(Inf, rep(1, p+q))
    QMLE.t <- nlminb(ini.para, LogLFunc(series, p, q, df), lower=low.cons, upper=up.cons)
    QMLE.t <- QMLE.t$par
    pred <- com.residue(alpha = QMLE.t[1:(q+1)], beta = QMLE.t[(q+2):(p+q+1)], series=series)
    print("Estimated as student's t error!")
    list(QMLE.t = QMLE.t, sigma.sq = pred$sig.sq, e = pred$e)
  } else if ((LogLFunc == "LogL_GARCH_PIV") && (!missing(params.PIV)))
  {
    LogLFunc <- LogL_GARCH_PIV
    ini.para <- rep(0.01, p+q+1)
    low.cons <- rep(0, p+q+1)
    up.cons <- c(Inf, rep(1, p+q))
    m <- params.PIV$m; nu <- params.PIV$nu; lambda <- params.PIV$location; a <- params.PIV$scale
    QMLE.PIV <- nlminb(ini.para, LogLFunc(series, p, q, lambda, a, nu, m), lower=low.cons, upper=up.cons)
    QMLE.PIV <- QMLE.PIV$par
    pred <- com.residue(alpha = QMLE.PIV[1:(q+1)], beta = QMLE.PIV[(q+2):(p+q+1)], series=series)
    print("Estimated as Pearson Type IV error!")
    list(QMLE.PIV = QMLE.PIV, sigma.sq = pred$sig.sq, e = pred$e)
  }
}
