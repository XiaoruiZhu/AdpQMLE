#' This is a function which can be used to estimate parameters of all kinds of models.
#' The critical idea of this methods is to estimate parameters according to data
#' themselves with unspecified assumption of error's distribution.
#'
#' @title Adaptive Quasi Maximum Likelihood Estimation
#' @param series The original time-series that need to be fitted as GARCH model
#' @param order The GARCH model orders. Includes GARCH terms and ARCH terms.
#'
#'
#' @return The estimation of parameters.
#' @export
#'
#' @examples
#' #test
A_tQMLE <- function(series, order = c(1,1)){
  # This part need to be changed and revised for efficient calculation.
  if (missing(series)) {
    stop("No input data for 'series'!")
  }
  q <- order[1]; p <- order[2]
  n <- length(series); max.iter <- 50
  Bdf.t <- c()
  # Estm contains parameters, est.df, ETA and SSE(total sum square error)
  Estm <- matrix(NA, max.iter, (p+q+1+3))
  Est.model <- QMLE(series=series, LogLFunc = "LogL_GARCH_Norm", order = order)
  para <- Est.model$QMLE.N
  e.t <- Est.model$e
  Estm <- c(Est.model$QMLE.N, NA, NA, NA)
  names(Estm) <- c("alpha0", "alpha1", "beta","est.df", "eta", "diff_Para")
  # The scale parameter eta_f is the boundary. Equal to 1 is the condition.
  new.df <- Estdf(e.t)
  new.YITA <- YITAtQMLE(e=e.t,dfest=new.df)
  iter <-  1; diff <- 1
  while (iter<max.iter & diff > 0.0000001) {
    iter <-  iter+1
    Est.model <- QMLE(series = series, LogLFunc = "LogL_GARCH_t", order = order, dfest = new.df)
    e.t <- Est.model$e
    new.para <- Est.model$QMLE.t
    new.diff <- sum((para-new.para)^2)/(p+q+1)
    Estm <- rbind(Estm, c(Est.model$QMLE.t, new.df, new.YITA, new.diff))
    new.df <- Estdf(e.t)
    new.YITA <- YITAtQMLE(e=e.t,dfest=new.df) # This is the crux! Using the assumed df to estimate yita!
    diff <- new.diff; para <- new.para
  }
  return(Estm)
}

