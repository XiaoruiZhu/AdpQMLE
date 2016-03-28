#' Simple Maximum Likelihood Estimation based on normal residual assumption.
#' 
#'
#' @param LogLFunc 
#' @param y The dependent variable or time series.
#' @param X The single independent variable for single linear regression. It could be 
#'          missing if use this function to estimate GARCH(p,q) model.
#' @param order 
#'
#' @return is the estimated parameters of GARCH(p,q)
#' @export
#'
#' @examples
#' # Test for GARCH(1,1) with t innovation #
#' xx <- GARCH_t(alpha = c(0.1, 0.4), beta = 0.4, n = 2000, rnd = "rt", df.t = 4)
#' y <- xx$x
#' plot(y, type = "l")
#' # Estimate parameters using "garch" function in the "tseries" package
#' x.arch <- garch(y, order = c(1,1)) 
#' est1 <- MLE(y = y, LogLFunc = LogL_GARCH_Norm, order = c(1,1))
#' est1
#' 
#' # Example for single linear regression model.
#' # Simulate from the model \eqn{Y=\beta_0+\beta_1*x+N(0,\sigma^2)}
#' # Inputs: intercept; slope; variance; vector of x; return sample or estimated
#' # Fix x values for all runs of the simulation; draw from an exponential
#' n <- 2000 # So we don't have magic #s floating around
#' beta.0 <- 5; beta.1 <- -2; sigma.sq <- 1; fixed.x <- rexp(n=n)
#' data <- gen.lin(intercept=beta.0, slope = beta.1, noise.variance=sigma.sq, x=fixed.x, dis.error = "rt", dft=3)
#' par(mfrow=c(1,1))
#' hist(fixed.x, freq = FALSE, breaks = 50, xlab = expression(x[i]), main = "")
#' hist(data$y, freq = FALSE, breaks = 50, xlab = expression(y[i]), main = "")
#' plot(data$x, data$y)
#' test <- lm(data$y~data$x)
#' test
#' y <- as.matrix(data$y); X <- as.matrix(data$x)
#' myMLE <- MLE(y, X, LogLFunc = "LogL_Linear_Norm")
#' myMLE
#' # Very good example for single linear regression by suing "MLE" function.


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
