##' Function to generate GARCH time series with normal, student's t and person type IV distribution 
##' innovations
##'
##' This function is used to generate GARCH time series with t-students distribution innovations. 
##' This kind of innovations will better perform the heavy-tailed characteristics of financial data. 
##' 
##' @title GARCH_Gene
##'
##' @param alpha vector include the intercept and the parameters of autoregression part, which is ARCH terms (q).
##' @param beta vector include the parameters of conditional variance, which is GARCH terms with order p.
##' @param n lenght of time series
##' @param rnd distribution of error(innovation) of GARCH model. Innovation distribution options includes 
##'           student's t, normal distributions, generalized Gaussian and Pearson Type IV distributions  
##' @param df.t degree of freedom of student's t distribution. df.t>2 continuous parameter 
##' @param params.PIV vector/list of length 4 containing parameters m, nu, location, scale for Pearson 
##'         type IV distribution (in this order!).
##' @param ntrans burn-in size, i.e. number of initial simulated data to be discarded
##'
##' @return A list includes time-series "x" and conditional sigma square "sig.sq". 
##' 
##' @importFrom PearsonDS rpearsonIV
##' @export
##' @author Xiaorui(Jeremy) Zhu
##' @examples 
##' # Test for GARCH(1,1) with t innovation #
##' xx <- GARCH_Gene(alpha = c(0.1, 0.4), beta = 0.4, n = 2000, rnd = "rt", df.t = 4)
##' y <- xx$x
##' plot(y, type = "l")
##' paraPIV <- list(m=2, nu=-2, location=0, scale=1)
##' x.PIV <- GARCH_Gene(alpha = c(0.1, 0.2), beta = 0.5, n = 1000, rnd = "rpearsonIV", params.PIV = paraPIV)
##' \dontrun{hist(x.PIV$x)}
##' \dontrun{plot(x.PIV$x, type = 'l')}
GARCH_Gene <- function (alpha, beta, n = 100, rnd = c("rt", "rnorm", "rpearsonIV"), df.t, params.PIV, ntrans = 100) {
  # alpha includ intercept alpha_0 and ARCH terms (q).
  if (!missing(alpha))
    q = length(alpha) - 1
  else stop("Alpha is missing! No series!")
  
  if (q == 0)
    stop("Check model: q=0! No ARCH terms.")
  
  if (!missing(beta))
    p <-  length(beta)
  else {
    p <- 0
    print(paste("No GARCH terms, series are ARCH", q, sep = ""))
  }
  
  if (!missing(rnd)) {
    rnd <- match.arg(rnd)
    if ((rnd == "rt") && (missing(df.t)))
      stop("Missing degree of freedom of t distribution!")
  } else {
    rnd <- "rnorm"
    print("No specification of error distribution, error was specified as normal.")
  }
  
  
  
  total.n <-  n + ntrans
  x <-  double(total.n)
  sig2t <-  x
  d <-  max(p, q)
  sigma2 <-  sum(alpha[-1])
  if (p > 0)
    sigma2 <-  sigma2 + sum(beta)
  if (sigma2 > 1)
    stop("Check model: it does not have finite variance")
  sigma2 <-  alpha[1] /(1 - sigma2)
  if (sigma2 <= 0)
    stop("Check model: it does not have positive variance")
  if (rnd == "rnorm") {
    x[1:d] = do.call(rnd, list(d, 0, 1))
    e = do.call(rnd, list(total.n, 0, 1))
  } else if ((rnd == "rt") & (!missing(df.t))) {
    if (df.t <= 2)
      stop("Degree of freedom of t distribution cannot equal or less than 2!")
    x[1:d] = do.call(rnd, list(d, df.t))
    e = do.call(rnd, list(total.n, df.t))
  } else if ((rnd == "rpearsonIV") & (!missing(params.PIV))) {
    n.params.d <- list(d, params.PIV$m, params.PIV$nu, params.PIV$location, params.PIV$scale)
    n.params.n <- list(total.n, params.PIV$m, params.PIV$nu, params.PIV$location, params.PIV$scale)
    x[1:d] = do.call(rnd, n.params.d)
    e = do.call(rnd, n.params.n)
  } 
  # Here can add more distribution! 
  
  sig2t[1:d] = sigma2
  if (p == 0) {
    # No GARCH terms. (or no conditional variances, only ARCH(q))
    for (i in (d + 1):total.n) {
      sig2t[i] = sum(alpha * c(1, x[i - (1:q)]^2))
      x[i] = e[i] * sqrt(sig2t[i])
    }
  }
  else {
    for (i in (d + 1):total.n) {
      sig2t[i] = sum(alpha * c(1, x[i - (1:q)]^2)) + sum(beta * sig2t[i - (1:p)])
      x[i] = e[i] * sqrt(sig2t[i])
    }
  }
#   return(invisible(x[(ntrans + 1):total.n]))
  list(x = invisible(x[(ntrans + 1):total.n]), sig.sq=sig2t[(ntrans + 1):total.n])
}
