##' Function to generate GARCH time series with t-students distribution innovations
##'
##' This function is used to generate GARCH time series with t-students distribution innovations. This kind of innovations will better perform the heavy-tailed characteristics of financial data. 
##' 
##' @title T.GARCH
##'
##' @param alpha The vector include the intercept and the parameters of autoregression part, which is ARCH terms (q).
##' @param beta The vector include the parameters of conditional variance, which is GARCH terms with order p.
##' @param n The lenght of time series
##' @param rnd The distribution of error(innovation) of GARCH model
##' @param df.t The df of student's t distribution. df.t>0 continuous parameter 
##' @param ntrans burn-in size, i.e. number of initial simulated data to be discarded
##'
##' @author Xiaorui.Zhu
GARCH_t <- function (alpha, beta, n = 100, rnd = c("rt", "rnorm"), df.t, ntrans = 100)
{
  # alpha includ intercept alpha_0 and ARCH terms (q).
  if (df.t <= 2)
    stop("Degree of freedom of t distribution cannot equal or less than 2!")
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

  rnd <- match.arg(rnd)
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
  x[1:d] = do.call(rnd, list(d, df.t))
  e = do.call(rnd, list(total.n, df.t))
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
  return(invisible(x[(ntrans + 1):total.n]))
}
