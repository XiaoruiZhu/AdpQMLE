##' .. content for \description{} (Function to generate GARCH time series with t-students distribution innovations) ..
##'
##' .. content for \details{} ..
##' @title T.GARCH
##' @param alpha The vector include the incept and the parameters of autoregression part
##' @param beta The vector include the parameters of conditional variancea
##' @param n The lenght of time series
##' @param rnd The distribution of error(innovation) of GARCH model
##' @param df.t The df of student's t distribution. df.t>0 continuous parameter 
##' @param ntrans burn-in size, i.e. number of initial simulated data to be discarded
##' @author Xiaorui.Zhu
GARCH_t=function (alpha, beta, n = 100, rnd=rt, df.t,ntrans = 100)
{
  if (!missing(beta))
    p = length(beta)
  else p = 0
  if (!missing(alpha))
    q = length(alpha) - 1
  else stop("beta is missing!")
  if (q == 0)
    stop("Check model: q=0!")
  total.n = n + ntrans
  x = double(total.n)
  sigt = x
  d = max(p, q)
  sigma2 = sum(alpha[-1])
  if (p > 0)
    sigma2 = sigma2 + sum(beta)
  if (sigma2 > 1)
    stop("Check model: it does not have finite variance")
  sigma2 = alpha[1] /(1 - sigma2)
  if (sigma2 <= 0)
    stop("Check model: it does not have positive variance")
  x[1:d] = rnd(d, df.t)
  e = rnd(total.n,df.t)
  sigt[1:d] = sigma2
  if (p == 0) {
    for (i in (d + 1):total.n) {
      sigt[i] = sum(alpha * c(1, x[i - (1:q)]^2))
      x[i] = e[i] * sqrt(sigt[i])
    }
  }
  else {
    for (i in (d + 1):total.n) {
      sigt[i] = sum(alpha * c(1, x[i - (1:q)]^2)) + sum(beta * sigt[i - (1:p)])
      x[i] = e[i] * sqrt(sigt[i])
    }
  }
#   return(invisible(x[(ntrans + 1):total.n]))
  list(xt=x[(ntrans + 1):total.n],sigt=sigt[(ntrans + 1):total.n])
}
