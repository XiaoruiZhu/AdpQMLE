#' The Log-likelihood function of GARCH model with Pearson's Type IV (PIV) distributed error
#'
#' @title log-likelihood function of GARCH(p,q) with PIV distribution
#'
#' @param series The time series that need to be fitted.
#' @param p The order of GARCH terms \eqn{\sigma^2}
#' @param q The order of ARCH terms \eqn{\epsilon^2}
#' @param lambda \eqn{\lambda} is the location parameter of Pearson's Type IV distribution 
#' @param a \eqn{a} is the scale parameters of PIV distribution with condition \eqn{a > 0}
#' @param nu \eqn{\nu} parameter of PIV distribution. \eqn{\nu} is related to the
#'        asymmetry of the distribution, and a positive (or negative) \eqn{\nu} stands 
#'        for a negatively (or positively) skewed distribution
#' 
#' @param m The \eqn{m} parameter of PIV distribution with condition \eqn{m > 1/2}. m 
#'        captures the leptokurtosis of the distribution, and a smaller value of m represents 
#'        a heavier tail of the distribution
#'
#' @importFrom pracma gammaz
#' 
#' @return Log-Likelihood function of GARCH with t error
#'
LogL_GARCH_PIV <- function(series, p, q, lambda, a, nu, m){
  GARCH_e_PIV <- function(para){
    n <- length(series)
    sig2 <- numeric(n)
    alpha <- para[1:(q+1)]
    beta <- para[(q+2):(q+1+p)]
    d <- max(q,p)
    sig2[1:(d)] <- alpha[1]/(1.0-sum(alpha[2:(q+1)])-sum(beta))
    for (t in (d+1):n) {
      sig2[t] <-  sum(alpha * c(1, series[t - (1:q)]^2)) + sum(beta * sig2[t - (1:p)])
    }
    if (c(alpha,beta) > rep(0,(q+p+1)) && (sum(alpha[2:(q+1)])+sum(beta)) < 1 && sig2>=0)
    {
      e <- series / (sqrt(sig2))
      # Here is an issue!!!
      cmplx <- complex(real = m, imaginary = nu/2)
      K <- 2^(2*m-2)*abs(gammaz(cmplx))^2/(a*pi*gammaz(2*m-1))
      f <- K * (1 + ((e - lambda)/a)^2)^(-m) * exp(-nu * atan((e - lambda)/a))
      return(sum(log(sqrt(sig2))-log(f)))
    }
    else return(Inf)
  }
  GARCH_e_PIV
}
