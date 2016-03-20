#' Example to generate GARCH(1,1) with normal innovation.
#'
#' @param n series number
#' @param a alpha include intercept and alpha1
#' @param b beta
#'
#' @return
#' @export
#'
#' @examples
GARCH1_1 <- function(n, a, b){
  e <- rnorm(n)
  x <- double(n)
  sig2t <- x
  x[1:1] <- rnorm(1, sd = sqrt(a[1]/(1.0-a[2]-b[1])))
  x[1]
  sig2t[1] <- a[1]/(1.0-a[2]-b[1])
  sig2t[1]
  for(i in 2:n) {
    # Generate GARCH(1,1) process
    sig2t[i] = sum(a * c(1, x[i-1]^2)) + sum(b[1] * sig2t[i - 1])
    x[i] = e[i]*sqrt(sig2t[i])
  }
  list(series = x, e = e, sig2t = sig2t)
}
