#' com.residue is a function used to calculate the residue of GARCH model so that the futher adaptive procedure can be move forward.  
#'
#' @param alpha Includes intercept and all parameters of ARCH terms. 
#' @param beta Includes all parameters of GARCH terms. 
#' @param series Time series.
#'
#' @return  Return a list contains estimators, residues and conditional sigma square. 
#'
#' @examples
com.residue <- function(alpha, beta, series){
  n <- length(series)
  sig2 <- numeric(n)
  q <- length(alpha)-1
  p <- length(beta)
  d <- max(q,p)
  if (alpha[1]==0) sig2[1:d] <- abs(series[1:d])
  else sig2[1:d] <- alpha[1]/(1.0-sum(alpha[2:(q+1)])-sum(beta))
  for (t in (d+1):n){
    sig2[t] <-  sum(alpha * c(1, series[t - (1:q)]^2)) + sum(beta * sig2[t - (1:p)])
    # It is changed based on my NOTES in red GARCH(P,Q)  
  }
  e <- series/(sqrt(sig2))
  list(sig.sq = sig2, e=e)
}