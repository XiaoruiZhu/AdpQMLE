#' Adaptive quasi-maximum likelihood estimation 
#' 
#' This estimation method works for GARCH(p,q) model with Pearson's type IV innovation.
#'
#' @param series The original time-series that need to be fitted as GARCH model
#' @param order The GARCH model orders. Includes GARCH terms and ARCH terms.
#'
#' @return Estimated parameters.
#' @export
#'
#' 
A_PIVQMLE <- function(series, order = c(1,1)){
  return()
}