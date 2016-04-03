#' The Log-likelihood function of linear model. Here is the y=a + b*x. Need to be changed.
#'
#' @title Log-likelihood function of single linear model.
#' @param X The independent variable x without intercept.
#' @param y The dependent variable y.
#'
#' @return Log-likelihood function
LogL_Linear_Norm <- function(X, y){
  Linear_Norm <- function(para){
    n <- nrow(X)
    m <- ncol(X)+1
    X <- cbind(rep(1,times = n), X)
    alpha <- para[1]
    beta <- para[2]
    s <- sd(y, na.rm = FALSE)
    e <- y - X %*% as.matrix(para, m, 1)
    return(n*log(s) + 1/(2*s^2) * sum(e^2))
  }
  Linear_Norm
}
