#' Example of optimx and nlminb functions.
#'
#' @return
#'
#' @examples
optimx_example <- function(){
  costFunc = function (x)
  {
    return(sum((x-1.3)^4))
  }
  
  grad = function(x) {
    return(4*(x-1.3)^3)
  }
  
  library(optimx)
  startTime = proc.time()
  results = optimx(seq(1, 50,.1), fn=costFunc, gr = grad, itnmax=50, method=c("Rcgmin"))
  proc.time() - startTime
  
  x <- seq(1, 50, .1)
  
  startTime = proc.time()
  results2 <- nlminb(start = c(0), objective = costFunc, lower = -Inf, upper = Inf)
  proc.time() - startTime
  results
  results2
}
