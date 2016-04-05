#' This function is the crux of whole adptive quasi maximum likelihood estimation.
#' It estimates degree of freedom \eqn{d_f} from the model residues under specified
#' assumption of distribution family of error. So the input is only model residues.
#'
#' @title Estimation of degree freedom \eqn{d_f} using tQMLE
#'
#' @param e The innovation caculated by estimate parameters
#'
#' @return Return value is the estimator of degree of freedom when the \eqn{\eta_{f} = 1}, which means the asymptotic distribution of sample residues.
#' @export
#'
#' @examples
#' # need revisions.
Estdf <- function(e){
  n <- length(e)
  like <- function(e){
    df.tL <- function(par){
      df <- par[1]
      #tQMLE.e=e/yi
      yi <- 1
      if (df>0)
      {
        #f=gamma((df+1)/2)/((pi*df)^0.5*gamma(df/2))*((df/(df-2))^0.5)*(1+tQMLE.e^2/(df-2))^(-(df+1)/2)
        f <- gamma((df+1)/2)/((pi*df)^0.5*gamma(df/2))*(1+(e/yi)^2/(df))^(-(df+1)/2)
        sum(log(yi)-log(f))/n
      }
      else Inf
    }
    df.tL
  }
  df.qmle <- nlminb(c(0.01),like(e),lower=c(0),upper=Inf)
  return(round(df.qmle$par))
}
