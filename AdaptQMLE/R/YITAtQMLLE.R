#' This function is used to estimate scale parameter \eqn{\eta_f} from the model residues under specified assumption of distribution of error. So the input includes degree of freedom and model residues.
#'
#' @title Estimation of scale parameter \eqn{\eta_f} using tQMLE
#'
#' @param e The innovation caculated by estimate parameters
#' @param dfest The parameter of Quais likelihood. Continuous parameter dfest>2 so that finite variance
#'
#' @return The estimator of eta
#' @export
#'
#' @examples
#' # need revisions
YITAtQMLE <- function(e,dfest){
  n <- length(e)
  like <- function(e,df){
    yitatL <- function(par){
      yi <- par[1]
      #tQMLE.e=e/yi
      if (yi>0)
      {
        #f=gamma((df+1)/2)/((pi*df)^0.5*gamma(df/2))*((df/(df-2))^0.5)*(1+tQMLE.e^2/(df-2))^(-(df+1)/2)
        f <- gamma((df+1)/2)/((pi*df)^0.5*gamma(df/2))*(1+(e/yi)^2/(df))^(-(df+1)/2)
        sum(log(yi)-log(f))/n
      }
      else Inf
    }
    yitatL
  }
  yitqmle <- nlminb(c(0.01),like(e,df=dfest),lower=c(0),upper=Inf)
  return(yitqmle$par)
}
