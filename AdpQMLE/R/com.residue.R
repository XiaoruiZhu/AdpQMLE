#' com.residue is a function used to calculate the residue of GARCH model so that the futher adaptive procedure can be move forward.  
#'
#' @param para is the parameters of GARCH model. Here only consider the GARCH(1,1), so there are three parameters. But this whole stuff should be revised for general models.
#' @param x is the time series. It could be daily stock returns, minutely stock returns, or monthly exchange rate.  
#'
#' @return  is the residues between the predicted time series and the original time series. 
#'
#' @examples
com.residue <- function(para,x){
  n <- length(x)
  sig2 <- numeric(n); tem1 <- numeric(1); tem2 <- numeric(1)
  w <- para[1]
  alpha <- para[2]
  beta <- para[3]
  if (w==0) sig2[1] <- abs(x[1])
  else sig2[1] <- w/(1-alpha-beta)
  for (t in 2:n){
    # tem1=tem1+beta^(t-2)
    # tem2=beta*(tem2)+x[t-1]^2
    # sig[t]=w/(1-beta)+alpha*(tem2)+beta^(t-1)*sig[1]
    tem2 <- beta*(tem2+tem1)
    tem1=x[t-1]^2
    sig2[t]=(w/(1-beta)+alpha*(tem1+tem2)) 
    #This part can be changed for GARCH(P,Q)
  }
  e=x/(sqrt(sig2))
  return(e)
}