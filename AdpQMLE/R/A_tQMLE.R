#' This is the function which can be used to estimate parameters of all kinds of models. The critical idea of this methods is to estimate parameters according to data themselves with unspecified assumption of error's distribution. 
#' 
#' @title Adaptive Quasi Maximum Likelihood Estimation
#' 
#' @param h 
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
A_tQMLE <- function(series, order = c(1,1)){
  # This part need to be changed and revised for efficient calculation.
  max.iter <- 50
  if (missing(series)) {
    stop("No input data for 'series'!")
  }
  q <- order[1]; p <- order[2]
  n <- length(series)
  Bdf.t <- c()
  # Estm contains parameters, est.df, YITA and SSE(total sum square error)
  Estm <- matrix(NA, max.iter, (p+q+1+3))
  # First step
  # but it's demand caution to make this whole adaptive-QMLE straigtforward. 
  Est.model <- tQMLE(series=series, LogLFunc = "LogL_GARCH_Norm", order = order) 
  para <- Est.model$QMLE.N
  e.t <- Est.model$e
  # y.hat <- sqrt(Est.model$sigma.sq)
  # diff <- sum((y.hat-series)^2)/n
  Estm[1,] <- c(Est.model$QMLE.N, NA, NA, NA) 
  # alpha <- Estm[1,1:(q+1)]; beta <- Estm[1, (q+2):(p+q+1)]
  # Second step!!
  # The scale parameter yita_f is the boundary. Equal to 1 is the condition.  
  new.df <- Estdf(e.t)
  new.YITA <- YITAtQMLE(e=e.t,dfest=new.df)
#   Qt.df=c(2.2,2.4,2.6,2.8,3,3.3,3.5,3.8,4,4.3,4.5,4.8,5,5.5,6,7,8,9,10,11,15,20,30,50,60,80)
#   ndf=length(Qt.df)
#   k=rep(NA,1)
#   samplesYITA=rep(NA,ndf)
#   Bdf.t[1]=100
#   for (i in 1:ndf){
#     samplesYITA[i] <- YITAtQMLE(e=e.t,dfest=Qt.df[i])
#   }
#   # 接下来用理论的rt残差与之对比看yita大概走向
#   new.df <- Qt.df[which(min(abs(samplesYITA-1))==abs(samplesYITA-1))]
#   new.YITA <- samplesYITA[which(min(abs(samplesYITA-1))==abs(samplesYITA-1))]
#   
  # Bdf.t <- c(Bdf.t, new.df)
  # 第二步开始估计，用的是tQMLE，其中t分布用的参数时第一步估计出来的Bestdf.t
  # source('EstmBestdf.R')
  iter = 1
  while (iter<max.iter & diff > 0.001) {
    iter <-  iter+1
    Est.model <- tQMLE(series = series, LogLFunc = "LogL_GARCH_t", order = order, dfest = new.df)  
    e.t <- Est.model$e
    # y.hat <- sqrt(Est.model$sigma.sq)
    # new.diff <- sum((y.hat-series)^2)/n
    new.para <- Est.model$QMLE.t
    new.diff <- sum((para-new.para)^2)/(p+q+1)
    Estm[iter,] <- c(Est.model$QMLE.t, new.df, new.YITA, new.diff)
    new.df <- Estdf(e.t)
    new.YITA <- YITAtQMLE(e=e.t,dfest=new.df) # This is the crux! Using the assumed df to estimate yita!
    diff <- new.diff
  }
  return(Estm)
}

