#' This is the function which can be used to estimate parameters of garch(1,1)
#' 
#' Adaptive Quasi Maximum Likelihood Estimation
#' 
#' @param h 
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
A_tQMLE=function(h,x){
  # This part need to be changed and revised for efficient calculation.
  Estm=matrix(NA,11,3)
  Bdf.t=rep(NA,11)
  # First step
  # but it's demand caution to make this whole adaptive-QMLE straigtforward. 
  Estm[1,]=MyMLE(h,x)$mle.N
  e.t=com.residue(Estm[1,],x)
  # Second step!!
  # Calculation of residues. The function is com.residue()
  # The scale parameter yita_f is the boundary. Equal to 1 is the condition.  
  ############## here is a big issue ###################
  ############## how to make this step more efficient #######
  ############## Think over the gradient descent ############
  Qt.df=c(2.2,2.4,2.6,2.8,3,3.3,3.5,3.8,4,4.3,4.5,4.8,5,5.5,6,7,8,9,10,11,15,20,30,50,60,80)
  ndf=length(Qt.df)
  k=rep(NA,1)
  samplesYITA=rep(NA,ndf)
  Bdf.t[1]=100
  for (i in 1:ndf){
    samplesYITA[i]=YITAtQMLE(h=h,e=e.t,dfest=Qt.df[i])
  }
  # 接下来用理论的rt残差与之对比看yita大概走向
  Bdf.t[2]=Qt.df[which(min(abs(samplesYITA-1))==abs(samplesYITA-1))]
  # 第二步开始估计，用的是tQMLE，其中t分布用的参数时第一步估计出来的Bestdf.t
  # source('EstmBestdf.R')
  for (j in 2:11){
    EstmBdf=EstmBestdf(h,x,bestdf=Bdf.t[j])
    Estm[j,]=EstmBdf$Est
    Bdf.t[j+1]=EstmBdf$B.df
    if ((Bdf.t[j]==Bdf.t[j-1])|(j==11)){
      k=j
      break}
  }
  Para.df=c(Estm[k,],Bdf.t[k])
  return(Para.df)
}

