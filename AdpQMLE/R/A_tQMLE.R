
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
  # 生成要用的序列，t error，parameter=,以及估计参数矩阵，best df矩阵
  Estm=matrix(NA,11,3)
  Bdf.t=rep(NA,11)
  # 第一步！！！！！
  Estm[1,]=MyMLE(h,x)$mle.N
  e.t=com.e(Estm[1,],x)
  # 计算MLE得出的残差e.t，这个是需要来寻找最优f的参数的，yita=1作为边界！
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
  source('EstmBestdf.R')
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

