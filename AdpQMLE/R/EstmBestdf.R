# 先用已得的Bestdf.t来tQMLE估计参数，用参数计算e，在计算yita一堆，找到最接近1的Bestdf.t
EstmBestdf=function(h,t,bestdf){
  Nest=MytQMLE(h,t,dfest=bestdf)$qmle.N
  e.t=com.e(Nest,x=t)
  # 计算MLE得出的残差e.t，这个是需要来寻找最优f的参数的，yita=1作为边界！
  Qt.df=c(2.2,2.4,2.6,2.8,3,3.3,3.5,3.8,4,4.3,4.5,4.8,5,5.5,6,7,8,9,10,11,15,20,30,50,60,80)
  samplesYITA=rep(NA,length(Qt.df))
  for (i in 1:length(Qt.df)){
    samplesYITA[i]=YITAtQMLE(h=h,e=e.t,dfest=Qt.df[i])
  }
  wherebestYita=which(min(abs(samplesYITA-1))==abs(samplesYITA-1))
  Bdf.t=Qt.df[wherebestYita]
  names(samplesYITA)=c(2.2,2.4,2.6,2.8,3,3.3,3.5,3.8,4,4.3,4.5,4.8,5,5.5,6,7,8,9,10,11,15,20,30,50,60,80)
  #   yitaall=samplesYITA[(wherebestYita-3):(wherebestYita+3)]
  list(Est=Nest,B.df=Bdf.t,samYita=samplesYITA)
  #        YITAall=yitaall)
}
