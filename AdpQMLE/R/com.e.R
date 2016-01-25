com.e=function(para,x){
  n=length(x)
  sig2=numeric(n);tem1=numeric(1);tem2=numeric(1)
  w=para[1]
  alpha=para[2]
  beta=para[3]
  if (w==0) sig2[1]=abs(x[1])
  else sig2[1]=w/(1-alpha-beta)
  for (t in 2:n){
    #     tem1=tem1+beta^(t-2)
    #     tem2=beta*(tem2)+x[t-1]^2
    #     sig[t]=w/(1-beta)+alpha*(tem2)+beta^(t-1)*sig[1]
    tem2=beta*(tem2+tem1)
    tem1=x[t-1]^2
    sig2[t]=(w/(1-beta)+alpha*(tem1+tem2)) #这按照笔记红色的通项公式改写可计算GARCH(P,Q)
  }
  e=x/(sqrt(sig2))
  return(e)
}
