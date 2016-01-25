# h is the number of samples
# e is the innovation caculated by estimate parameters
# dfest is the parameter of Quais likelihood. dfest>0 as R default(not >2 in Fan's paper) continuous parameter
# x is time series
YITAtQMLE=function(h,e,dfest){
  # yita的tQMLE的估计
  #df=dfest
  like=function(e,df){
    yitatL=function(par){
      yi=par[1]
      #tQMLE.e=e/yi
      if (yi>0) 
      {
        #     f=gamma((df+1)/2)/((pi*df)^0.5*gamma(df/2))*((df/(df-2))^0.5)*(1+tQMLE.e^2/(df-2))^(-(df+1)/2)
        #     ###这里用了t分布的分布密度,参照网上的，df》0的那种pdf，不要考虑Fan文章，然后对比时候将fan文章的减2对比
        f=gamma((df+1)/2)/((pi*df)^0.5*gamma(df/2))*(1+(e/yi)^2/(df))^(-(df+1)/2)
        sum(log(yi)-log(f))/h
      }
      else Inf
    }
    yitatL
  }
  yitqmle=nlminb(c(0.01),like(e,df=dfest),lower=c(0),upper=Inf)
  return(yitqmle$par)
}
