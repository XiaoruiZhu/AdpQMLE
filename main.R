
setwd('Y:/PH.D/Fields/R_packages/AdpQMLE/AdpQMLE') # set the working path as AdpQMLE
dir() # list all the files and folders in this path

plot(GARCH_t(alpha=c(0.1,0.2),beta=0.5,n=100,df.t=2)$xt,type='l')



##########################
library(xts)
xxx <- google.intraday.data("AAPL", freq = 60, period = "2d")
xxx

length(xxx$Open)
trunc(length(xxx$Open)/390)*390

##########################
