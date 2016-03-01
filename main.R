
setwd('Y:/PH.D/Fields/R_packages/AdpQMLE/AdpQMLE') # set the working path as AdpQMLE
dir() # list all the files and folders in this path
######## Let's wrep up whole stuff from the bottom up! ##############
############ 1. The GARCH generating function. ##############
# Test for GARCH_t function #
xx <- GARCH_t(alpha = c(0.1, 0.2), beta = 0.5, n = 100, rnd = "rt", df.t = 2.1)
plot(xx, type = "l")
kurtosis(xx)

##########################

##########################
# Test for MyMLE function #
n <- 1100
a <- c(0.1, 0.5)
b <- c(0.2) # GARCH(1,1) coefficients
e <- rnorm(n)
x <- double(n)
sig2t <- x
x[1:1] <- rnorm(1, sd = sqrt(a[1]/(1.0-a[2]-b[1])))
x[1]
sig2t[1] <- a[1]/(1.0-a[2]-b[1])
sig2t[1]
for(i in 2:n) {
  # Generate GARCH(1,1) process
  sig2t[i] = sum(a * c(1, x[i-1]^2)) + sum(b[1] * sig2t[i - 1])
  x[i] = e[i]*sqrt(sig2t[i])
}

x <- ts(x[101:1100])
x

plot(x, type = "l")
x.arch <- garch(x, order = c(0,2)) # Fit ARCH(2)
####
para <- MyMLE(x)
para

######################################### To be continue ###############

####
summary(x.arch) # Diagnostic tests
plot(x.arch)
data(EuStockMarkets)
dax <- diff(log(EuStockMarkets))[,"DAX"]
dax.garch <- garch(dax) # Fit a GARCH(1,1) to DAX returns
summary(dax.garch) # ARCH effects are filtered. However,
plot(dax.garch) # conditional normality seems to be violated
##########################

library(xts)
xxx <- google.intraday.data("AAPL", freq = 60, period = "1d")
xxx
xxx$Close
length(xxx$Open)
trunc(length(xxx$Open)/390)*390

AAPL.return <- diff(xxx$Close, lag = 1)/xxx$Close[-1]
AAPL.logreturn <- diff(log(xxx$Close), lag=1)

xxx$Close[1:4]
AAPL.return[1:3]
AAPL.logreturn[1:3]
plot(xxx$Close)
plot(AAPL.return)
plot(AAPL.logreturn)
AAPL.logreturn <- AAPL.logreturn[-1]
mean(AAPL.logreturn)
kurtosis(AAPL.logreturn)
h <- length(AAPL.logreturn)
h

AAPL.estm <- A_tQMLE(h, x <- AAPL.logreturn)
AAPL.estm

##########################

h=250
source("A_tQMLE.R")
a1=c(0.02,0.6); b1=0.3; dfsim=2
a2=c(0.02,0.05); b2=0.9
source('EstmBestdf.R')
whole.X <- GARCH_t(alpha=a1,beta=b1,rnd=rt,df.t=dfsim,n=h)
x <- whole.X$xt
plot(x,type='l')
sigt.x <- whole.X$sigt
plot(sigt.x, type = "l")

library(fBasics)
mean(x);var(x);kurtosis(x)
estimation <- A_tQMLE(h,x)
estimation

MyMLE(h,x)

# source("MyMLE.R")
## round(MyMLE(h,x)$mle.N,digits=3)
## MytQMLE(h,x,dfest=dfsim)

## 测试普通的garch下MYMLE能否估计准
## library(TSA)
## xtest=garch.sim(alpha=a1,beta=b1,n=h)
## plot(xtest,type='l')
## mean(xtest);var(xtest);kurtosis(xtest)
## round(MyMLE(h,xtest)$mle.N,digits=3)
## MytQMLE(h,xtest,dfest=100)

# 生成要用的序列，t error，parameter=,以及估计参数矩阵，best df矩阵
source("EstmBestdf.R")
Estm=matrix(NA,11,3)
Bdf.t=rep(NA,11)
Bdf.t
# 第一步！！！！！
sim1my=MyMLE(h,x)
Estm[1,]=sim1my$mle.N
Estm[1,]
source("YITAtQMLE.R")
source("com.e.R")
e.t=com.e(Estm[1,],x)
max(e.t);min(e.t)
mean(e.t); var(e.t); kurtosis(e.t);## plot(e.t,type='l')
hist(e.t)
library(fBasics)
normalTest(e.t,method='jb')
# 计算MLE得出的残差e.t，这个是需要来寻找最优f的参数的，yita=1作为边界！
Qt.df=c(0.5,0.9,1,1.4,1.8,2,2.2,2.4,2.6,2.8,3,3.3,3.5,3.8,4,4.3,4.5,4.8,5,5.5,6,7,8,9,10,11,15,20)
samplesYITA=rep(NA,length(Qt.df))
Bdf.t[1]=50
for (i in 1:length(Qt.df)){
  samplesYITA[i]=YITAtQMLE(h=h,e=e.t,dfest=Qt.df[i])
}
# 接下来用理论的rt残差与之对比看yita大概走向
wherebestYita=which(min(abs(samplesYITA-1))==abs(samplesYITA-1))
Bdf.t[2]=Qt.df[wherebestYita]
Bdf.t[2]
samplesYITA
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
round(Estm,digits=4)
Bdf.t

## estpara7=Estm[7,]
## estpara7
## e.t=com.e(estpara7,x)
## max(e.t);min(e.t)
## mean(e.t); var(e.t); kurtosis(e.t);plot(e.t,type='l')
## library(fBasics)
## normalTest(e.t,method='jb')

#做对比

dfsim
MyMLE(h,x)
NGQMLE(h,x,dfest=dfsim,Estm[1,])
source('MytQMLE.R')
testEst=MytQMLE(h,x,dfest=dfsim)$qmle.N
testEst

###测试出现问题的地方，直接一下df收敛太快超过了真实，比如t5直接得t3了如何往回搜索
## testEst=MytQMLE(h,x,dfest=15)$qmle.N
## testEst
## teste.t=com.e(testEst,x)
## mean(e.t); var(e.t); kurtosis(e.t)
## mean(teste.t); var(teste.t); kurtosis(teste.t)
## library(fBasics)
## normalTest(e.t,method='jb')
## normalTest(teste.t,method='jb')
## # 计算MLE得出的残差e.t，这个是需要来寻找最优f的参数的，yita=1作为边界！
## Qt.df=c(0.3,0.5,0.7,0.9,1,1.2,1.4,1.6,1.8,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,8,9,10,11,15,20,30,100)
## samplesYITA=rep(NA,length(Qt.df))
## Bdf.t[1]=200
## for (i in 1:length(Qt.df)){
##   samplesYITA[i]=YITAtQMLE(h=h,e=teste.t,dfest=Qt.df[i])
## }
## # 接下来用理论的rt残差与之对比看yita大概走向
## wherebestYita=which(min(abs(samplesYITA-1))==abs(samplesYITA-1))
## Bdf.t[2]=Qt.df[wherebestYita]
## Bdf.t[2]
## samplesYITA



ls()
rm(Bdf.t,Bdf.t3,Bdf.t4,Bdf.t5,Bdf.t6,Bdf.t7,t,est2,Estm3,Estm4,Estm5,Estm6,Estm7)

# 收敛啦，哈哈哈哈哈哈哈
#######test######
e.tfinal=com.e(Estm5,x=t)
plot(e.tfinal,type='l')
kurtosis(e.tfinal)
kurtosis(rt(h,dfsim))
# 很好，反应了残差接近t(2)的了，但是样本反映的只是一部分所以并不能完全测出真实分布




# 这块暂时不用，上面用笨办法直接算23次，得出最近的一个yita找到对应的t分布是哪个参数的，
# 然后用这个参数的tQMLE进行下一步估计，再算yita再估
yi.t.e=matrix(NA,28,2)
yi.t.e[1,1]=30
for (j in 1:27){
  yi.t.e[j,2]=YITAtQMLE(h=h,e=e.t,dfest=yi.t.e[j,1])
  if (yi.t.e[j,2]<1) {
    yi.t.e[j+1,1]=yi.t.e[j,1]-1
  }
  else yi.t.e[j+1,1]=yi.t.e[j,1]+1
}
yi.t.e
