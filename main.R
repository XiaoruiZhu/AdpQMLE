
######## Let's wrep up whole stuff from the bottom up! ##############

############ 1. The GARCH generating function. ##############
# Test for GARCH_Gene function #

# student's t error
xx <- GARCH_Gene(alpha = c(0.1, 0.2), beta = 0.5, n = 1000, rnd = "rt", df.t = 2.3)
# head(xx)
plot(xx$x, type = "l")
plot(xx$sig.sq, type = "l")
library(timeDate)
kurtosis(xx$x)

# Normal error 
xxnorm <- GARCH_Gene(alpha = c(0.1, 0.2), beta = 0.5, n = 1000, rnd = "rnorm")
plot(xxnorm$x, type = 'l')

# Pearson Type IV error
# m, nu, location(lambda), scale(a)
library(PearsonDS)
paraPIV <- list(m=4, nu=-2, location=0, scale=1)
paraPIV
x.PIV <- GARCH_Gene(alpha = c(0.1, 0.2), beta = 0.5, n = 1000, rnd = "rpearsonIV", params.PIV = paraPIV)
hist(x.PIV$x)
plot(x.PIV$x, type = 'l')

test.PIV <- rpearsonIV(1000, params=paraPIV)
hist(test.PIV,probability = TRUE, breaks = 20)

###########     OK, END      ###############

########### 2. The basic MLE method testing. ###############
# Test for GARCH(1,1) with normal innovation
alpha <- c(0.1, 0.5); beta <- c(0.2) # GARCH(1,1) coefficients
x.e.sig <- GARCH1_1(n=1000, a = alpha, b = beta)
x <- x.e.sig$series
plot(x, type = "l")
x.arch <- garch(x, order = c(1,1)) 
est1 <- MLE(y = x, LogLFunc = LogL_GARCH_Norm, order = c(1,1))
est1

est2 <- QMLE(series = x, LogLFunc = LogL_GARCH_t, order = c(1,1), dfest = 20)
est2$QMLE.t
plot(est2$e, type='l')

est3 <- QMLE(series = x, order = c(1,1))
summary(est3)
est3$QMLE.N

# This is too bad.

# Test for GARCH(1,1) with t innovation #
xx <- GARCH_Gene(alpha = c(0.1, 0.4), beta = 0.4, n = 2000, rnd = "rt", df.t = 4)
y <- xx$x
plot(y, type = "l")
library(tseries)
x.arch <- garch(y, order = c(1,1)) 
x.arch
est1 <- MLE(y = y, LogLFunc = LogL_GARCH_Norm, order = c(1,1))
est1
est2 <- QMLE(series = y, LogLFunc = "LogL_GARCH_Norm", order = c(1,1))
est2$QMLE.N

est3 <- QMLE(series = y, LogLFunc = "LogL_GARCH_t", order = c(1,1), dfest = 4)
est3$QMLE.t

# Test for GARCH(2,1)
xx <- GARCH_Gene(alpha = c(0.1, 0.1, 0.1), beta = 0.5, n = 1000, rnd = "rt", df.t = 5)
y2 <- xx$x

plot(y2, type = "l")

x.arch <- garch(y2, order = c(2,1)) # Fit GARCH(2,1)
x.arch
est1 <- MLE(y = y2, LogLFunc = "LogL_GARCH_Norm", order = c(2,1))
est1
est2 <- QMLE(series = y2, LogLFunc = "LogL_GARCH_t", order = c(2,1), dfest = 5)
est2$QMLE.t

# Finished test for MLE and GARCH with normal innovation.
# Test QMLE for GARCH with t
# Done, with known degree of freedom of t innovation, the QMLE is better than MLE with normal

#############################################
# Test QMLE for GARCH with Pearson's type IV error
library(PearsonDS)
paraPIV <- list(m=2, nu=-2, location=0, scale=1)
paraPIV
x.PIV <- GARCH_Gene(alpha = c(0.1, 0.2), beta = 0.5, n = 1000, rnd = "rpearsonIV", params.PIV = paraPIV)
hist(x.PIV$x)
plot(x.PIV$x, type = 'l')

test.PIV <- rpearsonIV(1000, params=paraPIV)
hist(test.PIV,probability = TRUE, breaks = 20)

# Quasi-maximum likelihood estimation
library(tseries)
x.arch <- garch(x.PIV$x, order = c(1,1)) # Fit GARCH(2,1)
x.arch
est1 <- MLE(y = x.PIV$x, LogLFunc = "LogL_GARCH_Norm", order = c(1,1))
est1
est2 <- QMLE(series = x.PIV$x, LogLFunc = "LogL_GARCH_t", order = c(1,1), dfest = 5)
est2$QMLE.t
library(pracma)
est3 <- QMLE(series = x.PIV$x, LogLFunc = "LogL_GARCH_PIV", order = c(1,1), params.PIV = paraPIV)
est3$QMLE.PIV
# For more samples, PIV works pretty good. 
est <- A_tQMLE(series = x.PIV$x, order = c(1,1))
est
library(timeDate)
kurtosis(test.PIV)
kurtosis(rt(5000, df = 3.59))

###########################################################################
# Simulation part 1.
###########################################################################
# Assumptions:
ini.alpha <- c(0.1, 0.2); ini.beta <- 0.3; ini.n <- 2000; ini.df.t <- 5

xx <- GARCH_Gene(alpha = ini.alpha, beta = ini.beta, n = ini.n, rnd = "rt", df.t = ini.df.t)
y <- xx$x
plot(y, type = "l")
testest <- QMLE(series = y, LogLFunc = "LogL_GARCH_t", order = c(1,1), dfest = ini.df.t)
testest$QMLE.t
Sim.A <- A_tQMLE(series = y, order = c(1,1))
Sim.A
l <- nrow(Sim.A)
sq.diff <- (Sim.A[, 1:3] - matrix(c(ini.alpha, ini.beta), l, 3, byrow = TRUE))^2
RMSD <- apply(sq.diff, 1, sum)
RMSD
# 
irat <- length(Sim.A[,"est.df"])
est.df <- Sim.A[,"est.df"]
d <- data.frame(No = seq(1:length(RMSD)), 
                df = c(est.df[1:irat], rep(est.df[irat], l-irat)),
                RMSD = RMSD)
min.df <- est.df[irat]; max.df <- est.df[2]

par(mar = c(5,5,2,5))
with(d, plot(No, df, type= 'l',lty=1, col="blue",  
             ylab=expression(df),
             ylim=c(ini.df.t,max.df)))
par(new = T)
abline(h=ini.df.t,col="red",lty=3)
par(new = T)
with(d, plot(No, RMSD, type='l', lty=2, axes=F, xlab=NA, ylab=NA, cex=1.2))
axis(side = 4)
mtext(side = 4, line = 3, 'RMSD')
legend("topright",
       legend=c("df", "RMSD", "Real df"), cex = 0.8,
       lty=c(1,2,3), col=c("blue", "black", "red"))

# par(mfrow=c(2,1))
# slope.sample <- replicate(1e4, coefficients(sim.lin.gauss(model=TRUE))["x"])
# hist(slope.sample,freq=FALSE,breaks=50,xlab=expression(hat(beta)[1]),main="")
# curve(dnorm(x,-2,sd=sqrt(3/(n*var(fixed.x)))), add=TRUE, col="blue")
# pred.sample <- replicate(1e4, predict(sim.lin.gauss(model=TRUE),
#                                       newdata=data.frame(x=-1)))
# hist(pred.sample, freq=FALSE, breaks=50, xlab=expression(hat(m)(-1)),main="")
# curve(dnorm(x, mean=beta.0+beta.1*(-1),
#             sd=sqrt((sigma.sq/n)*(1+(-1-mean(fixed.x))^2/var(fixed.x)))),
#       add=TRUE,col="blue")

#####################################################

############# 3. com.residue function writing and testing ###################
n <- 1000; df <- 4
teste.t <- rt(n,df)
library(timeDate)
mean(teste.t); var(teste.t); kurtosis(teste.t)
Qt.df=c(0.3,0.5,0.7,0.9,1,1.2,1.4,1.6,1.8,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,8,9,10,11,15,20,30,100)
samplesYITA=rep(NA,length(Qt.df)) 

# Bdf.t[1]=200
for (i in 1:length(Qt.df)){
  samplesYITA[i]=YITAtQMLE(e=teste.t,dfest=Qt.df[i])
  }
# 接下来用理论的rt残差与之对比看yita大概走向
samplesYITA
wherebestYita=which(min(abs(samplesYITA-1))==abs(samplesYITA-1))

newdf <- Qt.df[wherebestYita]
testYITA=YITAtQMLE(e=teste.t,dfest=newdf)
testYITA
newdf <- newdf/testYITA^3
testYITA=YITAtQMLE(e=teste.t,dfest=newdf)
testYITA
newdf <- newdf/testYITA^3
testYITA=YITAtQMLE(e=teste.t,dfest=newdf)
testYITA
newdf
################################


############# 5. A_tQMLE function writing and testing ###################
# Need to change the A_tQMLE function so that this function can optimize $\theta$ 
# and degree of freedom simultateously. Think over the "Collaborative Flitering Algothrim" 
# in the Machine Learning course.

################################
xx <- GARCH_t(alpha = c(0.1, 0.3), beta = 0.4, n = 1000, rnd = "rt", df.t = 4)
y <- xx$x
plot(y, type = "l")
library(tseries)
x.arch <- garch(y, order = c(1,1)) 
est.Norm <- QMLE(series = y, LogLFunc = "LogL_GARCH_Norm", order = c(1,1))
est.Norm$QMLE.N
e.norm <- est.Norm$e
plot(e.norm,type='l')
YITAtQMLE(e=e.norm, dfest = 80)
Estdf(e.norm)

est <- A_tQMLE(series = y, order = c(1,1))
est

est2 <- QMLE(series = y, LogLFunc = LogL_GARCH_t, order = c(1,1), dfest = 4)
est2$QMLE.t
############# 6. YITAtQMLE function writing and testing ###################
# Done!
################################

############# 7. google.intraday.data testing ###################
library('xts')
AMZN <- google.intraday.data(symbol = "AMZN", freq = 60, period =12)
head(AMZN)
length(AMZN[,1])

# length(AMZN[,1])/(6.5*60)
# length(AMZN[,1])
# # AMZN$minute_updown <- AMZN$Close -AMZN$Open
# AMZN$within_minuteR <- log(AMZN$Close/AMZN$Open)
# lagClose <- lag(AMZN$Close, k = 1)
# head(lagClose)
# AMZN$next_minute_return <- log(AMZN$Open/lagClose) 
# 
# temp <- rep(0, length(AMZN[,1]))
# 
# temp[(lagClose - AMZN$Open)<0] <- 3
# temp[(lagClose - AMZN$Open)==0] <- 2
# temp[(lagClose - AMZN$Open)>0] <- 1
# head(temp)
# 
# AMZN$updown_tplus1 <- lag(temp, k = -1) 
# head(AMZN)
# summary(AMZN$next_minute_return)
# hist(AMZN$next_minute_return, breaks = 20, xlim = c(-0.005, 0.005))
# AMZN <- AMZN[-1]
# length(AMZN[,1])
# write.csv(AMZN, file = "AMZN.csv") 

# split(AMZN, f = 'days')
################################
################################

################################
################################

########### To be continue ###############
