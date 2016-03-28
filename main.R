
######## Let's wrep up whole stuff from the bottom up! ##############

############ 1. The GARCH generating function. ##############
# Test for GARCH_t function #
xx <- GARCH_t(alpha = c(0.1, 0.2), beta = 0.5, n = 1000, rnd = "rt", df.t = 2.3)
# head(xx)
plot(xx$x, type = "l")
plot(xx$sig.sq, type = "l")
library(timeDate)
kurtosis(xx$x)
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

est2 <- tQMLE(series = x, LogLFunc = LogL_GARCH_t, order = c(1,1), dfest = 20)
est2
# This is too bad.

# Test for GARCH(1,1) with t innovation #
xx <- GARCH_t(alpha = c(0.1, 0.4), beta = 0.4, n = 2000, rnd = "rt", df.t = 4)
y <- xx$x
plot(y, type = "l")

x.arch <- garch(y, order = c(1,1)) 
est1 <- MLE(y = y, LogLFunc = LogL_GARCH_Norm, order = c(1,1))
est1

est2 <- tQMLE(series = y, LogLFunc = LogL_GARCH_t, order = c(1,1), dfest = 4)
est2

# Test for GARCH(2,1)
xx <- GARCH_t(alpha = c(0.1, 0.1, 0.1), beta = 0.5, n = 1000, rnd = "rt", df.t = 5)
y <- xx$x

plot(y, type = "l")

x.arch <- garch(y, order = c(2,1)) # Fit GARCH(2,1)
est1 <- MLE(y = y, LogLFunc = LogL_GARCH_Norm, order = c(2,1))
est1
est2 <- tQMLE(series = y, LogLFunc = LogL_GARCH_t, order = c(2,1), dfest = 5)
est2

# Finished test for MLE and GARCH with normal innovation.
# Test tQMLE for GARCH with t
# Done, with known degree of freedom of t innovation, the tQMLE is better than MLE with normal
###########################################################################

###########################################################################
# 3. Creating MLE for linear regression and test #####

# Fix x values for all runs of the simulation; draw from an exponential
n <- 2000 # So we don't have magic #s floating around
beta.0 <- 5
beta.1 <- -2
sigma.sq <- 1
fixed.x <- rexp(n=n)
# Simulate from the model Y=\beta_0+\beta_1*x+N(0,\sigma^2)
# Inputs: intercept; slope; variance; vector of x; return sample or estimated
data <- gen.lin(intercept=beta.0, slope = beta.1, noise.variance=sigma.sq, x=fixed.x, dis.error = "rt", dft=3)
par(mfrow=c(1,1))
hist(fixed.x, freq = FALSE, breaks = 50, xlab = expression(x[i]), main = "")
hist(data$y, freq = FALSE, breaks = 50, xlab = expression(y[i]), main = "")
plot(data$x, data$y)
test <- lm(data$y~data$x)
test
y <- as.matrix(data$y); X <- as.matrix(data$x)
myMLE <- MLE(y, X, LogLFunc = "LogL_Linear_Norm")
myMLE
# Very good example for single linear regression by suing "MLE" function.

# 
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

################################


############# 5. EstmBestdf function writing and testing ###################
################################

############# 6. YITAtQMLE function writing and testing ###################
################################

############# 7. google.intraday.data testing ###################
library('xts')
# AMZN <- google.intraday.data(symbol = "AMZN", freq = 60, period =12)
# head(AMZN)
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
# Get Data function.
# JPM <- as.xts(get.hist.quote("JPM",start="2000-01-02",
#                              provider ="yahoo",
#                              quote=c("Open", "High", "Low", "Close","Volume","AdjClose")))
# write.csv(JPM,file="Y:/DATA/JPM2013.csv")
# length(JPM)

################################
################################

################################
################################

########### To be continue ###############
