
setwd('Y:/PH.D/Fields/R_packages/AdpQMLE/AdpQMLE') # set the working path as AdpQMLE
dir() # list all the files and folders in this path
######## Let's wrep up whole stuff from the bottom up! ##############

############ 1. The GARCH generating function. ##############
# Test for GARCH_t function #
xx <- GARCH_t(alpha = c(0.1, 0.2), beta = 0.5, n = 200, rnd = "rt", df.t = 2.3)
plot(xx, type = "l")
library(timeDate)
kurtosis(xx)
###########     OK, END      ###############

########### 2. The basic MLE method testing. ###############
# Test for MLE function #
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

# x <- ts(x[101:1100])

plot(x, type = "l")
library(tseries)
x.arch <- garch(x, order = c(1,1)) # Fit GARCH(1,1)
est1 <- MLE(series = x, LogLFunc = LogL_GARCH_Norm)
est1
est2 <- tQMLE(series = x, logLFunc = LogL_GARCH_t)


xx.arch <- garch(xx, order = c(1,1)) # Fit GARCH(1,1)
para2 <- MyMLE(xx)
para2
para3 <- MytQMLE(xx, 2.3)
para3

############# change myMLE for GARCH(p,q) ###############


############# 3. com.residue function writing and testing ###################

################################

############# 4. MytQMLE function writing and testing ###################
################################

############# 5. EstmBestdf function writing and testing ###################
################################

############# 6. YITAtQMLE function writing and testing ###################
################################

############# 7. google.intraday.data testing ###################
library('xts')
AMZN <- google.intraday.data(symbol = "AMZN", freq = 60, period =12)
head(AMZN)
length(AMZN[,1])/(6.5*60)
length(AMZN[,1])
# AMZN$minute_updown <- AMZN$Close -AMZN$Open
AMZN$within_minuteR <- log(AMZN$Close/AMZN$Open)
lagClose <- lag(AMZN$Close, k = 1)
head(lagClose)
AMZN$next_minute_return <- log(AMZN$Open/lagClose) 

temp <- rep(0, length(AMZN[,1]))

temp[(lagClose - AMZN$Open)<0] <- 3
temp[(lagClose - AMZN$Open)==0] <- 2
temp[(lagClose - AMZN$Open)>0] <- 1
head(temp)

AMZN$updown_tplus1 <- lag(temp, k = -1) 
head(AMZN)
summary(AMZN$next_minute_return)
hist(AMZN$next_minute_return, breaks = 20, xlim = c(-0.005, 0.005))
AMZN <- AMZN[-1]
length(AMZN[,1])
write.csv(AMZN, file = "AMZN.csv") 

# split(AMZN, f = 'days')

################################

################################
################################

################################
################################

########### To be continue ###############
