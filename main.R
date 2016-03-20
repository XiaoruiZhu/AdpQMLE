
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
alpha <- c(0.1, 0.5)
beta <- c(0.2) # GARCH(1,1) coefficients
x.e.sig <- GARCH1_1(n=1000, a = alpha, b = beta)
x <- x.e.sig$series 

# x <- ts(x[101:1100])

plot(x, type = "l")
library(tseries)
x.arch <- garch(x, order = c(1,1)) # Fit GARCH(1,1)
est1 <- MLE(series = x, LogLFunc = LogL_GARCH_Norm)
est1
est2 <- tQMLE(series = x, LogLFunc = LogL_GARCH_t, dfest = 20)
est2

# Finished test for MLE and GARCH with normal innovation.


###########################################################################
# Test tQMLE for GARCH with t
xx <- GARCH_t(alpha = c(0.1, 0.2), beta = 0.5, n = 200, rnd = "rt", df.t = 2.3)
plot(xx, type = "l")
# library(timeDate)
kurtosis(xx)
estxxMLE <- MLE(series = xx, LogLFunc = LogL_GARCH_Norm)
estxxMLE
estxx <- tQMLE(series = xx, LogLFunc = LogL_GARCH_t, dfest = 2.1)
estxx
# Done, with known degree of freedom of t innovation, the tQMLE is better than MLE with normal
###########################################################################



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
