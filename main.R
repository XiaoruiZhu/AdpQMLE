
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
library(tseries)
x.arch <- garch(x, order = c(0,2)) # Fit ARCH(2)

####
para <- MyMLE(x)
para

############# 3. com.residue function writing and testing ###################
################################

############# 4. MytQMLE function writing and testing ###################
################################

############# 5. EstmBestdf function writing and testing ###################
################################

############# 6. YITAtQMLE function writing and testing ###################
################################

################################
################################

################################
################################

################################
################################

########### To be continue ###############
