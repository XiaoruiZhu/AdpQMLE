library('devtools') # 'devtools' is the most important tool for developing R packages.
create('Y:/PH.D/Fields/R_packages/AdpQMLE/AdpQMLE') 
# Creat the R folder for the package. AdpQMLE is the package's name.
setwd('Y:/PH.D/Fields/R_packages/AdpQMLE/AdpQMLE') # set the working path as AdpQMLE
dir() # list all the files and folders in this path
file.edit('DESCRIPTION')

load_all('AdpQMLE')
plot(GARCH_t(alpha=c(0.1,0.2),beta=0.5,n=100,df.t=2)$xt,type='l')
document()
build()
check()
test('AdpQMLE')
# During development
dev_mode()
install("AdpQMLE")
library(AdpQMLE) # uses the development version
dev_mode()
# Fresh R session
library(AdpQMLE) # uses the released package you've installed previously

library(RCurl)
fullurl <- 'https://www.google.com/finance/getprices?i=60&p=1d&f=d,c,h,l,o,v&df=cpct&q=BABA'
fullurl
# tmp <- getURL('https://www.google.com/finance/getprices?i=900&p=10d&f=d,o,h,l,c,v&df=cpct&q=AAPL')
# tmp
data <- read.csv(fullurl, skip=7, header=F, stringsAsFactors=F)

##########################
library(xts)
xxx <- google.intraday.data("AAPL", freq = 60, period = "2d")
xxx

length(xxx$Open)
trunc(length(xxx$Open)/390)*390

##########################
