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

instrument <- "MOMO"
url <- paste("http://www.google.com/finance/getprices?i=60&p=20d&f=d,o,h,l,c,v&df=cpct&q=", instrument, sep = "")
url
destfile <- tempfile()
destfile
method <- "auto"
if (is.null(method)) {
  method <- getOption("download.file.method")
  if (is.null(method)) 
    method <- "auto"
}
quiet <- FALSE
status <- tryCatch(download.file(url, destfile, method = method, 
                                 quiet = quiet), error = identity)
status
!inherits(status, "error")
x <- read.table(destfile, header = TRUE, sep = ",", as.is = TRUE, 
                fill = TRUE)
