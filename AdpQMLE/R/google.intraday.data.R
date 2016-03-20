#' google.intraday.data
#' 
#' This is a function used get Google minutely trading data of stocks.
#'
#' @param symbol is the stock symbol. This quote should correspond with Yahoo list. Symbols can be found here: http://www.marketwatch.com/tools/industry/stocklist.asp?bcind_ind=9537&bcind_period=3mo
#' @param freq is the interval of time. The unit of it is second. The minimal interval is 60 seconds. Like "60s". Please input numeric. 
#' @param period is the time period. The unit is day, "d". Please input numeric. 
#' @importFrom xts xts
#' @return Return data include data untill now.
#' @export
#'
#' @examples
google.intraday.data <- function(symbol, freq, period) {
  base.url <- "http://www.google.com/finance/getprices?"
  options.url <- paste("i=",freq,"&p=",period,"d&f=d,o,h,l,c,v&df=cpct&q=", symbol, sep="")
  full.url <- paste(base.url, options.url, sep="")
  
  data <- read.csv(full.url, skip=7, header=F, stringsAsFactors=F)
  
  starting.times.idx <- which(substring(data$V1, 1, 1)=="a")
  ending.seconds.idx <- c(starting.times.idx[-1]-1, nrow(data))
  r.str.idx.use <- paste(starting.times.idx, ":", ending.seconds.idx, sep="")
  
  starting.times <- as.numeric(substring(data[starting.times.idx,1],2))
  
  data[starting.times.idx, 1] <- 0

  clean.idx <- do.call(c, lapply(seq(1, length(r.str.idx.use)), function(i) starting.times[i] + freq*as.numeric(data[eval(parse(text=r.str.idx.use[i])),1])))
  data <- data[which(substring(data$V1, 1, 1) != "a"),]
  
  data.xts <- xts(data[,-1], as.POSIXct(clean.idx, origin="1970-01-01", tz="GMT"))
  
  indexTZ(data.xts) <- "America/New_York"
  colnames(data.xts) <- c("Close", "High", "Low", "Open", "Volume")
  
  data.xts
}