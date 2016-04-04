#' This function is used to download and save high frequent data of stock in the working directory. 
#' The source is Google.
#'
#' @title getHighFreqData
#'
#' @param start The start date of your period with the format "YYYY-MM-DD". Default start date is "1991-01-02".
#' @param end The end date of your period with the format "YYYY-MM-DD". The default end date is the day before the today in your system.
#' @param instrument The quote symbol of the stock. This quote should correspond with Yahoo list. Symbols can be found here: http://www.marketwatch.com/tools/industry/stocklist.asp?bcind_ind=9537&bcind_period=3mo
#' @param provider The source of data, contains options: "google", "yahoo", "onada".
#' @param savename The saving name the file including downloaded data. The default folder is the current working directory of the R process.
#'
#' @importFrom tseries get.hist.quote
#' @importFrom zoo index zoo index<-
#' @importFrom utils download.file
#' @importFrom its as.its
#' @return The time series data will be returned.
#' @export
#'
#' @examples
#' # need revisions
getHighFreqData=function(instrument, start, end, provider = c("google", "yahoo"), savename, method, quiet = FALSE){
  if (missing(start))
    start <- "1991-01-02"
  if (missing(end))
    end <- format(Sys.Date() - 1, "%Y-%m-%d")
  provider <- match.arg(provider)
  start <- as.Date(start)
  end <- as.Date(end)
  method <- if (missing(method)) 
    getOption("download.file.method", default = "auto")
  else match.arg(method, c("auto", "internal", "libcurl", "wget", 
                           "curl", "lynx"))
  if (provider == "google") {
    url <- paste("http://www.google.com/finance/getprices?i=60&p=20d&f=d,o,h,l,c,v&df=cpct&q=", instrument, sep = "")
    destfile <- tempfile()
    i <- 1L
    repeat {
      status <- tryCatch(download.file(url, destfile, method = method,
                                       quiet = quiet), error = identity)
      if (!inherits(status, "error") && (status == 0))
        break
      unlink(destfile)
      if (i >= 5L) {
        if (inherits(status, "error"))
          stop(conditionMessage(status))
        else stop(sprintf("download error, status %d",
                          status))
      }
      message("download error, retrying ...")
      i <- i + 1L
    }
    nlines <- length(count.fields(destfile, sep = "\n"))
    if (nlines == 1) {
      unlink(destfile)
      stop(paste("no data available for", instrument))
    }
    x <- read.table(url, skip=7, header=F, stringsAsFactors=F)
    x <- na.omit(x)
    if (nrow(x) >= 2L && x[1L, 1L] == x[2L, 1L]) {
      warning("first date duplicated, first instance omitted")
      x <- x[-1L, , drop = FALSE]
    }
    unlink(destfile)
    names(x) <- gsub("\\.", "", names(x))
    nser <- pmatch(quote, names(x)[-1]) + 1
    if (any(is.na(nser)))
      stop("this quote is not available")
    n <- nrow(x)
    dat <- as.Date(as.character(x[, 1]), "%Y-%m-%d")
    if (!quiet && (dat[n] != start))
      cat(format(dat[n], "time series starts %Y-%m-%d\n"))
    if (!quiet && (dat[1] != end))
      cat(format(dat[1], "time series ends   %Y-%m-%d\n"))
    if (retclass == "ts") {
      jdat <- unclass(julian(dat, origin = as.Date(origin)))
      ind <- jdat - jdat[n] + 1
      y <- matrix(NA, nrow = max(ind), ncol = length(nser))
      y[ind, ] <- as.matrix(x[, nser, drop = FALSE])
      colnames(y) <- names(x)[nser]
      y <- y[, seq_along(nser), drop = drop]
      return(ts(y, start = jdat[n], end = jdat[1]))
    }
    else {
      x <- as.matrix(x[, nser, drop = FALSE])
      rownames(x) <- NULL
      y <- zoo(x, dat)
      y <- y[, seq_along(nser), drop = drop]
      if (retclass == "its") {
        if (inherits(tryCatch(getNamespace("its"), error = identity),
                     "error"))
          warning("package its could not be loaded: zoo series returned")
        else {
          index(y) <- as.POSIXct(index(y))
          y <- its::as.its(y)
        }
      }
      return(y)
    }
  }
  else if (provider == "yahoo") {
      quote=get.hist.quote(quote,start=start,end=end,
                           provider ="yahoo",
                           quote=c("Open", "High", "Low", "Close","Volume","AdjClose"))
    }
    else {
      quote=get.hist.quote(quote,start=start,end=end,
                           provider ="yahoo",
                           quote=c("Open", "High", "Low", "Close","Volume","AdjClose"))
    }
  save=paste(savename,start,"_",end,".csv",sep='')
  write.csv(quote,file=save)
  return(invisible(quote))
}
