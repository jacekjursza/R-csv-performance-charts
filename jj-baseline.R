Sys.setlocale("LC_TIME", "C")

#data <- read.csv("/home/jacek/meltdown/metrics/baseline_increased_index.test//index445G.csv", header=T)
data <- read.csv("/home/jacek/meltdown/better-baseline/baseline.csv", header=T)
head(data)

unix2POSIXct  <-  function (time)   structure(time, class = c("POSIXt", "POSIXct")) 

summary(data)
length(data)
z = 241221244
z = data$now
x = as.POSIXct((z+0.1)/1000, origin = "1970-01-01")
x = ((data$now)/1000/60) %% 1000

Remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.5, .95), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

drawSliced <- function(xLastHours, dataVector, ylabel, useLines=FALSE ) {
  timeStamps <- z/1000
  filterLastHours <- function(timestamp, lastTimestamp, hours) { ifelse(lastTimestamp - timestamp < hours*3600, TRUE, FALSE) }
  ff = timeStamps[ filterLastHours(timeStamps, timeStamps[length(timeStamps)], xLastHours) == TRUE ]
  dataCurr <- dataVector
  dataStart <- length(dataCurr) - length(ff) + 1
  dataEnd <- length(dataCurr) 
  dataSliced <- dataCurr[dataStart:dataEnd]
  length(ff)
  length(dataSliced)
  plot(difftime(unix2POSIXct(ff[length(ff)]), unix2POSIXct(ff), units="hours"), Remove_outliers(dataSliced), ylab=ylabel, xlab="Time (hours from beginning of the experiment)", ylim=c(0,130), "l")  
  if (useLines) lines(lowess(difftime(unix2POSIXct(ff[length(ff)]), unix2POSIXct(ff), units="hours"), Remove_outliers(dataSliced)), col="red")
}

#drawSliced(4, data$solr.request_per_second, "median request time")
drawSliced(4, data$solr.select.medianRequestTime, "median request time")
drawSliced(4, data$solr.select.75thPcRequestTime, "75th percentile request time (MS)")
drawSliced(4, data$solr.select.95thPcRequestTime, "95th percentile request time (MS)")
drawSliced(4, data$solr.heap.used/1024/1024/1024, "heap memory used (GB)", FALSE)

abline(h=120, col="blue")
abline(h=80, col="red")

      plot(unix2POSIXct(z/1000), Remove_outliers(data$solr.heap.max/1024/1024/1024), ylab="heap memory used (GB)", xlab="time", "l")
help.search("plot")
help.search("difftime")

