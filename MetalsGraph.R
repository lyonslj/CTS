MetalsGraph <- function(mylst = "EXXARO", dt = "2016-07-01") {
y <- subset(JSEdat,JSEdat$Name == mylst & JSEdat$Date >= "2015-01-01")
z <- xts(y[,3:7],order.by = y[,2])  
chartSeries(z,name = mylst,subset = "2016-01-01::",type = "line",theme = "white",TA="addSMA(n=24);addMACD()")
}