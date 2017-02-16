DailyGraph <- function(mylst = instLive, dt = "2016-07-01") {
file.list <- list.files(path = "/Volumes/C/snet",pattern='*.csv')
setwd("/Volumes/C/snet")
# Chart #
for(i in mylst) {
        mypath <- paste(file.path("/Users/johnlyons/Documents/Personal/DataScience/R/Rplots/Daily/"),i,".png",sep="")
        y <- subset(JSEdat,JSEdat$Name == i & JSEdat$Date >= dt)
        y <- unique(y)
        z <- xts(y[,3:7],order.by = y[,2])        # convert to xts
        assign(i,z)
        range <- paste(dt,"::",sep="")
        png(file=mypath,width = 1024,height = 768)
        chartSeries(z,name =i,theme = "white",subset = range,TA="addOBV();addSMA(n=24);addMACD();addVo()")
        dev.off()
        chartSeries(z,name = i,theme = "white",subset = range,TA="addOBV();addSMA(n=24);addMACD();addVo()") 
}

#par(mfrow=c(1,2))
#chart_Series(z,name = i,subset = "2016-07-01::",TA="add_SMA(n=24);add_MACD();add_Vo()")
#chart_Series(to.weekly(z),name = paste(i,":Weekly"),TA="add_SMA(n=24);add_MACD();add_Vo()")
}

