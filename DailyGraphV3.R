DailyGraph <- function(mylst = instLive, dt = "2016-07-01") {

for(i in mylst) {
        mypath <- paste(file.path("/Users/johnlyons/Documents/Personal/DataScience/R/Rplots/Daily/"),i,".png",sep="")
        y <- subset(JSEdat,JSEdat$Name == i & JSEdat$Date >= Sys.Date()-200)
        y <- unique(y)
        z <- xts(y[,3:7],order.by = y$Date)        # convert to xts
        assign(i,z)
        range_daily <- paste(Sys.Date()-200,"::",sep="")
        
        daily_chart <- chartSeries(z, name =paste("Daily -",i,sep=" "),
                                theme = "white",
                               up.col = "blue", dn.col = "red",
                               subset = range_daily,
                               TA="addOBV();
                                        addSMA(n = c(21,50,100), col = c('blue','red','black'));
                                        addMACD();addVo()")
        
        png(file=mypath,width = 1024,height = 768)
        daily_chart
        #weekly_chart
        dev.off()
        
}

}

