DailyGraphNoPng <- function(mylst = instLive, dt = Sys.Date()-200, styl = "candlesticks") {
        
        for(i in mylst) {
                mypath <- paste(file.path("/Users/johnlyons/Documents/Personal/DataScience/R/Rplots/Daily/"),i,".png",sep="")
                y <- subset(JSEdat,JSEdat$Name == i & JSEdat$Date >= dt)
                y <- unique(y)
                rng <- max(min(y$Date),as.Date(dt, format="%Y-%m-%d"))
                z <- xts(y[,3:7],order.by = y$Date)        # convert to xts
                assign(i,z)
                ##      -- workout last macd X-Over
                macd = MACD(z[,4])
                signal = Lag(ifelse(macd$macd < macd$signal, -1, 1))
                signal$ind <- ifelse(signal$Lag.1 == Lag(signal$Lag.1),0,1)
                flag <- tail(which(signal$ind == 1),1)
                ##
                range_daily <- paste(rng,"::",sep="")
                ##- Setup SMA
                parms_sma <- ifelse(dim(y)[1] >= 21 & dim(y)[1] < 50, "addSMA(n = c(21), col = c('blue'))",
                                    ifelse(dim(y)[1] >= 50 & dim(y)[1] < 100, "addSMA(n = c(21,50), col = c('blue','red'))",
                                           ifelse(dim(y)[1] >=  100, "addSMA(n = c(21,50,100), col = c('blue','red','black'))", "")))
                
                TA.param=paste("\"addOBV();",parms_sma,";addMACD();addVo();addLines(v=",flag,")\"",sep="")
                
                daily_chart <- chartSeries(z, name =paste("Daily -",i,sep=" "),
                                           type =  styl,  
                                           theme = "white",
                                           up.col = "blue", dn.col = "red",
                                           subset = range_daily,
                                           TA=eval(parse(text=TA.param)))
                
                
                
                
                
                
                
        }
        
}

