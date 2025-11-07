WeeklyGraph <- function(mylst = instLive, dt = Sys.Date()-650) {
library(dygraphs)
for(i in mylst) {
        mypath <- paste(file.path("/Users/johnlyons/Documents/Personal/DataScience/R/Rplots/Daily/"),i,".png",sep="")
        y <- subset(JSEdat,JSEdat$Name == i & JSEdat$Date >= dt)
        y <- unique(y)
        y[,3:7] <- apply(y[,3:7],2,as.numeric)
        rng <- max(min(y$Date),as.Date(dt, format="%Y-%m-%d"))
        #z <- xts(y[,3:7],order.by = y[,2])        # convert to xts
        z <- to.weekly(xts(y[,3:7],order.by = y$Date))        # convert to xts
        colnames(z) <- c("Open","High","Low","Close","Volume")
        
        assign(i,z)
        ##      -- workout last macd X-Over
        if (dim(z)[1] >= 50) {
                macd = MACD(z[,4])
                signal = Lag(ifelse(macd$macd < macd$signal, -1, 1))
                signal$ind <- ifelse(signal$Lag.1 == Lag(signal$Lag.1),0,1)
                flag <- tail(which(signal$ind == 1),1)      
        }
        
        ##
        range_weekly <- paste(dt,"::",sep="")
        if (dim(z)[1] >= 21) {
                parms_sma <- ifelse(dim(z)[1] >= 21 & dim(z)[1] < 50, "addSMA(n = c(21), col = c('blue'))",
                                    ifelse(dim(z)[1] >= 50 & dim(z)[1] < 100, "addSMA(n = c(21,50), col = c('blue','red'))",
                                           ifelse(dim(z)[1] >=  100, "addSMA(n = c(21,50,100), col = c('blue','red','black'))", "")))
                
                TA.param <- ifelse(dim(z)[1] >= 50,
                        paste("\"addOBV();",parms_sma,";addMACD();addVo();addLines(v=",flag,")\"",sep=""),
                        paste("\"addOBV()",parms_sma,"addVo()\"",sep=";"))
                
                
                weekly_chart <- chartSeries(z,name =paste("Weekly -",i,sep=" "),theme = "white",
                                       up.col = "blue", dn.col = "red",
                                       subset = range_weekly,
                                       TA=eval(parse(text=TA.param)))
                
                ## Now do Dygrapf
               # i <- paste("`",i,"`",sep="")    ## add `` so that - is ignored
                #htmltools::browsable(htmltools::tagList(dygraph((eval(parse(text = i)))[,2:4],main = i) %>% dySeries(c("Low","Close","High"))))
                
                
                       
        
        }
        

}
        
        
        mylst <- paste("`",mylst,"`",sep="")    ## add `` so that - is ignored
        htmltools::browsable(htmltools::tagList(lapply(mylst, function(x) 
                dygraph((eval(parse(text = x)))[,2:4],main = x) %>% dySeries(c("Low","Close","High")))))


        
}

