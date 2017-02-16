MADirection <- function(period = 100 ) {
        #*****************************************************************
        #   For range istruments calculate how many instrm               #
        #   prices are above and below their moving average              #
        #*****************************************************************        
        library(plyr)           # allows rbind.fill of diff column lengths
        # Need data load from Daily
        #only need 24 records
        alldata2 <- subset(JSEdat,JSEdat$Date >= (Sys.Date()-(period*4)))    #45 to include weekends
        alldata2 <- alldata2[order(alldata2[,2]),]         #sort
        z <- aggregate(Close ~ Name, alldata2, length)                 #group by instm
        #choose instm who have more than 30 record
        lst <- subset(z$Name,z$Close > period)
        mydata <- NULL
        for(i in lst) {
                y <- subset(alldata2,alldata2$Name == i)
                y$ma <- SMA(y$Close,n= period)   # Calculate 24/50 dma of RSC, Need package TTR#
                mydata <- rbind.fill(y,mydata)
        }
        myposaggr <- aggregate(ma ~ Date,subset(mydata,mydata$Close - mydata$ma > 0),length)
        mynegaggr <- aggregate(ma ~ Date,subset(mydata,mydata$ma - mydata$Close > 0),length)
        aggrDaily <- merge(myposaggr,mynegaggr,by = "Date")
        names(aggrDaily) <- c("Date","PosMA","NegMA")
        aggrDaily$Nett <- aggrDaily$PosMA - aggrDaily$NegMA
        ix <- 2:4      #convert integers to numeric
        aggrDaily[ix] <- lapply(aggrDaily[ix], as.numeric)
        #plot_ly(aggrDaily,x = ~Date,y = ~PosMA, type = 'bar', name = 'Price rel to MA') %>%
        View(aggrDaily)
}
