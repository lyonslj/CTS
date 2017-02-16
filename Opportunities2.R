Opportunities <- function(DaysAgo = 1 ) {
#*****************************************************************
#   Find all instruments whose RSC(alsi40) > than its 24dma      #
#*****************************************************************        

library(reshape2)
y <- NULL        
mydata <- head(y,0)

#only need 24 records
alldata2 <- subset(JSEdat,JSEdat$Date >= "2016-05-01")
#group by instm
z <- aggregate(Close ~ Name, alldata2, length)
#choose instm who have more than 30 record
lst <- subset(z$Name,z$Close > 30)
alsi <- subset(alldata2[,2:6],alldata2$Name=="JH-ALSI40")
alsi <- alsi[,-2:-4]    # Drop columns
names(alsi) <- c("Date","Alsi.Close")
alldata2 <- merge(alldata2,alsi,by = "Date")
alldata2$rsc <- (alldata2$Close / alldata2$Alsi.Close) # Add rsc

                for(i in lst) {
                        y <- subset(alldata2,alldata2$Name == i)
                        y$rsc.ma24 <- SMA(y$rsc,n=24)   # Calculate 24 dma of RSC, Need package TTR#
                        y$pm <- (Delt(y$Close))*100     #Calculate daily %age move
                        mydata <- rbind(y,mydata)
                }
                opportunities <- as.character(subset(mydata$Name,mydata$Date == (Sys.Date() - DaysAgo) 
                                     & mydata$rsc > mydata$rsc.ma24 
                                     & mydata$Volume > 30000 
                                     & mydata$pm > 1))  
                backdt <- Sys.Date()-20  # show over 20 days
                pth = "RPlots/Scans/"
                for(i in opportunities){
                CumulativeReturns(backdt,i,i,pth)         # call to function to show %age moves
                                        }
}
