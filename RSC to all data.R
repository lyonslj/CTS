setwd("/Volumes/C/Equis")
alldata <- read.csv("allclean.csv", header = FALSE, sep = "")
names(alldata) <- c("Instm", "Date", "Close", "High", "Low", "Volume")
alldata$Date <- as.Date(as.character(alldata$Date), format="%Y%m%d")
ix <- 3:6      #convert integers to numeric
alldata[ix] <- lapply(alldata[ix], as.numeric)
alldata <- unique(alldata)

mydata <- head(y,0)

#only need 24 records
alldata2 <- subset(alldata,alldata$Date >= "2016-05-01")
#group by instm
z <- aggregate(Close ~ Instm, alldata2, length)
#choose instm who have more than 30 record
lst <- subset(z$Instm,z$Close > 30)
alsi <- subset(alldata2[,2:3],alldata2$Instm=="JSE-ALS40")
names(alsi) <- c("Date","Alsi.Close")
alldata2 <- merge(alldata2,alsi,by = "Date")
alldata2$rsc <- (alldata2$Close / alldata2$Alsi.Close) # Add rsc

                for(i in lst) {
                ##
                y <- subset(alldata2,alldata2$Instm == i)
                        y$rsc.ma24 <- SMA(y$rsc,n=24)   # Calculate 24 dma of RSC, Need package TTR#
                        #y$pm <- (Delt(y$Close))*100     #Calculate daily %age move
                        mydata <- rbind(y,mydata)
                }
backdt <- Sys.Date()-5
opportunities <- as.character(subset(mydata$Instm,mydata$Date == (Sys.Date()-2) & mydata$rsc > mydata$rsc.ma24 & mydata$Volume > 30000))  
CumulativeReturns(backdt,opportunities,"Opportunities")
