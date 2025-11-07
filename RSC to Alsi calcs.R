alldata <- read.csv("allclean.csv", header = FALSE, sep = "")
names(alldata) <- c("Instm", "Date", "Close", "High", "Low", "Vol")
#Convert DATE to date formatx??
alldata$Date <- as.Date(alldata$Date, format="%Y%m%d")
#alldata$CLOSE <- as.numeric(alldata$CLOSE)
#alldata$HIGH <- as.numeric(alldata$HIGH)
#alldata$LOW <- as.numeric(alldata$LOW)
#alldata$VOL <- as.numeric(alldata$VOL)
#Convert to xts
#xalld <- xts(alldata, order.by=alldata$DATE)
#Filter instrm
#har <- subset(xalld[,3:6],xalld$INSTRM=="HARMONY")
#plot.zoo(har)

har <- unique(har)
alsi <- subset(alldata[,2:3],alldata$Instm=="JSE-ALS40")
alsi <- unique(alsi)
#convert to xts
harxts <- xts(har[,-1],order.by = har[,1])
alsixts <- xts(alsi[,-1],order.by = alsi[,1])
x <- "Alsi.Close"
colnames(alsixts) <- x
#
# Merge har and Alsi.Close
harxts_rsc <- merge(harxts,alsixts) # Add alsi close
harxts_rsc <- merge(harxts_rsc,harxts_rsc$Close / harxts_rsc$Alsi.Close) # Add rsc
harxts_rsc <- merge(harxts_rsc,SMA(harxts_rsc$Close.1,24)) # Calculate 24 dma of RSC
#
zz <- harxts_rsc$Close.1 > harxts_rsc$SMA # Is RSC > MaRsc
harxts_rsc <- merge(harxts_rsc,zz)
#%age moves
pm <- ((harxts_rsc$Close - lag(harxts_rsc$Close,1))/lag(harxts_rsc$Close,1))*100 #daily
harxts_rsc <- merge(harxts_rsc,pm)

