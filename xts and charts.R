alldata <- read.csv("allclean.csv", header = FALSE, sep = "")
cnames <- c("Instm", "Date", "Close", "High", "Low", "Vol")
colnames(alldata) <- cnames
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

har <- subset(alldata[,2:6],alldata$Instm=="HARMONY")
alsi <- subset(alldata[,2:3],alldata$Instm=="JSE-ALS40")
#convert to xts
harxts <- xts(har[,-1],order.by = har[,1])

chartSeries(harxts)
barChart(harxts,theme="white.mono",bar.type = 'hlc')
zoomChart("2016")
#last month
last(harxts,'month')
#convert to monthly data
to.period(harxts,'months')
chartSeries(to.period(harxts[,1],'months')) # this produces hlc



