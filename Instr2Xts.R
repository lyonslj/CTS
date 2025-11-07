#*****************************************************************
# Load historical data
#*****************************************************************
#library(curl)
#con = gzcon(curl('https://github.com/systematicinvestor/SIT/raw/master/sit.gz','rb'))
#source(con)
#close(con)
#install.packages('quantmod')
setwd("/Volumes/C/Equis")

#alldata <- read.csv("allclean.csv", header = FALSE, sep = "")
#names(alldata) <- c("Instm", "Date", "Close", "High", "Low", "Vol")
#Convert DATE to date formatx??
#alldata$Date <- as.Date(alldata$Date, format="%Y%m%d")
        #mylst <- c("JSE-CONS","JSE-PLAT","JSE-GOLD","JSE-IND25","JSE-ALS40","JSE-FIN15","I-SP500","JSE-BANK","I-NIKKEI","M-GOLDZAR","JSE-INDM")
#Currencies 
        #mylst <- as.character(unique(alldata[grep("^C-",alldata$Instm),1]))
#Metals
        #mylst <- as.character(unique(alldata[grep("^M-",alldata$Instm),1]))
#JSE-Indexes        
        #mylst <- as.character(unique(alldata[grep("^JSE-",alldata$Instm),1]))
#Indexes        
        mylst <- as.character(unique(alldata[grep("^I-",alldata$Instm),1]))
        #
alldata2 <- subset(alldata,alldata$Date >= "2016-06-14")
#*********************************************
# Build individual xts sets of closing prices*
#*********************************************
for(i in mylst) {
        ##
        y <- subset(alldata2,alldata2$Instm == i)
        y <- unique(y)
        z <- xts(y[,3],order.by = y[,2])
        assign(i,z)
        }

mm <- sapply(mylst,as.name)
#*********************************************
#merge all closing prices of mylist into myxts
#*********************************************
myxts <- do.call(merge,sapply(mylst,as.name))
day1 <- as.numeric(myxts[1,])           #get closing price of first record

dp <- head(myxts,0)
for(i in 1:ncol(myxts)) {
       pcage<- ((myxts[,i] - day1[i])  / day1[i] )*100          #*%age move
        dp  <- cbind(dp,pcage)
        }
#**************************************
# Get top 5 and bottom 3 instruments  #
#**************************************
templst <- as.data.frame(tail(dp,1))
templst <- melt(templst)                        #cast long
templst <- templst[order(templst[,2]),]         #sort
newlst <- rbind(head(templst,3),tail(templst,5))
mylst <- as.character(newlst[,1])


#*********************************************
#            Graph the series                *
#*********************************************
zoo.dp <- as.zoo(dp)                    #convert to zoo to keep column names
# Set a color scheme:
tsRainbow <- rainbow(ncol(zoo.dp))
# Plot the overlayed series
plot(x = zoo.dp, ylab = "Cumulative Return %age", main = "Cumulative Returns",col = tsRainbow, screens = 1)
# Set a legend in the upper left hand corner to match color to return series
legend(x = "topleft", legend = mylst, lty = 1,col = tsRainbow)




