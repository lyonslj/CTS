CumulativeReturns <- function(dt = "2016-01-01",mylst = c("IMPLATS","HARMONY","GFIELDS","SIBANYE"), nm = "Live.png" ) {
#*****************************************************************
# Load historical data
#*****************************************************************
#library(curl)
#con = gzcon(curl('https://github.com/systematicinvestor/SIT/raw/master/sit.gz','rb'))
#source(con)
#close(con)
#install.packages('quantmod')
setwd("/Volumes/C/Equis")

alldata <- read.csv("allclean.csv", header = FALSE, sep = "")
names(alldata) <- c("Instm", "Date", "Close", "High", "Low", "Vol")
#Convert DATE to date formatx??
alldata$Date <- as.Date(alldata$Date, format="%Y%m%d")
        #mylst <- c("I-GLDBUGS","I-BALTIC","JSE-CONS","JSE-PLAT","JSE-GOLD","JSE-IND25","JSE-ALS40","JSE-FIN15","I-SP500","JSE-BANK","I-NIKKEI","M-GOLDZAR","JSE-INDM")
        #nm <- "KeyIndixes.png"
        #Live
        #mylst <- c("IMPLATS","HARMONY","GFIELDS","SIBANYE")
        #nm <- "Live.png"
#Watchlist
        #mylst <- c("ADVTECH","CHROMETCO","AVENG","WBHO")
        #nm <- "Watchlist.png"
#Currencies 
        #mylst <- as.character(unique(alldata[grep("^C-",alldata$Instm),1]))
        #nm <- "Currencies.png"
#Metals
        #mylst <- as.character(unique(alldata[grep("^M-",alldata$Instm),1]))
        #nm <- "Metals.png"
#JSE-Indexes        
        #mylst <- as.character(unique(alldata[grep("^JSE-",alldata$Instm),1]))
        #nm <- "JSE Indixes.png"
#Indexes        
        #mylst <- as.character(unique(alldata[grep("^I-",alldata$Instm),1]))
        #nm <- "Indexes.png"
#GoldShares        
        #mylst <- c("ANGGOLD","COAL","DRDGOLD","HARMONY","GFIELDS","SIBANYE")
        #nm <- "GOLD.png"
#Plats      
        #mylst <- c("AMPLATS","EASTPLATS","IMPLATS","NORTHAM","LONMIN")  
        #nm <- "Plat.png"
#GeneralMining/Indust      
        #mylst <- c("ANGLO","ARM","ATLATSA","BILLITON","EXXARO","JUBILEE","KUMBA".+?,"MERAFE","PETMIN")  
        #nm <- "Gen Mining Indust"
        #
        #mylst <- c("I-GLDBUGS","I-BALTIC","JSE-PLAT","JSE-GOLD")
alldata2 <- subset(alldata,alldata$Date >= dt)
#*********************************************
# Build individual xts sets of closing prices*
#*********************************************
for(i in mylst) {
        ##
        y <- subset(alldata2,alldata2$Instm == i)
        y <- unique(y)
        z <- xts(y[,3],order.by = y[,2])        # convert to xts
        assign(i,z)
        }
mm <- sapply(mylst,as.name)                     # assign string from mylst names to xts sets
#*********************************************
#merge all closing prices of mylist into myxts
#*********************************************
myxts <- do.call(merge,sapply(mylst,as.name))   # build single xts frame of instr closing prices
day1 <- as.numeric(myxts[1,])                   #get closing price of first record
dp <- head(myxts,0)                             # price on day1 of set
for(i in 1:ncol(myxts)) {                       # build xts frame of instr and %age moves
       pcage<- ((myxts[,i] - day1[i])  / day1[i] )*100         
        dp  <- cbind(dp,pcage)
        }
#*********************************************
#            Graph the series                *
#*********************************************
zoo.dp <- as.zoo(dp)                    #convert to zoo to keep column names
# Set a color scheme:
tsRainbow <- rainbow(ncol(zoo.dp))
# Plot the overlayed series
plot(x = zoo.dp, ylab = "Cumulative Return %age", main = "Cumulative Returns", col = tsRainbow, screens = 1,lwd = 2)
grid(NULL,NULL)
# Set a legend in the upper left hand corner to match color to return series
legend("topleft",legend = mylst,inset=.01,cex = 0.5,lty=c(1,1),lwd=c(2,2),bg="grey96",col = tsRainbow ) 
#View(tor)
#*********************************************
#            Write to file               *
#*********************************************
mypath <- file.path("/Users/johnlyons/Documents/Personal/DataScience/R",nm)
png(file=mypath,width = 1024,height = 768)
plot(x = zoo.dp, ylab = "Cumulative Return %age", main = "Cumulative Returns",col = tsRainbow, screens = 1,lwd =2)
grid(NULL,NULL)
legend("topleft",legend = mylst,inset=.01,cex = 1.2,lty=c(1,1),lwd=c(2,2),bg="grey96",col = tsRainbow ) 
dev.off()
}

#**************************************
# Get top 5 and bottom 3 instruments  #
#**************************************
#templst <- as.data.frame(tail(dp,1))
#templst <- melt(templst)                        #cast long
#templst$value <- round(templst$value,2)
#tor <- templst[order(templst[,2],decreasing = TRUE),]     
#templst <- paste(templst$variable,templst$value)
#templst <- as.list(templst)
#newlst <- rbind(head(templst,3),tail(templst,5))
#mylst <- as.character(newlst[,1])
#XtsBuild(mylst)


