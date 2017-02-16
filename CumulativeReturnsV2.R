CumulativeReturns <- function(dt = "2016-10-01",mylst = instLive, nm = "Live" , pth = "Rplots/") {
#*****************************************************************
# Load historical data
# Loaded in Daily function        
#*****************************************************************
alldata2 <- unique(subset(JSEdat,JSEdat$Date >= dt))

#*********************************************
# Build individual xts sets of closing prices*
#*********************************************
for(i in mylst) {
        y <- subset(alldata2,alldata2$Name == i)
        y <- y[,c("Date","Close")]
        y <- aggregate(Close ~ Date, y, mean)   # get rid of dups and multiple entries
        z <- xts(y[,2],order.by = y[,1])        # convert to xts
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
plot(x = zoo.dp, ylab = "Cumulative Return %age", main = nm, col = tsRainbow, screens = 1,lwd = 2.5)
grid(NULL,NULL)
abline(h=0)
legend("topleft",legend = mylst,inset=.01,cex = 0.7,lty=c(1,5),lwd=c(3,3),bg="grey96",col = tsRainbow ) 
"#*********************************************"
#            Write graph to file               *
#*********************************************
mypath <- paste(file.path("/Users/johnlyons/Documents/Personal/DataScience/R/"),pth,nm,".png",sep="")
png(file=mypath,width = 1024,height = 768)
plot(x = zoo.dp, ylab = "Cumulative Return %age", main = nm, col = tsRainbow, screens = 1,lwd =3)
grid(NULL,NULL)
abline(h=0)
legend("topleft",legend = mylst,inset=0.01,cex = 1.2,lty=c(1,1),lwd=c(4,4),bg="grey96",col = tsRainbow ) 
dev.off()
#**************************************
#       Write returns to table        #
#**************************************
templst <- as.data.frame(tail(dp,1))
templst <- melt(templst)                        #cast long
templst$value <- round(templst$value,2)
tor <- templst[order(templst[,2],decreasing = TRUE),]
writepath <- paste(file.path("/Users/johnlyons/Documents/Personal/DataScience/R/RPlots/"),nm,".",Sys.Date(),".txt",sep="")
write.table(tor,writepath,sep="\t", col.names = F, row.names = F, quote = FALSE)
}



