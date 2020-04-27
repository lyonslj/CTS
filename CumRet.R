CumRet <- function(mylst = instLive, dt = "2017-10-01", nm = "Live" , pth = "Rplots/") {
        


#*********************************************
# Build individual xts sets of closing prices*
#*********************************************
for(i in mylst) {
        y <- JSEdat[JSEdat$Name == i & JSEdat$Date >= dt, ]
        
        #y <- subset(JSEdat,JSEdat$Name == i & JSEdat$Date >= dt)
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
#**************************************
#       Legend with %return           #
#**************************************
templst <- as.data.frame(tail(dp,1))
templst <- melt(templst)                        #cast long
templst$value <- round(templst$value,2)
tor <- templst[order(templst[,2],decreasing = TRUE),]

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
legend("topleft",legend = paste(tor$variable,tor$value,sep=" "),inset=.01,cex = 0.7,lty=c(1,5),lwd=c(3,3),bg="grey96")
       #,col = tsRainbow ) 

}


