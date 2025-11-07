fnCumRet <- function(dt = "2018-01-04",mylst = Watchlist) {
#*****************************************************************
# Load historical data
# Loaded in Daily function        
#*****************************************************************
#This needs to be calculated in GraphInstLive
#alldata2 <- unique(subset(JSEdat,JSEdat$Date >= dt))

#*********************************************
# Build individual xts sets of closing prices*
#*********************************************
for(i in mylst) {
        y <- subset(JSEdat,JSEdat$Name == i & JSEdat$Date >= dt)
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
dp <- dp[complete.cases(dp),]
#*********************************************
#            Generate data for Graph         *
#*********************************************
#zoo.dp <- as.zoo(dp)                    #convert to zoo to keep column names
assign("zoo.dp",as.zoo(dp),envir = .GlobalEnv) #create a variable that is returned to the environment
# Set a color scheme:
tsRainbow <- rainbow(ncol(zoo.dp))
# Plot the overlayed series


#**************************************
#       Write returns to table        #
#**************************************
templst <- as.data.frame(tail(dp,1))
templst <- melt(templst)                        #cast long
templst$value <- round(templst$value,2)
#tor <- templst[order(templst,decreasing = TRUE),]
assign("tor",templst[order(templst[,2],decreasing = TRUE),], envir = .GlobalEnv) #create a variable that is returned to the environme
#return(tor)

}



