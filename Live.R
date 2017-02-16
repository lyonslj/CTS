LiveCumulative <- function(portfolio = "LiveP", basis = "pm") {
library(readxl)
Trades <- read_excel("/Users/johnlyons/Dropbox/CTS/Trades.xlsx")
Trades$Purchase <- as.Date(Trades$Purchase)
Trades$`Exit Dt` <- as.Date(Trades$`Exit Dt`)
Trades <- as.data.frame(Trades)
alldata2 <- subset(alldata,alldata$Date >= "2016-01-01")
if(portfolio == "LiveP") { 
        mylst <- subset(Trades,is.na(Trades$Exit))      # Live
        } else { 
        mylst <- subset(Trades,!is.na(Trades$Exit)) }    # Historical
        mylst <- as.data.frame(mylst)
inst <- unique(mylst[,1])
inst <- as.data.frame(inst)
inst <- inst[!is.na(inst)]
#*********************************************
# Build individual xts sets of closing prices*
#*********************************************
for(i in inst) {
        rown <- which(mylst$Instrm == i)
        if(portfolio == "LiveP") { 
                y <- subset(alldata2,alldata2$Instm == mylst[rown,1]  & alldata2$Date >= mylst[rown,2])
                y <- unique(y)
        } else { 
                y <- subset(alldata2,alldata2$Instm == mylst[rown,1]  & alldata2$Date <= mylst[rown,7]  & alldata2$Date >= mylst[rown,2]) }   
        if(basis == "pm"){
                y$pm <- ((y[,3] - mylst[rown,3])/mylst[rown,3])*100           # change in P * Q held
                pm <- xts(y[,7],order.by = y[,2])        # convert to xts
                assign(mylst[rown,1],pm)
        } else {
                y$pl <- ((y[,3] - mylst[rown,3])*mylst[rown,5])/100   
                pl <- xts(y[,7],order.by = y[,2])        # convert to xts
                assign(mylst[rown,1],pl)
        }
}
#*********************************************
#merge all closing prices of mylist into myxts
#*********************************************
myxts <- do.call(merge,sapply(inst,as.name))   # build single xts frame of instr closing prices
dm <- rowSums(myxts,na.rm = T)           # Cumulative Total
zoo.myxts <- as.zoo(myxts)                    #convert to zoo to keep column names
mypath <- paste(file.path("/Users/johnlyons/Documents/Personal/DataScience/R/RPlots/"),portfolio,".png",sep="")
png(file=mypath,width = 1024,height = 768)
# Set a color scheme:
tsRainbow <- rainbow(ncol(zoo.myxts))
# Plot the overlayed series
plot(x = zoo.myxts, ylab = "Cumulative Return %age", main = portfolio, col = tsRainbow, screens = 1,lwd = 2)
grid(NULL,NULL)
abline(h=0)
# Set a legend in the upper left hand corner to match color to return series
legend("topleft",legend = inst,inset=.01,cex = 1.2,lty=c(1,1),lwd=c(2,2),bg="grey96",col = tsRainbow ) 
dev.off()
plot(x = zoo.myxts, ylab = "Cumulative Return %age", main = portfolio, col = tsRainbow, screens = 1,lwd = 2)
grid(NULL,NULL)
abline(h=0)
# Set a legend in the upper left hand corner to match color to return series
legend("topleft",legend = inst,inset=.01,cex = 0.5,lty=c(1,1),lwd=c(2,2),bg="grey96",col = tsRainbow )
}





