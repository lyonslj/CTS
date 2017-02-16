ProffitLoss <- function() {
        library(readxl)
        library(dplyr)
        Trades <- read_excel("/Users/johnlyons/Dropbox/CTS/Trades.xlsx")
        Trades$Purchase <- as.Date(Trades$Purchase)
        Trades$`Exit Dt` <- as.Date(Trades$`Exit Dt`)
        Trades <- as.data.frame(Trades)
        alldata2 <- subset(JSEdat,JSEdat$Date >= "2016-01-01")
        #*********************************************
        # Build xts PLsets of Live Port              *
        #*********************************************
        mylst <- subset(Trades,is.na(Trades$Exit)) 
        mylst <- as.data.frame(mylst)
        instLive <- mylst[,1]
        instLive <- as.data.frame(instLive)
        instLive <- instLive[!is.na(instLive)]          #remove NA's
        LiveP <- NULL
        for(i in instLive) {
                rown <- which(mylst$Instrm == i)
                y <- subset(alldata2,alldata2$Name == mylst[rown,1]  & alldata2$Date >= mylst[rown,2])
                y$pl <- ((y[,6] - mylst[rown,3])*mylst[rown,5])/100           # change in P * Q held
                y$RealisedPL <- 0
                LiveP <- rbind(LiveP,y)
        }
        #*********************************************
        # Build xts PL sets of old Trds*
        #*********************************************
        instOld <- which(Trades$Exit >0)                #return index number
        OldP <- NULL
        for(i in instOld) {
                y_pre <- subset(alldata2,alldata2$Name == Trades[i,1]  & alldata2$Date >= as.Date(Trades[i,2]) & alldata2$Date < as.Date(Trades[i,7]))
                y_pre$pl <- ((y_pre[,3] - Trades[i,3])*Trades[i,5])/100
                y_pre$RealisedPL <- 0
                y_post <- subset(alldata2,alldata2$Name == Trades[i,1] & alldata2$Date >= as.Date(Trades[i,7]))
                y_post$pl <- 0
                y_post$RealisedPL <- ((Trades[i,6]- Trades[i,3])*Trades[i,5])/100
                y_combined <- rbind(y_pre,y_post)
                OldP <- rbind(OldP,y_combined)
        }
                              
        #*********************************************
        #merge all closing prices of mylist into myxts
        #*********************************************
        myNett <- rbind(unique(LiveP),unique(OldP))
        myNett <- aggregate(cbind(pl,RealisedPL) ~ Date, myNett, FUN = sum)
        myNett$Nett <- myNett$pl + myNett$RealisedPL
        Nettxts <- xts(myNett[,2:4],order.by = myNett[,1]) 
        zoo.Nettxts <- as.zoo(Nettxts)                    #convert to zoo to keep column names
        legnd <- names(zoo.Nettxts)
        mypath <- paste(file.path("/Users/johnlyons/Documents/Personal/DataScience/R/RPlots/PL"),".png",sep="")
        png(file=mypath,width = 1024,height = 768)
        # Set a color scheme:
        tsRainbow <- rainbow(ncol(zoo.Nettxts))
        # Plot the overlayed series
        plot(x = zoo.Nettxts, main = "Nett", col = tsRainbow, screens = 1,lwd = 2)
        grid(NULL,NULL)
        abline(h=0)
        # Set a legend in the upper left hand corner to match color to return series
        legend("topleft",legend = legnd,inset=.01,cex = 1.2,lty=c(1,1),lwd=c(2,2),bg="grey96",col = tsRainbow ) 
        dev.off()
        plot(x = zoo.Nettxts, main = "Nett", col = tsRainbow, screens = 1,lwd = 2)
        grid(NULL,NULL)
        abline(h=0)
        # Set a legend in the upper left hand corner to match color to return series
        legend("topleft",legend = legnd,inset=.01,cex = 0.5,lty=c(1,1),lwd=c(2,2),bg="grey96",col = tsRainbow )
}



