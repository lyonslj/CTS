ProffitLoss <- function() {
        library(readxl)
        library(dplyr)
        Trades <- read_excel("/Users/johnlyons/Dropbox/CTS/Trades.xlsx")
        Trades$Purchase <- as.Date(Trades$Purchase)
        Trades$`Exit Dt` <- as.Date(Trades$`Exit Dt`)
        Trades <- as.data.frame(Trades)
        alldata2 <- subset(alldata,alldata$Date >= "2016-01-01")
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
                y <- subset(alldata2,alldata2$Instm == mylst[rown,1]  & alldata2$Date >= mylst[rown,2])
                y$pl <- ((y[,3] - mylst[rown,3])*mylst[rown,5])/100           # change in P * Q held
                y$RealisedPL <- 0
                #pl <- xts(y[,7],order.by = y[,2])        # convert to xts
                LiveP <- rbind(LiveP,y)
                #assign(mylst[rown,1],y)
        }
        #myNett <- aggregate(pl ~ Date, LiveP, FUN = sum)

        #*********************************************
        # Build xts PL sets of old Trds*
        #*********************************************
        #mylst <- subset(Trades,!is.na(Trades$Exit))     # Historical
        #mylst <- as.data.frame(mylst)
        #instOld <- mylst[,1]
        instOld <- which(Trades$Exit >0)                #return index number
        OldP <- NULL
        for(i in instOld) {
                #rown <- which(mylst$Instrm == i)
                #y <- subset(alldata2,alldata2$Instm == mylst[rown,1]  & alldata2$Date >= mylst[rown,2]) 
                y_pre <- subset(alldata2,alldata2$Instm == Trades[i,1]  & alldata2$Date >= Trades[i,2] & alldata2$Date < Trades[i,7]) 
                #y_pre <- y[(which(y$Date < mylst[rown,7])),]
                #y_pre$pl <- ((y_pre[,3] - mylst[rown,3])*mylst[rown,5])/100
                y_pre$pl <- ((y_pre[,3] - Trades[i,3])*Trades[i,5])/100
                y_pre$RealisedPL <- 0
                #y_post <- y[(which(y$Date >= mylst[rown,7])),]
                y_post <- subset(alldata2,alldata2$Instm == Trades[i,1] & alldata2$Date >= Trades[i,7])
                y_post$pl <- 0
                #y_post$RealisedPL <- ((mylst[rown,6]- mylst[rown,3])*mylst[rown,5])/100
                y_post$RealisedPL <- ((Trades[i,6]- Trades[i,3])*Trades[i,5])/100
                y_combined <- rbind(y_pre,y_post)
                OldP <- rbind(OldP,y_combined)
                #pl <- xts(y_combined[,7],order.by = y_combined[,2]) 
                #assign(mylst[rown,1],y_combined)                # extract pl in row 7
        }
                              
        #*********************************************
        #merge all closing prices of mylist into myxts
        #*********************************************
        #myOld <- do.call(rbind,sapply(instOld,as.name))         #old Trds
        #myLive <- do.call(rbind,sapply(instLive,as.name))       #Live Trds
        #myNett <- rbind(myOld,myLive)
        myNett <- rbind(unique(LiveP),unique(OldP))
        myNett <- aggregate(cbind(pl,RealisedPL) ~ Date, myNett, FUN = sum)
        myNett$Nett <- myNett$pl + myNett$RealisedPL
        Nettxts <- xts(myNett[,2:4],order.by = myNett[,1]) 
        zoo.Nettxts <- as.zoo(Nettxts)                    #convert to zoo to keep column names
        legnd <- names(zoo.Nettxts)
        mypath <- paste(file.path("/Users/johnlyons/Documents/Personal/DataScience/R/RPlots/PL"),".png",sep="")
        png(file=mypath,width = 1024,height = 768)
        # Set a color scheme:
        tsRainbow <- rainbow(ncol(zoo.myxts))
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



