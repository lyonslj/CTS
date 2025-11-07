P&L <- function(portfolio = "LiveP") {
        library(readxl)
        Trades <- read_excel("/Users/johnlyons/Dropbox/CTS/Trades.xlsx")
        Trades$Purchase <- as.Date(Trades$Purchase)
        Trades$`Exit Dt` <- as.Date(Trades$`Exit Dt`)
        alldata2 <- subset(alldata,alldata$Date >= "2016-01-01")
        #if(portfolio == "LiveP") { 
                     # Live
        #} else { 
        #        mylst <- subset(Trades,!is.na(Trades$Exit)) }    # Historical
        
        #*********************************************
        # Build xts PLsets of Live Port              *
        #*********************************************
        mylst <- subset(Trades,is.na(Trades$Exit)) 
        instLive <- mylst[,1]
        for(i in instLive) {
                rown <- which(mylst$Instrm == i)
                y <- subset(alldata2,alldata2$Instm == mylst[rown,1]  & alldata2$Date >= mylst[rown,2])
                y$pl <- ((y[,3] - mylst[rown,3])*mylst[rown,5])/100           # change in P * Q held
                pl <- xts(y[,7],order.by = y[,2])        # convert to xts
                assign(mylst[rown,1],pl)
                }

        #*********************************************
        # Build xts PL sets of old Trds*
        #*********************************************
        mylst <- subset(Trades,!is.na(Trades$Exit))     # Historical
        instOld <- mylst[,1]
        for(i in instOld) {
                rown <- which(mylst$Instrm == i)
                y <- subset(alldata2,alldata2$Instm == mylst[rown,1]  & alldata2$Date <= mylst[rown,7]  & alldata2$Date >= mylst[rown,2])    
                y$pl <- ((y[,3] - mylst[rown,3])*mylst[rown,5])/100           # change in P * Q held
                pl <- xts(y[,7],order.by = y[,2])        # convert to xts
                assign(mylst[rown,1],pl)
                }                                
        #*********************************************
        #merge all closing prices of mylist into myxts
        #*********************************************
        inst <- append(instLive,instOld)
        myxts <- do.call(merge,sapply(inst,as.name))   # build single xts frame of instr closing prices
        myxts$Nett <- rowSums(myxts,na.rm = T)          
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





