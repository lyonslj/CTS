WrWeek <- function() {
        #*****************************************************************
        #   Find all instruments whose RSC(alsi40) > than its 24dma      #
        #*****************************************************************        
        #only need 20 records
        alldata2 <- subset(JSEdat,JSEdat$Date >= Sys.Date()-20)
        allsort <- alldata2[with(alldata2, order(Date)), ]      #sort by data
        lastdt <- tail(allsort$Date,1)                          #get last dt
        NameSet <- subset(allsort$Name,allsort$Volume > 40000 & allsort$Date == lastdt)         # Vol > 40000
        
        WrdLst <- NULL
        for(i in NameSet) {
                y <- subset(allsort,allsort$Name == i)
                z <- xts(y[,3:6],order.by = y[,2])              
                z <- to.weekly(z)
                stv <- as.numeric(as.character(tail(z,2)))      # last 2 weeks
                if(
                        (length(stv) == 8 & (stv[1] < stv[7]) & stv[8] > stv[7] ) & (stv[2] < stv[1]) |
                        (length(stv) == 8 & (stv[1] > stv[7]) & stv[8] > stv[1] ) & (stv[2] < stv[7]) |
                   (length(stv) == 8 & (stv[1] < stv[7]) & stv[8] < stv[1] ) & (stv[2] > stv[7]) |
                   (length(stv) == 8 & (stv[1] > stv[7]) & stv[8] < stv[7] ) & (stv[2] > stv[1])) {    
                        dat <- subset(JSEdat,JSEdat$Name == i & JSEdat$Date >= "2015-01-01")
                        z <- xts(dat[,3:7],order.by = dat[,2])  
                        chartSeries(to.weekly(z),name = paste(i,":Weekly"),theme = "white",TA="addOBV();addVo();addMACD()")
                        WrdLst <- append(WrdLst,i)
                        #,TA="addSMA(n=24);addOBV();addMACD();addVo()")
                        }
        }
        print(WrdLst)
}
   
 
