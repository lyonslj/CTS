AdvDecl <- function(){
        #*****************************************************************
        #   Calculate Advance Decline ratio                              #
        #*****************************************************************        
        setwd("/Volumes/C/Equis")
        alldata <- read.csv("allclean.csv", header = FALSE, sep = "")
        names(alldata) <- c("Instm", "Date", "Close", "High", "Low", "Volume")
        alldata$Date <- as.Date(as.character(alldata$Date), format="%Y%m%d")
        ix <- 3:6      #convert integers to numeric
        alldata[ix] <- lapply(alldata[ix], as.numeric)
        alldata <- unique(alldata)
        mydata <- head(y,0)
        alldata2 <- subset(alldata,alldata$Date >= "2016-06-01")
        #group by instm
        z <- aggregate(Close ~ Instm, alldata2, length)
        #choose instm who have more than 30 record
        lst <- subset(z$Instm,z$Close > 30)
        for(i in lst) {
                ##
                y <- subset(alldata2,alldata2$Instm == i)
                x <- as.zoo(y$Close)
                x1 <- lag(x,-1,na.pad = TRUE)
                y$dm <- as.numeric(x - x1)
                #y$pm <- (Delt(y$Close))*100     #Calculate daily %age move
                mydata <- rbind(y,mydata)
        }
        advance <- subset(mydata,mydata$dm > 0)
        decline <- subset(mydata,mydata$dm < 0)
        days_add <- aggregate(dm ~ Date,advance, length)
        days_decl <- aggregate(dm ~ Date,decline, length)
        AD <- cbind(days_add,days_decl$dm)
        names(AD) <- c("Date","adv","decl")
        AD$nett <- AD$adv - AD$decl
        mypath <- file.path("/Users/johnlyons/Documents/Personal/DataScience/R/RPlots/AD.png")
        png(file=mypath,width = 1024,height = 768)
        barplot((AD$nett),col = "green",names.arg = format(AD$Date,"%d%m"),main = "Advance Decline")
        grid(NULL,NULL)
        dev.off()
        }
