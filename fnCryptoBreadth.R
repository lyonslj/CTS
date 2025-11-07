fnCryptoBreadth <- function() {
        library(tidyr)
        library(rvest)
        library(openxlsx)
        library(dygraphs)
        library(dplyr)
        data <- read_html("https://www.coingecko.com/en/coins/all")
        ## Identify all tables
        tbls <- html_nodes(data,"table")
        ## Extract data from the table you want
        dat <-  html_table(tbls,header = NA,"gecko-table")
        ## View the data
        crypts_dat <- dat[[1]]
        names(crypts_dat) <- c("rank","Coin","Symbol","Price","1h","24h","7d","30d","24hVol:USD","CirculatingSupply","TotSupply","MktCap")
        crypts_dat$Symbol <- as.character(crypts_dat$Symbol)
        crypts_dat$Symbol <- gsub("\\\n","",crypts_dat$Symbol)
        crypts_dat <- apply(crypts_dat, 2, function(y) as.character(gsub("%", "", y)))
        crypts_dat <- apply(crypts_dat, 2, function(y) as.character(gsub("\\$", "", y)))
        crypts_dat <- apply(crypts_dat, 2, function(y) as.character(gsub(",", "", y)))
        crypts_dat <- as.data.frame(crypts_dat)
        fields <- c("Price","1h","24h","7d","30d","24hVol:USD", "CirculatingSupply", "MktCap")
        crypts_dat[fields] <- sapply(crypts_dat[fields], as.character)
        crypts_dat[fields] <- sapply(crypts_dat[fields], as.numeric)
        crypts_dat$Dt <- Sys.Date()
        ##
        ## Return price in BTC
        btc_price <- crypts_dat[crypts_dat$Symbol == "BTC","Price"]
        crypts_dat <- mutate(crypts_dat,Price_BTC = Price / btc_price)
        ## Return price in ETH
        eth_price <- crypts_dat[crypts_dat$Symbol == "ETH","Price"]
        crypts_dat <- mutate(crypts_dat,Price_ETH = Price / eth_price)
        file <- paste("/Users/johnlyons/Documents/Personal/DataScience/R/Stuff/Gecko:",
                      Sys.Date(),".xlsx",sep="")
        write.xlsx(crypts_dat,file)
        ##
        View(crypts_dat)
        Pos1h <- length(which(crypts_dat$`1h` > 0))
        Pos24h <- length(which(crypts_dat$`24h` > 0))
        Pos7d <- length(which(crypts_dat$`7d` > 0))
        Pos30d <- length(which(crypts_dat$`30d` > 0))
        Neg1h <- length(which(crypts_dat$`1h` <= 0))
        Neg24h <- length(which(crypts_dat$`24h` <= 0))
        Neg7d <- length(which(crypts_dat$`7d` <= 0))
        Neg30d <- length(which(crypts_dat$`30d` <= 0))
        Mean1hr <- mean(na.omit(crypts_dat$`1h`))
        Median1hr <- median(na.omit(crypts_dat$`1h`))
        Mean24h <- mean(na.omit(crypts_dat$`24h`))
        Median24hr <- median(na.omit(crypts_dat$`24h`))
        Mean7d <- mean(na.omit(crypts_dat$`7d`))
        Median7d <- median(na.omit(crypts_dat$`7d`))
        Mean30d <- mean(na.omit(crypts_dat$`30d`))
        Median30d <- median(na.omit(crypts_dat$`30d`))
        ##
        MedNegs1h <- median(na.omit(crypts_dat[crypts_dat$`1h` <0, "1h"]))
        MedNegs24h <- median(na.omit(crypts_dat[crypts_dat$`24h` <0, "24h"]))
        MedNegs7d <- median(na.omit(crypts_dat[crypts_dat$`7d` <0, "7d"]))
        MedNegs30d <- median(na.omit(crypts_dat[crypts_dat$`30d` <0, "30d"]))
        ##
        MedPos1h <- median(na.omit(crypts_dat[crypts_dat$`1h` > 0, "1h"]))
        MedPos24h <- median(na.omit(crypts_dat[crypts_dat$`24h` > 0, "24h"]))
        MedPos7d <- median(na.omit(crypts_dat[crypts_dat$`7d` > 0, "7d"]))
        MedPos30d <- median(na.omit(crypts_dat[crypts_dat$`30d` > 0, "30d"]))
        PosMoves <- c(Pos1h,Pos24h,Pos7d,Pos30d)
        NegMoves <- c(Neg1h,Neg24h,Neg7d,Neg30d)
        MeanMoves <- c(Mean1hr,Mean24h,Mean7d,Mean30d)
        MedianMoves <- c(Median1hr,Median24hr,Median7d,Median30d)
        MedianNegMoves <- c(MedNegs1h,MedNegs24h,MedNegs7d,MedNegs30d)
        MedianPosMoves <- c(MedPos1h,MedPos24h,MedPos7d,MedPos30d)
        CryptMoves <- rbind(PosMoves,NegMoves,MeanMoves,MedianMoves,MedianNegMoves,MedianPosMoves)
        CryptMoves <- as.data.frame(CryptMoves)
        #CryptMoves[1,] <- round(CryptMoves[1,2:4],0)
        names(CryptMoves) <- c("1h","24h","7d","30d")
        CryptMoves
        #return(,PosMoves))
}
