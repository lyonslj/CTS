fnGecko <- function() {
        
        library(rvest)
        library(openxlsx)
        data <- read_html("https://www.coingecko.com/en")
        ## Identify all tables
        tbls <- html_nodes(data,"table") 
        ## Extract data from the table you want
        dat <-  html_table(tbls,header = NA,"gecko-table") 
        
        ## View the data
        crypts_dat <- dat[[1]]  
        ## Structure the data
        crypts <- crypts_dat[,c(3,4,8,9)]
        colnames(crypts) <- c("Coin","Price_USD","24hVol","MarketCap")
        ## Get rid comma and Convert to numeric
        crypts <- apply(crypts, 2, function(y) as.character(gsub(",", "", y)))
        crypts <- apply(crypts, 2, function(y) as.character(gsub("\\$", "", y)))
        crypts <- data.frame(crypts)
        colnames(crypts) <- c("Coin","Price_USD","24hVol","MarketCap")
        ## Convert to numeric
        fields <- c("Price_USD","24hVol","MarketCap")
        options(scipen = 999) # disable scientific notation
        crypts[fields] <- sapply(crypts[fields],as.character)
        crypts[fields] <- sapply(crypts[fields],as.numeric)
        ## Add dt column
        crypts$Dt <- Sys.Date()
        ## Seperate coin field
        crypts <- separate(crypts, Coin, into = c("junk","CoinName","CoinSymbol"))
        crypts <- crypts[-1]
        
        View(crypts)
        
        
        file <- paste("/Users/johnlyons/Documents/Personal/DataScience/R/Stuff/Gecko:",
                      Sys.Date(),".xlsx",sep="")
        write.xlsx(crypts,file)
        
}


