fnGecko <- function() {
        
        library(tidyr)
        library(rvest)
        library(openxlsx)
        library(dygraphs)
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
        ## Return price in BTC
        btc_price <- crypts[crypts$CoinSymbol == "BTC","Price_USD"]
        crypts <- mutate(crypts,Price_BTC = Price_USD / btc_price)
        ## Return price in ETH
        eth_price <- crypts[crypts$CoinSymbol == "ETH","Price_USD"]
        crypts <- mutate(crypts,Price_ETH = Price_USD / eth_price)
        file <- paste("/Users/johnlyons/Documents/Personal/DataScience/R/Stuff/Gecko:",
                      Sys.Date(),".xlsx",sep="")
        write.xlsx(crypts,file)
        return(View(crypts))
        
}

######################################################


        # Look for new files
        file.list <- list.files(path = "/Users/johnlyons/Documents/Personal/DataScience/R/Stuff",pattern='.*Gecko*')
        
        
        ## Now append them to existing file
        setwd("/Users/johnlyons/Documents/Personal/DataScience/R/Stuff")
        df.lst <- lapply(file.list ,read.xlsx, detectDates = TRUE)        # read new csv's#
        crypts_hist <- ldply(df.lst, rbind)           # combine into one datafram
        setwd("/Users/johnlyons/Documents/Personal/DataScience/R/JL CTS scripts")
        ## Remove old extracts ie where there is no price in ETH
        ch_mod <- crypts_hist[is.na(crypts_hist$CoinName) &
                                      is.na(crypts_hist$CoinSymbol) &
                                      is.na(crypts_hist$Price_USD) &
                                      is.na(crypts_hist$`24hVol`) &
                                      is.na(crypts_hist$MarketCap),]
        
        ## Grouping by date
        ch_mod_grp <- ch_mod %>%
                select(Dt,`24h`) %>%
                mutate(Pos = ifelse(`24h` > 0, 1,0),
                       Neg = ifelse(`24h` <= 0,1,0))  %>%
                group_by(Dt) %>%
                dplyr::summarise(PosCount =  sum(Pos, na.rm = TRUE),
                                 NegCount = sum(Neg, na.rm = TRUE),
                                 DailyMediamMove = median(`24h`, na.rm = TRUE))
        
        ch_mod_grp <- xts(ch_mod_grp, order.by = as.POSIXct(ch_mod_grp$Dt))
                                 
                            
        
       
        dygraph(ch_mod_grp[,4], main = "Percent", group = "stock") %>%
                dyOptions(colors = RColorBrewer::brewer.pal(10, "Paired")) %>%
                dyAxis("y", label = "Median", valueRange = c(-20, 20)) %>%
                dyLimit(0, color = "black") %>% 
                dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
                dyLegend(show = "onmouseover" ,hideOnMouseOut = TRUE) %>%
                dyLegend(width = 800)
        
        
        
       
        
        htmltools::browsable(htmltools::tagList(dygraph(ch_mod_grp, main = "Percent", group = "stock") %>%
                                                        dyOptions(colors = RColorBrewer::brewer.pal(10, "Paired")) %>%
                                                        dyLimit(150, color = "black") %>% 
                                                        dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
                                                        dyLegend(show = "onmouseover" ,hideOnMouseOut = TRUE) %>%
                                                        dyLegend(width = 800)))
        