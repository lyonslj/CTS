fnVirtDaily <- function () {

library(dygraphs)
library(plyr)
library(dplyr)
library(tidyr)
library(quantmod)
library(xts)
library(flexdashboard)
library(readxl)
library(openxlsx)
library(gplots)
library(RColorBrewer)
library(reshape2)
library(jsonlite)
library(tidyr)  
library(gplots)
library(RColorBrewer)
library(coinmarketcapr)
library(TTR)
library(htmltools)
        

source('~/Documents/Personal/DataScience/R/JL CTS scripts/RscHeatmapV.R')
source('~/Documents/Personal/DataScience/R/JL CTS scripts/RelativeStrengthComparativeV.R')
source('~/Documents/Personal/DataScience/R/JL CTS scripts/LoadCryptoPrices.R')
  

# Call function to load latest prices and expose to global env
all_prices <- get_crypto_data()
all_data <<- all_prices
# Generate RSC
mylst <- c("PAXG","AAVE","MKR","BTC","ETH","EOS","BNB","ADA","XRP","LTC","LINK","SNX","XLM","HEX","VET","MATIC","DOGE","ETC","SOL","SHIB","CRO","TRX","THETA","XRD","AVAX")
wtchlst <- c("TAO","PENDLE","RENDER","ONDO")
lst  <- c(mylst, wtchlst)
RSCV(lst)
fnAboveMA(all_data)
fnDyGraph(unique(c("BTC",to_graph)),all_data)

###############################################################################
#             Determine if price is above or below its 21SMA                  #
###############################################################################
# Create xts object (as provided)
z <- xts(all_data[, c("Close", "High", "Low")], order.by = all_data$Date)

# Initialize an empty list to store results for each cryptocurrency
results <- list()

# Get unique cryptocurrencies
crypts <- unique(all_data$Name)

# Loop through each cryptocurrency
for (i in crypts) {
        # Subset data for the current cryptocurrency
        temp_data <- all_data[all_data$Name == i, ]
        
        # Check if temp_data has rows
        if (nrow(temp_data) == 0) {
                warning(paste("No data for cryptocurrency:", i))
                next
        }
        
        # Create xts object for the current cryptocurrency
        temp_xts <- tryCatch(
                {
                        xts(temp_data[, c("Close", "High", "Low")], order.by = temp_data$Date)
                },
                error = function(e) {
                        warning(paste("Error creating xts for", i, ":", e$message))
                        return(NULL)
                }
        )
        
        # Skip if xts creation failed
        if (is.null(temp_xts)) next
        
        # Calculate 21-day and 50-day SMA for the Close price
        temp_xts$Sma21 <- SMA(temp_xts$Close, n = 21)
        temp_xts$Sma50 <- SMA(temp_xts$Close, n = 50)
        
        # Check if SMA calculations are valid
        if (all(is.na(temp_xts$Sma21)) || all(is.na(temp_xts$Sma50))) {
                warning(paste("SMA calculation failed for", i))
                next
        }
        
        # Calculate res based on Close, Sma21, and Sma50
        temp_xts$res <- ifelse(
                (temp_xts$Close > temp_xts$Sma21) & (temp_xts$Sma21 > temp_xts$Sma50), 2, # Close > 21 > 50
                ifelse(
                        (temp_xts$Sma21 > temp_xts$Close) & (temp_xts$Close > temp_xts$Sma50), 1, # 21 > Close > 50
                        ifelse(
                                (temp_xts$Sma50 > temp_xts$Close) & (temp_xts$Close > temp_xts$Sma21), 0, # 50 > Close > 21
                                ifelse(
                                        (temp_xts$Close > temp_xts$Sma50) & (temp_xts$Sma50 > temp_xts$Sma21), 1, # Close > 50 > 21
                                        -2
                                )
                        )
                )
        )
        
        # Keep only the res column and name it with the cryptocurrency
        temp_xts <- temp_xts[, "res"]
        colnames(temp_xts) <- i
        
        # Store in results list
        results[[i]] <- temp_xts
}

# Check if results list is empty
if (length(results) == 0) {
        stop("No valid xts objects were created. Check data for all cryptocurrencies.")
}

# Merge all results into a single xts object
sma_set <- do.call(merge.xts, results)
 
x <- tail(sma_set,80)
x <- t(x)
RscHeatmap(x,nm = "Crypts21MA", cumret = FALSE)
        
}