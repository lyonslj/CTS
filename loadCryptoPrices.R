library(jsonlite)
library(lubridate)
library(dplyr)

# Define paths and instruments
data_dir <- "/Users/johnlyons/Documents/Personal/DataScience/R/"
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

mylst <- c("PAXG","AAVE","MKR","BTC","ETH","EOS","BNB","ADA","XRP","LTC","LINK","SNX","XLM","HEX","VET","MATIC","DOGE","ETC","SOL","SHIB","CRO","TRX","THETA","XRD","AVAX")
wtchlst <- c("TAO","PENDLE")
all_inst <- c(mylst, wtchlst)

# File path for saved data
data_file <- file.path(data_dir, "crypto_data.rds")

# Function to remove duplicates from data frame
remove_df_duplicates <- function(df) {
        if (nrow(df) > 0) {
                # Keep the last entry for each Name-Date combination
                df <- df %>%
                        group_by(Name, Date) %>%
                        slice_tail(n = 1) %>%
                        ungroup()
        }
        return(df)
}

# Function to fetch new data from CryptoCompare
fetch_crypto_data <- function(symbol, from_date = NULL) {
        if (is.null(from_date)) {
                # Initial load: get last 200 days
                url <- paste0("https://min-api.cryptocompare.com/data/histoday?fsym=", symbol, "&tsym=USD&limit=200&aggregate=1")
        } else {
                # Update: get data from one day before specified date to current day
                to_timestamp <- as.numeric(as.POSIXct(Sys.Date() + days(1))) # Include today
                from_timestamp <- as.numeric(as.POSIXct(from_date))
                url <- paste0("https://min-api.cryptocompare.com/data/histoday?fsym=", symbol, "&tsym=USD&fromTs=", from_timestamp, "&toTs=", to_timestamp, "&aggregate=1")
        }
        
        y <- fromJSON(url)$Data
        if (length(y$time) == 0) return(NULL) # Handle empty response
        y$time <- as.Date(as.POSIXct(y$time, origin = "1970-01-01", tz = "GMT"))
        df <- data.frame(
                Name = symbol,
                Date = y$time,
                Open = y$open,
                High = y$high,
                Low = y$low,
                Close = y$close,
                Volume = y$volumeto
        )
        # Remove duplicates from fetched data
        remove_df_duplicates(df)
}

# Function to load or initialize data
load_or_initialize_data <- function() {
        if (file.exists(data_file)) {
                # Load existing data
                all_data <- readRDS(data_file)
                # Deduplicate data
                all_data <- remove_df_duplicates(all_data)
                return(all_data)
        }
        
        # Initial load
        all_data <- data.frame()
        for (symbol in all_inst) {
                message("Fetching initial data for ", symbol)
                df <- fetch_crypto_data(symbol)
                if (!is.null(df)) {
                        all_data <- rbind(all_data, df)
                }
                Sys.sleep(1) # Avoid API rate limits
        }
        
        # Deduplicate combined data
        all_data <- remove_df_duplicates(all_data)
        
        # Save to file
        saveRDS(all_data, data_file)
        
        # Create individual close series in global environment
        for (symbol in mylst) {
                symbol_data <- all_data %>% filter(Name == symbol) %>% select(Date, Close)
                if (nrow(symbol_data) > 0) {
                        assign(paste0(symbol, "_close"), xts(symbol_data$Close, order.by = symbol_data$Date), envir = .GlobalEnv)
                }
        }
        
        return(all_data)
}

# Function to update data
update_crypto_data <- function() {
        all_data <- load_or_initialize_data()
        
        for (symbol in all_inst) {
                message("Checking updates for ", symbol)
                symbol_data <- all_data %>% filter(Name == symbol)
                if (nrow(symbol_data) > 0) {
                        last_date <- max(symbol_data$Date)
                        
                        # Remove last day's data to avoid duplicates
                        all_data <- all_data %>% filter(!(Name == symbol & Date >= last_date))
                        
                        # Fetch new data from one day before last date
                        new_data <- fetch_crypto_data(symbol, last_date - days(1))
                        
                        if (!is.null(new_data) && nrow(new_data) > 0) {
                                # Append new data
                                all_data <- rbind(all_data, new_data)
                                # Deduplicate after appending
                                all_data <- remove_df_duplicates(all_data)
                                
                                # Update close series in global env
                                symbol_close <- all_data %>% filter(Name == symbol) %>% select(Date, Close)
                                if (nrow(symbol_close) > 0) {
                                        assign(paste0(symbol, "_close"), xts(symbol_close$Close, order.by = symbol_close$Date), envir = .GlobalEnv)
                                }
                        }
                }
                Sys.sleep(1) # Avoid API rate limits
        }
        
        # Save updated data
        saveRDS(all_data, data_file)
        
        return(all_data)
}

# Main function to get current data
get_crypto_data <- function() {
        if (file.exists(data_file)) {
                # Always update to get latest intra-day prices
                message("Updating data...")
                return(update_crypto_data())
        } else {
                message("Performing initial data load...")
                return(load_or_initialize_data())
        }
}

# Usage example:
# all_prices <- get_crypto_data()
