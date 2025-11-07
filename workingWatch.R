# Load necessary libraries
library(xts)
library(data.table)
library(TTR) # For ROC calculation
library(lubridate) # For date calculations


# Convert to correct format
JSEloaded_dat <- read.csv("/Users/johnlyons/Documents/Personal/DataScience/R/JSEdat.csv", header = TRUE, sep="")
## -- Convert to correct format
JSEloaded <- JSEloaded_dat
JSEloaded$Date <- as.Date(JSEloaded$Date, "%Y-%m-%d")
# Use a data.table for efficiency
JSEdat <- as.data.table(JSEloaded)
JSEdat <- unique(JSEdat[complete.cases(JSEdat),])
last_100_dates <- sort(unique(JSEdat$Date), decreasing = TRUE)[1:100]
JSEdat_filtered <- JSEdat[Date %in% last_100_dates]

# Ensure columns are numeric
cols <- c("Open", "High", "Low", "Close", "Volume")
JSEdat_filtered[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]             # := assignment operator It modifies the columns directly without creating a copy of the entire data table, which is highly memory-efficient. .SD: Stands for "Subset of Data

# Call exclusion lists
lists <- fnLists()
excl_list <- lists$excl_list

flagged_instruments <- NULL

# --- Main Processing Logic ---

# Create a function to perform the required filtering and flagging
process_jse_data <- function(dat_t, excl_list, min_records = 50) {
        
        # Ensure required columns are present
        # Make sure 'Volume' is included in req_cols to be passed to flag_instrument
        req_cols <- c("Name", "Date", "Open", "High", "Low", "Close", "Volume")
        if (!all(req_cols %in% names(dat_t))) {
                stop("Required columns (Name, Date, Open, High, Low, Close, Volume) are missing.")
        }
        
        # --- Step 1: Filter instruments ---
        # Exclude instruments in the excl_list
        dat_t <- dat_t[!Name %in% excl_list]
        
        # Filter out instruments with fewer than `min_records`
        dat_t[, record_count := .N, by = Name]                  # .N:  symbol in data.table calcs the number of rows 
        dat_t <- dat_t[record_count >= min_records]
        dat_t[, record_count := NULL] # Remove temporary column
        
        # --- Step 2: Flag instruments based on price conditions ---
        # Define the flagging function for each instrument
        flag_instrument <- function(instrument_data) {
                
                # Sort data by date to ensure correct order
                instrument_data <- instrument_data[order(Date)]
                
                # **FIX:** Remove any rows with NA values before performing calculations
                instrument_data <- na.omit(instrument_data)
                
                # Get the latest day's data (last row)
                latest_day <- tail(instrument_data, 1)
                
                # Check if there is enough data for the 21-day SMA after NA removal
                if (nrow(instrument_data) < 21) {
                        return(FALSE)
                }
                
                # This assumes the JSEdat object is accessible from the function's environment
                max_date_dataset <- max(JSEdat$Date, na.rm = TRUE)
                
                # Get the latest day's data (last row)
                latest_day <- tail(instrument_data, 1)
                
                # Get the latest date for the instrument
                latest_date_instrument <- latest_day$Date
                
                # Calculate the simple moving average of the Close prices
                sma_21 <- TTR::SMA(instrument_data$Close, n = 21)
                #sma_50 <- TTR::SMA(instrument_data$Close, n = 50)
                
                # Get the value of the SMAs on the latest day
                latest_sma_21 <- tail(sma_21, 1)
                #latest_sma_50 <- tail(sma_50, 1)
                
                # Get the latest day's close, high, and low prices
                latest_close <- latest_day$Close
                latest_high <- latest_day$High
                latest_low <- latest_day$Low
                
                
                
                # Get the previous 2 weeks of data (10 trading days excluding the latest day)
                previous_2_weeks_data <- tail(instrument_data[Date < latest_day$Date], 10)
                
                if (nrow(previous_2_weeks_data) < 1) {
                        return(FALSE)
                }
                
                # Calculate the highest high of the previous 2 weeks
                high_of_prev_2_weeks <- max(previous_2_weeks_data$High)
                
                # Check the conditions
                condition1 <- latest_close >= high_of_prev_2_weeks
                condition2 <- (latest_close - latest_low) / (latest_high - latest_low) >= 0.80
                condition3 <- median(previous_2_weeks_data$Volume, na.rm = TRUE) >= 10000
                condition4 <- latest_close >= latest_sma_21
                #condition5 <- latest_close >= latest_sma_50
                
                return(condition1 && condition2 && condition3 && condition4 )
        }
        
        # Apply the flagging function to each instrument group
        # Ensure all required columns are passed to .SD (Subset of Data)
        dat_t[, flagged := flag_instrument(.SD), by = Name, .SDcols = req_cols]
        
        # Return the data.table with the new 'flagged' column
        return(dat_t)
}
# Run the function on the cleaned data
 JSE_processed <- process_jse_data(dat_t = JSEdat_filtered, excl_list = excl_list, min_records = 40)

# View the flagged instruments
 flagged_instruments <- JSE_processed[flagged == TRUE]
 flagged_instruments <- (unique(flagged_instruments$Name))
 RSC(flagged_instruments,"Flagged,Weekly","Weekly")
 fnDyGraph(flagged_instruments)
