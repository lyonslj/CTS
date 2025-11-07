library(xts)
library(data.table)
library(TTR)
library(lubridate)


fnOppo <- function() {
        # Source fnLists safely
        # Source fnLists and fnDyGraph safely
        tryCatch({
                source("/Users/johnlyons/Documents/Personal/DataScience/R/JL CTS scripts/fnLists.R")
                source("/Users/johnlyons/Documents/Personal/DataScience/R/JL CTS scripts/fnDyGraph.R")
        }, error = function(e) {
                warning("Failed to source a required R file: ", e$message)
        })
        
        # Check if JSEdat exists
        if (exists("JSEdat", envir = .GlobalEnv)) {
                message("JSEdat data is already in the environment. No action taken.")
                JSEdat <- get("JSEdat", envir = .GlobalEnv)
        } else {
                JSEdat <- read.csv("/Users/johnlyons/Documents/Personal/DataScience/R/JSEdat.csv", 
                                   header = TRUE, sep = "")
                JSEdat$Date <- as.Date(JSEdat$Date, "%Y-%m-%d")
                assign("JSEdat", JSEdat, envir = .GlobalEnv)
                message("JSEdat data was not found and has been loaded.")
        }
        
        # Convert JSEdat to data.table and remove duplicates and NAs
        JSEdatdt <- as.data.table(JSEdat)
        JSEdatdt <- unique(JSEdatdt[complete.cases(JSEdatdt), ])
        
        # Get the last 100 unique dates
        last_100_dates <- sort(unique(JSEdatdt$Date), decreasing = TRUE)[1:100]
        
        # Filter JSEdat for rows with dates in last_100_dates
        JSEdatdt_filtered <- JSEdatdt[Date %in% last_100_dates]
        
        # Verify JSEdatdt_filtered structure
        if (nrow(JSEdatdt_filtered) == 0) {
                stop("JSEdatdt_filtered is empty. No data for the last 100 dates.")
        }
        if (!"Name" %in% colnames(JSEdatdt_filtered)) {
                stop("JSEdatdt_filtered is missing the 'Name' column.")
        }
        
        # Ensure columns are numeric
        cols <- c("Open", "High", "Low", "Close", "Volume")
        JSEdatdt_filtered[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
        
        # Call exclusion lists
        if (exists("fnLists") && is.function(fnLists)) {
                lists <- fnLists()
                excl_list <- lists$excl_list
                indexes <- if (!is.null(lists$Indexes)) lists$Indexes else character(0)
        } else {
                warning("fnLists is not defined. Using empty exclusion list and indexes.")
                excl_list <- character(0)
                indexes <- character(0)
        }
        
        # Initialize flagged_instruments
        flagged_instruments <- NULL
        
        # --- Main Processing Logic ---
        process_jse_data <- function(dat_t, excl_list, min_records = 50) {
                req_cols <- c("Name", "Date", "Open", "High", "Low", "Close", "Volume")
                if (!all(req_cols %in% names(dat_t))) {
                        stop("Required columns (Name, Date, Open, High, Low, Close, Volume) are missing.")
                }
                
                # Exclude instruments in the excl_list
                dat_t <- dat_t[!Name %in% excl_list, ]
                
                # Filter out instruments with fewer than min_records
                dat_t[, record_count := .N, by = Name]
                dat_t <- dat_t[record_count >= min_records]
                dat_t[, record_count := NULL]
                
                # --- Step 2: Flag instruments based on price conditions ---
                flag_instrument <- function(instrument_data) {
                        # Sort data by date
                        instrument_data <- instrument_data[order(Date)]
                        
                        # Remove any rows with NA values
                        instrument_data <- na.omit(instrument_data)
                        
                        # Check if enough data for 21-day SMA
                        if (nrow(instrument_data) < 21) {
                                return(FALSE)
                        }
                        
                        # Get the latest day's data
                        latest_day <- tail(instrument_data, 1)
                        latest_date_instrument <- latest_day$Date
                        
                        # Verify latest date matches max date in dataset
                        max_date_dataset <- max(JSEdatdt$Date, na.rm = TRUE)
                        if (latest_date_instrument < max_date_dataset) {
                                return(FALSE)
                        }
                        
                        # Calculate 21-day SMA
                        sma_21 <- TTR::SMA(instrument_data$Close, n = 21)
                        latest_sma_21 <- tail(sma_21, 1)
                        
                        # Get latest day's prices
                        latest_close <- latest_day$Close
                        latest_high <- latest_day$High
                        latest_low <- latest_day$Low
                        
                        # Get previous 2 weeks of data (10 trading days excluding latest day)
                        #previous_2_weeks_data <- tail(instrument_data[Date < latest_day$Date], 10)
                        previous_2_weeks_data <- tail(instrument_data[instrument_data$Date < latest_day$Date, ], 10)
                        if (nrow(previous_2_weeks_data) < 1) {
                                return(FALSE)
                        }
                        
                        # Calculate highest high of previous 2 weeks
                        high_of_prev_2_weeks <- max(previous_2_weeks_data$High)
                        
                        # Check conditions
                        condition1 <- latest_close >= high_of_prev_2_weeks
                        condition2 <- (latest_close - latest_low) / (latest_high - latest_low) >= 0.70
                        condition3 <- median(previous_2_weeks_data$Volume, na.rm = TRUE) >= 10000
                        condition4 <- latest_close >= latest_sma_21
                        
                        # Define instruments that bypass condition3
                        cond3_bypass <- c("C-BTCZAR", "C-ETHZAR", indexes)
                        
                        # Get instrument name
                        instrument_name <- unique(instrument_data$Name)
                        
                        # Apply conditions based on instrument
                        if (instrument_name %in% cond3_bypass) {
                                return(condition1 && condition2 && condition4)
                        } else {
                                return(condition1 && condition2 && condition3 && condition4)
                        }
                }
                
                # Apply the flagging function to each instrument group
                dat_t[, flagged := flag_instrument(.SD), by = Name, .SDcols = req_cols]
                
                # Return the data.table with the flagged column
                return(dat_t)
        }
        
        # Run the function on the cleaned data
        JSE_processed <- process_jse_data(dat_t = JSEdatdt_filtered, excl_list = excl_list, min_records = 40)
        
        # Extract flagged instruments
        oppo_instruments <<- JSE_processed[flagged == TRUE, unique(Name)]
        fnDyGraph(oppo_instruments)
        #return(flagged_instruments)
        # Call visualization function or fallback
        #if (length(flagged_instruments) > 0) {
        #                fnDyGraph(flagged_instruments)
        #        } else {
        #                warning("fnDyGraph is not defined. or no instruuments identified.")
        #                }
        
        return(oppo_instruments)
        #
}
