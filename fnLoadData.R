# Required packages
library(httr)
library(readr)
library(dplyr)
library(R.utils)  # For gunzip/untar functionality

# Function to get the last working day (Monday to Friday)
get_last_working_day <- function() {
        today <- Sys.Date()
        day_of_week <- as.numeric(format(today, "%w")) # 0 = Sunday, 1 = Monday, ..., 6 = Saturday
        
        if (day_of_week == 0) { # Sunday
                last_working_day <- today - 2
        } else if (day_of_week == 1) { # Monday
                last_working_day <- today - 3
        } else { # Tuesday to Saturday
                last_working_day <- today - 1
        }
        return(format(last_working_day, "%y-%m-%d"))
}

# Function to download and unarchive the file


download_and_unarchive <- function(date) {
        url <- paste0("http://www.sharenet.co.za/snet/csv/data/", date, ".lzh")
        download_path <- paste0("/Users/johnlyons/Downloads/", date, ".lzh")
        # csv_path is now set dynamically, so no initial assignment needed
        username <- "ljlyons"
        password <- "rmiZ@E%3$JC_?)E"
        
        
        # Download the file with authentication
        tryCatch({
                # Download the file
                GET(url, 
                    authenticate(username, password),
                    write_disk(download_path, overwrite = TRUE))
                message("File downloaded: ", download_path)
                
                # Check if unar is available
                unar_available <- system("which unar", intern = TRUE, ignore.stderr = TRUE)
                if (length(unar_available) == 0) {
                        stop("unar is not installed. Please install it (e.g., via Homebrew: brew install unar).")
                }
                
                # Wait briefly to ensure file is written
                Sys.sleep(1)
                
                # Extract the .lzh file directly to the current folder using -D
                unar_cmd <- paste("unar", "-D", download_path, "-o", "/Users/johnlyons/Downloads/")
                unar_status <- system(unar_cmd, ignore.stderr = FALSE)  # Capture status
                message("unar command executed. Exit status: ", unar_status)
                if (unar_status != 0) {
                        stop("unar command failed with exit status ", unar_status)
                }
                
                # Wait longer to ensure extraction is complete
                Sys.sleep(2)  # Increased from 1 to 2 seconds
                
                # Dynamically detect the extracted CSV file
                extracted_files <- list.files("/Users/johnlyons/Downloads/", pattern = "\\.csv$", full.names = TRUE)
                message("Extracted files found: ", paste(extracted_files, collapse = ", "))
                if (length(extracted_files) == 0) {
                        stop("No CSV file found after unarchiving in /Users/johnlyons/Downloads/")
                } else if (length(extracted_files) > 1) {
                        warning("Multiple CSV files found. Using the first one: ", extracted_files[1])
                }
                csv_path <- extracted_files[1]  # Use the first CSV file found
                
                # Verify if the CSV was created
                if (file.exists(csv_path)) {
                        message("File unarchived: ", csv_path)
                        # Optional: Clean up the .lzh file
                        file.remove(download_path)
                        message("Original archive removed: ", download_path)
                        return(csv_path)
                } else {
                        stop("Unarchiving failed or CSV not found at ", csv_path)
                }
        }, error = function(e) {
                message("Error downloading or unarchiving: ", e$message)
                # Clean up downloaded file if it exists and unarchiving failed
                if (file.exists(download_path)) file.remove(download_path)
                message("Cleaned up downloaded file: ", download_path)
                return(NULL)
        })
}

# Test the function
date <- get_last_working_day()
result <- download_and_unarchive(date)
if (!is.null(result)) {
        message("Successfully processed. CSV path: ", result)
}

# Function to move previous day's CSV to ~/Desktop/snet
move_previous_csv <- function(current_csv) {
        download_dir <- "/Users/johnlyons/Downloads/"
        snet_dir <- "/Users/johnlyons/Desktop/snet/"
        
        # Ensure snet directory exists
        if (!dir.exists(snet_dir)) {
                dir.create(snet_dir)
        }
        
        # Get all CSV files in Downloads
        csv_files <- list.files(download_dir, pattern = "*.csv", full.names = TRUE)
        
        # Move all CSV files except the current one
        for (file in csv_files) {
                if (file != current_csv) {
                        file.rename(file, paste0(snet_dir, basename(file)))
                        message("Moved: ", file, " to ", snet_dir)
                }
        }
}

# Your existing fnLoadData2 function (unchanged)
fnLoadData2 <- function(file) {
        pathfile <- paste("/Users/johnlyons/Downloads/", file, sep = "")
        df.lst <- read.csv(pathfile, skip = 5, header = FALSE, sep = ",", stringsAsFactors = FALSE)
        df.clean <- df.lst[, 1:11]
        df.clean <- df.clean[, -c(4, 5)]
        names(df.clean) <- c("Name", "Exchange", "Instm", "Date", "Open", "High", "Low", "Close", "Volume")
        new.dat <- subset(df.clean[, -3], df.clean$Exchange %in% c("JSE", "SPOT", "FOREX"))
        new.dat <- new.dat[, -2]
        new.dat$Date <- as.Date(as.character(new.dat$Date), format = "%Y%m%d")
        new.dat$Volume <- as.numeric(new.dat$Volume)
        return(new.dat)
}

# Main automation function
automate_data_processing <- function() {
        # Step 1: Get the last working day's date
        date <- get_last_working_day()
        message("Processing data for: ", date)
        
        # Step 2: Download and unarchive the file
        csv_path <- download_and_unarchive(date)
        if (is.null(csv_path)) {
                stop("Failed to download or unarchive the file.")
        }
        
        # Step 3: Move previous CSVs to ~/Desktop/snet
        move_previous_csv(csv_path)
        
        # Step 4: Process the new CSV
        csv_file <- basename(csv_path)
        data <- fnLoadData2(csv_file)
        
        # Step 5: Append to JSEdat.csv
        output_file <- "/Users/johnlyons/Documents/Personal/DataScience/R/JSEdat.csv"
        write.table(data, output_file, append = TRUE, row.names = FALSE, quote = FALSE, col.names = !file.exists(output_file))
        message("Data appended to: ", output_file)
        
        # Step 6: Load and process JSEdat
        JSEloaded_dat <- read.csv(output_file, header = TRUE, sep = ",")
        JSEloaded <- JSEloaded_dat
        JSEloaded$Date <- as.Date(JSEloaded$Date, "%Y-%m-%d")
        JSEdat <- distinct(JSEloaded)
        
        # Convert columns to numeric
        cols <- 3:7
        JSEdat[, cols] <- apply(JSEdat[, cols], 2, as.numeric)
        
        # Step 7: Subset and calculate BTCZAR and ETHZAR
        BTC <- JSEdat[JSEdat$Name == "C-BTCUSD", ]
        ETH <- JSEdat[JSEdat$Name == "C-ETHUSD", ]
        ZARUSD <- JSEdat[JSEdat$Name == "C-USDZAR", ]
        NIK <- JSEdat[JSEdat$Name == "NIKKEI", ]
        
        # Merge BTC and ZARUSD by Date
        BTCZAR <- merge(BTC, ZARUSD, by = "Date", suffixes = c("_BTC", "_ZARUSD"))
        BTCZAR <- data.frame(
                Name = "C-BTCZAR",
                Date = BTCZAR$Date,
                Open = BTCZAR$Close_BTC * BTCZAR$Open_ZARUSD,
                High = BTCZAR$Close_BTC * BTCZAR$High_ZARUSD,
                Low = BTCZAR$Close_BTC * BTCZAR$Low_ZARUSD,
                Close = BTCZAR$Close_BTC * BTCZAR$Close_ZARUSD,
                Volume = 0
        )
        BTCZAR$Low <- ifelse(BTCZAR$Close < BTCZAR$Low, BTCZAR$Close, BTCZAR$Low)
        BTCZAR$High <- ifelse(BTCZAR$Close > BTCZAR$High, BTCZAR$Close, BTCZAR$High)
        
        # Merge ETH and ZARUSD by Date
        ETHZAR <- merge(ETH, ZARUSD, by = "Date", suffixes = c("_ETH", "_ZARUSD"))
        ETHZAR <- data.frame(
                Name = "C-ETHZAR",
                Date = ETHZAR$Date,
                Open = ETHZAR$Close_ETH * ETHZAR$Open_ZARUSD,
                High = ETHZAR$Close_ETH * ETHZAR$High_ZARUSD,
                Low = ETHZAR$Close_ETH * ETHZAR$Low_ZARUSD,
                Close = ETHZAR$Close_ETH * ETHZAR$Close_ZARUSD,
                Volume = 0
        )
        ETHZAR$Low <- ifelse(ETHZAR$Close < ETHZAR$Low, ETHZAR$Close, ETHZAR$Low)
        ETHZAR$High <- ifelse(ETHZAR$Close > ETHZAR$High, ETHZAR$Close, ETHZAR$High)
        
        # Remove existing C-BTCZAR and C-ETHZAR from JSEdat
        JSEdat <- JSEdat[!JSEdat$Name %in% c("C-BTCZAR", "C-ETHZAR"), ]
        
        # Append the new datasets to JSEdat
        JSEdat <- rbind(JSEdat, BTCZAR, ETHZAR)
        
        # Write the final JSEdat back to the file
        write.csv(JSEdat, output_file, row.names = FALSE)
        message("Final processed data written to: ", output_file)
        
        # Return the last loaded date
        return(max(sort(JSEdat$Date)))
}

# Run the automation
last_date <- automate_data_processing()
message("Last loaded date: ", last_date)