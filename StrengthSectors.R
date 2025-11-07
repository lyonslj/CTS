StrengthSec <- function(lst, freq = "Weekly", bCurr = "GOLD-R", bIndex = "JH=ALSI40", to_console = FALSE) {
        
        ##      --      Pass to this function a list of Instruments and it will produce a pdf heatmap        
        library(dplyr)
        library(tidyr)
        library(quantmod)
        library(reshape2)
        source('~/Documents/Personal/DataScience/R/JL CTS scripts/RscHeatmap.R')
        
        label <- gsub(" ", "", label)                
        fnJunkInstrm <- function(variables) {}
        
        ##      --      Define lst to search
        
        lst <- unique(c(lst,bCurr,bIndex))
        
        ##      --      Filter data
        ##      --      Use  %in% -- lot faster 
        mydat <- filter(JSEdat, JSEdat$Name %in% lst) %>% arrange(Date)
        
        
        ## frequency
        if(tolower(freq) == "daily") {
               # mydat <- tail(mydat, 400)  # Reduced to 400 for daily (as per your 400)
                cat("Daily mode")
        } else {
                # Weekly mode
                max_date <- max(mydat$Date, na.rm = TRUE)
                if (is.na(max_date)) stop("No valid dates in data")
                ldl <- weekdays(max_date)
                cat("Weekly mode: Filtering to", ldl, "data\n")
                
                # Filter to the weekday, but handle NA dates
                mydat_weekly <- mydat[!is.na(mydat$Date) & weekdays(mydat$Date) == ldl, c("Name", "Date", "Close")]
                
                if (nrow(mydat_weekly) == 0) {
                        stop("No data for weekday '", ldl, "'. Check your dates or try a different weekday.")
                }
                
                mydat <- mydat_weekly %>% arrange(Name, Date)
                cat("Filtered to", nrow(mydat), "rows\n")
        }
        
        # Check if mydat has data
        if (nrow(mydat) == 0) {
                stop("No data after processing. Check 'freq' and data dates.")
        }
        
        cat("Data after processing:\n")
        str(mydat)
        
        # Aggregate – now safe
        y <- aggregate(Close ~ Date + Name, mydat, mean, na.rm = TRUE)
        str(y)
        baseCurr <- filter(y,y$Name == bCurr)
        baseIndex <- filter(y,y$Name == bIndex)
        
        ## Remove rows from dataframe for base currency and index
        #subset_y <- y[which(!y$Name %in% c(bCurr, bIndex)), ]
        str(subset_y)
        
        ## Join the data to bring in baseCurr and baseIndex
        # Perform the left join
        #subset_y_merged <- subset_y %>%
        subset_y_merged <- y %>%
                left_join(
                        baseCurr %>%
                                select(Date, Close) %>% # Keep only Date and Close
                                rename(baseCurrClose = Close), # Rename Close to baseCurrClose
                        by = "Date"
                )
        subset_y_merged <- subset_y_merged %>%
                left_join(
                        baseIndex %>%
                                select(Date, Close) %>% # Keep only Date and Close
                                rename(baseIndexClose = Close), # Rename Close to baseCurrClose
                        by = "Date") %>% arrange(Date)
        str(subset_y_merged)

        ## Calc RSC
        # Check for NAs in key columns
        cat("NAs in Close:", sum(is.na(subset_y_merged$Close)), "\n")
        cat("NAs in baseCurrClose:", sum(is.na(subset_y_merged$baseCurrClose)), "\n")
        cat("NAs in baseIndexClose:", sum(is.na(subset_y_merged$baseIndexClose)), "\n")
        
        # Calculate ratios - replace Inf with NA
        subset_y_merged <- subset_y_merged %>%
                mutate(
                        rscBaseCurr = ifelse(is.finite(Close / baseCurrClose), Close / baseCurrClose, NA),
                        rscBaseIndex = ifelse(is.finite(Close / baseIndexClose), Close / baseIndexClose, NA)
                )
        str(subset_y_merged)
        
        # Fill NAs in ratios with linear interpolation (zoo::na.approx)
        #subset_y_merged <- subset_y_merged %>%
        #        mutate(
        #                rscBaseCurr = zoo::na.approx(rscBaseCurr, na.rm = FALSE),  # Interpolate, keep leading/trailing NA
        #                rscBaseIndex = zoo::na.approx(rscBaseIndex, na.rm = FALSE)
        #        )
        
        # Now apply SMA using zoo::rollmean (handles NAs in window)
        calcDat <- subset_y_merged %>%
                group_by(Name) %>%
                mutate(
                        # 2. Calculate the 21-day rolling mean for rscBaseCurr within each Name group
                        rscBaseCurrMa21 = zoo::rollmean(
                                rscBaseCurr, 
                                k = 21, 
                                # na.rm = TRUE is usually necessary for rolling means
                                na.rm = TRUE, 
                                # fill = NA pads the beginning of the series (the first k-1 rows) with NA
                                fill = NA, 
                                # align = "right" (or "center") ensures the average is calculated correctly
                                # "right" is typical for a trailing moving average
                                align = "right" 
                        ),
                        # 3. Calculate the 21-day rolling mean for rscBaseIndex within each Name group
                        rscBaseIndexMa21 = zoo::rollmean(
                                rscBaseIndex, 
                                k = 21, 
                                na.rm = TRUE, 
                                fill = NA, 
                                align = "right"
                        )
                ) %>%
                # 4. Remove the grouping to simplify subsequent operations
                ungroup() %>%
                # 5. Ensure the data is sorted by Date (and Name, though grouping handles that)
                arrange(Name, Date)
        
        # Verify output
        calcDat <- as.data.frame(na.omit(calcDat))                               # get rid of NA values
        cat("Data after processing:\n")
        str(calcDat)
        tail(calcDat)
        
        ## Calc Result
        # Calculate res based on Close, Sma21, 
        calcDat$res <- ifelse(
                (calcDat$rscBaseCurr > calcDat$rscBaseCurrMa21) & (calcDat$rscBaseIndex > calcDat$rscBaseIndexMa21), 2,  # Condition for 2 (both >)
                ifelse(
                        (calcDat$rscBaseCurr < calcDat$rscBaseCurrMa21) & (calcDat$rscBaseIndex > calcDat$rscBaseIndexMa21), 1,  # Condition for 1 (1st <, 2nd >)
                        ifelse(
                                (calcDat$rscBaseCurr > calcDat$rscBaseCurrMa21) & (calcDat$rscBaseIndex < calcDat$rscBaseIndexMa21), 1,  # Condition for 1 (1st >, 2nd <)
                                0  # Default case: 0 (This includes both < or mixed where one is equal, or both equal)
                        )
                )
        )
        
        # Keep only the res column and name it 
        calcDat <- calcDat[, -c(3:9)]
        
        calcDat_wide <- calcDat %>%
                # 1. Pivot the data wider
                pivot_wider(
                        id_cols = Date,        # Keep the 'Date' column as the unique row identifier
                        names_from = Name,     # Take values from the 'Name' column and use them for new column names
                        values_from = res      # Populate the new columns with values from the 'res' column
                )
        
        # 1. Convert the tibble to a data.frame
        calcDat_df <- as.data.frame(calcDat_wide)
        
        # 2. Assign the values of the first column (Date) as the row names
        # Note: The Date column must contain unique values for this to work.
        rownames(calcDat_df) <- calcDat_df[, 1]
        
        # 3. Remove the now-redundant Date column
        calcDat_df <- calcDat_df[, -1]
        
        df.z <- tail(data.frame(calcDat_df),40)  
        colnames(df.z) <- gsub("\\.","-",names(df.z))
        df.z <- as.data.frame(na.omit(df.z))            # get rid of NA values
        ## -------- Returned to calling function eg to do a DyGraph
        to_graph_rsc <<- names(df.z)[df.z[nrow(df.z), ] > 0]      # Get names where the value in the last row of df.z is greater than 0
        fnDyGraph(to_graph)
        df.z <- t(df.z)                                 #transpose
        #df.z <- df.z %>% select(where(~!all(is.na(.)))) # delete NA column
        # If last entry is NA move that column to beginning
        N <- ncol(df.z)
        if (is.na(df.z[nrow(df.z), N])) df.z <- df.z[, c(N, 1:(N - 1))]
        #
        df.z <- df.z[order(df.z[,ncol(df.z)]),]
        str(df.z)
        
        
             
        
        ##  --------------------   Create heatmap -------------------
        label <- paste("Sector Strength",bCurr,bIndex,sep = " ")
      
        RscHeatmap(df.z, label, freq, to_console)                
        return(to_graph_rsc)
        
        
        

}


