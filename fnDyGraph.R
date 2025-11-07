fnDyGraph <- function(mylst,data) {
        
        # Description:This function creates an interactive dygraph to visualize harts and MA's
        # The graph shadows prices for weeks high and lows
        # Produces individual Graph per instrument
        
        
        library(TTR)
        library(quantmod)
        library(reshape2)
        library(plyr)
        library(dplyr)
        library(tidyr)
        library(readxl)
        library(reshape2)
        library(DT)
        library(plotly)
        library(lubridate)
        library(xts)
        library(htmltools)
        library(dygraphs)
        library(htmltools)
        library(RColorBrewer)
        library(htmlwidgets)
        
        # Check if data is missing and set default to JSEdat
        if (missing(data)) {
                data <- JSEdat
        }
## ----------------------------- ##
        # Function to compute SMAs for a given instrument
      
        fnMa <- function(x) {
                # Subset data for the instrument
                z <- data[data$Name == x, c("Date", "Open", "High", "Low", "Close")]
                
                z$Close <- as.numeric(as.character(z$Close))
                
                # Check if data is non-empty
                if (nrow(z) == 0) {
                        warning(paste("No data found for", x))
                        return(NULL)
                }
                
                # Convert to xts
                # Ensure Date column is in Date format
                z$Date <- as.Date(z$Date)
                z_xts <- xts(z[, c("Open", "High", "Low", "Close")], order.by = as.Date(z$Date))
                z_xts <- na.omit(z_xts)
                
                # Add 21-day and 50-day SMAs for Close
                z_xts$SMA21 <- SMA(z_xts$Close, n = 21)
                z_xts$SMA50 <- SMA(z_xts$Close, n = 50)
                
                # Handle the special case for "NIKKEI"
                if (x == "NIKKEI") {
                        # For NIKKEI, WeekHigh and WeekLow are based on the Close price
                        z_xts$WeekHigh <- rollapply(
                                z_xts$Close,
                                width = 5,
                                FUN = max,
                                align = "right",
                                fill = NA
                        )
                        z_xts$WeekLow <- rollapply(
                                z_xts$Close,
                                width = 5,
                                FUN = min,
                                align = "right",
                                fill = NA
                        )
                } else {
                        # For all other instruments, use High and Low prices as originally intended
                        z_xts$WeekHigh <- rollapply(
                                z_xts$High,
                                width = 5,
                                FUN = max,
                                align = "right",
                                fill = NA
                        )
                        z_xts$WeekLow <- rollapply(
                                z_xts$Low,
                                width = 5,
                                FUN = min,
                                align = "right",
                                fill = NA
                        )
                }
                
                # Remove NA values
                z_xts <- na.omit(z_xts)
                
                # Return last 140 rows
                return(tail(z_xts, 140))
        }
        # Check if mylst is defined and not empty
        if (!exists("mylst") || length(mylst) == 0) {
                stop("mylst is undefined or empty. Please define valid instrument names.")
        }
        

###############   -------------- Generate the Graphs -----------------   ################

p <- lapply(unique(mylst), function(x) {
        # Generate the xts dataset for the current instrument
        z_xts <- fnMa(x)
        
        # Check if data was returned and create the dygraph
        if (!is.null(z_xts) && nrow(z_xts) > 0) {
                dygraph(tail(z_xts[,c("WeekHigh", "Close", "WeekLow", "SMA21", "SMA50")], 200), 
                        main = paste("Price and SMAs for", x, sep = " ")) %>%
                        dySeries(c("WeekHigh", "Close", "WeekLow")) %>%
                        # Now, explicitly define the series with your desired styles
                        dySeries("SMA21", label = "SMA21", color = "blue", strokeWidth = 1) %>%
                        dySeries("SMA50", label = "SMA50", color = "red", strokeWidth = 1) %>%
                        
                        # Customize legend
                        dyLegend(width = 800, show = "always", hideOnMouseOut = TRUE) %>%
                        # Add range selector for zooming
                        dyRangeSelector()
        } else {
                # Return NULL if no data is available
                return(NULL)
        }
})

# Filter out any NULL elements that might have been returned for instruments with no data
p <- p[!sapply(p, is.null)]

# Make the plots browsable
htmltools::browsable(htmltools::tagList(p))

}


