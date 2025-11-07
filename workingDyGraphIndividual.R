fnDyGraph <- function(mylst) {
        
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

## ----------------------------- ##
# Function to compute SMAs for a given instrument
fnMa <- function(x) {
        # Subset JSEdat for the instrument
        z <- JSEdat[JSEdat$Name == x, c("Date", "Open", "High", "Low", "Close")]
        z$Close <- as.numeric(as.character(z$Close))
        # Check if data is non-empty
        if (nrow(z) == 0) {
                warning(paste("No data found for", x))
                return(NULL)
        }
        # Convert to xts
        z_xts <- xts(z[, c("Open", "High", "Low", "Close")], order.by = as.Date(z$Date))
        z_xts <- na.omit(z_xts)
        # Add 21-day and 50-day SMAs for Close
        z_xts$SMA21 <- SMA(z_xts$Close, n = 21)
        z_xts$SMA50 <- SMA(z_xts$Close, n = 50)
        # Calculate the weekly high from the 'High' column
        z_xts$WeekHigh <- rollapply(
                z_xts$High, 
                width = 5, 
                FUN = max, 
                align = "right", 
                fill = NA
        )
        # Calculate the weekly low from the 'Low' column
        z_xts$WeekLow <- rollapply(
                z_xts$Low, 
                width = 5, 
                FUN = min, 
                align = "right", 
                fill = NA
        )
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
                dygraph(tail(z_xts[,c("High", "Close", "Low", "SMA21", "SMA50")], 200), 
                        main = paste("Price and SMAs for", x, sep = " ")) %>%
                        dySeries(c("High", "Close", "Low")) %>%
                        # Now, explicitly define the series with your desired styles
                        dySeries("SMA21", label = "SMA21", color = "blue", strokeWidth = 1) %>%
                        dySeries("SMA50", label = "SMA50", color = "red", strokeWidth = 1) %>%
                        
                        # Customize legend
                        dyLegend(width = 800, show = "always", hideOnMouseOut = FALSE) %>%
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

