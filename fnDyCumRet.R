fnDyCumRet <- function(dataset, label = "", days = 220, data) {
        
# Description:This function creates an interactive dygraph to visualize percentage gains for multiple time series
# The graph displays percent gains relative to the first
# The graph includes a range selector, custom y-axis formatting (percentages), and no grid lines for clarity.
# The legend is positioned at the top-left with a semi-transparent background and separate lines for each series.
        
        
        
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
       
#*********************************************
#             Filter dataset                 * 
#  Use %in%. Much more efficient than a loop *
#*********************************************    
dt = Sys.Date()-days  
y <- data[data$Name %in% dataset & data$Date >= dt, ]
y <- y[complete.cases(y), ]
y$Close <- as.numeric(y$Close)
y <- y[,c("Name","Date","Close")]
y <- aggregate(Close ~ (Name +Date), y, mean)   # get rid of dups and
filt_dat <- y

##       -----    Cast Wide as XTS   -------                 
wide_dat <- pivot_wider(
        filt_dat,
        id_cols = Date,
        names_from = Name,
        values_from = Close,
        values_fn = first # THIS IS THE KEY ADDITION
)
wide_dat <- wide_dat %>%
        distinct()
xts_dat <- xts(wide_dat[, -1], order.by = wide_dat$Date)        # convert to xts

#*********************************************
##       -----      Prep Graph         -------  
#*********************************************
   
        ## Date range that the slider will display
        dateWindow <- c(max(index(xts_dat))-20, max(index(xts_dat)))
        ##       -----      Calc order         ------- 
        first_values <- as.numeric(head(xts_dat,1))
        last_values <- as.numeric(tail(xts_dat,1))
        returns <- (last_values-first_values)/first_values
        ordered_cols <- order(returns, decreasing = TRUE)
        ordered_cols <- ordered_cols[1:20]      # Take top 20
        xts_dat_ordered <- xts_dat[, ordered_cols]
        
        # Create the dygraph with reordered xts_dat
        gg <- htmltools::browsable(htmltools::tagList(
                # Wrap the dygraph in a div that establishes a positioning context
                htmltools::div(style = "position: relative; width: 800px; height: 400px;", # Set dimensions for the graph container
                               dygraph(xts_dat_ordered, main = paste(label, " Percent Gains Top20 last ",days," days" , sep = ""), group = "stock") %>%
                                       dyRebase(percent = TRUE) %>%
                                       dyRangeSelector(dateWindow = dateWindow) %>%
                                       dyOptions(
                                               colors = c("#FF0000", "#00FF00", "#0000FF", "#FF00FF", "#FFA500", "#00FFFF",
                                                          "#1E90FF", "#32CD32", "#FFD700", "#9400D3", "#DC143C", "#20B2AA"),
                                               gridLineColor = "transparent"  # Remove grid lines
                                       ) %>%
                                       dyLimit(0, color = "black") %>%
                                       dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
                                       dyAxis("y", valueFormatter = htmlwidgets::JS("
                     function(x) {
                       var formatted = (x * 100).toFixed(2);
                       return formatted + '%';
                     }
                   ")) %>%
                                       dyLegend(
                                               hideOnMouseOut = TRUE,
                                               show = "always",
                                               labelsSeparateLines = TRUE,
                                               labelsDiv = "legendDiv" # Legend will render into this div
                                       ),
                               # This div will contain the legend, absolutely positioned over the chart
                               htmltools::div(
                                       id = "legendDiv",
                                       style = "
                     position: absolute;
                     left: 80px;
                     top: 20px;
                     width: 170px;
                     background-color: rgba(255, 255, 255, 0.85);
                     border: 1px solid #ccc;
                     padding: 5px;
                     z-index: 10;
                     box-shadow: 2px 2px 5px rgba(0,0,0,0.2);
                     font-size: 10px;
                   "
                               )
                )
        ))
        
        gg
}
