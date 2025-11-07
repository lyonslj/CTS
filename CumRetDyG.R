CumRetDyG <- function(mylst = instLive, dt = Sys.Date()-90) {
        
library(dygraphs)

#*********************************************
# Build individual xts sets of closing prices*
#*********************************************
        for(i in mylst) {
                y <- subset(JSEdat,JSEdat$Name == i)
                y <- y[,c("Date","Close")]
                y$Close <- as.numeric(y$Close)
                y <- aggregate(Close ~ Date, y, mean)   # get rid of dups and multiple entries
                z <- xts(y[,2],order.by = y[,1])        # convert to xts
                assign(i,z)
        }
#*********************************************
#merge all closing prices of mylist into myxts
#*********************************************
        

myxts <- do.call(merge,sapply(mylst,as.name))
len <- length(mylst)

dateWindow <- c(as.character(dt), as.character(Sys.Date()))

htmltools::browsable(htmltools::tagList(dygraph(myxts, main = "Percent", group = "stock") %>%
        dyRebase(percent = TRUE) %>%
        dyRangeSelector(dateWindow = dateWindow) %>%
        dyOptions(colors = RColorBrewer::brewer.pal(len, "Paired")) %>%
        dyLimit(0, color = "black") %>% 
        dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
        dyLegend(show = "onmouseover" ,hideOnMouseOut = TRUE) %>%
        dyLegend(width = 800)))


library(htmltools)
browsable(
        tagList(
                dygraph(z, main = "Percent", group = "stock") %>%
                                 dyRebase(percent = TRUE) %>%
                                 #dyRoller(rollPeriod = 21) %>%
                                 dyRangeSelector(dateWindow = dateWindow),
                dygraph(z, main = "None", group = "stock") %>%
                                dyRoller(rollPeriod = 21) %>%
                                 dyRangeSelector(dateWindow = dateWindow)))


}

