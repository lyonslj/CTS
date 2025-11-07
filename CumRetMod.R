CumRet <- function(mylst = instLive, dt = Sys.Date()-250, nm = "Cumulative Returns Top20" , 
                   data = JSEdat) {
        
        # Description: This function calculates and plots a zoo.dp cumulative returns of a list of financial instruments.
        # It takes a list of instrument names, a start date, a title for the plot, and a path for the output file.
        # The function first retrieves the closing prices for each instrument from a global data frame `JSEdat`
        # and merges them into a single `xts` object. It then calculates the percentage change from the first day
        # for each series to get the cumulative returns. The series are then sorted by their final cumulative
        # return and a plot is generated showing the top 20 performing instruments. The plot includes a custom
        # x-axis with monthly labels and a legend displaying the instrument names and their final cumulative
        # return percentage.
        #
        # Arguments:
        # mylst: A list of character strings representing the names of the financial instruments.
        #        (Default: instLive)
        # dt: The start date for the data retrieval. Data is filtered to be on or after this date.
        #     (Default: Sys.Date()-250)
        # nm: The main title for the generated plot.
        #     (Default: "Cumulative Returns Top20")
        # pth: The directory path where the plot should be saved.
        #      (Default: "Rplots/")
        #
        # Output:
        # A plot is generated to the current graphics device, displaying the cumulative returns of the
        # top 20 performing instruments over the specified time period.
        #

#*********************************************
# Build individual xts sets of closing prices*
#*********************************************
for(i in mylst) {
        y <- data[data$Name == i & data$Date >= dt, ]
        y <- y[,c("Date","Close")]
        y$Close <- as.numeric(y$Close)
        y <- aggregate(Close ~ Date, y, mean)   # get rid of dups and multiple entries
        z <- xts(y[,2],order.by = y[,1])        # convert to xts
        assign(i,z)
}

#*********************************************
#merge all closing prices of mylist into myxts
# then convert to % cumulative return        
#*********************************************
        
myxts <- do.call(merge,sapply(mylst,as.name))   # build single xts frame of instr closing prices
day1 <- as.numeric(myxts[1,])                   #get closing price of first record
daydp <- head(myxts,1)                             # price on day1 of set
for (i in 1:ncol(myxts)) {
        # Calculate cumulative percentage change from day1
        pcage <- ((myxts[, i] - day1[i]) / day1[i]) * 100
        # Overwrite the existing column with the new values
        myxts[, i] <- pcage
        }
  
#**************************************
#       Legend with %return           #
#**************************************

templst <- as.data.frame(tail(myxts, 1))
templst <- reshape2::melt(templst)
templst$value <- round(templst$value, 2)
tor <- templst[order(templst[, 2], decreasing = TRUE), ]
tor <- head(tor,20) # only take the top 20
##      --      Order for legend to plot
# Reorder myxts columns based on tor$variable
ord <- as.character(tor$variable)
zoo.dp <- as.zoo(myxts[, ord])  # Reorder columns

#*********************************************
#            Graph the series                *
#*********************************************
# Set a color scheme
tsRainbow <- rainbow(ncol(zoo.dp))

# Plot the series
# Plot the series with smaller title size
plot(x = zoo.dp, ylab = "Cumulative Return %age", axes = FALSE,  # Suppress default axes
     main = nm, cex.main = 0.8,  # Reduce title size
     col = tsRainbow, screens = 1, lwd = 1.5)
grid(NULL, NULL)
abline(h = 0)

# Add custom x-axis with months
# Get the date range
dates <- index(zoo.dp)
# Create monthly breaks (first day of each month)
month_breaks <- seq(as.Date(format(min(dates), "%Y-%m-01")), 
                    as.Date(format(max(dates), "%Y-%m-01")), 
                    by = "month")
# Add x-axis with month abbreviations
axis(1, at = month_breaks, labels = format(month_breaks, "%b %Y"), cex.axis = 0.8)

# Add y-axis
axis(2, cex.axis = 0.8)

# Create legend with original column order and corresponding returns
legend_labels <- paste(colnames(zoo.dp), " ", 
                       sapply(colnames(zoo.dp), function(x) templst$value[templst$variable == x]), 
                       "%", sep = "")
legend("topleft", legend = legend_labels, inset = 0.01, cex = 0.5, 
       lty = c(1, 5), lwd = c(3, 3), bg = "grey96", col = tsRainbow)
}



