library(ggplot2)
library(plotly)
library(lubridate)
library(scales)
library(dplyr)
library(openxlsx)


dat  <- read.xlsx("Invest.xlsx", sheet = "Sheet0")
dat <- dat[!is.na(dat$TrdClose), ]           ## only trades tht have been closed
dat <- dat[,c(1:14)]
dat$TrdOpen <- as.Date(dat$TrdOpen, origin = "1899-12-30")     # fix excel origin 
dat$TrdClose <- as.Date(dat$TrdClose, origin = "1899-12-30")     # fix excel origin 
dat$days <- as.numeric(dat$TrdClose - dat$TrdOpen)
dat$OpenP <- as.numeric(dat$OpenP)
dat$CloseP <- as.numeric(dat$CloseP)
dat$Profit <- as.numeric(dat$Profit)
dat$StopLoss <- as.numeric(dat$StopLoss)
dat$Fees <- as.numeric(dat$Fees)
dat$Size <- as.numeric(dat$Size)
dat$Amount <- as.numeric(dat$Amount)
dat$Amount[is.na(dat$Amount)] <- 0
dat$Risk <- dat$Size*(dat$OpenP - dat$StopLoss)
dat$R <- dat$Profit/dat$Risk
dat$R1 <- ifelse(dat$R < -1, -1, dat$R)    ## R1 Remodel Risk so max R is always -1 
dat$Year <- year(dat$TrdClose)             ## Year

############################# Calculated Fields ############################


dat <- dat %>%
        arrange(TrdClose) %>%
        mutate(
                CapitalBal = cumsum(replace(Profit, is.na(Profit), 0) + Amount),  ## Replace NA with 0 for cumsum
                CumWinCount = cumsum(replace(Profit >= 0, is.na(Profit), FALSE)),  ## NA as FALSE for wins
                CumLossCount = cumsum(replace(Profit < 0, is.na(Profit), FALSE)),  ## NA as FALSE for losses
                CumWinRate = round(CumWinCount / (CumWinCount + CumLossCount), 2),  ## WinRate, NA handled upstream
                AvgRWin = cumsum(replace(R1 * (R1 > 0), is.na(R1), 0)) / 
                        cumsum(replace(R1 > 0, is.na(R1), FALSE)),  ## Avg of R where R > 0, NA as 0/FALSE
                CumExpectancy = (CumWinCount / (CumWinCount + CumLossCount) * AvgRWin) - 
                        (CumLossCount / (CumWinCount + CumLossCount)),  ## Expectancy, NA handled upstream
                AvgR1 = cumsum(replace(R1, is.na(R1), 0)) / sum(!is.na(R1))  ## Running mean of R1, NA as 0
        )

############################# Key Metrics ###################################
median(dat$R[dat$R>0], na.rm = TRUE)   ## 1.12      R median +ve Trades
mean(dat$R[dat$R>0], na.rm = TRUE)      ## 1.58      R mean +ve Trades     _____________**
median(dat$R[dat$R<0], na.rm = TRUE)   ## -1.1      R median -ve Trades
median(dat$R1[dat$R1<0], na.rm = TRUE)  ## -1        R1 median -ve Trades
mean(dat$R[dat$R<0], na.rm = TRUE)    ## -1.12     R mean -ve Trades
mean(dat$R1[dat$R1>0], na.rm = TRUE) ## 1.44     R1 mean +ve Trades     _____________**
mean(dat$R1[dat$R1<0], na.rm = TRUE)   ## -0.71     R1 mean -ve Trades     _____________**
median(dat$R, na.rm = TRUE)             ## 0.24      R median Trades
mean(dat$R, na.rm = TRUE)              ## 0.57      R mean Trades
mean(dat$Risk, na.rm = TRUE)           ## 25,900    Risk mean
median(dat$Risk, na.rm = TRUE)         ## 15,540    Risk median
mean(dat$R1, na.rm = TRUE)             ## 0.711     Overall profitability  _____________** 
##           Expect 0.71 per every R1 risked  
mean(dat$Profit, na.rm = TRUE)         ## 9,166     Avg profit
mean(dat$Profit[dat$Profit > 0], na.rm = TRUE) ## 26,511   Profit +ve trades 
mean(dat$Profit[dat$Profit < 0], na.rm = TRUE) ## 20,004   Loss -ve trades
sum(dat$Profit, na.rm = TRUE)          ## 540,834   Overall
sum(dat$Risk, na.rm = TRUE, na.rm = TRUE)            ## 1,529,557
Profit <- sum(dat$Profit[dat$Profit >0], na.rm = TRUE)     ## 980,922 Winning Trades
Loss <- abs(sum(dat$Profit[dat$Profit <0], na.rm = TRUE))  ## 440,087 Losing Trades
ProfitFactor <- Profit/Loss                  ## 2.22 ie ProfitFactor  ______**
## R2.22 made for every R1 lost   



############################### Graphs #####################################
##
## 1) Win Rate
##
yrs <- unique(year(dat$TrdClose))
result <- dat %>%
        mutate(Year = year(ymd(TrdClose))) %>%  # Extract year from TrdClose
        group_by(Year) %>%  # Group by distinct years
        summarise(
                Winners = length(Profit[Profit >= 0]),  # Count trades with non-negative profit
                Losers = length(Profit[Profit < 0]))   %>%  # Count trades with negative profit
        mutate(Total = Winners + Losers)

plot_data <- data.frame()
for (i in 1:nrow(result)) {
        plot_data <- rbind(plot_data, data.frame(
                Year = result$Year[i],
                Start = c(0, result$Winners[i]),
                End = c(result$Winners[i], result$Total[i]),
                Color = c("green", "red")
        ))
}

ggplot(plot_data, aes(xmin = Start, xmax = End, ymin = as.numeric(Year) - 0.45, ymax = as.numeric(Year) + 0.45, fill = Color)) +
        geom_rect(position = position_dodge(width = 0.6)) +  # Minimize dodge to reduce gap
        scale_fill_manual(values = c("green", "red"), guide = "none") +  # Set colors without legend
        geom_text(aes(x = (Start + End) / 2, y = as.numeric(Year), label = Label),  # Center label
                  color = "black", size = 5, hjust = 0.5) +
        # Add win rate labels for each year using annotate
        annotate("text", x = result$Total / 2, y = as.numeric(result$Year) + 0.2, 
                 label = paste("Win Rate:", round(result$Winners / result$Total * 100, 2), "%"),
                 size = 3, fontface = "bold", hjust = 0.5) +
        labs(title = "Trading System Performance by Year",
             x = "Number of Trades",
             y = "Year") +
        scale_x_continuous(breaks = seq(0, max(result$Total), by = 5)) +  # Custom x-axis ticks
        scale_y_discrete(breaks = result$Year, labels = result$Year) +  # Y-axis as discrete factors
        theme_light() +
        theme(
                plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                axis.text.y = element_text(size = 12),
                axis.text.x = element_text(size = 12),
                axis.title = element_text(size = 12),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())  # Increase bottom margin




##
## 2) Cumulative Balance
##
# Create a column to select every other point for labeling
dat$label_flag <- seq_len(nrow(dat)) %% 2 == 1  # Label odd-numbered rows (e.g., 1st, 3rd, 5th)

ggplot(dat, aes(x = ymd(TrdClose), y = CapitalBal)) +  # Convert TrdClose to Date
        geom_line() +
        theme_light() +
        geom_text(data = subset(dat, label_flag),  # Only label every other point
                  aes(x = ymd(TrdClose), y = CapitalBal, label = comma(CapitalBal)), 
                  size = 2, color = "black", nudge_y = 1000, check_overlap = TRUE) +  # Avoid overlap
        theme(
                axis.text = element_text(size = 6),  # Set axis text size
                legend.position = "none",           # Remove legend
                strip.text = element_text(size = 6) # Set facet strip text size (if faceting is used)
        ) +
        labs(title = "Capital Balance Over Time", x = "Trade Close Date", y = "Capital Balance") +
        scale_x_date(date_labels = "%Y-%m", date_breaks = "2 months")  # Customize date format and breaks

##
## 3) Expectancy, RRR and Win Rate over time 
##
ggplot(dat[dat$Year > 2023, ], aes(x = ymd(TrdClose))) +  # Base layer with x-axis
        geom_line(aes(y = AvgR1, color = "RRR")) +  # Second line
        geom_line(aes(y = CumWinRate, color = "Win Rate")) +  # Third line
        theme_light() +
        geom_text(data = subset(dat[dat$Year > 2023, ], label_flag),  # Labels for AvgR1
                  aes(x = ymd(TrdClose), y = AvgR1, label = round(AvgR1, 2)),
                  size = 2, color = "black", nudge_y = 0.0, check_overlap = TRUE) +
        geom_text(data = subset(dat[dat$Year > 2023, ], label_flag),  # Labels for CumWinRate
                  aes(x = ymd(TrdClose), y = CumWinRate, label = round(CumWinRate, 2)),
                  size = 2, color = "purple", nudge_y = 0.02, check_overlap = TRUE) +
        scale_color_manual(values = c("RRR" = "red", 
                                      "Win Rate" = "purple")) +  # Define colors
        theme(
                axis.text = element_text(size = 6),  # Set axis text size
                legend.position = "bottom",          # Move legend to bottom
                legend.title = element_blank(),      # Remove legend title
                strip.text = element_text(size = 6)  # Set facet strip text size
        ) +
        labs(title = "Average RRR, WinRate Over Time - %age Return per Trade Risk",
             x = "Date", y = "Value") +
        scale_x_date(date_labels = "%Y-%m", date_breaks = "2 months")  # Customize date format and breaks

##
## 4) Plot Trades over time
##

ggplot(dat, aes(x = ymd(TrdClose), y = CapitalBal)) +  # Base layer with x-axis and CapitalBal
        geom_point(aes(size = abs(Profit), color = Profit > 0)) +  # Points with size and color
        geom_line(aes(y = CapitalBal, color = "Win Rate")) +  
        geom_text(data = dat,  # Labels for Profit on every other point
                  aes(x = ymd(TrdClose), y = CapitalBal, 
                      label = paste(tolower(Symbol),comma(Profit),sep="-")),
                  size = 2, color = "black", nudge_x = -5, check_overlap = TRUE) +  # Avoid overlap
        theme_light() +
        scale_color_manual(values = c("TRUE" = "green", "FALSE" = "red"), 
                           labels = c("Profit", "Loss"), 
                           name = "Trade Type") +  # Legend for profit/loss
        theme(
                axis.text = element_text(size = 6),  # Set axis text size
                legend.position = "bottom",          # Move legend to bottom
                strip.text = element_text(size = 6)  # Set facet strip text size
        ) +
        labs(title = "Trades Over Time", x = "Trade Close Date", y = "Capital Balance") +
        scale_x_date(date_labels = "%b-%y", date_breaks = "1 months") +  # Customize date format and breaks
        scale_size_continuous(name = "Profit Magnitude")  # Legend for point size



############################################## Old Calcs #########################################################
## Mean RRR 
r <- dat %>%
        select(R, Year) %>%
        arrange(Year) %>%
        group_by(Year) %>%
        summarise(
                PosR = mean(R[R >= 0], na.rm = TRUE),  # Mean of positive profits
                NegR = mean(R[R < 0], na.rm = TRUE),   # Mean of negative profits
                avgR = mean(R, na.rm = TRUE)  
        )

rRev <- dat %>%
        select(R1, Year) %>%
        arrange(Year) %>%
        group_by(Year) %>%
        summarise(
                PosR = mean(R1[R1 >= 0], na.rm = TRUE),  # Mean of positive profits
                NegR = mean(R1[R1 < 0], na.rm = TRUE),   # Mean of negative profits
                avgR = mean(R1, na.rm = TRUE)  
        )

## Mean P&L 

PL <- dat %>%
        select(Profit, Year) %>%
        arrange(Year) %>%
        group_by(Year) %>%
        summarise(
                avgWin = mean(Profit[Profit >= 0], na.rm = TRUE),  # Mean of positive profits
                avgLoss = mean(Profit[Profit < 0], na.rm = TRUE),   # Mean of negative profits
                avgPL = mean(Profit, na.rm = TRUE)  
        )

