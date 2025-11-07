
lst <- "DRDGOLD"
mydat <- filter(JSEdat, JSEdat$Name %in% lst)
df <- mydat[,c(1,2,6)]

df_wide <- df %>%
        pivot_wider(
                names_from = Name,  # This column's values will become new column names
                values_from = Close,  # This column's values will fill the new columns
                id_cols = "Date"  # This column will remain as is, defining rows
        )

df_returns <- df_wide %>%
        mutate(across(2:7, 
                      function(x) (x - lag(x)) / lag(x), 
                      .names = "{.col}_return")) %>%
        select(Date, ends_with("_return")) %>%
        na.omit()  # Remove the first row where returns cannot be calculated


correlations <- cor(df_returns[[7]],  # Gold returns are in the second column of df_returns
                    df_returns[, 2:6]) 

############################ Weekly Correlation ##########################
library(lubridate)
# Aggregate data by week
df_weekly <- df_wide %>%
        mutate(Week = floor_date(Date, "week")) %>%
        group_by(Week) %>%
        summarise(across(2:7, last))  # Taking the last value of the week as an example

# Calculate weekly returns
df_weekly_returns <- df_weekly %>%
        mutate(across(2:7, 
                      function(x) (x - lag(x)) / lag(x), 
                      .names = "{.col}_return")) %>%
        select(Week, ends_with("_return")) %>%
        na.omit()

# Calculate correlation between weekly gold returns and each counter's weekly returns
correlations_weekly <- cor(df_weekly_returns[[7]],  # Gold returns are in the second column of df_weekly_returns
                           df_weekly_returns[, 2:6])  # Counter returns are from column 3 to 6
#############################################################################
df_long <- df_weekly_returns %>%
        pivot_longer(cols = -Week, names_to = "Counter", values_to = "Return")

ggplot(tail(df_long,200), aes(x = Week, y = Return, color = Counter)) +
        geom_line() +
        geom_point() +
        labs(title = "Weekly Returns of Share Counters",
             x = "Date",
             y = "Weekly Return") +
        theme_minimal() +
        theme(legend.position = "bottom")

##########
##########
# Function to calculate correlations for each week
calculate_weekly_correlation <- function(df) {
        # Calculate weekly correlations
        correlations <- df_weekly_returns %>%
                group_by(Week) %>%
                summarise(across(2:6, 
                                 ~ cor(.x, df[[7]]), 
                                 .names = "correlation_{.col}"),
                          .groups = "drop")
        
        # Convert to long format for plotting
        correlations_long <- correlations %>%
                pivot_longer(cols = starts_with("correlation"), names_to = "Counter", values_to = "Correlation")
        
        return(correlations_long)
}

# Apply the function to your data
weekly_correlations <- calculate_weekly_correlation(df_weekly_returns)

# Plot the changing correlations over time
ggplot(weekly_correlations, aes(x = Week, y = abs(Correlation), color = Counter)) +  # Use abs for leverage
        geom_line() +
        geom_point() +
        labs(title = "Weekly Leverage of Counters to Gold Price Changes",
             x = "Week",
             y = "Leverage (Absolute Correlation)",
             color = "Counter") +
        theme_minimal() +
        theme(legend.position = "bottom")

Explanation:
        
        Function for Weekly Correlations: The calculate_weekly_correlation function computes the correlation between each counter and gold for each week. We use summarise with across to apply the cor function to each counter against gold's returns (column 7).
    Data Transformation: We then reshape the data into a long format where each row represents a counter's correlation with gold for a specific week.
Plotting: 
        ggplot is used to create a line plot where each line represents one counter's absolute correlation over time. 
        abs(Correlation) is used to show the magnitude of leverage, treating positive and negative correlations equally in terms of impact or leverage.
        geom_line and geom_point help visualize the trend and points for each week.

This visualization will show you how the leverage (measured as the absolute correlation) of each counter to gold price changes over weeks. If you're interested in the direction of the correlation (whether it's positive or negative), you could plot Correlation directly without abs(). Remember, a high absolute correlation suggests high leverage, but the sign of the correlation tells you if the counter moves in the same or opposite direction to the gold price.











#########
#########







df <- PanExtract %>%
        mutate(
                Return = (Close - lag(Close)) / lag(Close)*100
        ) %>%
        na.omit() 

df$Volatility  <- sd(df$Return)

df$annual_volatility <- df$volatility * sqrt(252)# Assuming 252 trading days per year


############

df_long <- df %>%
  pivot_longer(cols = -Date, names_to = "Counter", values_to = "Return")
