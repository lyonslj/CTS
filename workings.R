library(jsonlite)
library(httr)

# Get the CoinGecko API key
api_key <- "YOUR_API_KEY"

# Get the BTC price data for the last 30 days
url <- paste0("https://api.coingecko.com/api/v3/coins/bitcoin/market_chart?vs_currency=usd&days=3")

response <- GET(url, add_headers(Authorization = paste0("Bearer ", api_key)))

if (response$status_code == 200) {
        # The request was successful
        data <- fromJSON(content(response, "text"))
        
        # Print the BTC price data
        print(data)
} else {
        # The request failed
        print(paste0("Error: ", response$status_code))
}

bitty <- data$prices
bitty <- as.data.frame(bitty)
colnames(bitty)[1] <- "Date"
colnames(bitty)[2] <- "Price"


# Convert milliseconds to seconds
bitty$Date <- bitty$Date / 1000

# Convert Unix timestamp to POSIX datetime
bitty$Date <- as.POSIXct(bitty$Date, origin = "1970-01-01", tz = "UTC")

# Format the POSIX datetime as %y-%m-%d
bitty$Date <- format(bitty$Date, "%y-%m-%d %H:%M")





######

library(coinmarketcapr)
library(jsonlite)
library(curl)
library(ggplot2)
library(data.table)
library(cli)
library(crayon)

latest_marketcap <- get_global_marketcap('EUR')

# Get the list of the top 300 coins
top_300_coins <- cmc_top(limit = 300)

# Get the daily closing prices for the last 5 days
closing_prices <- cmc_ohlcv(
        coins = "BTC",
        start_date = "2023-08-20",
        end_date = "2023-08-25",
        interval = "daily"
)


##### Upgrade
tmp <- installed.packages()
installedpkgs <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
xx <- installedpkgs
save(installedpkgs, file = "installedpkgs.RData")


load("installedpkgs.RData")

for (package_name in xx[,1]) {
        install.packages(package_name)
}

getwd()
