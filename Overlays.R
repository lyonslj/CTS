#*****************************************************************
# Load historical data
#*****************************************************************
load.packages('quantmod')

tickers = 'SPY,GLD,IEF,IYR,EEM'
        #SPY, # S&P 500
        #GLD, # Gold
        #IEF, # 10 Year Treasury
        #IYR, # US Real Estate
        #EEM, # Emerging Markets


data <- new.env()
getQuote(tickers, src = 'yahoo', from = '1970-01-01', auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na')


kibrary(tidyquant)
tickers = `GC=F`
getSymbols(tickers, from = '2021-05-01',
           warnings = FALSE,
           auto.assign = TRUE)

Metals <- c("XAU","XAG","XPT","XPD")

getMetals(Metals,
          from = Sys.Date()-180,
          base.currency="USD",
          env = .GlobalEnv,
          verbose = FALSE,
          warning = TRUE,
          auto.assign = TRUE)

#*****************************************************************
# Code Strategies
#*****************************************************************
prices = data$prices
n = ncol(prices)
month.ends = endpoints(prices, 'months')

plota.matplot(scale.one(prices))
