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
getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na')

#*****************************************************************
# Code Strategies
#*****************************************************************
prices = data$prices
n = ncol(prices)
month.ends = endpoints(prices, 'months')

plota.matplot(scale.one(prices))
