# -*- coding: utf-8 -*-
"""
import yfinance as yf
import pandas as pd
from datetime import date, timedelta

# List of revised tickers for Yahoo Finance
tickers = "GDX RING SGDM GOAU SILJ SLVR XME GBUG GDXJ NLR METL ^GSPC ^DJI ^NDX XLF SIL XLV XLI XOP ITA SMH REMX IBOT BITO MSTR COPX ARTY QTUM"

# Download the data
# The 'download' function returns a pandas DataFrame with all data (Open, High, Low, Close, etc.)
data = yf.download(tickers, period="200d")

# Extract only the "Close" prices
closing_prices = data['Close']

# Save the data to a CSV file
closing_prices.to_csv('200_Day_Closing_Prices.csv')

print("Data downloaded and saved to 200_Day_Closing_Prices.csv")
print(closing_prices.head())
"""

