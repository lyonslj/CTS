library(quantmod)
library(dplyr)
library(xts)

getSymbols("MSFT")
getSymbols("AAPL")
getSymbols("COKE")
getSymbols("PEP")

#Get the return
MSFT.Return <- diff(MSFT)/stats::lag(MSFT)
AAPL.Return <- diff(AAPL)/stats::lag(AAPL)
COKE.Return <- diff(COKE)/stats::lag(COKE)
PEP.Return <- diff(PEP)/stats::lag(PEP)

#Get the return for last two months and only get close price return.
#because in my data I only have the close price. 
MSFT.Close <- MSFT.Return['2018-10::', 'MSFT.Close']
AAPL.Close <- AAPL.Return['2018-10::', 'AAPL.Close']
COKE.Close <- COKE.Return['2018-10::', 'COKE.Close']
PEP.Close <- PEP.Return['2018-10::', 'PEP.Close']

pdf(sprintf("%s.pdf","ExampleGraph"), width=11.69, height=8.27)

layout(matrix(1:4, ncol=2)) 

#Get the difference in return
techDifference <- MSFT.Close - AAPL.Close
bevDifference <- COKE.Close - PEP.Close

#Rename columns
colnames(MSFT.Close)[1] <- "MSFT"
colnames(AAPL.Close)[1] <- "AAPL"
colnames(techDifference)[1] <- "Difference"

colnames(COKE.Close)[1] <- "COKE"
colnames(PEP.Close)[1] <- "PEP"
colnames(bevDifference)[1] <- "Difference"

#Combine into two tables
tech <- cbind(MSFT.Close, AAPL.Close,techDifference)
bev <- cbind(COKE.Close, PEP.Close,bevDifference)

# 1 Plot charts - 1 and 2
chartSeries(tech, order=1,up.col='green', name='MSFT & AAPL', layout=NULL,
            TA=c("addTA(tech,order=2,on=1,layout=NULL);
                addTA(tech$Difference,legend='Difference',type='h',layout=NULL)"))

# 2 - charts 3&4
chartSeries(bev, order=1,up.col='green', name='COKE & PEP', layout=NULL,
            TA=c("addTA(bev,order=2,on=1,layout=NULL);
addTA(bevDifference$Difference,legend='Difference',type='h',layout=NULL)"))

#Take the cumulative difference for each sector 
techCumulative <- cumsum(abs(techDifference))
bevCumulative <- cumsum(abs(bevDifference))
diffCumulative <- techCumulative - bevCumulative 

#Rename columns
colnames(techCumulative)[1] <- "Tech"
colnames(bevCumulative)[1] <- "Beverage"
#If I set the name as Volume, I can use addVo() and get nice barplot.
#Problem with that is the legend name will be Volume but I would like to
#have it Difference and of course I'm using wrong column name. 
colnames(diffCumulative)[1] <- "Volume"

#Combine into one table
cumulative <- cbind(techCumulative,bevCumulative,diffCumulative)

# 3 Plot chart
chartSeries(cumulative,order=1,up.col='green', name='Cumulative Difference',
            layout=NULL,
            TA=c("addTA(cumulative,order=2,on=1,layout=NULL)", addVo()))

#Get the correlation matric
correlationTable <- cbind(tech[,1:2],bev[,1:2])
correlation <- cor(correlationTable)
corTable <- as.table(correlation)
corrFormatted <- formatC(corTable, format = "f", digits = 3)
textplot(corrFormatted,valign="top",col.data=colors()[300],
         col.rownames=colors()[300],col.colnames=colors()[300])
title("Correlation",cex.main=2.5,col.main=colors()[300])

dev.off()

#####
chart_Series(z, 
             up.col = "blue", dn.col = "red",
             subset = range_daily)
add_TA(SMA(z$Close, 21))
add_TA(SMA(z$Close, 50), on = 2, col = "green")
add_TA(SMA(z$Close, 100), on = 2, col = "red")

