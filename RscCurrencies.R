RscCurr <- function() {
library(dplyr)
library(tidyr)
library(quantmod)

myCurr <- getFX(c("USD/EUR","USD/GBP","USD/ZAR","USD/CAD","USD/CHF","USD/JPY","EUR/GBP","EUR/ZAR","EUR/CAD",
        "EUR/CHF","EUR/JPY","GBP/ZAR","GBP/CAD","GBP/CHF","GBP/JPY","ZAR/CAD","ZAR/CHF","ZAR/JPY",
        "CAD/CHF","CAD/JPY","CHF/JPY","NZD/AUD","NZD/USD","NZD/EUR","NZD/GBP","NZD/ZAR","NZD/CAD","NZD/CHF"
        ,"NZD/JPY","AUD/USD","AUD/EUR","AUD/GBP","AUD/ZAR","AUD/CAD","AUD/CHF","AUD/JPY"))

### --------------------------------------------###
### Calc and add columns for rsc 7 and 55 ma's ###
### --------------------------------------------###
### Initialise your final data frame set ###
df.rsc_calc_set <- data.frame(NULL)                                             # this can be problematic with the rbind
for(i in myCurr) {
        ### 7 mda ###
        tt <- as.name(paste(i,"$ma7 <-","SMA(",i,"[,1],n=7)",sep="",collapse = ""))         # paste string for SMA statement ie SMA(column,7)
        eval(parse(text = tt))                                       # create new column by parsing string as reg expression
        ### 55 mda ###
        tt <- as.name(paste(i,"$ma55 <-","SMA(",i,"[,1],n=55)",sep="",collapse = ""))         # paste string for SMA statement ie SMA(column,7)
        eval(parse(text = tt))                                       # create new column by parsing string as reg expression
        ### working set ###
        set <- eval(parse(text = i))
        df.rsc_set <- data.frame(Date = index(set),set)                       # convert from xts back to df
        #xx <- df.rsc_set[,c("Date",as.name(a),as.name(b),as.name(c))]
        xx <- cbind(i,df.rsc_set)
        names(xx) <- c("Pair","Date","Value","Value7","Value55")
        df.rsc_calc_set <- rbind(df.rsc_calc_set,xx)
}
df.rsc_calc_set$Result<-ifelse((df.rsc_calc_set$Value > df.rsc_calc_set$Value7) & (df.rsc_calc_set$Value7 > df.rsc_calc_set$Value55),2,     
                               ifelse((df.rsc_calc_set$Value < df.rsc_calc_set$Value7) & (df.rsc_calc_set$Value7 < df.rsc_calc_set$Value55), -2,1))
df.new <- separate(df.rsc_calc_set, Pair, into = c("Base","Quoted"), sep = 3)                         # Split pair into base and quoted instrm
### Create compute frame ###
BaseFrame <- filter(df.new, df.new$Result == 2) 
BaseFrameSelect <- select(BaseFrame,Base,Date,Result)
names(BaseFrameSelect) <- c("Instr","Date","Result")
QuoteFrame <- filter(df.new, df.new$Result == -2) 
QuoteFrameSelect <- select(QuoteFrame,Quoted,Date,Result)
QuoteFrameSelect$Result <- 2                                                            # convert -ve to +ve
names(QuoteFrameSelect) <- c("Instr","Date","Result")
BQ <- filter(df.new, df.new$Result == 1) 
BQSelect1 <- select(BQ,Base,Date,Result)
names(BQSelect1) <- c("Instr","Date","Result")
BQSelect2 <- select(BQ,Quoted,Date,Result)
names(BQSelect2) <- c("Instr","Date","Result")
### Bind the sets ###
FinalFrame <- rbind(BaseFrameSelect,QuoteFrameSelect,BQSelect1,BQSelect2)
FinalAggregate <- aggregate(Result ~ Instr + Date,FinalFrame,sum)                       # Aggregate
FinalAggregateSpread <- spread(FinalAggregate,Instr,Result) 
z <- xts(FinalAggregateSpread[,-1],order.by = FinalAggregateSpread[,1])        # convert to xts
mypath <- paste(file.path("/Users/johnlyons/Documents/Personal/DataScience/R/RPlots/Correlation/"),"RscCurrencies.csv",sep="")
df.z <- data.frame(z)                                           #convert to df to include Date for xlsx view
write.csv(df.z,mypath)
}



