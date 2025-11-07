Daily <- function() {
library(quantmod)
library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)
library(readxl)
### load functions ###
source('~/Documents/Personal/DataScience/R/JL CTS scripts/CumulativeReturnsV2.R')
source('~/Documents/Personal/DataScience/R/JL CTS scripts/Opportunities2.R')
source('~/Documents/Personal/DataScience/R/JL CTS scripts/DailyGraphV3.R')
source('~/Documents/Personal/DataScience/R/JL CTS scripts/WeeklyGraph.R')
source('~/Documents/Personal/DataScience/R/JL CTS scripts/PLv2.R')
source('~/Documents/Personal/DataScience/R/JL CTS scripts/LiveCumulative.R')
source('~/Documents/Personal/DataScience/R/JL CTS scripts/MADirection.R')
source('~/Documents/Personal/DataScience/R/JL CTS scripts/Opportunities2.R')
source('~/Documents/Personal/DataScience/R/JL CTS scripts/Correlation.R')
source('~/Documents/Personal/DataScience/R/JL CTS scripts/RelativeStrengthComparative.R')
source('~/Documents/Personal/DataScience/R/JL CTS scripts/RscHeatmapOrdered.R')    
source('~/Documents/Personal/DataScience/R/JL CTS scripts/fnLoadData.R')     
source('~/Documents/Personal/DataScience/R/JL CTS scripts/fnInstrm.R')           
        
      
        
### Get saved data ###
        ## Previously Loaded data
        JSEloaded_dat <- read.csv("/Users/johnlyons/Documents/Personal/DataScience/R/JSEdat.csv", 
                                  stringsAsFactors = FALSE)
        ## -- Convert to correct format
        JSEloaded <- JSEloaded_dat[,-1]
        JSEloaded$Date <- as.Date(JSEloaded$Date, "%Y-%m-%d")
        #JSEloaded <- JSEloaded[JSEloaded$Date != "2020-04-9",]
        ## -- Check for last loaded date
        max(sort(JSEloaded$Date))
        JSEdat <- JSEloaded
        tail(unique(JSEdat$Date))
        #JSEdat <- JSEdat[JSEdat$Date != "2019-07-18",]          # remove dirty data
        
### Run to bring in daily file, by default passes the previous days file - format "04-09-20.csv"  ###   
        fnLoadData()
        
### Live Trades ###        
        Trades <- read_excel("/Users/johnlyons/Dropbox/CTS/Trades.xlsx",col_names = TRUE,na = "")
        Trades <- as.data.frame(Trades)
        Trades$Purchase <- as.Date(Trades$Purchase)
        Trades$`Exit Dt` <- as.Date(Trades$`Exit Dt`)
        mylst <- subset(Trades,is.na(Trades$Exit)) 
RSC()

## Search for instr     ##
        ##      unique(JSEdat[grepl("SILV",JSEdat$Name),1])
        ##      
        ##      View(unique(sort(bigdf[,2])))   ##View of all inst
        ##      View((((bigdf[bigdf$Date == "20200401",]))))

        ### my Lists ###

### ----------------###
### Generate Graphs ###
### ----------------###
fnInstrm()
CumulativeReturns(Sys.Date()-7,to_graph,"TopPerformers7")
CumulativeReturns(Sys.Date()-30,to_graph,"TopPerformers30")
CumulativeReturns(Sys.Date()-60,to_graph,"TopPerformers60")
CumulativeReturns(Sys.Date() - 20,Food,"Food")
CumulativeReturns("2018-11-04",Phar,"Phar")
CumulativeReturns("2019-01-04",Metl,"Metl")
CumulativeReturns(Sys.Date() - 20,JlRes,"JLRes")
CumulativeReturns("2019-01-10",Indm,"Indm1")
CumulativeReturns(Sys.Date() - 20,KeyIndixes,"KeyIndixes")
CumulativeReturns(Sys.Date()-10,JSE,"JSE")
CumulativeReturns(Sys.Date()-60,mylst[,1],"Live60")
CumulativeReturns(Sys.Date()-7,mylst[,1],"Live7")
CumulativeReturns(Sys.Date()-20,IntInd,"IntInd")
CumulativeReturns("2019-01-04","NIKKEI","Nikkei")
CumulativeReturns("2019-01-04","BDI","Baltic")
CumulativeReturns("2019-01-04","USALB","TNote10")
CumulativeReturns("2019-01-04",Banks,"Banks")
CumulativeReturns(Sys.Date()-60,Cons,"Cons60")
CumulativeReturns(Sys.Date()-30,Cons,"Cons30")
CumulativeReturns(Sys.Date()-120,Tele,"Telecoms120")
CumulativeReturns("2018-08-04",Plat,"Plat LT")
CumulativeReturns(Sys.Date() - 30,Plat,"Plat")
CumulativeReturns("2018-08-05",Gold,"Gold")
CumulativeReturns(Sys.Date() - 30,Gold,"Gold1")
CumulativeReturns(Sys.Date()-10,Metals,"Metals10")
CumulativeReturns(Sys.Date()-40,Tech,"Tech40")
CumulativeReturns(Sys.Date()-20,CurrUSD,"CurrUSD20")
CumulativeReturns("2019-01-04",CurrZAR,"CurrZAR")
CumulativeReturns("2019-01-04","USD-INDEX","USD")
CumulativeReturns(Sys.Date() - 10,Watchlist,"Watchlist")
CumulativeReturns("2019-01-04",jl,"JL")
CumulativeReturns("2019-01-04",jl1,"JL1")
CumulativeReturns("2018-12-02",jl2,"GoldMetals")
CumulativeReturns("2019-11-04",jl3,"UsdJpy-Au")
CumulativeReturns("2019-08-04",YenZar,"YenZar")
CumulativeReturns("2018-08-04",CorrHealthPhar,"HealthPhar")
CumulativeReturns("2019-02-01",Health,"Health")
CumulativeReturns("2019-11-20",Corr1,"MainInd")
#CumulativeReturns("2017-01-01",CorrYenZar,"CorrYenZar")
CumulativeReturns("2019-08-04",Corr3,"Corr3-Note Inverse relation Gold to Banks")
CumulativeReturns("2019-03-15",Corr4,"JSE AU - Plat")
CumulativeReturns("2019-08-04",Corr6,"Plat : Plat-$")
CumulativeReturns("2019-08-04",VolZar,"VolZar")
DailyGraph(KeyIndixes,"2018-04-01")
DailyGraph(instLive)
DailyGraph(Watchlist,"2018-04-01")
DailyGraph(JlRes,"2018-04-01")
DailyGraph(Banks,"2018-09-01")
DailyGraph(Tech,"2018-02-01")
DailyGraph(Gold,"2019-04-01")
DailyGraph(Plat,"2018-07-01")
DailyGraph(WatchFilt)
DailyGraph(Metl,"2017-04-01")
DailyGraph(Indm,"2018-04-01")
DailyGraph(Metals)
DailyGraph(IntInd)
DailyGraph(Health)
DailyGraph(Phar,"2017-03-01")
DailyGraph(jl1)
DailyGraph(CurrZAR,"2016-07-01")
DailyGraph(Greenlst,"2016-04-01")
DailyGraph(Redlst,"2016-04-01")
ProffitLoss()
LiveCumulative()


Correlation()
MADirection()
#WrWeek()
Opportunities()
getSymbols(tickers)
getFX(currLst)
chartSeries(BTCZAR,multi.col = TRUE, theme = "black", TA="addMACD();addBBands()",subset = "2016-04::")
chartSeries(NZDZAR,multi.col = TRUE, theme = "black", TA="addMACD();addBBands()",subset = "2016-04::")
chartSeries(JPYZAR,multi.col = TRUE, theme = "black", TA="addMACD();addBBands()",subset = "2016-04::")
chartSeries(USDJPY,multi.col = TRUE, theme = "black", TA="addMACD();addBBands()",subset = "2016-04::")
chartSeries(N225,multi.col = TRUE, theme = "black", TA="addMACD();addBBands()",subset = "2016-11-15::")
chartSeries(GSPC,multi.col = TRUE, theme = "black", TA="addMACD();addBBands()",subset = "2016-11::")
chartSeries(`C-USDZAR`,multi.col = TRUE, theme = "black", TA="addMACD();addBBands()",subset = "2016-11::")
chartSeries(`SIBANYE-S`,multi.col = TRUE, theme = "black", TA="addMACD();addBBands()",subset = "2016-11::")
}

