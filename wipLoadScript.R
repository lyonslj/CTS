library(quantmod)
library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)
library(readxl)
library(plotly)
if (!require("gplots")) {
        install.packages("gplots", dependencies = TRUE)
        library(gplots)
}
if (!require("RColorBrewer")) {
        install.packages("RColorBrewer", dependencies = TRUE)
        library(RColorBrewer)
}

### load functions ###




setwd("/Users/johnlyons/Desktop/snet")

        ## Previously Loaded data
        #
        JSEloaded_dat <- read.csv("/Users/johnlyons/Documents/Personal/DataScience/R/JSEdat.csv", 
                                  stringsAsFactors = FALSE)
        ## -- Convert to correct format
        JSEloaded <- JSEloaded_dat[,-1]
        JSEloaded$Date <- as.Date(JSEloaded$Date, "%Y-%m-%d")
        
        JSEloaded <- JSEloaded[JSEloaded$Date != "2020-04-9",]
        
        ## -- Check for last loaded date
        max(sort(JSEloaded$Date))
        
        ## -- function to loaddata 
        fnLoadData <- function(file = paste(format(Sys.Date()-1,"%d-%m-%y"),"csv",sep=".")) {
                pathfile <- paste("/Users/johnlyons/Desktop/snet/",file, sep = "")
                df.lst <- read.csv(pathfile,skip = 5, header = FALSE, sep =",", stringsAsFactors = FALSE)        # read new csv's#
                df.clean <- df.lst[,1:11]
                df.clean <- df.clean[,-c(4,5)]
                names(df.clean) <- c("Name","Exchange","Instm", "Date","Open", "High", "Low", "Close", "Volume")
                new.dat <- subset(df.clean[,-3],df.clean$Exchange %in% c("JSE","SPOT","FOREX"))
                new.dat <- new.dat[,-2]
                new.dat$Date <- as.Date(as.character(new.dat$Date),format="%Y%m%d")               #convert to char then date
                new.dat$Volume <- as.numeric(new.dat$Volume)
                # Now merger to main set
                JSEdat <- rbind(JSEloaded,new.dat)
                
        }

JSEloaded_dates <- unique(as.Date(as.character(JSEloaded_dat$Date),format="%Y-%m-%d"))     

# Look for new files
file.list <- list.files(path = "/Users/johnlyons/Desktop/snet",pattern='*.csv')
file.llst <- gsub(".csv","",file.list)
file.llst.mod <- as.Date(file.llst, format = "%d-%m-%y")       #get them to the same format
dts_loaded <- unique(JSEloaded_dat$Date)
dts.to.be.loaded <- (setdiff(as.character(file.llst.mod), as.character(JSEloaded_dates)))         #dts to be loaded
dts.to.be.loaded <- as.Date(dts.to.be.loaded,"%Y-%m-%d")
dts.to.be.loaded <- format(dts.to.be.loaded,"%d-%m-%y")
#now convert to orig dt format and append .csv
dts.to.be.loaded <- paste(dts.to.be.loaded,"csv", sep=".")

## Now append them to existing file
df.lst <- lapply(dts.to.be.loaded ,read.csv,skip = 5, header = FALSE, sep =",")        # read new csv's#
bigdf <- ldply(df.lst, rbind)           # combine into one datafram
bigdf <- bigdf[,1:11]
bigdf <- bigdf[,-4]
bigdf <- bigdf[,-4]
names(bigdf) <- c("Name","Exchange","Instm", "Date","Open", "High", "Low", "Close", "Volume")
JSEdat <- subset(bigdf[,-3],bigdf$Exchange %in% c("JSE","SPOT","FOREX"))
JSEdat <- JSEdat[,-2]
JSEdat$Date <- as.Date(as.character(JSEdat$Date),format="%Y%m%d")               #convert to char then date
# Now merger to main set
JSEdat <- rbind(JSEloaded_dat,JSEdat)
JSEdat$Volume <- as.numeric(JSEdat$Volume)  
write.csv(JSEdat,file = "//Users/johnlyons/Desktop/snet/JSEdat.csv")


## *****
## *****
##
## Search JSE indexes


## Indexes
JSE <- c("JSE-OILP","JSE-GOLD","JSE-COAL","JSE-PLAT","JSE-METL","JSE-INDM","JSE-ALSH","JSE-RESI","JSE-INDI","JSE-FINI","JSE-ALTX","JSE-AUTM","JSE-BEVR","JSE-FOOD","JSE-PHAR","JSE-OILG","JSE-HEAL","JSE-CONS","JSE-TELE","JSE-TECH","JSE-BANK","JSE-LIFE")

CurrUSD <- c("C-USDAUD","C-USDCAD","C-USDCHF","C-USDGBP","C-USDEUR","C-USDJPY","C-USDZAR","C-USDEUR","C-USDCNY","C-BTCUSD")
CurrZAR <- c("C-EURZAR","C-GBPZAR","C-USDZAR","C-NZDZAR","C-ZARJPY")
Plat <- c("PLAT-$","RBPLAT","PLAT-R","JSE-PLAT","ANGLOPLAT","IMPLATS","LONMIN","NORTHAM")
Gold <- c("ANGGOLD"  ,"JSE-GOLD" ,"PAN-AF", "DRDGOLD" ,  "HARMONY" ,  "GFIELDS" ,  "SIBANYE",   "GOLD-R","GOLD-PM")
KeyIndixes <- c("BDI","JSE-CONS","JSE-PLAT","JSE-GOLD","JSE-INDI","JH-ALSI40","JSE-FINI","SP500","JSE-BANK","GOLD-R","JSE-RESI","JSE-INDM","C-YENZAR","USALB","NIKKEI","C-JPYUSD","BRENT")
MtlsIndexes <- c("JSE-PHAR","JSE-CONS","JSE-PLAT","JSE-GOLD","JSE-INDI","JH-ALSI40","JSE-FINI","SP500","JSE-BANK","GOLD-R","JSE-INDM","C-YENZAR","USALB","NIKKEI","C-JPYUSD","BRENT","PLAT-$","PALLAD-$","NICKEL","ZINC-$","COPPER-$","SILV-$","BDI")
indi <- c("NETCARE","LIFEHC","STEINHOFF","PICKNPAY","WOOLIES","TRUWTHS","RICHEMONT","MTN","MEDCLIN","CLICKS","VODACOM","BIDVEST","TFG","MRPRICE","IMPERIAL","SPAR","SHOPRIT","REMGRO","BIDCORP","ASPEN","TIGBRANDS","BATS","NASPERSN")
Metals <- c("COPPER-$","ZINC-$","NICKEL","BRENT","PALLAD-$","GOLD-R","PLAT-$","SILV-$","SILV-R")
Indm <- c("ARCMITTAL","SOUTH32","KUMBAIO")
Banks <- c("B-AFRICA","STANBANK","NEDCOR","FIRSTRAND","CAPITEC","DISCOVERY","JSE-BANK")
Food <- c("RCL","CLOVER","RHODES","A-V-I","OCEANA","ASTRAL","TONGAAT","PNR-FOODS","TIGBRANDS","JSE-FOOD")
Metl <- c("GLENCORE","ARM","ANGLO","BILLITON","ASSORE")
Cons <- c("ADVTECH","BIDCORP","CHOPPIES","CITYLDG","CLICKS","CURRO","CASHBIL","FAMBRANDS","HOLDSPORT","ITLTILE",
          "MRPRICE","MASSMART","NASPERSN","PICKNPAY","SHOPRIT","SPAR","SUNINT","TFG","TRUWTHS","WOOLIES")
IntInd <- c("NASDAQ",            "MSCI-EMM",            "SP500",            "DAX-INDEX",            "ALL-ORDS",            "TSE300",            "CAC-40",            "HANG-SENG",            "SET-INDEX",            "FT-100",            "NIKKEI",            "SENSEX",            "SHANGHAIC",            "SMI",            "MEX-IPC",            "BRAZ-BOV")

#*********************************************
# Build individual xts sets of closing prices*
#*********************************************
dt <- "2017-01-03"
alldata2 <- unique(subset(JSEdat,JSEdat$Date >= dt))
Index <- Cons
for(i in Index) {
        y <- subset(alldata2,alldata2$Name == i)
        y <- y[,c("Date","Close")]
        y <- aggregate(Close ~ Date, y, mean)   # get rid of dups and multiple entries
        z <- xts(y[,2],order.by = y[,1])        # convert to xts
        colnames(z) <- "Close"
        z$Previous <- lag.xts(z,lag=1)
        z$DysMove <- z$Close - z$Previous
        nm <- paste(i,"%age",sep="")
        z$`nm` <- round((z$DysMove/z$Previous)*100,1)
        colnames(z)[4] <- nm
        assign(i,z)
}



myxts <- do.call(merge,sapply(Index,as.name)) 
perc_cols <- grep("age", names(myxts), value = TRUE)         # extract %age columns
myxts_perc <- myxts[,perc_cols]


# creates a own color palette from red to green
my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)

# (optional) defines the color breaks manually for a "skewed" color transition
col_breaks = c(seq(-4,-1,length=100),  # for red
               seq(0,1,length=100),           # for yellow
               seq(2,10,length=100))             # for green

mydat <- tail(data.matrix(myxts_perc),24)
c_names <- colnames(mydat)
c_names <- gsub(".age","",c_names)
colnames(mydat) <- c_names

mydat <- t(mydat)

#png("/Users/johnlyons/Documents/Personal/DataScience/R/Rplots/DailyReturns.png")
heatmap.2(mydat,
          cellnote = mydat,  # same data set for cell labels
          main = "Daily Returns", # heat map title
          notecol="black",      # change font color of cell labels to black
          notecex=1.0,
          density.info="none",  # turns off density plot inside color legend
          trace="none",         # turns off trace lines inside the heat map
          margins =c(12,9),     # widens margins around plot
          col=my_palette,       # use on color palette defined earlier
          keysize=0.25,
          breaks=col_breaks,   # enable color transition at specified limits
          dendrogram="row",    # only draw a row dendrogram
          Colv = FALSE,          # turn off column clustering
          Rowv = FALSE)         # stop reorder o
#dev.off()




mm <- sapply(mylst,as.name)      



JSE.Ind.dat <- subset(JSEdat,JSEdat$Name %in% JSE)
JSE.Ind.dat <- JSE.Ind.dat[complete.cases(JSE.Ind.dat),]
JSE.Ind.dat <- JSE.Ind.dat[,c(2:8)]
max.dt <- JSE.Ind.dat[which.max(JSE.Ind.dat$Date),2]
dt.rng <- sort(unique(JSE.Ind.dat$Date))
prev.dt <- dt.rng[length(dt.rng)-1]
last.2.dys <- c(prev.dt,max.dt)
dat.extract <- subset(JSE.Ind.dat,JSE.Ind.dat$Date %in% last.2.dys)
## Convert to xts
dat.extract <- as.xts(dat.extract)

## Approach2 with xts
JSEdat <- JSEdat[,-1]
JSEdat <- JSEdat[complete.cases(JSEdat),]
JSEdat <- xts(JSEdat[,c(1:7)], order.by = JSEdat[,2])   ## Convert to xts
JSEdatlag1 <- lag.xts(JSEdat,lag = 1)
xxx <- subset(JSEdatlag1,JSEdatlag1$Name %in% JSE)


s## Work out lag
dat.extract <- dat.extract %>%
        group_by(Name) %>%
        mutate(c.diff = lag(Close,n=2))


JSEdat$Date <- as.Date(JSEdat$Date, format = "%Y-%m-%d")
### Live Trades ###        
Trades <- read_excel("/Users/johnlyons/Dropbox/CTS/Trades.xlsx",col_names = TRUE,na = "")
Trades <- as.data.frame(Trades)
Trades$Purchase <- as.Date(Trades$Purchase)
Trades$`Exit Dt` <- as.Date(Trades$`Exit Dt`)
mylst <- subset(Trades,is.na(Trades$Exit)) 
RSC()