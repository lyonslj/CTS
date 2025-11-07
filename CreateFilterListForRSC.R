
Watchlist<-c("ANGLO","ANGGOLD","ANGLOPLAT","ARM","ASSORE","ASPEN","BATS","BIDCORP","BIDVEST","CLICKS","CAPITEC","CML","DATATEC","DRDGOLD","EXXARO","IMPERIAL","KUMBAIO","RMBH","BILLITON","CAPITEC","FIRSTRAND","GFIELDS","GROUP-5","GRANPRADE","IMPLATS","MASSMART","MEDCLIN","MRPRICE","NEDCOR","NORTHAM","QUILTER","RAUBEX","SANTAM","SIBANYE","SHOPRIT","SPAR","VODACOM")

dataset <- Watchlist
#dataset <- Longs
name <- "LongWatch"

WatchShorts <- c("ADVENT","ASCENDIS","ASPEN","ASSORE","BIDVEST","TELKOM","RICHEMONT","BLUETEL","BARWORLD","CAPITEC","CASHBIL","CITYLDG","DISCOVERY","ADCOCK","GROWPNT","AFORBES","EOH","FIRSTRAND","SANLAM","AVENG","AVI","FAMBRANDS","IMPERIAL","INVICTA","NEDCOR","PICKNPAY","REDEFINE","SUNINT","TRUWTHS","TONGAAT","REMGRO","LEWIS","JSE","BIDVEST","PPC","WOOLIES")

x <- paste(sort(WatchShorts),collapse="\",\"")

RHODES
SYGNIA
NASPERSN

dataset <- WatchShorts
dataset <- Shorts
name = "WatchShorts"

Resi <- c("ANGLO","ANGGOLD","BILLITON","EXXARO","GFIELDS","GLENCORE","SAPPI","MONDI","SIBANYE","SASOL")
Shorts <- c(Plat,Gold,Tech,Food)

lst <- WatchShorts
lst <- Watchlist
x <- paste("JSEdat$Name == ",dQuote(lst),sep = "")
col <- paste(x,collapse="|")             # parse x to a single variable with Collapse
z <- paste("filter(JSEdat,",col,")",sep="")
zz <- eval(parse(text=z))
mydat <- zz





CurrExCrypt <- JSEdat[!grepl("^C-YENUSD|^C-BTC|^C-BCH|^C-BFP|^C-BTG|^C-DASH|^C-ETH|^C-LTC|^C-XRP", JSEdat$Name), ]
Curr <- unique(CurrExCrypt[grep("^C-", CurrExCrypt$Name), 1])


currLst <- c("C-AUDZAR","C-CHFUSD","C-EURCAD","C-EURGBP","C-EURJPY","C-EURZAR","C-GBPCAD","C-GBPSEK","C-GBPZAR","C-HKDUSD","C-JPYAUD","C-JPYUSD","C-NZDZAR",   "C-SGDUSD","C-USDAUD","C-USDBRL","C-USDBWP","C-USDCAD","C-USDDKK","C-USDEUR","C-USDFJD","C-USDGBP","C-USDGHS","C-USDGIP","C-USDINR","C-USDIRR","C-USDKRW","C-USDMXN","C-USDNOK","C-USDNZD","C-USDPGK","C-USDRUB","C-USDSHP","C-USDTWD","C-USDZAR","C-YENZAR","C-ZARCAD","C-ZAREUR","C-ZARNZD","C-EURCHF","C-GBPJPY","C-USDCHF","C-USDCNY","C-USDJPY","C-ZARCHF","C-ZARJPY")


mydat <- omit.na(mysa)


zz <- na.omit(zz)
