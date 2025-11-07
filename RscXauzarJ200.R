library(zoo)




# Loop through stock symbols and create chartSeries plots
for (symbol in stock_symbols) {
        # Download stock data
        stock_data <- getSymbols(symbol, from = "2023-01-01", auto.assign = FALSE)
        
        # Create chartSeries plot
        chartSeries(stock_data, name = symbol, type = "candlesticks")
}

instrm <- "TELKOM"

##      --------- Load dat
## Previously Loaded data
JSEloaded_dat <- read.csv("/Users/johnlyons/Documents/Personal/DataScience/R/JSEdat.csv", header = TRUE, sep="")
#                          stringsAsFactors = FALSE)
## -- Convert to correct format
#JSEloaded <- JSEloaded_dat[,-1]
JSEloaded <- JSEloaded_dat
JSEloaded$Date <- as.Date(JSEloaded$Date, "%Y-%m-%d")
#JSEloaded <- JSEloaded[JSEloaded$Date != "2020-04-9",]
## -- Check for last loaded date
max(sort(JSEloaded$Date))
JSEdat <- JSEloaded

JSE.recent.dat <- JSEdat[JSEdat$Date >= Sys.Date()-450,]
JSE.recent.dat <- unique(JSE.recent.dat[complete.cases(JSE.recent.dat),])
cols <- 3:7
JSE.recent.dat[, cols] <- apply(JSE.recent.dat[, cols], 2, as.numeric)

#Do your base RSC instruments 


#Filter for "GOLD-R" and "J200" with case-insensitive match
instrm_xauzar <- JSE.recent.dat %>%
        filter(tolower(Name) == tolower("GOLD-R"))
colnames(instrm_xauzar)[6] <- "xauZarClose"
instrm_xauzar <- instrm_xauzar[,c(2,6)]
instrm_J200 <- JSE.recent.dat %>%
        filter(tolower(Name) == tolower("JH-ALSI40"))
colnames(instrm_J200)[6] <- "J200Close"
instrm_J200 <- instrm_J200[,c(2,6)]

# MA's
instrm_xauzar$MA21 <- rollmean(instrm_xauzar$Close, k = 21, fill = NA, align = "right")
instrm_J200$MA21 <- rollmean(instrm_J200$Close, k = 21, fill = NA, align = "right")


## instrm data
# Filter for TELKOM and ensure sorted by Date
instrm_telk <- JSE.recent.dat[JSE.recent.dat$Name == "TELKOM", ]
instrm_telk <- instrm_telk[order(instrm_telk$Date), ]
# Calculate 21-day moving average on Close
instrm_telk$MA21 <- rollmean(instrm_telk$Close, k = 21, fill = NA, align = "right")

## Join RSC prices for those Dates
instrm_telk_join <- instrm_telk %>%
        left_join(instrm_xauzar, by = c("Date"))
instrm_telk_join <- instrm_telk_join %>%
        left_join(instrm_J200, by = c("Date"))
instrm_telk_join <- instrm_telk_join[,c(1,2,6,9,10)]

## Calc RSC prices and MA's
instrm_telk_join$RscCXauZarC <- instrm_telk_join$Close / instrm_telk_join$xauZarClose
instrm_telk_join$RscCJ200C <- instrm_telk_join$Close / instrm_telk_join$J200Close
instrm_telk_join$MA21RscXauZar <- rollmean(instrm_telk_join$RscCXauZarC, k = 21, fill = NA, align = "right")
instrm_telk_join$MA21RscJ200 <- rollmean(instrm_telk_join$RscCJ200C, k = 21, fill = NA, align = "right")

## Conditions

instrm_telk_join$XauZarCond <- ifelse(instrm_telk_join$RscCXauZarC >= instrm_telk_join$MA21RscXauZar, TRUE, FALSE)
instrm_telk_join$J200Cond <- ifelse(instrm_telk_join$RscCJ200C >= instrm_telk_join$MA21RscJ200, TRUE, FALSE)
instrm_telk_join$DoubleCond <- ifelse((instrm_telk_join$XauZarCond & instrm_telk_join$J200Cond == "TRUE"), TRUE, FALSE)
## Filter Data
tel_tmp <- instrm_telk_join[instrm_telk_join$DoubleCond == "TRUE",]

cols <- 3:7    
par(mfrow=c(2,2))
y <- filter(JSEdat, JSEdat$Name %in% ll4 & JSEdat$Date >= dt)
y[, cols] <- apply(y[, cols], 2, as.numeric)    # remove factors
y <- unique(y)
rng <- max(min(y$Date),as.Date(dt, format="%Y-%m-%d"))
rownames(y) <- y$Name
z <- xts(y[,3:7],order.by = y$Date)        # convert to xts


assign(i,z)



dys  <- as.numeric(Sys.Date()) - as.numeric(as.Date("2009-01-03"))
dys  <- as.numeric(as.Date("2027-12-31")) - as.numeric(as.Date("2009-01-03"))


