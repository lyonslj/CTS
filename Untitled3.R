fnLoadData2 <- function(file = csv_file) {
        
        
        
        # get file based on date
        pathfile <- paste("/Users/johnlyons/Downloads/",file, sep = "")
        
        # read file with format and cleanups
        df.lst <- read.csv(pathfile,skip = 5, header = FALSE, sep =",", stringsAsFactors = FALSE)        # read new csv's#
        df.clean <- df.lst[,1:11]
        df.clean <- df.clean[,-c(4,5)]
        names(df.clean) <- c("Name","Exchange","Instm", "Date","Open", "High", "Low", "Close", "Volume")
        new.dat <- subset(df.clean[,-3],df.clean$Exchange %in% c("JSE","SPOT","FOREX"))
        new.dat <- new.dat[,-2]
        new.dat$Date <- as.Date(as.character(new.dat$Date),format="%Y%m%d")               
        new.dat$Volume <- as.numeric(new.dat$Volume)
        
        
        #tail(sort(unique(new.dat$Date)))
        #tail(sort(unique(JSEdat$Date)))
        #max(sort(JSEloaded$Date))
        # Return the data
        return(new.dat)
        
 #       unique(JSEdat[grep("BHG",JSEdat$Name),1])
        
}


# Loop to run through all files and create JSEdat
library(readr)

# Create a list of the CSV files to read
csv_files <- list.files(path = "/Users/johnlyons/Downloads/", pattern = "*.csv")

# Create a new file to append the data to
output_file <- "/Users/johnlyons/Documents/Personal/DataScience/R/JSEdat.csv"

# Iterate over the list of CSV files
for (csv_file in csv_files) {
        # Load the data from the file
        data <- fnLoadData2(csv_file)
        
        # Append the data to the output file
        write.table(data, output_file, append = TRUE, row.names = FALSE, quote = FALSE)
       
}



##########################

JSEloaded_dat <- read.csv("/Users/johnlyons/Documents/Personal/DataScience/R/JSEdat.csv", header = TRUE, sep="")
JSEloaded <- JSEloaded_dat
JSEloaded$Date <- as.Date(JSEloaded$Date, "%Y-%m-%d")
#JSEloaded <- JSEloaded[JSEloaded$Date != "2020-04-9",]
## -- Check for last loaded date
JSEdat <- distinct(JSEloaded)   #Remove duplicates - keep row order

# Factors to numerics
cols <- 3:7
JSEdat[, cols] <- apply(JSEdat[, cols], 2, as.numeric)

#*******------------
# Subset the data
BTC <- JSEdat[JSEdat$Name == "C-BTCUSD", ]
ETH <- JSEdat[JSEdat$Name == "C-ETHUSD", ]  # Renamed to ETH for clarity
ZARUSD <- JSEdat[JSEdat$Name == "C-USDZAR", ]
NIK <- JSEdat[JSEdat$Name == "NIKKEI", ]

# Merge BTC and ZARUSD by Date
BTCZAR <- merge(BTC, ZARUSD, by = "Date", suffixes = c("_BTC", "_ZARUSD"))

# Calculate C-BTCZAR
BTCZAR <- data.frame(
        Name = "C-BTCZAR",
        Date = BTCZAR$Date,
        Open = BTCZAR$Close_BTC * BTCZAR$Open_ZARUSD,
        High = BTCZAR$Close_BTC * BTCZAR$High_ZARUSD,
        Low = BTCZAR$Close_BTC * BTCZAR$Low_ZARUSD,
        Close = BTCZAR$Close_BTC * BTCZAR$Close_ZARUSD,
        Volume = 0
)

BTCZAR$Low <- ifelse(BTCZAR$Close < BTCZAR$Low, BTCZAR$Close, BTCZAR$Low)               # Fix high and low adjusting for currency
BTCZAR$High <- ifelse(BTCZAR$Close > BTCZAR$High, BTCZAR$Close, BTCZAR$High)

# Merge ETH and ZARUSD by Date
ETHZAR <- merge(ETH, ZARUSD, by = "Date", suffixes = c("_ETH", "_ZARUSD"))

# Calculate C-ETHZAR
ETHZAR <- data.frame(
        Name = "C-ETHZAR",
        Date = ETHZAR$Date,
        Open = ETHZAR$Close_ETH * ETHZAR$Open_ZARUSD,
        High = ETHZAR$Close_ETH * ETHZAR$High_ZARUSD,
        Low = ETHZAR$Close_ETH * ETHZAR$Low_ZARUSD,
        Close = ETHZAR$Close_ETH * ETHZAR$Close_ZARUSD,
        Volume = 0
)
ETHZAR$Low <- ifelse(ETHZAR$Close < ETHZAR$Low, ETHZAR$Close, ETHZAR$Low)               # Fix high and low adjusting for currency
ETHZAR$High <- ifelse(ETHZAR$Close > ETHZAR$High, ETHZAR$Close, ETHZAR$High)

# Remove existing C-BTCZAR and C-ETHZAR from JSEdat (if any) to avoid duplicates
JSEdat <- JSEdat[!JSEdat$Name %in% c("C-BTCZAR", "C-ETHZAR"), ]

# Append the new datasets to JSEdat
JSEdat <- rbind(JSEdat, BTCZAR, ETHZAR)

# Ipdate GOLD-R to more realistic price - remove lag


# Extract the relevant subsets for multiplication, ensuring unique Dates
gold_pm <- JSEdat %>%
        filter(Name == "GOLD-PM") %>%
        distinct(Date, .keep_all = TRUE) %>%  # Deduplicate by Date, keeping first occurrence
        select(Date, Open_pm = Open, High_pm = High, Low_pm = Low, Close_pm = Close)

c_usdzar <- JSEdat %>%
        filter(Name == "C-USDZAR") %>%
        distinct(Date, .keep_all = TRUE) %>%  # Deduplicate by Date, keeping first occurrence
        select(Date, Open_zar = Open, High_zar = High, Low_zar = Low, Close_zar = Close)

# Update only the GOLD-R rows by joining on Date and multiplying
JSEdat <- JSEdat %>%
        left_join(gold_pm, by = "Date") %>%
        left_join(c_usdzar, by = "Date") %>%
        mutate(
                Open   = ifelse(Name == "GOLD-R", Open_pm * Open_zar,   Open),
                High   = ifelse(Name == "GOLD-R", High_pm * High_zar,   High),
                Low    = ifelse(Name == "GOLD-R", Low_pm * Low_zar,     Low),
                Close  = ifelse(Name == "GOLD-R", Close_pm * Close_zar, Close)
        ) %>%
        select(-ends_with("_pm"), -ends_with("_zar"))

# JSE-GOLD Index
##
GOLD <- c("ANGGOLD", "PAN-AF", "DRDGOLD", "HARMONY", "GFIELDS")

# Step 1: Filter data for the symbols
gold_data <- JSEdat[JSEdat$Name %in% GOLD, ]

# Step 2: Compute daily returns for Close, Open, High, Low for each instrument
gold_returns <- gold_data %>%
        group_by(Name) %>%
        arrange(Date) %>%
        mutate(
                ret_close = (Close - lag(Close)) / lag(Close),
                ret_open = (Open - lag(Open)) / lag(Open),
                ret_high = (High - lag(High)) / lag(High),
                ret_low = (Low - lag(Low)) / lag(Low)
        ) %>%
        ungroup()

gold_returns <- na.omit(gold_returns)

# Step 3: Average returns across symbols per date for each price type
overall_returns <- gold_returns %>%
        group_by(Date) %>%
        summarise(
                avg_ret_close = mean(ret_close, na.rm = TRUE),
                avg_ret_open = mean(ret_open, na.rm = TRUE),
                avg_ret_high = mean(ret_high, na.rm = TRUE),
                avg_ret_low = mean(ret_low, na.rm = TRUE),
                .groups = "drop"
        )

# Step 4: Compute index levels for each price type (starting from the average on the first date with data)
first_date <- min(overall_returns$Date)
first_avg_open <- mean(gold_data$Open[gold_data$Date == first_date], na.rm = TRUE)
first_avg_high <- mean(gold_data$High[gold_data$Date == first_date], na.rm = TRUE)
first_avg_low <- mean(gold_data$Low[gold_data$Date == first_date], na.rm = TRUE)
first_avg_close <- mean(gold_data$Close[gold_data$Date == first_date], na.rm = TRUE)

overall_returns <- overall_returns %>%
        mutate(
                index_open = first_avg_open * cumprod(c(1, 1 + avg_ret_open[-1])),
                index_high = first_avg_high * cumprod(c(1, 1 + avg_ret_high[-1])),
                index_low = first_avg_low * cumprod(c(1, 1 + avg_ret_low[-1])),
                index_close = first_avg_close * cumprod(c(1, 1 + avg_ret_close[-1]))
        )

# Step 5: Create a mapping dataframe for index values by date
index_map <- data.frame(
        Date = overall_returns$Date,
        JSE_GOLD_Open = overall_returns$index_open,
        JSE_GOLD_High = overall_returns$index_high,
        JSE_GOLD_Low = overall_returns$index_low,
        JSE_GOLD_Close = overall_returns$index_close,
        stringsAsFactors = FALSE
)

# Step 6: Overwrite JSEdat by merging the JSE-GOLD values into Open, High, Low, Close for Name == "JSE-GOLD"
JSEdat <- JSEdat %>%
        left_join(index_map, by = "Date") %>%
        mutate(
                Open = ifelse(Name == "JSE-GOLD", JSE_GOLD_Open, Open),
                High = ifelse(Name == "JSE-GOLD", JSE_GOLD_High, High),
                Low = ifelse(Name == "JSE-GOLD", JSE_GOLD_Low, Low),
                Close = ifelse(Name == "JSE-GOLD", JSE_GOLD_Close, Close)
        ) %>%
        select(-JSE_GOLD_Open, -JSE_GOLD_High, -JSE_GOLD_Low, -JSE_GOLD_Close)

# View structure to confirm (should still have 7 columns)
str(JSEdat)

# Optional: Inspect a sample for JSE-GOLD rows
#tail(JSEdat[JSEdat$Name == "JSE-GOLD", ], 100)

## Create Index for PLAT
#########

PLAT <- c("RBPLAT", "IMPLATS", "NORTHAM", "SIBANYE-S", "THARISA")

# Step 1: Filter data for the symbols
plat_data <- JSEdat[JSEdat$Name %in% PLAT, ]

# Step 2: Compute daily returns for Open, High, Low, Close for each instrument
plat_returns <- plat_data %>%
        group_by(Name) %>%
        arrange(Date) %>%
        mutate(
                ret_close = (Close - lag(Close)) / lag(Close),
                ret_open = (Open - lag(Open)) / lag(Open),
                ret_high = (High - lag(High)) / lag(High),
                ret_low = (Low - lag(Low)) / lag(Low)
        ) %>%
        ungroup()

plat_returns <- na.omit(plat_returns)

# Step 3: Average returns across symbols per date for each price type
overall_returns <- plat_returns %>%
        group_by(Date) %>%
        summarise(
                avg_ret_close = mean(ret_close, na.rm = TRUE),
                avg_ret_open = mean(ret_open, na.rm = TRUE),
                avg_ret_high = mean(ret_high, na.rm = TRUE),
                avg_ret_low = mean(ret_low, na.rm = TRUE),
                .groups = "drop"
        )

# Step 4: Compute index levels for each price type (starting from the average on the first date with data)
first_date <- min(overall_returns$Date)
first_avg_open <- mean(plat_data$Open[plat_data$Date == first_date], na.rm = TRUE)
first_avg_high <- mean(plat_data$High[plat_data$Date == first_date], na.rm = TRUE)
first_avg_low <- mean(plat_data$Low[plat_data$Date == first_date], na.rm = TRUE)
first_avg_close <- mean(plat_data$Close[plat_data$Date == first_date], na.rm = TRUE)

overall_returns <- overall_returns %>%
        mutate(
                index_open = first_avg_open * cumprod(c(1, 1 + avg_ret_open[-1])),
                index_high = first_avg_high * cumprod(c(1, 1 + avg_ret_high[-1])),
                index_low = first_avg_low * cumprod(c(1, 1 + avg_ret_low[-1])),
                index_close = first_avg_close * cumprod(c(1, 1 + avg_ret_close[-1]))
        )

# Step 5: Create a mapping dataframe for index values by date
index_map <- data.frame(
        Date = overall_returns$Date,
        JSE_PLAT_Open = overall_returns$index_open,
        JSE_PLAT_High = overall_returns$index_high,
        JSE_PLAT_Low = overall_returns$index_low,
        JSE_PLAT_Close = overall_returns$index_close,
        stringsAsFactors = FALSE
)

# Step 6: Add new rows for "JSE-PLAT" to JSEdat with index values, preserving other columns as NA
plat_dates <- data.frame(
        Name = "JSE-PLAT",
        Date = overall_returns$Date,
        Open = index_map$JSE_PLAT_Open,
        High = index_map$JSE_PLAT_High,
        Low = index_map$JSE_PLAT_Low,
        Close = index_map$JSE_PLAT_Close,
        Volume = NA_real_,
        stringsAsFactors = FALSE
)

# Append the new rows to JSEdat
JSEdat <- rbind(JSEdat, plat_dates)

# Ensure Date is Date class if needed
JSEdat$Date <- as.Date(JSEdat$Date)



# Optional: Inspect a sample for JSE-PLAT rows
tail(JSEdat[JSEdat$Name == "JSE-PLAT", ], 100)




max(sort(JSEloaded$Date))
## --------*******************************************************************************--------
lst <- lists$zar
RSC(lst,"Zar","Weekly",to_console = FALSE)
lst <- lists$Indexes
dflst <- filter(JSEdat, JSEdat$Name %in% lst) %>% arrange(Date)
RSC(lst,"Indexes","Daily",to_console = FALSE)  # generate rsc heatmap
fnAboveMA(dflst)                                # generate heatmap of inst above 21 and 50 dma
to_graph_ind <- to_graph
jl <- unique(c(lists$RESI,lists$TELE, "GOLD-R",  lists$GOLD, "SILV-R","GRINDROD", "PLAT-R","C-ETHZAR","C-BTCZAR"))
RSC(jl,"TopList","Weekly",to_console = FALSE)
to_graph_top <- to_graph

sat <- unique(JSEdat[grep(c("SATRIX"),JSEdat$Name,"STXFIN"),1])  
RSC(sat,"Satrix","Weekly",to_console = FALSE)
to_graph_sat <- to_graph
lst <- unique(c("GOLD-R","SILV-R", "PLAT-R","C-ETHZAR","C-BTCZAR","JSE-GOLD","JSE-TELE","JSE-TECH","C-ZARUSD",JSEdat[grep(c("SATRIX"),JSEdat$Name),1]))
StrengthSec(Indexes,"Daily")
RSC(to_graph_rsc,"RSC StrengthSectors","Daily")
StrengthSec(Indexes,"Weekly")
RSC(to_graph_rsc,"RSC StrengthSectors","Weekly")
fnOppo() 
fnDyGraph(to_graph_ind)
fnDyGraph(to_graph_top)
fnDyGraph(oppo_instruments)
fnDyGraph(to_graph_sat)
## --------*******************************************************************************--------



sat <- unique(JSEdat[grep(c("SATRIX"),JSEdat$Name),1])
maxdt <- max(sort(JSEloaded$Date))
mydat <- JSEdat %>% filter(Name %in% sat, Date == max(Date), Volume > 5000) %>% group_by(Name) %>% 
        filter(n() > 500)               # Return more than 500 records

sat <- c(mydat[,1],"C-BTCUSD","GOLD-R","C-ETHUSD")





fnCopy = function(x,sep="\t",col.names=T,...) { 
        write.table(x
                    ,file = pipe("pbcopy")
                    ,sep=sep
                    ,col.names = col.names
                    ,row.names = F
                    ,quote = F,...)
}

my_data <- read.table(pipe("pbpaste"), sep="\t", quote = "",fill = FALSE,header = TRUE)



##
##   Create structures to convert prices to ZAR - ETH, BTC
##

EthExtract  <- JSEdat[grep("C-ETHUSD",JSEdat$Name),c(1,2,6)]
BtcExtract <- JSEdat[grep("C-BTCUSD",JSEdat$Name),c(1,2,6)]
UsdZarExtract <- JSEdat[grep("C-USDZAR",JSEdat$Name),c(1,2,6)]
EthEMod <- cbind(EthExtract,UsdZarExtract,2)
me <- merge(BtcExtract,UsdZarExtract, by = c("Date","Date"))
me$Close.x <- as.numeric(as.character(me$Close.x))   
me$Close.y <- as.numeric(as.character(me$Close.y))
me$BTCZAR <- me$Close.x *  me$Close.y
me$Name <- "BTCZAR"
colnames(me)[6] <- "Close"
me <- me[,c(-2,-3,-4,-5)]
#me$Close <- as.factor(me$Close)
me$Name < - as.factor(me$Name)
me$Open <- NA
me$High <- NA
me$Low <- NA
me$Volume <- NA
JSEdat <- rbind(JSEdat,me, fill = NA)


################ Create list of latest instruments
## Filter JSdat to get all prices for those instrument
## XTS the set, 
## Get ROC, Daily, Weekly Monthly
latest_dt <- max(sort(JSEloaded$Date))
latest_set <- JSEdat[JSEdat$Date = latest_dt,]
latest_set <- filter(JSEdat, JSEdat$Date == max(sort(JSEloaded$Date)))
l_set <- latest_set[,1]
# Filter JSE insts
jse_insts <- l_set[1182:1505]
#l_set_extract <- filter(JSEdat, JSEdat$Name %in% l_set)
l_set_extract <- na.omit(l_set_extract)
# reduce to only JSE instruments
# Filter JSE insts
jse_insts <- l_set[1182:1505]
x_extract <- filter(l_set_extract, l_set_extract$Name %in% jse_insts)
## Using summarize daily
x_calc <- x_extract %>% 
        arrange(Name,Date) %>% group_by(Name) %>% 
                                  mutate(pct_change = ((Close/lag(Close) - 1) * 100))
# Extract max date data to send to table
x_calc_d <- x_calc[x_calc$Date == latest_dt,]        

## Using summarize weekly
x_extract$by_week <- cut(x_extract$Date, breaks = "weeks") 
x_calc_w <- x_extract %>% 
        arrange(Name,Date) %>% group_by(Name,by_week) %>% 
        mutate(w_pct_change = ((Close/lag(Close) - 1) * 100))
# Extract max date data
x_calc_d <- x_calc[x_calc$Date == latest_dt,]        

# Datatable
datatable(x_calc_d, 
          filter = "top", 
          rownames = FALSE,
          options = list(
                  pageLength = 50, 
                  autoWidth = TRUE))


## XTS
x_extract <- xts(l_set_extract[,c(1,6)], order.by = l_set_extract[,2])
# Remove the NA values from the Close column
x_extract <- na.omit(x_extract)
x_extract <- x_extract[!is.na(as.numeric(x_extract$Close)),]




# Split by instrument
instruments <- split(x_extract, make.names(x_extract$Name))

names(instruments) <- make.names(names(instruments))
# Loop through instruments
roc_results <- lapply(instruments, function(xts) {
        close <- as.numeric(xts$Close)
        roc <- ROC(close, n=1, type="discrete") 
        xts.ROC <- roc
        return(xts)
})

# Initialize empty list
roc_cols <- vector("list", length(roc_results)) 

for(i in seq_along(roc_results)) {
        roc_cols[[i]] <- roc_results[[i]]$ROC
}

# Bind ROC columns 
x_extract$ROC <- do.call(c, roc_cols)

# Bind xts
x_extract <- do.call(rbind, roc_results)





# Add ROC as new column 
x_extract$ROC <- do.call(c, roc_results)

# Convert to data frame
df <- data.frame(Date = index(x_extract), coredata(x_extract))

# Rearrange columns
df <- df[,c("Date", "Name", "ROC")]

head(df)


# Bind to orig data
x_extract$d_roc <- do.call(c, roc_results)

xts_copy <- x_extract
xts_copy$Close <- as.numeric(xts_copy$Close)
close_vals <- as.numeric(xts_copy$Close)
xts_copy$d_roc <- ROC(xts_copy$Close, n=1, type="discrete")

x_extract$d_roc <- ROC(numeric_df$Close, n=1, type="discrete")


x_extract <- x_extract[complete.cases(x_extract),]
bad_rows <- !is.numeric(x_extract$Close)
x_extract[bad_rows, "Name"] <- NA 


x_extract$Nm2 <- x_extract$Name
x_extract$Cl2 <- as.numeric(x_extract$Close)
x_extract <- x_extract[,-c(1,2)]
x_extract$d_roc <- ROC(x_extract$Cl2, n=1, type="discrete")
x_extract$close_vals <- as.numeric(x_extract$Cl2)

roc <- round(ROC(x_extract$Close, n = 1, type = "discrete"))



x_extract$Close <- as.numeric(x_extract$Close)

# Calculate the ROC
x_extract <- x_extract[!is.na(as.numeric(x_extract$Close)),]
x_extract$d_roc <- ROC(x_extract$Close, n=1, type="discrete")



x_extract <- x_extract[!is.na(as.numeric(x_extract$close_vals)),]


x_extract$d_roc <- ROC(x_extract$Close, n = 1, type = "discrete")

x_extract$d_roc <- ROC(x_extract$Close, n = 1, type = "discrete")
#############

