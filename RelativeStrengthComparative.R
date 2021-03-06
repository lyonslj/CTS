RSC <- function(lst, label = "") {
        
##      --      Pass to this function a list of Instruments and it will produce a heatmap        
library(dplyr)
library(tidyr)
library(quantmod)
library(reshape2)
source('~/Documents/Personal/DataScience/R/JL CTS scripts/RscHeatmap.R')
        
fnJunkInstrm <- function(variables) {

}

##      -- New workings to simplify
##      --      Use  filter for exact matches
         mydat <- filter(JSEdat, JSEdat$Name %in% lst)
         mydat <- mydat[mydat$Date >= Sys.Date()-300,]

mydat_select <- dplyr::select(mydat,Name,Date,Close)            # select certain columns
y <- aggregate(Close ~ Date + Name, mydat_select, mean)  # get rid of duplicates
y_reworked <- spread(y,Name,Close)                       # spread Names to Rows
#y_reworked <- y_reworked[ , apply(y_reworked, 2, function(x) !any(is.na(x)))]
y_reworked <- na.omit(y_reworked)                        # get rid of NA values
cols <- ncol(y_reworked)                               
y_xts <- xts(y_reworked[,2:cols],order.by = y_reworked[,1],check.names = TRUE) #xts
y_xts <- tail(y_xts,200)        ## restrict to 200 entries
cols_xts <- ncol(y_xts)
### Calculate RSC ratios for all column pairs ###
for(z in (1:(cols_xts-1))) {
        for(i in z:(cols_xts-1)){                                                       
                y_xts$new_col_name <- y_xts[,z]/y_xts[,i+1]                             # calculate pair ratio z fixed 1, i 1:15 # z 2, i 2:15
                new_col_name <- paste(names(y_xts[,z]),names(y_xts[,i+1]), sep="_",collapse=".")    #
                names(y_xts)[ncol(y_xts)] <- new_col_name                              # get last column place and name it
        }
        z <- z+1
}
#names(y_xts)
rsc_set <- y_xts[,-1:-cols_xts]                                                         # remove base set to leave rsc pairs
rsc_set_omit <- na.omit(rsc_set)                                                        # remove N/A's
df.rsc_set <- data.frame(Date = index(rsc_set_omit),rsc_set_omit)                       # convert from xts back to df

### --------------------------------------------###
### Calc and add columns for rsc 11 and 89 ma's ###
### --------------------------------------------###
set <- names(df.rsc_set)                                                                # column names to use 105
set1 <- set[-1]                                                                         # exclude the Date column
### Initialise your final data frame set ###
df.rsc_calc_set <- data.frame(NULL)                                             # this can be problematic with the rbind
for(i in set1) {
        ### 11 mda ###
        pairma11 <- paste(i,"ma11",sep="_")
        tt <- as.name(paste("SMA(df.rsc_set$",i,",n=11)",sep="",collapse = ""))         # paste string for SMA statement ie SMA(column,11)
        df.rsc_set$pair <- eval(parse(text = tt))                                       # create new column by parsing string as reg expression
        col <- length(names(df.rsc_set))
        names(df.rsc_set)[col] <- pairma11                                              # rename column
        ### 89 mda ###
        pairma89 <- paste(i,"ma89",sep="_")
        tt <- as.name(paste("SMA(df.rsc_set$",i,",n=89)",sep="",collapse = ""))         # paste string for SMA statement
        df.rsc_set$pair <- eval(parse(text = tt))                                       # create new column by parsing string as reg expression
        col <- length(names(df.rsc_set))
        names(df.rsc_set)[col] <- pairma89
        ### working set ###
        a <- paste("df.rsc_set$",i,sep="",collapse = "")   
        b <- paste("df.rsc_set$",pairma11,sep="",collapse = "")
        c <- paste("df.rsc_set$",pairma89,sep="",collapse = "")   
        pair <- eval(parse(text = a))
        pair.ma11 <- eval(parse(text = b))
        pair.ma89 <- eval(parse(text = c))
        ### Extract pairs to new set df.rsc_set and rbind together ###
        xx <- df.rsc_set[,c("Date",i,pairma11,pairma89)]
        xx <- cbind(i,xx)
        names(xx) <- c("Pair","Date","Value","Value11","Value89")
        df.rsc_calc_set <- rbind(df.rsc_calc_set,xx)
}
df.rsc_calc_set$Result<-ifelse((df.rsc_calc_set$Value > df.rsc_calc_set$Value11) & (df.rsc_calc_set$Value11 > df.rsc_calc_set$Value89),2,     
                               ifelse((df.rsc_calc_set$Value < df.rsc_calc_set$Value11) & (df.rsc_calc_set$Value11 < df.rsc_calc_set$Value89), -2,1))
df.new <- separate(df.rsc_calc_set, Pair, into = c("Base","Quoted"), sep = "_")                         # Split pair into base and quoted instrm
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
mypath <- paste(file.path("/Users/johnlyons/Documents/Personal/DataScience/R/RPlots/"),"
                .csv",sep="")
df.z <- tail(data.frame(z),30)  
colnames(df.z) <- gsub("\\.","-",names(df.z))
df.z <- df.z[complete.cases(df.z),]
df.z <- t(df.z)
df.z <- df.z[order(df.z[,ncol(df.z)]),]



to_graph <<- rownames(head(df.z[order(-df.z[,ncol(df.z)]),],20))  ##Longs

RscHeatmap(df.z, label)                #Create heatmap
return(to_graph)

#write.csv(df.z,mypath)
}
