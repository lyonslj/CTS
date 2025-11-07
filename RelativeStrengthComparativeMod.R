RSC <- function(lst,  label = "", freq = "daily", to_console = FALSE, data = JSEdat) {
        
##      --      Pass to this function a list of Instruments and it will produce a pdf heatmap        
library(dplyr)
library(tidyr)
library(quantmod)
library(reshape2)
source('~/Documents/Personal/DataScience/R/JL CTS scripts/RscHeatmap.R')

label <- gsub(" ", "", label)                
fnJunkInstrm <- function(variables) {}

##      --      New workings to simplify
##      --      Use  %in% -- lot faster 
         mydat <- filter(data, data$Name %in% lst) %>% arrange(Date)
         
         ## frequency
         if(tolower(freq) == "daily") {
                mydat <- tail(mydat,4000)   ## daily calc
                 } else {                   ## weekly calc based on last day loaded
                        ldl <- weekdays(max((mydat$Date)))      # Convert Weekly on last weekday data loaded
                        mydat <- mydat[weekdays(mydat$Date) == ldl, ] %>% arrange(Date) 
                 }        
        
         
## -----------------  Create XTS data ------------------- ##           
        mydat$Close <- as.numeric(mydat$Close)
        mydat_select <- dplyr::select(mydat,Name,Date,Close)            # select certain columns
        y <- aggregate(Close ~ Date + Name, mydat_select, mean)         # get rid of duplicates
        y_filtered <- y %>%
                group_by(Name) %>%
                filter(n() >= 89) %>%
                ungroup()                                               # ensure minimum 89 records
        y_reworked <- spread(y_filtered,Name,Close)                     # spread Names to Rows
        y_reworked <- as.data.frame(na.omit(y_reworked))                               # get rid of NA values
        cols <- ncol(y_reworked)                               
        y_xts <- xts(y_reworked[,2:cols],
                     order.by = y_reworked[,1],check.names = TRUE)      #xts
        y_xts <- tail(y_xts,200)                                        ## restrict to 200 entries
        cols_xts <- ncol(y_xts)

## ---------------- Create RSC pairs and calculate RSC ratios ----------------- ##
for(z in (1:(cols_xts-1))) {
        for(i in z:(cols_xts-1)){                                                       
                y_xts$new_col_name <- y_xts[,z]/y_xts[,i+1]             # calculate pair ratio z fixed 1, i 1:15 # z 2, i 2:15
                new_col_name <- paste(names(y_xts[,z]),
                                      names(y_xts[,i+1]), sep="_",collapse=".")    #
                names(y_xts)[ncol(y_xts)] <- new_col_name               # get last column place and name it
        }
        z <- z+1
}
## ------------- Cleanup and convert back to df -----------------##
        rsc_set <- y_xts[,-1:-cols_xts]                                                 # remove base set to leave rsc pairs
        rsc_set_omit <- na.omit(rsc_set)                                                # remove N/A's
        df.rsc_set <- data.frame(Date = index(rsc_set_omit),rsc_set_omit)               # convert from xts back to df
        

## -------------- Calc and add columns for rsc 11 and 89 ma's ----------------##
        set <- names(df.rsc_set)                                                        # column names to use 
        set1 <- set[-1]                                                                 # exclude the Date column
        df.rsc_calc_set <- NULL                                                         # Initialise

# ----------------Loop through set1--------------------#



for (i in set1) {
        
        # Construct the new column SMA11
        pairma11 <- paste(i, "ma11", sep = "_")
        # Calculate the 11-period Simple Moving Average and assign it directly
        df.rsc_set[[pairma11]] <- SMA(df.rsc_set[[i]], n = 11)
        
        # Construct the new column SMA89
        pairma89 <- paste(i, "ma89", sep = "_")
        # Calculate the 89-period Simple Moving Average and assign it directly
        df.rsc_set[[pairma89]] <- SMA(df.rsc_set[[i]], n = 89)
        
        # Directly access the columns using their names
        pair <- df.rsc_set[[i]]                                         # The instrument
        pair.ma11 <- df.rsc_set[[pairma11]]                             # 11SMA
        pair.ma89 <- df.rsc_set[[pairma89]]                             # 89SMA
        
        ### Extract pairs to new set df.rsc_set and rbind together ###
        xx <- df.rsc_set[,c("Date",i,pairma11,pairma89)]
        xx <- cbind(i,xx)
        names(xx) <- c("Pair","Date","Value","Value11","Value89")
        
        # rbind Join the new pair to df.rsc_calc_set
        df.rsc_calc_set <- rbind(df.rsc_calc_set,xx)
}

        df.rsc_calc_set <- na.omit(df.rsc_calc_set)                     # Get rid NA's

# ---------------------- Calculate values based on strength -------------------------------------
        
        df.rsc_calc_set$Result<-ifelse((df.rsc_calc_set$Value > df.rsc_calc_set$Value11) & 
                                       (df.rsc_calc_set$Value11 > df.rsc_calc_set$Value89),2,     
                                        ifelse((df.rsc_calc_set$Value < df.rsc_calc_set$Value11) & 
                                                       (df.rsc_calc_set$Value11 < df.rsc_calc_set$Value89), -2,1))

        df.new <- separate(df.rsc_calc_set, Pair, 
                           into = c("Base","Quoted"), sep = "_")        # Split pair into base and quoted instrm
        
# ----------------------------- Create compute frame ---------------------------------------#
        
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
        z <- xts(FinalAggregateSpread[,-1],order.by = FinalAggregateSpread[,1])                 # convert to xts
        mypath <- paste(file.path("/Users/johnlyons/Documents/Personal/DataScience/R/RPlots/"),"
                        .csv",sep="")
        df.z <- tail(data.frame(z),40)  
        colnames(df.z) <- gsub("\\.","-",names(df.z))
        df.z <- t(df.z)
        df.z <- df.z[order(df.z[,ncol(df.z)]),]
        
        
        ## -------- Returned to calling function eg to do a DyGraph
        to_graph <<- rownames(head(df.z[order(-df.z[,ncol(df.z)]),],20))                        ## Top20
        
        ##  --------------------   Create heatmap -------------------
        
        RscHeatmap(df.z, label, freq, to_console)                
        return(to_graph)
}


