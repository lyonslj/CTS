RSC <- function(lst, label = "", to_console = FALSE) {
        
        ##      --      Pass to this function a list of Instruments and it will produce a pdf heatmap        
        library(dplyr)
        library(tidyr)
        library(quantmod)
        library(reshape2)
        source('~/Documents/Personal/DataScience/R/JL CTS scripts/RscHeatmap.R')
        
        label <- gsub(" ", "", label)                
        fnJunkInstrm <- function(variables) {}
        
        ##      --      New inner function to handle frequency-specific calculation
        calculate_rsc <- function(freq_type) {
                
                mydat <- filter(JSEdat, JSEdat$Name %in% lst) %>% arrange(Date)
                
                ## frequency selection logic
                if(tolower(freq_type) == "daily") {
                        mydat <- tail(mydat, 4000)   ## daily calc
                } else if(tolower(freq_type) == "weekly") {
                        ## weekly calc based on last day loaded
                        ldl <- weekdays(max((mydat$Date)))      # Convert Weekly on last weekday data loaded
                        mydat <- mydat[weekdays(mydat$Date) == ldl, ] %>% arrange(Date) 
                } else {
                        stop("Invalid frequency type provided to calculate_rsc.")
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
                # ... (RSC pair calculation logic remains unchanged) ...
                for(z in (1:(cols_xts-1))) {
                        for(i in z:(cols_xts-1)){                                                       
                                y_xts$new_col_name <- y_xts[,z]/y_xts[,i+1]
                                new_col_name <- paste(names(y_xts[,z]),
                                                      names(y_xts[,i+1]), sep="_",collapse=".")
                                names(y_xts)[ncol(y_xts)] <- new_col_name
                        }
                        z <- z+1
                }
                ## ------------- Cleanup and convert back to df -----------------##
                rsc_set <- y_xts[,-1:-cols_xts]
                rsc_set_omit <- na.omit(rsc_set)
                df.rsc_set <- data.frame(Date = index(rsc_set_omit),rsc_set_omit)
                
                
                ## -------------- Calc and add columns for rsc 11 and 89 ma's ----------------##
                set <- names(df.rsc_set)
                set1 <- set[-1]
                df.rsc_calc_set <- NULL
                
                # ----------------Loop through set1--------------------#
                # ... (Moving Average calculation logic remains unchanged) ...
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
                        pair <- df.rsc_set[[i]]
                        pair.ma11 <- df.rsc_set[[pairma11]]
                        pair.ma89 <- df.rsc_set[[pairma89]]
                        
                        ### Extract pairs to new set df.rsc_set and rbind together ###
                        xx <- df.rsc_set[,c("Date",i,pairma11,pairma89)]
                        xx <- cbind(i,xx)
                        names(xx) <- c("Pair","Date","Value","Value11","Value89")
                        
                        # rbind Join the new pair to df.rsc_calc_set
                        df.rsc_calc_set <- rbind(df.rsc_calc_set,xx)
                }
                
                df.rsc_calc_set <- na.omit(df.rsc_calc_set)
                
                # ---------------------- Calculate values based on strength -------------------------------------
                
                df.rsc_calc_set$Result<-ifelse((df.rsc_calc_set$Value > df.rsc_calc_set$Value11) & 
                                                       (df.rsc_calc_set$Value11 > df.rsc_calc_set$Value89),2,     
                                               ifelse((df.rsc_calc_set$Value < df.rsc_calc_set$Value11) & 
                                                              (df.rsc_calc_set$Value11 < df.rsc_calc_set$Value89), -2,1))
                
                df.new <- separate(df.rsc_calc_set, Pair, 
                                   into = c("Base","Quoted"), sep = "_")
                
                # ----------------------------- Create compute frame ---------------------------------------#
                
                BaseFrame <- filter(df.new, df.new$Result == 2) 
                BaseFrameSelect <- select(BaseFrame,Base,Date,Result)
                names(BaseFrameSelect) <- c("Instr","Date","Result")
                QuoteFrame <- filter(df.new, df.new$Result == -2) 
                QuoteFrameSelect <- select(QuoteFrame,Quoted,Date,Result)
                QuoteFrameSelect$Result <- 2
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
                z <- xts(FinalAggregateSpread[,-1],order.by = FinalAggregateSpread[,1])
                df.z <- tail(data.frame(z),40)  
                colnames(df.z) <- gsub("\\.","-",names(df.z))
                df.z <- t(df.z)
                df.z <- df.z[order(df.z[,ncol(df.z)]),]
                
                return(df.z) # Return the calculated RSC matrix
        }
        
        ## -----------------  Calculate Daily and Weekly results ------------------- ##
        daily_result <- calculate_rsc("daily")
        weekly_result <- calculate_rsc("weekly")
        
        ## -------- Returned to calling function eg to do a DyGraph (Use Daily for top 20)
        to_graph <<- rownames(head(daily_result[order(-daily_result[,ncol(daily_result)]),],20))                        ## Top20
        
        ##  --------------------   Create heatmap -------------------
        
        # Pass both results to the modified RscHeatmap
        RscHeatmap(daily_result, weekly_result, label, to_console)                
        return(to_graph)
}
