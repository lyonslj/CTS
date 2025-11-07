fnCryptsPL <- function() {
        library(readxl)
        library(dplyr)
        library(ggplot2)
        library(scales)
        library(knitr)
        library(kableExtra)
        library(rvest)
        library(openxlsx)
        library(dygraphs)
        
        #############################################################################
        ##                              Read trades data                           ##
        #############################################################################
        Trades <- read_excel("Export complete order history.xlsx")
        cryptsDat <- Trades[!is.na(Trades$status) & Trades$status != "Canceled",]
        fieldsToConvert <- c("Order Price", "Order Amount", "AvgTrading Price", "Filled", "Total")
        cryptsDat[fieldsToConvert] <- sapply(cryptsDat[fieldsToConvert],as.numeric)
        
        #############################################################################
        ##                              Get current prices                         ##
        #############################################################################
        
        
        data <- read_html("https://www.coingecko.com/en")
        ## Identify all tables
        tbls <- html_nodes(data,"table") 
        ## Extract data from the table you want
        dat <-  html_table(tbls,header = NA,"gecko-table") 
        
        ## View the data
        crypts_dat <- dat[[1]] 
        
        ## Structure the data
        crypts <- crypts_dat[,c(3,4,8,9)]
        colnames(crypts) <- c("Coin","Price_USD","24hVol","MarketCap")
        
        crypts <- separate(crypts, Coin, into = c("Coin1","Coin2","Coin3","Coin4","Coin5"))
        
        crypts$len <- nchar(substr(crypts$Coin5,1,1))
        
        crypts$Coin5 <- ifelse(
                nchar(substr(crypts$Coin5,1,1)) == 0,
                                     crypts$Coin4, crypts$Coin5)
                
        
        crypts <- crypts[,c(2,5,6,7,8)]
        
        ## Get rid comma and Convert to numeric
        crypts <- apply(crypts, 2, function(y) as.character(gsub(",", "", y)))
        crypts <- apply(crypts, 2, function(y) as.character(gsub("\\$", "", y)))
        crypts <- data.frame(crypts)
        colnames(crypts) <- c("Coin","Symbol","Price_USD","24hVol","MarketCap")
        ## Convert to numeric
        fields <- c("Price_USD","24hVol","MarketCap")
        options(scipen = 999) # disable scientific notation
        crypts[fields] <- sapply(crypts[fields],as.character)
        crypts[fields] <- sapply(crypts[fields],as.numeric)
        ## Add dt column
        crypts$Dt <- Sys.Date()
        btc_price <- crypts[crypts$Symbol == "BTC","Price_USD"]
        crypts <- mutate(crypts,Price_BTC = Price_USD / btc_price)
        ## Return price in ETH
        eth_price <- crypts[crypts$Symbol == "ETH","Price_USD"]
        crypts <- mutate(crypts,Price_ETH = Price_USD / eth_price)
        crypts <- crypts[,c(2,3,6,7,8)]
        crypts_p <- gather(crypts,Basis,Price, -c(Symbol,Dt))
        crypts_p$Basis <- gsub("Price_USD","USDT",crypts_p$Basis)
        crypts_p$Basis <- gsub("Price_BTC","BTC",crypts_p$Basis)
        crypts_p$Basis <- gsub("Price_ETH","ETH",crypts_p$Basis)
        crypts_p$Symbol <- paste(crypts_p$Symbol,crypts_p$Basis,sep="")
        
        #############################################################################
        ##                              Join trades to current prices              ##
        #############################################################################
        
        cryptsTrades <- merge(cryptsDat, crypts_p[,c(1,4)],
                              by.x = "Pair", by.y = "Symbol", all.x = TRUE
                                      )
        
        ##      --      Some cleanup
        cryptsTrades$DtTime <- cryptsTrades$`Date(UTC)`
        cryptsTrades <- separate(cryptsTrades, `Date(UTC)`, into = c("Dt"), sep = " ")
        cryptsTrades$Dt <-  as.Date(cryptsTrades$Dt, origin = "1899-12-30", format = "%Y-%m-%d")
        cryptsTrades <- cryptsTrades[cryptsTrades$Dt >= "2020-09-08",]

        #############################################################################
        ##                              Start Netting off trades                   ##
        #############################################################################
        
        
        cryptsTrades$`Order Amount` <- ifelse(grepl("SELL",cryptsTrades$Type),
                                              -1*cryptsTrades$`Order Amount`,
                                              cryptsTrades$`Order Amount`
                                              )
        
        cryptsTrades <- cryptsTrades %>%
                arrange(DtTime) %>%
                group_by(Pair) %>% 
                mutate(QuantityCum = cumsum(`Order Amount`),
                       PriorPurchP = lag(`Order Price`),
                       OpenClose = ifelse(lead(Type == "SELL"),"Close","Open")
                       ) %>%
                arrange(Pair,DtTime)
        
        
        #############################################################################
        
        cryptsOpenTrades <- cryptsTrades[!grepl("Close",cryptsTrades$OpenClose),]
        cryptsOT <- cryptsOpenTrades %>%
                mutate(PL = ifelse(Type == "SELL", 
                                   (Price - PriorPurchP)*QuantityCum,
                                   (Price-`Order Price`)*Filled),
                       StopPL = ifelse(Type == "BUY",
                                        abs(0.1*`AvgTrading Price`*`Order Amount`),
                                        # adjust Risk for Q sold
                                        abs(0.1*`AvgTrading Price`*`Order Amount`) *QuantityCum / Filled)) %>%
                arrange(PL)
        
        cryptsOT$Pair <- paste(cryptsOT$Pair,rownames(cryptsOT),sep= " ")
       
        
         riskp <- ggplot(data=cryptsOT, aes(y=Risk, x=Pair)) +
               # geom_bar(stat="identity",aes(fill="red")) +
                #geom_text(aes(x=Name,y=Risk),label=round(z$Risk,0), size=3, color= "black", nudge_x = 0.2) +
                ##      --      unrealised profit
                geom_bar(stat="identity",aes(y=PL, x=Pair), 
                         fill="green", alpha = 0.6) +
                geom_text(aes(x=Pair,y=0),label=round(cryptsOT$PL,0), size=2.5, 
                          color= ifelse(cryptsOT$PL >0, "black", "red"),
                                   nudge_x = -0.2, nudge_y = 50) +
                ##      --      profit with stops & %age move to stop
                ##geom_bar(stat="identity",aes(y=-StopPL, x=Pair), fill="purple", alpha = 0.6) +
                ##geom_text(aes(x=Name,y=0),label=round(z$StopPL,0), 
                ##          size=2.5, color= "black", nudge_y = -1400, nudge_x = 0.3) +
                ##geom_text(aes(x=Name,y=0),label=z$MoveToStop,
                ##          size=2.5, color= "black", nudge_y = -1500) +
                #geom_text(aes(x=Name,y=0),label= z$StopLoss,
                #          size=2.5, color= "black", nudge_x = -0.5) +
                ##      --      exposure
                ##geom_bar(stat="identity",aes(y=exposure/2, x=Name), fill="lightgrey", alpha = 0.4) +
                ##geom_text(aes(x=Name,y=exposure/2),label=round(z$exposure,0), size=2.5, color= "black") +
                theme_light() +
                theme(strip.text.y = element_text(angle=0),
                      axis.text = element_text( size = 8 ),
                      legend.position = "none") +
                #geom_hline(yintercept = 5300, color = "darkgreen", linetype = "dotdash") +
                geom_hline(yintercept = -220, color = "red", linetype = "dotdash", size=0.6) +
                geom_hline(yintercept = 0, color = "black", size=0.5) +
                #axis.text.x = element_text(angle=90),
                #axis.text.y=element_blank()) +
                labs(x=NULL,y=NULL, title = "Risk Profile",
                     subtitle = paste("StopPL", round(sum(na.omit(cryptsOT$PL))),sep = " "))  +
                coord_flip()
         riskp
         
         
         #######################################
        
         
         z_worked <- separate(z, Name, into = c("jnk","Name"))
         
         ws <- z_worked %>% 
                # filter(pl > 0) %>%
                 group_by(Name) %>%
                 summarise(Profit = sum(pl),
                           Risk = sum(Risk),
                           Exposure = sum(exposure)) %>%
                 arrange(desc(Profit))
         
         ws$Profit <- round(ws$Profit)
         ws$P_age <- paste(round((ws$Profit / sum(ws$Profit)*100),1),"%",sep="") 
         ws$R_age <- paste(round((ws$Risk / sum(ws$Risk)*100),1),"%",sep="") 
         ws$E_age <- paste(round((ws$Exposure / sum(ws$Exposure)*100),1),"%",sep="")
         ws <- ws[,c("Name", "Profit", "P_age", "Risk", "R_age","Exposure","E_age")]
         
         tab <- kable(ws, caption = "Profit - Risk - Exposure") %>%
                 kable_styling(bootstrap_options = "striped", font_size = 10) 
         
         tab
         #######################################
        to_ret <- list(riskp, tab)
         return(to_ret)
        
        
        
        #*********************************************
        # Build xts PL sets of old Trds*
        #*********************************************
        Trades$`Exit Dt` <- as.Date(as.numeric(Trades$`Exit Dt`), origin = "1899-12-30", format = "%Y-%m-%d")
        Trades$PL <- round((Trades$Exit - Trades$EntryPrice)*Trades$Quantity/100,0)
        
        
        TradesOld <- Trades[Trades$Exit > 0 & !is.na(Trades$`Exit Dt`) & Trades$`Exit Dt` >= "2019-01-01" , ]
        TradesOld <- TradesOld[order(TradesOld$`Exit Dt`),]
        TradesOld$ExitYr <- format(TradesOld$`Exit Dt`, "%Y")
        TradesOld$ExitYr <- factor(TradesOld$ExitYr)
        #TradesOld$ExitYr <- as.integer(TradesOld$ExitYr)
        TradesOld$ExitMnDy <- format(TradesOld$`Exit Dt`,"%d-%b")
        TradesOld$ExitMn <- months(TradesOld$`Exit Dt`)
        TradesOld$ExitMn <- factor(TradesOld$ExitMn, levels = unique(TradesOld$ExitMn))
        TradesOld <- group_by(TradesOld,ExitYr) %>% 
                mutate(YearlyCum = cumsum(PL),
                       YearlyCumAvg = cummean(PL))
        
        
        
       # TradesOld$CumTot <- cumsum(TradesOld$PL)
        #TradesOld$CumAvg <- cummean(TradesOld$PL)
        TradesOld$MnthDt <- as.Date(TradesOld$`Exit Dt`, "%Y-%m-%d")
        TradesOld$MnthDt <- format(TradesOld$`Exit Dt`, "%b-%Y")
        TradesOld$CumRR <- dplyr::cummean(TradesOld$RR)
       # TradesOld$CumTrades <- dplyr::cummean(cum(TradesOld$Instrm))
        
                #
        # Graph historical Trades
        #
        
        
        
        trades2020 <- TradesOld[TradesOld$ExitYr == "2020",]
        winners <- length(which(trades2020$PL>0))
        losers <- length(which(trades2020$PL<0))
        win_loose <- round((winners*100)/(winners+losers),1)
        
        TradesG <- ggplot(data=trades2020, aes(x=`Exit Dt`, y=PL)) +
                geom_bar(stat="identity",aes(fill=Instrm),size=5) +
                geom_line(aes(x=`Exit Dt`, y=YearlyCumAvg),color="red") +
                facet_wrap(ExitYr ~ ., scales = "free_x") +
                theme_light() +
                theme(strip.text.y = element_text(angle=90),
                      axis.text.x = element_text( size = 8 , angle = 90),
                      legend.position = "none") +
                geom_hline(yintercept = 0, color = "black") +
                #axis.text.x = element_text(angle=90),
                #axis.text.y=element_text(angle=90) +
                scale_x_date(breaks = trades2020$`Exit Dt`,labels = date_format("%b-%y"), date_breaks = "4 weeks") +
                scale_y_continuous(labels = comma, breaks=seq(-100000, 1000000, 10000)) +
                labs(x=NULL,y=NULL, title = "Trades",
                     subtitle = paste(nrow(trades2020), "Trades", "Avg Return", 
                                      round(mean(trades2020$PL),0), 
                                      "Wins:", winners, "Loss:", losers, "-",win_loose, "%",
                                      sep=" "))  
        TradesG
        
        
        
        
        CumTotG <- ggplot(data=trades2020, aes(x=`Exit Dt`, y=YearlyCum)) +
                geom_line(size = 1, aes(color = format(`Exit Dt`,"%y"))) +
                #geom_line(aes(x=`ExitMnDy`, y=CumAvg),color="purple", size=1) +
                facet_wrap(ExitYr ~ ., scales = "free_x") +
                theme_light() +
                theme(axis.text.x = element_text(angle=90),
                      legend.position = "none") +
                geom_hline(yintercept = 0, color = "black") +
                labs(x=NULL,y=NULL, title = "Trades: Cumulative Total",
                     subtitle = paste(nrow(trades2020), "Trades", "Avg Return", round(mean(trades2020$PL),0), sep=" ")) +
             #   scale_x_date(breaks = TradesOld$ExitMnDy,labels = date_format("%d-%b"), date_breaks = "4 weeks") +
                scale_y_continuous(labels = comma, breaks=seq(-100000, 10000000, 20000))
        
        
        CumTotG
        
        
        #################################################################################
        ##                            Plot R                                           ##
        #################################################################################
        
        dat_period <- TradesOld[TradesOld$ExitYr == "2019",]
        
        RR_G <- ggplot(data=dat_period, aes(x=`Exit Dt`, y=dplyr::cummean(dat_period$RR))) +
              ##  geom_bar(stat="identity",aes(fill=Instrm),size=5) +
                geom_line(aes(x=`Exit Dt`, y=dplyr::cummean(dat_period$RR)),color="red") +
                facet_wrap(ExitYr ~ ., scales = "free_x") +
                theme_light() +
                theme(strip.text.y = element_text(angle=90),
                      axis.text.x = element_text( size = 8 , angle = 90),
                      legend.position = "none") +
                geom_hline(yintercept = 0, color = "black") +
                #axis.text.x = element_text(angle=90),
                #axis.text.y=element_text(angle=90) +
                scale_x_date(breaks = dat_period$`Exit Dt`,labels = date_format("%b-%y"), date_breaks = "4 weeks") +
                scale_y_continuous(breaks=seq(0, 3.00, 0.25)) +
                labs(x=NULL,y=NULL, title = paste("R Ratio - ",round(mean(dat_period$RR),2)),
                     subtitle = paste(nrow(dat_period), "Trades", "R", 
                                      "Wins:", length(which(dat_period$PL>0)), "Loss:", length(which(dat_period$PL<0)), "-",win_loose, "%",
                                      sep=" "))  
        RR_G
        
        #################################################################################
        
        fnJnk <- function(variables) { 
                
        CumTotG <- ggplot(data=TradesOld, aes(x=`Exit Dt`, y=CumTot)) +
                geom_line(color="green",size = 1) +
                geom_line(aes(x=`Exit Dt`, y=CumAvg),color="purple", size=1) +
                #facet_grid(ExitYr ~ .) +
                theme_light() +
                theme(axis.text.x = element_text(angle=90),
                      legend.position = "none") +
                geom_hline(yintercept = 0, color = "black") +
                labs(x=NULL,y=NULL, title = "Trades: Cumulative Total",
                     subtitle = paste(nrow(TradesOld), "Trades", "Avg Return", round(mean(TradesOld$PL),0), sep=" ")) +
                scale_x_date(breaks = TradesOld$`Exit Dt`,labels = date_format("%b-%y"), date_breaks = "4 weeks") +
                scale_y_continuous(labels = comma, breaks=seq(0, 10000000, 25000))
       
         
        CumTotG
        
        #TradesOld$MnthDt <-factor(TradesOld$MnthDt,levels=unique(TradesOld$`Exit Dt`))
        
        
            
    
        
        BoxG <- ggplot(TradesOld) +
                geom_boxplot(aes(format(TradesOld$`Exit Dt`, "%b"),RR), varwidth=T, fill="plum") + 
                theme_light() +
                geom_hline(yintercept = 0, color = "black", size=0.5) +
                geom_hline(yintercept = 1, color = "darkgreen", linetype = "dotdash") +
                geom_hline(yintercept = -1, color = "red", linetype = "dotdash") +
                labs(x=NULL,y=NULL, title = "2019 R",
                     subtitle = paste(nrow(TradesOld), "Trades:", "Avg R", round(mean(TradesOld$R),
                                                                                 0),
                                      "Average RR", round(mean(TradesOld$RR),2),sep=" ")) +
               # scale_x_date(breaks = TradesOld$`Exit Dt`,labels = date_format("%b")) +
                scale_y_continuous(labels = comma, breaks=seq(-2, 5, 1))
                
        BoxG
        
        meanRR <- TradesOld %>%
                group_by(MnthDt) %>%
                dplyr::summarise(no_trades = n(), monthlyRR = mean(RR))
        
        rret <- ggplot(TradesOld) +
                geom_line(aes(`Exit Dt`,CumRR), color = "green") + 
                #geom_point(meanRR, aes(x=MnthDt, y=monthlyRR, size = monthlyRR), color="grey") +
                theme_light() +
                geom_hline(yintercept = 0, color = "black", size=0.5) +
                geom_hline(yintercept = 1, color = "darkgreen", linetype = "dotdash") +
                #geom_hline(yintercept = -1, color = "red", linetype = "dotdash") +
                labs(x=NULL,y=NULL, title = "2019 Trades",
                     subtitle = paste(nrow(TradesOld), "Trades:", 
                                      "Avg R", round(mean(TradesOld$R),0),
                                      "Average RR", round(mean(TradesOld$RR),2),sep=" ")) +
                scale_x_date(breaks = TradesOld$`Exit Dt`,
                             labels = date_format("%b-%y"), date_breaks = "4 weeks") +
                scale_y_continuous(labels = comma, breaks=seq(-2, 5))
        
        rret
        

        
        }
}



