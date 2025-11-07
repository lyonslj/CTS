fnProffitLoss <- function() {
        library(readxl)
        library(dplyr)
        library(ggplot2)
        library(scales)
        library(knitr)
        library(kableExtra)
        
        Trades <- read_excel("/Users/johnlyons/Dropbox/CTS/Trades.xlsx")
        Trades$Purchase <- as.Date(Trades$Purchase)
        Trades$`Exit Dt` <- as.Date(Trades$`Exit Dt`)
        Trades <- as.data.frame(Trades)
        alldata2 <- subset(JSEdat,JSEdat$Date >= "2022-01-01")
        mylst <- subset(Trades,is.na(Trades$Exit)) 
        mylst <- mylst[!is.na(mylst$Instrm),] 
        mylst <- as.data.frame(mylst)
        
        ## 
        
        ##
        LiveP <- NULL
        for(i in 1:nrow(mylst)) {
                y <- subset(alldata2[,c(1,2,6)],alldata2$Name == mylst[i,1]  & alldata2$Date >= mylst[i,2])
                y$pl <- ((y[,3] - mylst[i,3])*mylst[i,5])/100           # change in P * Q held
                y$entry <- mylst[i,3]
                y$Risk <- ((y[,3] - mylst[i,4])*mylst[i,5])/100
                y$Quant <- mylst[i,5]
                y$StopLoss <- mylst[i,4]
                y$PlRisk < y$pl/y$Risk
                LiveP <- rbind(LiveP,y)
        }
        
        z <- LiveP[LiveP$Date == max(LiveP$Date),] 
        z$no <- 1:nrow(z) #add unique identifier so as to to convert instr --> factor
        z$Name <- paste(z$no, z$Name, sep=" ")
        z <- z[order(z$Risk), ]  # order for plotting   
        z$Name <- factor(z$Name, levels = z$Name) # convert to factor for plotting
        z$StopPL <- (z$StopLoss - z$entry)*z$Quant/100
        z$exposure <- z$Quant * z$Close / 100
        
        z$MoveToStop <- (z$Close-z$StopLoss)/z$Close
        z$MoveToStop <- paste(round(z$MoveToStop*100,digits=1),"%",sep="")
        tot_risk <- round(colSums(z[,6, drop=FALSE]),0)
        tot_P <-round(colSums(z[,4, drop=FALSE]),0)
        tot_stopPL <- round(colSums(z[,10, drop=FALSE]),0)
        tot_exp <- round(colSums(abs(z[,11, drop=FALSE])),0)
        
        
        riskp <- ggplot(data=z, aes(y=Risk, x=Name)) +
                # geom_bar(stat="identity",aes(fill="red")) +
                #geom_text(aes(x=Name,y=Risk),label=round(z$Risk,0), size=3, color= "black", nudge_x = 0.2) +
                ##      --      unrealised profit
                geom_bar(stat="identity",aes(y=pl, x=Name), fill="green", alpha = 0.6) +
                geom_text(aes(x=Name,y=pl),label=round(z$pl,0), size=2.5, 
                          color= "black", nudge_x = -0.2, nudge_y = 1100) +
                ##      --      profit with stops & %age move to stop
                geom_bar(stat="identity",aes(y=StopPL, x=Name), fill="purple", alpha = 0.6) +
                geom_text(aes(x=Name,y=0),label=round(z$StopPL,0), 
                          size=2.5, color= "black", nudge_y = -1400, nudge_x = 0.3) +
                geom_text(aes(x=Name,y=0),label=z$MoveToStop,
                          size=2.5, color= "black", nudge_y = -1500) +
                #geom_text(aes(x=Name,y=0),label= z$StopLoss,
                #          size=2.5, color= "black", nudge_x = -0.5) +
                ##      --      exposure
                geom_bar(stat="identity",aes(y=exposure/2, x=Name), fill="lightgrey", alpha = 0.4) +
                geom_text(aes(x=Name,y=exposure/2),label=round(z$exposure,0), size=2.5, color= "black") +
                theme_light() +
                theme(strip.text.y = element_text(angle=0),
                      axis.text = element_text( size = 8 ),
                      legend.position = "none") +
                geom_hline(yintercept = 9000, color = "darkgreen", linetype = "dotdash") +
                geom_hline(yintercept = -9000, color = "red", linetype = "dotdash", size=0.6) +
                geom_hline(yintercept = 0, color = "black", size=0.5) +
                #axis.text.x = element_text(angle=90)
                #axis.text.y=element_blank()) +
                # scale_x_continuous(labels = function(x) format(x, big.mark = ",",
                #                                                scientific = FALSE))  +
                labs(x=NULL,y=NULL, title = "Risk Profile",
                     subtitle = paste("StopPL", tot_stopPL, "- Unrealised P", tot_P, 
                                      "- Risk", tot_risk, "- Exposure", tot_exp,"- RE%",round(tot_risk/tot_exp*100,2),sep=" "))   +
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
        ws$Risk_Exp_age <- paste(round((ws$Risk / ws$Exposure)*100,1),"%",sep="")
        ws <- ws[,c("Name", "Profit", "P_age", "Risk", "R_age","Exposure","E_age",
                    "Risk_Exp_age")]
        
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
        
        
        
        trades2023 <- TradesOld[TradesOld$ExitYr == "2023",]
        winners <- length(which(trades2023$PL>0))
        losers <- length(which(trades2023$PL<0))
        win_loose <- round((winners*100)/(winners+losers),1)
        
        TradesG <- ggplot(data=trades2023, aes(x=`Exit Dt`, y=PL)) +
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
                scale_x_date(breaks = trades2023$`Exit Dt`,labels = date_format("%b-%y"), date_breaks = "4 weeks") +
                scale_y_continuous(labels = comma, breaks=seq(-100000, 1000000, 10000)) +
                labs(x=NULL,y=NULL, title = "Trades",
                     subtitle = paste(nrow(trades2023), "Trades", "Avg Return", 
                                      round(mean(trades2023$PL),0), 
                                      "Wins:", winners, "Loss:", losers, "-",win_loose, "%",
                                      sep=" "))  
        TradesG
        
        
        
        
        CumTotG <- ggplot(data=trades2023, aes(x=`Exit Dt`, y=YearlyCum)) +
                geom_line(size = 1, aes(color = format(`Exit Dt`,"%y"))) +
                #geom_line(aes(x=`ExitMnDy`, y=CumAvg),color="purple", size=1) +
                facet_wrap(ExitYr ~ ., scales = "free_x") +
                theme_light() +
                theme(axis.text.x = element_text(angle=90),
                      legend.position = "none") +
                geom_hline(yintercept = 0, color = "black") +
                labs(x=NULL,y=NULL, title = "Trades: Cumulative Total",
                     subtitle = paste(nrow(trades2023), "Trades", "Avg Return", round(mean(trades2023$PL),0), sep=" ")) +
                #   scale_x_date(breaks = TradesOld$ExitMnDy,labels = date_format("%d-%b"), date_breaks = "4 weeks") +
                scale_y_continuous(labels = comma, breaks=seq(-100000, 10000000, 20000))
        
        
        CumTotG
        
        
        #################################################################################
        ##                            Plot R                                           ##
        #################################################################################
        
        dat_period <- TradesOld[TradesOld$ExitYr == "2023",]
        
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
                labs(x=NULL,y=NULL, title = "R Ratio",
                     subtitle = paste(nrow(dat_period), "Trades", "R", 
                                      round(mean(dat_period$RR),2), 
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
