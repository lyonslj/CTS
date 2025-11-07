library(TTR)
library(quantmod)
library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)
library(readxl)
library(reshape2)
library(DT)
if (!require("gplots")) {
        install.packages("gplots", dependencies = TRUE)
        library(gplots)
}
if (!require("RColorBrewer")) {
        install.packages("RColorBrewer", dependencies = TRUE)
        library(RColorBrewer)
}

JSE.recent.dat <- JSEdat[JSEdat$Date >= Sys.Date()-450,]
JSE.recent.dat <- unique(JSE.recent.dat[complete.cases(JSE.recent.dat),])


list.of.i <- NULL
df.daily.roc <- NULL
df.weekly.roc <- NULL
df.monthly.roc <- NULL
for(i in unique(JSE.recent.dat$Name)) {
        ## -- Get list of instruments  
                y <- JSE.recent.dat[JSE.recent.dat$Name == i,]
                z <- xts(y[,3:7],order.by = y[,2])        # convert to xts
                zm <- to.period(z,"months")
                 
                zm.mar <- zm["2020-03"]
        ## -- Test for volume
                if ((dim(zm.mar)[1] == 1) && (as.numeric(zm.mar$z.Volume) > 1000000 )) {
                        list.of.i <- append(list.of.i, i)
                        assign(paste(i,"-M",sep=""),zm)         #Monthly data
                        assign(i,z)                             #Daily data
                ## -- produce weekly data
                        zw <- to.period(z,"weeks")
                        assign(paste(i,"-W",sep=""),zw) 
                ## -- ROC data
                        ## -- daily
                        if (dim(z)[1] > 30) {
                                nm <- paste(i,".roc", sep="")
                                roc <- round(ROC(z$Close, n = 1, type = "discrete")*100,2) 
                                assign(nm, roc)
                                i.tab <- data.frame(Date = index(roc), coredata(roc))
                                i.tab$Insrm <- i
                                df.daily.roc <- rbind(df.daily.roc, i.tab)
                                
                        } 
                        ## -- weekly
                        if (dim(zw)[1] > 10) {
                                nm <- paste(i,".roc.W", sep="")
                                roc <- round(ROC(zw$z.Close, n = 1, type = "discrete")*100,2)  
                                assign(nm, roc)
                                i.tab <- data.frame(Date = index(roc), coredata(roc))
                                i.tab$Insrm <- i
                                df.weekly.roc <- rbind(df.weekly.roc, i.tab)
                        } 
                        ## -- monthly
                        if (dim(zm)[1] > 3) {
                                nm <- paste(i,".roc.M", sep="")
                                roc <- round(ROC(zm$z.Close, n = 1, type = "discrete")*100,2) 
                                assign(nm, roc)
                                i.tab <- data.frame(Date = index(roc), coredata(roc))
                                i.tab$Insrm <- i
                                df.monthly.roc <- rbind(df.monthly.roc, i.tab)
                        }
                }
              

}
        ## -- Get rid NA's
                df.daily.roc <- na.omit(df.daily.roc)
                df.weekly.roc <- na.omit(df.weekly.roc)
                df.monthly.roc <- na.omit(df.monthly.roc)
                
        ## -- Prepare tables for heatmap
                fnPrepData <- function(dat,period) {
                        colnames(dat)[2] <- "Close"
                        tm.frame <- ifelse(period == "w", 50,
                                ifelse(period == "m", 150, 7))
                        df.d.latest <- dat[!grepl(excl_list, dat$Insrm) &
                                                   dat$Date >= max(dat$Date)- tm.frame ,]
                        dd <- spread(df.d.latest, Date, Close)
                        lcol <- ncol(dd)
                        dd$M <- round(apply(dd[,c(2:lcol)],1,mean),2)
                        row.names(dd) <- dd$Insrm
                        dd <- dd[,-1]
                        dd <- dd[order(dd$M),]
                        dd <- data.matrix(dd)
                        x <- tail(dd,30)
                        #x <- t(x)
                }
                
                excl_list <- ("AGLSBB|AMSSBE|AMSSBP|ANGSBA|ANGSBO|ANGSBY|APNSBG|APNSBH|ASHINF|ASHMID|ASHT40|ASHWGB|BLUSBH|BHPSBF|CSP500|CSPROP|CTOP50|DIPULA-A|DIPULA-B|EQUITES|ETF5IT|ETFGLD|ETFPLD|ETFPLT|ETFSAP|FIRSTRNDP|GFISBC|GFISBD|GFISBT|HARSBW|HARSBO|IMPSBH|IMPSBI|IMPSBV|INDLU|J200USD|JH-ALEX|JH-ALSI40|JH-ASIN|JH-FLED|JH-MIDCAP|JH-RES|JH-SMALL|JSE|JSE-ALPI|JSE-ALSH|JSE-ALT15|JSE-ALTX|JSE-AUTM|JSE-BANK|JSE-BASM|JSE-BEVR|JSE-CALS|JSE-CHES|JSE-CIN25|JSE-COAL|JSE-CONG|JSE-CONM|JSE-CONS|JSE-CPI|JSE-CTOP|JSE-DALS|JSE-DIVP|JSE-DTOP|JSE-ELEE|JSE-EQII|JSE-FINA|JSE-FINDI|JSE-FINI|JSE-FJGI|JSE-FJVI|JSE-FOOD|JSE-FOOR|JSE-FORE|JSE-FTEL|JSE-GENF|JSE-GENI|JSE-GERE|JSE-GOLD|JSE-HCOM|JSE-HEAL|JSE-HEES|JSE-HOUS|JSE-IIND|JSE-INDE|JSE-INDI|JSE-INDM|JSE-INDT|JSE-LGCAP|JSE-LIFE|JSE-MEDI|JSE-METL|JSE-MINI|JSE-MTEL|JSE-NLIF|JSE-OILG|JSE-OILP|JSE-PERG|JSE-PHAR|JSE-PLAT|JSE-PRUT|JSE-PULS|JSE-REDS|JSE-REIV|JSE-RESI|JSE-SAPI|JSE-SCOM|JSE-SCTP|JSE-SRI|JSE-SUPS|JSE-SW40|JSE-SWALS|JSE-SXTRI|JSE-TABA|JSE-TECH|JSE-TELE|KIOSBN|MRPSBG|NPNSBA|NPNSBB|NPNSBC|NPNSBP|NPNSBZ|Nedbank-P|PRXSBQ|PSG-KST|SATRIX40|SATRIX500|SATRIXDIV|SATRIXEMG|SATRIXNDQ|SATRIXRAF|SATRIXRES|SATRIXSWX|SATRIXWRD|SBEN05|SBKSBO|SGLSBC|SGLSBD|SGLSBE|SGLSBU|SGLSTD|SHPSBG|SOLSBC|SOLSBE|STANBANKP|STXFIN|STXILB|STXIND|STXPRO|STXQUA|SYG4IRGE|SYGEURO50|SYGSP500|SYGSWIX40|TOPSBA|TOPSBB|TOPSBN|TOPSBO|TOPSBW|TOPSBX|TOPSBY|VODSBG|ZA077|ZA079|WHLSBD|WHLSBE|ZAMBEZI-P")
              
                tbl.daily.roc <-   fnPrepData(df.daily.roc,"d")
                tbl.weekly.roc <-   fnPrepData(df.weekly.roc,"w")
                tbl.monthly.roc <-   fnPrepData(df.monthly.roc,"m")
                
                
                ##      ---------------------   ##      ----------------------  #
                ##                      Datatable
                
                dat <- tbl.daily.roc
                brks <- quantile(dat, probs = seq(.05, .95, .05), na.rm = TRUE, names = FALSE)
                clrs <- 
                clrs <- round(seq(255, 0, length.out = length(brks) + 1), 0) %>%
                {paste0("rgb(", .,",", .,",0)")}
                
                fnDtTable <- function(dat,x)     { 
                        DT::datatable(dat, caption = "TEST", 
                                      class = 'cell-border stripe', rownames = TRUE, filter = "top",
                                      editable = TRUE, extensions = "Buttons", 
                                      options = list(dom="Bfrtip",buttons=c("copy","excel","pdf","print"),
                                                     pageLength = 100, autoWidth = TRUE)) %>%          
                                formatStyle(columns = c(0:ncol(dat)), fontSize = '60%',
                                            backgroundColor = styleInterval(c(-5,0,5,10), 
                                                                            c("red",'orange','yellow','lightgreen','green')))
                                            
                }
                
                datatable(dat,options = list(
                        autoWidth = TRUE, columnDefs = list(list(width = '100px',targets = 1:5))
                ))%>%formatStyle(names(dat),background = styleInterval(brks,clrs))
                
                
                my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)
                
                # (optional) defines the color breaks manually for a "skewed" color transition
                col_breaks = c(seq(-5,0,length=100),  # for red
                               seq(1,7,length=100),           # for yellow
                               seq(8,20,length=100)) 
                
                heatmap.2(x,
                          cellnote = x,  # same data set for cell labels
                          main = "roc",         # heat map title
                          notecol="black",      # change font color of cell labels to black
                          density.info="none",  # turns off density plot inside color legend
                          trace="none",         # turns off trace lines inside the heat map
                          margins =c(12,9),     # widens margins around plot
                          col=my_palette,       # use on color palette defined earlier
                          keysize=0.5,
                          cexRow = 0.8,
                          cexCol = 0.8,
                          breaks=col_breaks,   # enable color transition at specified limits
                          dendrogram="row",    # only draw a row dendrogram
                          Colv = NA,           # turn off column clustering
                          Rowv = F) 
                dev.off()
                
                
                
               
                
                
                df.d.latest <- df.d.latest[order(-df.d.latest$Date),]
                #
                df.w.latest <- df.weekly.roc[df.weekly.roc$Date >= max(df.weekly.roc$Date)-49,]
                df.w.latest <- tail(df.w.latest[plyr::order(df.w.latest$z.Close),],20)
                #
                df.m.latest <- df.monthly.roc[df.monthly.roc$Date >= max(df.monthly.roc$Date),]
                df.m.latest <- tail(df.m.latest[order(df.m.latest$z.Close),],20)
                #
        ## -- exclisions list
                
                
                incl_instrm <- sort(unique(df.d.latest[!grepl(excl_list,df.d.latest$Insrm),3]))
                
                
                
                ggplot(df.d.latest, aes(Date, Insrm, fill = Close)) + 
                        geom_tile(colour = "white") + 
                       # facet_grid(year~monthf) + 
                        scale_fill_gradient(low="red", high="green") 
                        labs(x="Week of Month",
                             y="",
                             title = "Time-Series Calendar Heatmap", 
                             subtitle="Yahoo Closing Price", 
                             fill="Close")
                
                
                heatmap(df.m.latest)
        heatmap.2(df.m.latest,
                  cellnote = mat_data,  # same data set for cell labels
                  main = "RSC",         # heat map title
                  notecol="black",      # change font color of cell labels to black
                  density.info="none",  # turns off density plot inside color legend
                  trace="none",         # turns off trace lines inside the heat map
                  margins =c(12,9),     # widens margins around plot
                  col=my_palette,       # use on color palette defined earlier
                  keysize=0.6,
                  breaks=col_breaks,   # enable color transition at specified limits
                  dendrogram="row",    # only draw a row dendrogram
                  Colv = NA,           # turn off column clustering
                  Rowv = F)         
                
