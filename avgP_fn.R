avgP_fn <- function(data_set,symbol,start,end) {
        mean(subset(data_set,data_set$Symbol == symbol & 
        as.Date(data_set$Date) >= as.Date(start) & 
        as.Date(data_set$Date) <= as.Date(end))$Close)
}
avgP_fn(data,"BAC","2015-01-01","2015-09-01")


WavgP_fn <- function(ds, symbol, start, end) {
        tmp <- subset(ds,ds$Symbol == symbol & as.Date(ds$Date) >= as.Date(start) &as.Date(ds$Date) <= as.Date(end))
                     
        tmp$WP <- tmp$Close * tmp$Volume
        sum(tmp$WP)/sum(as.numeric(tmp$Volume))
}
WavgP_fn(data, "GE", '2016-01-01', '2016-12-31')

pm <- ((harxts_rsc$Close - lag(harxts_rsc$Close,1))/lag(harxts_rsc$Close,1))*100

data.return <- data.frame(Date = data.close$Date[-1], sapply(data.close[-1], function(x)
{
        diff(x)/x[-length(x)]+1
}))