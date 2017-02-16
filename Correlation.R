Correlation <- function(lst = MtlsIndexes) {
lst <- append(MtlsIndexes,"SAVI")
y <- 0
days <- c(360,180,90,60,30,10,5)
mergecor <- NULL
mycor <- NULL
for(Period in days) {
        prices <- as.data.frame(Period)
        for(i in lst) {
                        y <- subset(JSEdat[,6],JSEdat$Name == i)
                        y <- as.data.frame(tail(y,Period))
                        colnames(y) <- i
                        prices <- cbind(prices,y)
                        }
        mycor <- cor(prices[,-1],use="complete.obs")
        mycor <- cbind(Period,mycor)
        mergecor <- rbind(mergecor,mycor)
}
dt <- Sys.Date()
mypath <- paste(file.path("/Users/johnlyons/Documents/Personal/DataScience/R/RPlots/Correlation/"),dt,".csv",sep="")
write.csv(mergecor,mypath)
}
