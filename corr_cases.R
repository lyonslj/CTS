corr_cases <- function(dir = "/Users/johnlyons/Documents/Personal/DataScience/R/specdata", cc = 0) {
        ## directory is a char vector of length =1 
        ## indicating location of csv
        ## load all the data
        setwd(dir)
        data <- lapply(dir(),read.csv,header=TRUE,sep=",")
        #convert to df
        df <- do.call(rbind , data)
        # exclude NA's
        frame1 <- subset(df, !is.na(sulfate) & !is.na(nitrate), select = c(sulfate,nitrate,ID))
        # no of complete cases per id
        fram1 <- aggregate(cbind(sulfate, nitrate) ~ ID, frame1, length)
        # filter complete cases based in input variable ie cc
        fram2 <- subset(fram1,fram1$sulfate > cc & fram1$nitrate > cc)
        # isolate ID's from fram2
        fram2ID <- fram2$ID
        # bases on these ID's build a data frame (build.dat) of all sulfate and nitrate values
        # initialise build.dat
        build.dat <- head(fram1,0)
        for(i in fram2ID) {
                dat <- subset(frame1[,1:2],frame1$ID == i)
                cor.dat <- cor(dat$sulfate,dat$nitrate)
                build.dat <- rbind(build.dat,cor.dat)
                        }
        paste(nrow(fram2),"complete cases",sep=" ")
        output <- build.dat[,1]
}


sulf <- aggregate(df, list(df$ID), length, na.rm=T)
nit <- aggregate(frame1$nitrate, by=list(frame1$ID), FUN=length)
