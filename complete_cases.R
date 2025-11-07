complete_cases <- function(dir = "/Users/johnlyons/Documents/Personal/DataScience/R/specdata", id = 1) {
        ## directory is a char vector of length =1 
        ## indicating location of csv
        ## load all the data
        setwd(dir)
        data <- lapply(dir(),read.csv,header=TRUE,sep=",")
        #convert to df
        df <- do.call(rbind , data)
        build.dat <- head(df[,2:4],0)
        for(i in id){
                frame1 <- subset(df, ID == i & !is.na(sulfate) & !is.na(nitrate), select = c(sulfate,nitrate,ID))
                build.dat <- rbind(build.dat,frame1)
                }
        fram1 <- aggregate(nitrate ~ ID, build.dat, length)
        set.seed(42)
        cc <- fram1[,2]
        use <- sample(332, 10)
        print(cc[use, "nobs"])
        
}
