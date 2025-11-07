pollutantmean <- function(dir = "/Users/johnlyons/Documents/Personal/DataScience/R/specdata", pollutant = "nitrate", id_from = 1, id_to = 332) {
        ## directory is a char vector of length =1 
        ## indicating location of csv
        ## load all the data
        setwd(dir)
        data <- lapply(dir(),read.csv,header=TRUE,sep=",")
        #convert to df
        df <- do.call(rbind , data)
        df.ss <- subset(df,df$ID >= id_from & df$ID <= id_to)
        if(pollutant == "nitrate"){
                nitrate <- df.ss[,3]
                nit_na <- nitrate[!is.na(nitrate)]
                round(mean(nit_na),3)}
        else if(pollutant == "sulfate") {
                sulfate <- df.ss[,2]
                sulf_na <- sulfate[!is.na(sulfate)]
                round(mean(sulf_na),3)}
}

