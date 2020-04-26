## -- function to load latest data file
## -- input is a file ie "04-09-20.csv

### --  Download snet file  ###  
        dt_file <- paste(format(Sys.Date()-1,"%y-%m-%d"),"lzh",sep=".")
        dn_file <- paste("https://www.sharenet.co.za/snet/csv/data/",dt_file,sep="")
        dest <- paste("/Users/johnlyons/Desktop/snet/",dt_file,sep="")
        download.file(dn_file, dest, method =  "auto")
        unzip(dest)

fnLoadData <- function(file = paste(format(Sys.Date()-1,"%d-%m-%y"),"csv",sep=".")) {
        pathfile <- paste("/Users/johnlyons/Desktop/snet/",file, sep = "")
        df.lst <- read.csv(pathfile,skip = 5, header = FALSE, sep =",", stringsAsFactors = FALSE)        # read new csv's#
        df.clean <- df.lst[,1:11]
        df.clean <- df.clean[,-c(4,5)]
        names(df.clean) <- c("Name","Exchange","Instm", "Date","Open", "High", "Low", "Close", "Volume")
        new.dat <- subset(df.clean[,-3],df.clean$Exchange %in% c("JSE","SPOT","FOREX"))
        new.dat <- new.dat[,-2]
        new.dat$Date <- as.Date(as.character(new.dat$Date),format="%Y%m%d")               
        new.dat$Volume <- as.numeric(new.dat$Volume)
        # Now merger to main set
        JSEdat <<- rbind(JSEdat,new.dat)
        write.csv(JSEdat,file = "/Users/johnlyons/Documents/Personal/DataScience/R/JSEdat.csv")
        tail(sort(unique(new.dat$Date)))
        tail(sort(unique(JSEdat$Date)))
        
}
