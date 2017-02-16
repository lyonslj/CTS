DataTree <- function(PbsSvc) {
library(data.tree)
PBS <- Node$new("Parent Business Service")
CBS <- PBS$AddChild("Child Business Service")
PTS <- CBS$AddChild("Parent Technical Service")
CTS <- PTS$AddChild("Child Technical Service")
Application <- CTS$AddChild("Application")
#print(PBS)
##
library(treemap)

library(readxl)
setwd("/Volumes/C/JLTemp")
Services <- read_excel("ServicesTree.xlsx")
Svcs_data <- as.data.frame(Services)
mydat <- subset(Svcs_data,Svcs_data$PBS == PbsSvc)
lev1 <- unique(mydat[,1])
col2 <- "darkorchid1"
lev2 <- unique(paste("SetNodeStyle(Svcs$",mydat[,1],"$",mydat[,2],",inherit = FALSE, fillcolor = col2)",sep=""))
col3 <- "hotpink"
lev3 <- unique(paste("SetNodeStyle(Svcs$",mydat[,1],"$",mydat[,2],"$",mydat[,4],",inherit = FALSE, fillcolor = col3)",sep=""))
col4 <- "gold"
lev4 <- unique(paste("SetNodeStyle(Svcs$",mydat[,1],"$",mydat[,2],"$",mydat[,4],"$",mydat[,5],",inherit = FALSE, fillcolor = col4)",sep=""))
col5 <- "darkturquoise"
lev5 <- unique(paste("SetNodeStyle(Svcs$",mydat[,1],"$",mydat[,2],"$",mydat[,4],"$",mydat[,5],"$",mydat[,3],",inherit = FALSE, fillcolor = col5)",sep=""))
#
mydat$pathString <- paste("Services",mydat$PBS,mydat$CBS,mydat$PTS,mydat$CTS,mydat$Application, sep = "/")
Svcs <- as.Node(mydat)
#
        SetNodeStyle(Svcs, style = "filled,rounded", shape = "box", fillcolor = "GreenYellow", fontname = "helvetica", tooltip = GetDefaultTooltip)
        #SetNodeStyle(Svcs, inherit = FALSE, fillcolor = "darkturquoise")
        for(i in lev2) {eval(parse(text = i))}
        for(i in lev3) {eval(parse(text = i))}
        for(i in lev4) {eval(parse(text = i))}
        #for(i in lev5) {eval(parse(text = i))}
        plot(Svcs)
        
        
}
      