DataTree <- function(PbsSvc) {
        library(data.tree)
        PBS <- Node$new("Parent Business Service")
        CBS <- PBS$AddChild("Child Business Service")
        PTS <- CBS$AddChild("Parent Technical Service")
        CTS <- PTS$AddChild("Child Technical Service")
        Application <- CTS$AddChild("Application")
        ServerClassification <- Application$AddChild("ServerClassification")
        Server <- ServerClassification$AddChild("Server")
        library(treemap)
        library(readxl)
        setwd("/Volumes/C/JLTemp")
        Services <- read_excel("ServicesTree.xlsx")
        Svcs_data <- as.data.frame(Services)
        mydat <- subset(Svcs_data,Svcs_data$PBS == "Investor_Services_PBS")
        mydat$pathString <- paste(mydat$PBS,mydat$CBS,mydat$PTS,mydat$CTS,mydat$Application, sep = "/")
        Svcs <- as.Node(mydat)
        SetNodeStyle(Svcs, style = "filled,rounded", shape = "box", fillcolor = "GreenYellow", fontname = "helvetica", tooltip = GetDefaultTooltip)
        plot(Svcs)
        
        
        #mydat$pathString <- paste(mydat$PBS,mydat$Application,mydat$ServerClassification,mydat$Server,sep = "/")
        
        
}
