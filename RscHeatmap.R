RscHeatmap <- function(dataset) {
#########################################################
### A) Installing and loading required packages
#########################################################

if (!require("gplots")) {
        install.packages("gplots", dependencies = TRUE)
        library(gplots)
}
if (!require("RColorBrewer")) {
        install.packages("RColorBrewer", dependencies = TRUE)
        library(RColorBrewer)
}


#########################################################
### B) Reading in data and transform it into matrix format
#########################################################

data <- read.csv("/Users/johnlyons/Documents/Personal/DataScience/stuff/heatmaps_in_r.csv", comment.char="#")
rnames <- data[,1]                            # assign labels in column 1 to "rnames"
#mat_data <- data.matrix(data[,2:ncol(data)])  # transform column 2-5 into a matrix
#source('~/Documents/Personal/DataScience/R/RelativeStrengthComparative.R')
myset <- tail(dataset,30)
mat_data <- data.matrix(myset[,2:ncol(myset)])
rnames <- rownames(mat_data)                  # assign row names


#########################################################
### C) Customizing and plotting the heat map
#########################################################

# creates a own color palette from red to green
my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)

# (optional) defines the color breaks manually for a "skewed" color transition
col_breaks = c(seq(0,15,length=100),  # for red
               seq(16,39,length=100),           # for yellow
               seq(40,46,length=100))             # for green

# creates a 5 x 5 inch image
#png("../images/heatmaps_in_r.png",    # create PNG for the heat map        
#    width = 5*300,        # 5 x 300 pixels
#    height = 5*300,
#    res = 300,            # 300 pixels per inch
#    pointsize = 8)        # smaller font size

heatmap.2(mat_data,
          cellnote = mat_data,  # same data set for cell labels
          main = "RSC", # heat map title
          notecol="black",      # change font color of cell labels to black
          density.info="none",  # turns off density plot inside color legend
          trace="none",         # turns off trace lines inside the heat map
          margins =c(12,9),     # widens margins around plot
          col=my_palette,       # use on color palette defined earlier
          keysize=0.75,
          breaks=col_breaks,    # enable color transition at specified limits
          dendrogram="row",     # only draw a row dendrogram
          Colv="NA")            # turn off column clustering

#dev.off()               # close the PNG device
}