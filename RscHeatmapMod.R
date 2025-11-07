RscHeatmap <- function(dataset, nm = "CumulativeReturns", freq = "", to_console = FALSE, data = JSEdat) {
        # A) Installing and loading required packages
        if (!require("gplots")) {
                install.packages("gplots", dependencies = TRUE)
                library(gplots)
        }
        if (!require("RColorBrewer")) {
                install.packages("RColorBrewer", dependencies = TRUE)
                library(RColorBrewer)
        }
        
        source("/Users/johnlyons/Documents/Personal/DataScience/R/JL CTS scripts/CumRetDyG.R")
        
        # B) Data preparation
        myset <- dataset
        myset <- myset[order(myset[, ncol(myset)]), ]
        
        # round numbers
        myset <- apply(myset, 2, function(v) {
                round(v, 0)
        })
        
        # transform into matrix format
        mat_data <- data.matrix(myset)
        mat_datat <- na.omit(t(mat_data))       #Transpose
        instruments <- colnames(mat_datat)
        
        # C) Customizing and plotting the heat map
        my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)
        
        brks <- quantile(mat_data, probs = seq(.15, .95, .05), na.rm = TRUE, names = FALSE)
        col_breaks <- c(seq(min(brks), max(brks), length = 300))
        
        # --- Conditional output based on 'to_console' flag ---
        if (!to_console) {
                # --- Open PDF graphics device with adjusted size ---
                output_filename <- paste(file.path("/Users/johnlyons/Documents/Personal/DataScience/R/Rplots/Daily/"), nm, "_", Sys.Date(), "_", freq, ".pdf", sep = "")
                pdf(file = output_filename,
                    width = 16,
                    height = 20)
                
                # --- This line removes the default PDF page margins for the first plot ---
                par(mai = c(4, 1, 1, 1))
        }
        
        # Generate heatmap (always happens, regardless of output method)
        heatmap.2(mat_datat,
                  cellnote = mat_datat,
                  notecex = 0.8,
                  main = paste(nm, freq, sep = " "),
                  notecol = "black",
                  density.info = "none",
                  trace = "none",
                  margins = c(20, 20), # <--- Increased the bottom margin value
                  cexRow = 1,
                  cexCol = 1,
                  col = my_palette,
                  keysize = 0.75,
                  breaks = col_breaks,
                  key = FALSE,
                  dendrogram = "none",
                  Colv = FALSE,
                  Rowv = FALSE)
        
        # If outputting to PDF, also generate the second plot, close the device, and open the file
        if (!to_console) {
                # Page layout for new plot
                plot.new()  # Start a new page
                # Use top 50% height, 2/3 width and center
                par(fig = c(0.167, 0.833, 0.5, 1), new = TRUE)
                par(mai = c(1, 0.5, 0.5, 0.5))  # Smaller margins for compact plot
                
                # Generate graph of Cumulative returns
                
                #CumRet(instruments, Sys.Date()-250, nm, data  )
                
                # --- Close the PDF device. This is also critical. ---
                dev.off()
                
                # --- Open the PDF file ---
                sysname <- Sys.info()["sysname"]
                if (sysname == "Darwin") {
                        system(paste("open", output_filename))
                } else if (sysname == "Windows") {
                        shell.exec(output_filename)
                } else {
                        system(paste("xdg-open", output_filename))
                }
        }
        
        
}
