RscHeatmap <- function(dataset, label = "Heatmap", freq = "", to_console = FALSE, cumret = TRUE) {
        # A) Installing and loading required packages
        if (!require("gplots")) {
                install.packages("gplots", dependencies = TRUE)
                library(gplots)
        }
        if (!require("RColorBrewer")) {
                install.packages("RColorBrewer", dependencies = TRUE)
                library(RColorBrewer)
        }
        
        
        # B) Data preparation
        myset <- dataset
        myset <- myset[order(myset[, ncol(myset)]), ]
        
        # round numbers
        myset <- apply(myset, 2, function(v) {
                round(v, 0)
        })
        
        # transform into matrix format
        mat_data <- data.matrix(myset)
        mat_datat <- t(mat_data)       #Transpose
        
        # C) Customizing and plotting the heat map
        my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)
        
        brks <- quantile(mat_data, probs = seq(.15, .95, .05), na.rm = TRUE, names = FALSE)
        col_breaks <- c(seq(min(brks), max(brks), length = 300))
        
        # --- Conditional output based on 'to_console' flag ---
        if (!to_console) {
                # --- Open PDF graphics device with adjusted size ---
                # Use path.expand('~') to dynamically get the current user's home directory
                # Corrected block to create the path dynamically and ensure the directory exists
                output_dir <- file.path(path.expand("~"), "Documents", "Personal", "DataScience", "R", "Rplots", "Daily")
                # Ensure the directory path exists, creating it if necessary (recursive=TRUE)
                if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
                # Construct the final filename using the dynamic directory path
                output_filename <- file.path(output_dir, paste(label, "_", Sys.Date(), "_", freq, ".pdf", sep = ""))
                pdf(file = output_filename,
                    width = 16,
                    height = 20)
                
                # --- This line removes the default PDF page margins ---
                par(mai = c(4, 1, 1, 1))
        }
        
        # Generate heatmap
        heatmap.2(mat_datat,
                  cellnote = mat_datat,
                  notecex = 0.8,
                  main = paste(label, freq, sep = " "),
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
        
        # If outputting to PDF, also generate the second plot and close the device
        if (!to_console) {
                # Page layout for new plot
                plot.new()  # Start a new page
                # Use top 50% height, 2/3 width and center
                par(fig = c(0.167, 0.833, 0.5, 1), new = TRUE)
                par(mai = c(1, 0.5, 0.5, 0.5))  # Smaller margins for compact plot
                
                # Generate graph of Cumulative returns
                # Make sure the source for CumRet is available.
                # It's better to pass it as a parameter or ensure it's in a known location.
                # For this example, I've kept your original source() call.
                source(file.path(path.expand("~"), "Documents", "Personal", "DataScience", "R", "JL CTS scripts", "CumRetDyG.R"))
                
                # Call Cumret to pdf
                if (!cumret) {
                        NULL
                } else { CumRet(colnames(mat_datat)) 
                        }
                
                
                # --- Close the PDF device. This is also critical. ---
                dev.off()
                
                # --- Open the PDF file ---
                sysname <- Sys.info()["sysname"]
                if (sysname == "Darwin") {
                        system(paste("open", shQuote(output_filename)))
                } else if (sysname == "Windows") {
                        shell.exec(output_filename) # shell.exec handles spaces fine
                } else {
                        system(paste("xdg-open", shQuote(output_filename)))
                }
        }
}
