RscHeatmap <- function(dataset_daily, dataset_weekly, label = "Heatmap", to_console = FALSE, cumret = TRUE) {
        # A) Installing and loading required packages
        if (!require("gplots")) {
                install.packages("gplots", dependencies = TRUE)
                library(gplots)
        }
        if (!require("RColorBrewer")) {
                install.packages("RColorBrewer", dependencies = TRUE)
                library(RColorBrewer)
        }
        
        
        # Function to process and plot a single dataset
        plot_heatmap_page <- function(dataset, title_suffix) {
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
                
                # --- This line removes the default PDF page margins ---
                par(mai = c(4, 1, 1, 1))
                
                # Generate heatmap
                heatmap.2(mat_datat,
                          cellnote = mat_datat,
                          notecex = 0.8,
                          main = paste(label, title_suffix, sep = " - "), # Updated title
                          notecol = "black",
                          density.info = "none",
                          trace = "none",
                          margins = c(20, 20),
                          cexRow = 1,
                          cexCol = 1,
                          col = my_palette,
                          keysize = 0.75,
                          breaks = col_breaks,
                          key = FALSE,
                          dendrogram = "none",
                          Colv = FALSE,
                          Rowv = FALSE)
                
                return(colnames(mat_datat)) # Return column names for CumRet plot
        }
        
        
        # --- Conditional output based on 'to_console' flag ---
        if (!to_console) {
                # --- Open PDF graphics device ONCE with adjusted size ---
                output_dir <- file.path(path.expand("~"), "Documents", "Personal", "DataScience", "R", "Rplots", "Combined") # New folder for combined plots
                if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
                output_filename <- file.path(output_dir, paste(label, "_Combined_", Sys.Date(), ".pdf", sep = "")) # Updated filename
                
                pdf(file = output_filename,
                    width = 16,
                    height = 20)
                
                # --- PLOT 1: DAILY HEATMAP ---
                instr_names <- plot_heatmap_page(dataset_daily, "Daily")
                
                # --- PLOT 2: WEEKLY HEATMAP (New Page) ---
                # A new plot on the next page is created automatically by the heatmap.2 call
                plot_heatmap_page(dataset_weekly, "Weekly")
                
                # --- PLOT 3: CUMULATIVE RETURNS GRAPH (New Page) ---
                if (cumret) {
                        plot.new()  # Start a new page for the CumRet graph
                        # Use top 50% height, 2/3 width and center
                        par(fig = c(0.167, 0.833, 0.5, 1), new = TRUE)
                        par(mai = c(1, 0.5, 0.5, 0.5))
                        
                        source(file.path(path.expand("~"), "Documents", "Personal", "DataScience", "R", "JL CTS scripts", "CumRetDyG.R"))
                        CumRet(instr_names) # Use instrument names from the last plotted set (Weekly)
                }
                
                # --- Close the PDF device. ---
                dev.off()
                
                # --- Open the PDF file ---
                sysname <- Sys.info()["sysname"]
                if (sysname == "Darwin") {
                        system(paste("open", shQuote(output_filename)))
                } else if (sysname == "Windows") {
                        shell.exec(output_filename)
                } else {
                        system(paste("xdg-open", shQuote(output_filename)))
                }
        } else {
                # Still need to call plot_heatmap_page for console mode for consistency
                plot_heatmap_page(dataset_daily, "Daily")
        }
}
