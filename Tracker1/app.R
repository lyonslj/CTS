library(shiny)
library(DT)
library(readxl)

# Read Excel file from default directory
data <- read_excel("/Users/johnlyons/Documents/Personal/DataScience/R/JL CTS scripts/Tracker.xlsx", sheet = "Sheet0")

# Convert specified columns to Date with the format mask
data[, c("Open", "Close")] <- lapply(data[, c("Open", "Close")], as.Date, format = "%d-%b-%y")

# Convert other columns to numeric
data[, c("Quantity", "EntryPrice", "ExitPrice", "Fee", "Stop Loss")] <-
        apply(data[, c("Quantity", "EntryPrice", "ExitPrice", "Fee", "Stop Loss")], 2, as.numeric)

newdat <- data

ui <- fluidPage(
        titlePanel("Editable Data"),
        helpText("Click on a cell to edit its value."),
        actionButton("save_button", "Update 'newdat' in R", icon = icon("sync")),
        DTOutput("my_datatable")
)

server <- function(input, output, session) {
        
        # Reactive value to hold the editable data
        editable_data_reactive <- reactiveVal(newdat[, !names(newdat) %in% c("Net Profit")])
        
        # Reactive expression to calculate Net Profit dynamically
        calculated_data <- reactive({
                current_data <- editable_data_reactive()
                current_data$`Net Profit` <- 
                        current_data$Quantity * (current_data$ExitPrice - current_data$EntryPrice) - current_data$Fee
                
                return(current_data)
        })
        
        # Render the datatable
        output$my_datatable <- renderDT({
                datatable(
                        calculated_data(),
                        editable = TRUE,
                        options = list(
                                pageLength = 10,
                                scrollX = TRUE
                        ),
                        callback = JS(
                                "
        var format_net_profit = function(data, type, row) {
          if (type === 'display' || type === 'filter') {
            return data.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
          }
          return data;
        };
        var table = this.api();
        table.column('Net Profit:name').render('display', format_net_profit);
        "
                        )
                ) %>%
                        formatCurrency("Net Profit", currency = "$", digits = 2) # Use formatCurrency for better formatting
        })
        
        # Observe user edits and update the editable reactive data
        observeEvent(input$my_datatable_cell_edit, {
                info <- input$my_datatable_cell_edit
                
                row <- info$row
                col <- info$col
                value <- info$value
                
                current_data <- editable_data_reactive()
                
                if (is.numeric(current_data[[col]])) {
                        current_data[row, col] <- as.numeric(value)
                } else if (class(current_data[[col]]) == "Date") {
                        current_data[row, col] <- as.Date(value, format = "%Y-%m-%d")
                } else {
                        current_data[row, col] <- value
                }
                
                editable_data_reactive(current_data)
        })
        
        # Observe the save button click and update the global 'newdat' dataframe
        observeEvent(input$save_button, {
                updated_data <- calculated_data() # Use the calculated data for saving
                newdat <<- updated_data
                showNotification("The 'newdat' dataframe has been updated in the R session.", type = "message")
        })
}

shinyApp(ui, server)