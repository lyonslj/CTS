library(shiny)
library(DT)
library(readxl)

# Read Excel file from default directory
data <- read_excel("/Users/johnlyons/Documents/Personal/DataScience/R/JL CTS scripts/Tracker.xlsx", sheet = "Sheet0")

# Convert specified columns to numeric
data[, c("Quantity", "EntryPrice", "ExitPrice", "Fee", "Net Profit", "Stop Loss")] <- 
        apply(data[, c("Quantity", "EntryPrice", "ExitPrice", "Fee", "Net Profit", "Stop Loss")], 2, as.numeric)

##
datatable(data,filter = "top",options = list(scrollX = TRUE,
                                                  pageLength=50,sScrollY = '75vh', scrollCollapse = TRUE)) %>%
        formatStyle(columns = c(1:ncol(data)), fontSize = '70%') 
##

# UI
ui <- fluidPage(
        # Custom CSS to reduce sidebar width by ~80%
        tags$style(HTML("
    .sidebar {
      width: 80px;
    }
    .main-panel {
      margin-left: 80px;
    }
  ")),
        titlePanel("Investment Dashboard"),
        tabsetPanel(
                # Existing Tracker Tab
                tabPanel("Investment Tracker Dashboard",
                         sidebarLayout(
                                 sidebarPanel(
                                         selectInput("instrument", "Select Instrument:", choices = c("All", unique(data$Instrument)))
                                 ),
                                 mainPanel(
                                         DTOutput("trackerTable")
                                 )
                         )
                ),
                # New Checklist Tab
                tabPanel("Checklist",
                         sidebarLayout(
                                 sidebarPanel(
                                         actionButton("add_row", "Add Row")
                                 ),
                                 mainPanel(
                                         h4("Checklist Data"),
                                         DTOutput("checklistTable")
                                 )
                         )
                )
        )
)

# Server
server <- function(input, output, session) {
        # Reactive value to store checklist data with at least one row for initialization
        checklistValues <- reactiveVal(data.frame(
                Live = FALSE,
                Portfolio = "Investment",
                Open = as.Date(Sys.Date()),
                Instrument = "",
                Capital = 0,
                `%Risk` = 1,
                `Entry Price` = 0,
                `Stop Loss` = 0,
                `Mv to Stop` = 0,
                Quantity = 0,
                `Trans. Cost` = 0,
                `At Risk` = 0,
                Target = 0,
                RR = 0,
                stringsAsFactors = FALSE
        ))
        
        # Filter data for Tracker tab
        filteredData <- reactive({
                if (input$instrument == "All") {
                        data
                } else {
                        data[data$Instrument == input$instrument, ]
                }
        })
        
        # Render Tracker table
        output$trackerTable <- renderDT({
                datatable(filteredData(), options = list(pageLength = 5, lengthMenu = c(5, 10, 15)))
        })
        
        # Add new row when "Add Row" button is clicked
        observeEvent(input$add_row, {
                new_row <- data.frame(
                        Live = FALSE,
                        Portfolio = "Investment",
                        Open = as.Date(Sys.Date()),
                        Instrument = "",
                        Capital = 0,
                        `%Risk` = 1,
                        `Entry Price` = 0,
                        `Stop Loss` = 0,
                        `Mv to Stop` = 0,
                        Quantity = 0,
                        `Trans. Cost` = 0,
                        `At Risk` = 0,
                        Target = 0,
                        RR = 0,
                        stringsAsFactors = FALSE
                )
                current_data <- checklistValues()
                updated_data <- rbind(current_data, new_row)
                checklistValues(updated_data)
        })
        
        # Render editable Checklist table with DT
        output$checklistTable <- renderDT({
                datatable(checklistValues(),
                          editable = list(target = "cell", disable = list(columns = c(0))), # Disable editing first column if needed
                          options = list(
                                  pageLength = 5,
                                  lengthMenu = c(5, 10, 15),
                                  dom = 't',
                                  keys = TRUE, # Enable keyTable extension for navigation
                                  initComplete = JS(
                                          "function(settings, json) {",
                                          "  var table = this.api();",
                                          "  table.on('key', function(e, datatable, key, cell, originalEvent) {",
                                          "    if (key === 9) { // Tab key (key code 9)",
                                          "      e.preventDefault();",
                                          "      var currentCell = table.cell(cell.index());",
                                          "      var nextCell = table.cell(currentCell.index().row, currentCell.index().column + 1);",
                                          "      if (nextCell.length) {",
                                          "        nextCell.node().focus();",
                                          "        table.keys.moveTo(nextCell.index());",
                                          "      }",
                                          "    }",
                                          "  });",
                                          "}"
                                  )
                          ),
                          callback = JS("table.on('click.dt', 'td', function() {
                var cell = table.cell(this);
                var row = cell.index().row;
                var col = cell.index().column;
                Shiny.setInputValue('checklistTable_cell_click', {row: row, col: col});
              });")
                )
        })
        
        # Handle cell edits
        observeEvent(input$checklistTable_cell_edit, {
                info <- input$checklistTable_cell_edit
                row <- info$row
                col <- info$col
                value <- info$value
                
                # Update the reactive data frame
                current_data <- checklistValues()
                current_data[row + 1, col + 1] <- value  # Adjust for 0-based indexing
                checklistValues(current_data)
                
                # Replace the data in the table
                replaceData(proxy, checklistValues(), resetPaging = FALSE)
        })
        
        # Proxy for DT table to update dynamically
        proxy <- dataTableProxy("checklistTable")
}

# Run the application 
shinyApp(ui = ui, server = server)