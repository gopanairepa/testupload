library(shiny)
library(shinydashboard)
library(DT)
library(shinyWidgets)

# Function to scan file with ClamAV
scan_file_with_clamav <- function(file_path) {
  tryCatch({
    # Run clamscan on the uploaded file
    result <- system2("clamscan", 
                      args = c("--no-summary", "--infected", file_path),
                      stdout = TRUE, 
                      stderr = TRUE)
    
    # Check return code
    exit_code <- attr(result, "status")
    
    if (is.null(exit_code)) {
      exit_code <- 0  # No status means success
    }
    
    # Parse results
    if (exit_code == 0) {
      return(list(
        status = "CLEAN",
        message = "File is clean - no viruses detected",
        details = paste(result, collapse = "\n")
      ))
    } else if (exit_code == 1) {
      return(list(
        status = "INFECTED",
        message = "VIRUS DETECTED! File is infected",
        details = paste(result, collapse = "\n")
      ))
    } else {
      return(list(
        status = "ERROR",
        message = "Error scanning file",
        details = paste(result, collapse = "\n")
      ))
    }
  }, error = function(e) {
    return(list(
      status = "ERROR",
      message = paste("ClamAV scan failed:", e$message),
      details = ""
    ))
  })
}

# Function to get ClamAV version and database info
get_clamav_info <- function() {
  tryCatch({
    version <- system2("clamscan", args = "--version", stdout = TRUE)
    db_info <- system2("sigtool", args = "--info=/var/lib/clamav/main.cvd", stdout = TRUE)
    
    list(
      version = ifelse(length(version) > 0, version[1], "Unknown"),
      database = ifelse(length(db_info) > 0, paste(db_info, collapse = "\n"), "Unknown")
    )
  }, error = function(e) {
    list(version = "ClamAV not available", database = "")
  })
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "ClamAV File Scanner"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("File Scanner", tabName = "scanner", icon = icon("shield-alt")),
      menuItem("System Info", tabName = "info", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .status-clean { color: #28a745; font-weight: bold; }
        .status-infected { color: #dc3545; font-weight: bold; }
        .status-error { color: #ffc107; font-weight: bold; }
        .scan-result { 
          margin: 10px 0; 
          padding: 15px; 
          border-radius: 5px; 
          border-left: 4px solid;
        }
        .result-clean { 
          background-color: #d4edda; 
          border-left-color: #28a745; 
        }
        .result-infected { 
          background-color: #f8d7da; 
          border-left-color: #dc3545; 
        }
        .result-error { 
          background-color: #fff3cd; 
          border-left-color: #ffc107; 
        }
      "))
    ),
    
    tabItems(
      # File Scanner Tab
      tabItem(tabName = "scanner",
              fluidRow(
                box(
                  title = "Upload File for Scanning", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  
                  fileInput("file", 
                            "Choose File to Scan",
                            multiple = FALSE,
                            accept = NULL),
                  
                  br(),
                  
                  conditionalPanel(
                    condition = "output.fileUploaded",
                    actionBttn("scanBtn", 
                               "Scan File", 
                               style = "jelly", 
                               color = "primary",
                               icon = icon("search"))
                  )
                )
              ),
              
              fluidRow(
                conditionalPanel(
                  condition = "output.showResults",
                  box(
                    title = "Scan Results", 
                    status = "info", 
                    solidHeader = TRUE, 
                    width = 12,
                    
                    uiOutput("scanResults")
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Scan History", 
                  status = "success", 
                  solidHeader = TRUE, 
                  width = 12,
                  
                  DT::dataTableOutput("scanHistory")
                )
              )
      ),
      
      # System Info Tab
      tabItem(tabName = "info",
              fluidRow(
                box(
                  title = "ClamAV System Information", 
                  status = "info", 
                  solidHeader = TRUE, 
                  width = 12,
                  
                  verbatimTextOutput("clamavInfo")
                )
              ),
              
              fluidRow(
                box(
                  title = "Update Virus Definitions", 
                  status = "warning", 
                  solidHeader = TRUE, 
                  width = 12,
                  
                  p("Keep your virus definitions up to date for best protection."),
                  actionBttn("updateBtn", 
                             "Update Definitions", 
                             style = "jelly", 
                             color = "warning",
                             icon = icon("download")),
                  br(), br(),
                  verbatimTextOutput("updateOutput")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    scan_history = data.frame(
      Timestamp = character(),
      Filename = character(),
      Size = character(),
      Status = character(),
      Details = character(),
      stringsAsFactors = FALSE
    ),
    current_scan = NULL
  )
  
  # Check if file is uploaded
  output$fileUploaded <- reactive({
    return(!is.null(input$file))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)
  
  # Show results panel
  output$showResults <- reactive({
    return(!is.null(values$current_scan))
  })
  outputOptions(output, 'showResults', suspendWhenHidden = FALSE)
  
  # Scan file when button is clicked
  observeEvent(input$scanBtn, {
    req(input$file)
    
    withProgress(message = 'Scanning file...', value = 0, {
      incProgress(0.3, detail = "Preparing scan")
      
      file_path <- input$file$datapath
      file_name <- input$file$name
      file_size <- paste(round(input$file$size / 1024, 2), "KB")
      
      incProgress(0.5, detail = "Running ClamAV scan")
      
      # Perform the scan
      scan_result <- scan_file_with_clamav(file_path)
      
      incProgress(0.9, detail = "Processing results")
      
      # Store current scan result
      values$current_scan <- scan_result
      
      # Add to history
      new_row <- data.frame(
        Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        Filename = file_name,
        Size = file_size,
        Status = scan_result$status,
        Details = scan_result$details,
        stringsAsFactors = FALSE
      )
      
      values$scan_history <- rbind(new_row, values$scan_history)
      
      incProgress(1, detail = "Complete")
    })
    
    # Show notification
    if (values$current_scan$status == "CLEAN") {
      showNotification("File is clean!", type = "message", duration = 3)
    } else if (values$current_scan$status == "INFECTED") {
      showNotification("VIRUS DETECTED!", type = "error", duration = 10)
    } else {
      showNotification("Scan completed with warnings", type = "warning", duration = 5)
    }
  })
  
  # Display scan results
  output$scanResults <- renderUI({
    req(values$current_scan)
    
    result <- values$current_scan
    
    # Determine CSS class based on status
    css_class <- switch(result$status,
                        "CLEAN" = "result-clean",
                        "INFECTED" = "result-infected",
                        "ERROR" = "result-error")
    
    status_class <- switch(result$status,
                           "CLEAN" = "status-clean",
                           "INFECTED" = "status-infected", 
                           "ERROR" = "status-error")
    
    div(class = paste("scan-result", css_class),
        h4(span(class = status_class, result$status)),
        p(strong("Message: "), result$message),
        if (nchar(result$details) > 0) {
          div(
            strong("Details:"),
            pre(result$details)
          )
        }
    )
  })
  
  # Display scan history table
  output$scanHistory <- DT::renderDataTable({
    if (nrow(values$scan_history) == 0) {
      return(data.frame(Message = "No scans performed yet"))
    }
    
    # Format the table for display
    display_data <- values$scan_history[, c("Timestamp", "Filename", "Size", "Status")]
    
    DT::datatable(display_data, 
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE) %>%
      DT::formatStyle("Status",
                      backgroundColor = DT::styleEqual(
                        c("CLEAN", "INFECTED", "ERROR"),
                        c("#d4edda", "#f8d7da", "#fff3cd")
                      ))
  })
  
  # Display ClamAV system information
  output$clamavInfo <- renderText({
    info <- get_clamav_info()
    paste("ClamAV Version:", info$version, "\n\n",
          "Database Information:\n", info$database)
  })
  
  # Update virus definitions
  observeEvent(input$updateBtn, {
    withProgress(message = 'Updating virus definitions...', value = 0, {
      incProgress(0.5, detail = "Running freshclam")
      
      result <- tryCatch({
        system2("freshclam", stdout = TRUE, stderr = TRUE)
      }, error = function(e) {
        paste("Error:", e$message)
      })
      
      incProgress(1, detail = "Complete")
      
      output$updateOutput <- renderText({
        paste(result, collapse = "\n")
      })
      
      showNotification("Virus definitions update completed", type = "message")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)