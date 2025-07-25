library(shiny)

ui <- fluidPage(
  fileInput("file", "Upload a file"),
  verbatimTextOutput("info")
)

server <- function(input, output, session) {
  observeEvent(input$file, {
    # Copy uploaded file to tempdir() with a fixed name
    tmp_path <- file.path(tempdir(), paste0("copy_", input$file$name))
    file.copy(input$file$datapath, tmp_path, overwrite = TRUE)
    
    message("Copied uploaded file to: ", tmp_path)
    
    # Schedule deletion after 2 minutes (120 seconds)
    later::later(function() {
      if (file.exists(tmp_path)) {
        file.remove(tmp_path)
        message("Deleted file after 2 minutes: ", tmp_path)
      }
    }, delay = 120)
  })
  
  output$info <- renderPrint({
    req(input$file)
    
    cat("Original upload path:\n")
    print(input$file$datapath)
    
    cat("\nCopied to tempdir() as:\n")
    print(file.path(tempdir(), paste0("copy_", input$file$name)))
    
    cat("\nListing contents of tempdir():\n")
    print(list.files(tempdir(), full.names = TRUE))
  })
}

shinyApp(ui, server)
