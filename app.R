library(shiny)

ui <- fluidPage(
  fileInput("file", "Upload a file"),
  verbatimTextOutput("info")
)

server <- function(input, output) {
  output$info <- renderPrint({
    req(input$file)
    
    cat("File uploaded to temp path:\n")
    print(input$file$datapath)

    cat("\nListing tempdir():\n")
    print(list.files(tempdir(), full.names = TRUE))
  })
}

shinyApp(ui, server)
