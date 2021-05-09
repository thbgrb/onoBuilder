## server of the shiny app

server <- function(input, output){
  
  ## read and show the csv data imported
  output$dataImported <- renderTable({
    req(input$fileToRead)
    df <- read.csv(input$fileToRead$datapath)
  })
  
  
}

shinyServer(server)
