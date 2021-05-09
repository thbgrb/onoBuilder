## server of the shiny app

server <- function(input, output){
  
  ## read and show the csv data imported
  output$dataImported<-DT::renderDataTable({
    req(input$fileToRead)
    df <- read.csv(input$fileToRead$datapath)
    datatable(data = df)
  })
  
  
}

shinyServer(server)
