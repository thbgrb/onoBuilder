### shiny app server
server <- function(input, output, session){
  
  ## read and view of the file imported
  output$dataImported <- renderDataTable({
    req(input$fileToRead) # a file is required
    tryCatch({
      df <- read.csv(input$fileToRead$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote)
      datatable(data = df)
      
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    })
  })
  
  observe({
    req(input$fileToRead) # a file is required
    tryCatch({
      df <- read.csv(input$fileToRead$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote)
      updateSelectInput(session, "select_time", choice=colnames(df))
      updateSelectInput(session, "select_event_type", choice=colnames(df))
      updateSelectInput(session, "select_group", choice=colnames(df))
      updateSelectInput(session, "select_subject", choice=colnames(df))
      updateSelectInput(session, "select_behavior", choice=colnames(df))
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    })
  })
  
}

shinyServer(server)
