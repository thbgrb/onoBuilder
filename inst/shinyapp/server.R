### shiny app server

library(dplyr)
library(tidyr)

server <- function(input, output, session){
  
  ## read and view of the file imported
  output$dataImported <- renderDataTable({
    
    req(input$fileToRead) # a file is required
    df <- get_data_table(input$fileToRead, input$header, input$sep, input$quote)
    
    datatable(data = df)
  })
  
  observe({
    
    req(input$fileToRead) # a file is required
    df <- get_data_table(input$fileToRead, input$header, input$sep, input$quote)
    
    updateSelectInput(session, "Event_Type", 
                      choices = colnames(df), selected = "Event_Type")
    updateSelectInput(session, "Time_Relative_sf", 
                      choices = colnames(df), selected = "Time_Relative_sf")
    updateSelectInput(session, "Observation",
                      choices = colnames(df), selected = "Observation")
    updateSelectInput(session, "Subject", 
                      choices = colnames(df), selected = "Subject")
    updateSelectInput(session, "Behavior",
                      choices = colnames(df), selected = "Behavior")
    
  })
  
  observeEvent(input$runConvert, {
    req(input$fileToRead) # a file is required
    df <- get_data_table(input$fileToRead, input$header, input$sep, input$quote)
    result <- build_start_stop_table(df,
                           input$Event_Type,
                           input$Time_Relative_sf,
                           input$Observation,
                           input$Subject,
                           input$Behavior
                           )
    
    View(result)
    
    #finding all groups to create a vector
    groups <- c()
    for(b in result$observation){
      if((b %in% groups)==0){
        groups <- c(groups, b)
      }
    }
    
    #finding all subjects to create a vector
    subjects <- c()
    for(s in result$subject){
      if((s %in% subjects)==0){
        subjects <- c(subjects, s)
      }
    }
    
    #finding all behaviors to create a vector
    behaviors <- c()
    for(b in result$behavior){
      if((b %in% behaviors)==0){
        behaviors <- c(behaviors, b)
      }
    }
    
  })
 
}

shinyServer(server)
