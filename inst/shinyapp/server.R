### shiny app server

server <- function(input, output, session){
  
  #observe the import button
  observeEvent(input$import, {
    
    #reading and viewing the file imported
    output$dataImportedView <- renderDataTable({
      req(input$fileToRead) # a file is required
      df <- get_data_table(input$fileToRead, input$header, input$sep, input$quote)
      datatable(data = df)
    })
    
    #update items 
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
  
  #when click on next step conversion button
  observeEvent(input$buildStartStop, {
    req(input$fileToRead) # a file is required
    df <- get_data_table(input$fileToRead, input$header, input$sep, input$quote)
    
    SS_TABLE <<- build_start_stop_table(df,
                                        input$Event_Type,
                                        input$Time_Relative_sf,
                                        input$Observation,
                                        input$Subject,
                                        input$Behavior
    )
    
    #reading and viewing the file imported
    output$startStopView <- renderDataTable({
      datatable(data = SS_TABLE)
    })
    
    #finding all var to create global vectors
    OBSERVATIONS <<- get_all_observations_labels(SS_TABLE)
    updateCheckboxGroupInput(session, "selected.observations", choices = OBSERVATIONS)
  })
  
  observeEvent(input$selected.observations, {
    SUBJECTS <<- get_all_subjects_labels(SS_TABLE, input$selected.observations)
    updateCheckboxGroupInput(session, "selected.subjects", choices = SUBJECTS)
  })
  
  observeEvent(input$selected.subjects, {
    BEHAVIORS <<- get_all_behaviors_labels(SS_TABLE, input$selected.observations, input$selected.subjects)
    updateCheckboxGroupInput(session, "selected.behaviors", choices = BEHAVIORS)
  })
  
  #when click on conversion button
  observeEvent(input$runConvert, {
    
    for(o in input$selected.observations){
      for(s in input$selected.subjects){
        possibleError <- tryCatch({

          result <- SS_TABLE %>%
            filter(observation == o) %>%
            filter(subject == s) %>%
            filter(behavior %in% input$selected.behaviors)%>%
            build_ono_data(select_behavior = input$selected.behaviors) %>%
            write.csv(., paste0("export/ono-",
                                o,
                                s,
                                ".csv"))
        },
        error=function(e) {
          print(e)
        },
        warning=function(e){
          print(e)
        })
        if(inherits(possibleError, "error")) next
        if(inherits(possibleError, "warning")) next
        }
      }
    
    
  })
}

shinyServer(server)
