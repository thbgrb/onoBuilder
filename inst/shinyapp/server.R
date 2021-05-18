### shiny app server

library(dplyr)
library(tidyr)

server <- function(input, output, session){
  
  updateConfigCsv <- function(){
    #update items 
    updateSelectInput(session, "Event_Type", 
                      choices = colnames(DF), selected = "Event_Type")
    updateSelectInput(session, "Time_Relative_sf", 
                      choices = colnames(DF), selected = "Time_Relative_sf")
    updateSelectInput(session, "Observation",
                      choices = colnames(DF), selected = "Observation")
    updateSelectInput(session, "Subject", 
                      choices = colnames(DF), selected = "Subject")
    updateSelectInput(session, "Behavior",
                      choices = colnames(DF), selected = "Behavior")
  }
  
  output$cond <- reactive({
    req(input$fileToRead) # a file is required
    fileExtension = tolower(substr(input$fileToRead$name, 
                                   nchar(input$fileToRead$name) - 3,
                                   nchar(input$fileToRead$name)))
    (fileExtension == ".csv")
  })
  outputOptions(output, "cond", suspendWhenHidden = FALSE)
  
  #reading and viewing the file imported
  observe(output$dataImportedView <- renderDataTable({
    req(input$fileToRead) # a file is required
    DF <<- get_data_table(input$fileToRead, input$header, input$sep, input$quote)
    updateConfigCsv()
    datatable(data = DF)
  }))
  
  #observe the import button
  observeEvent(c(input$header, input$sep, input$quote), {
    req(input$fileToRead)
    updateConfigCsv()
  })
  
  #when click on next step conversion button
  observeEvent(input$buildStartStop, {
    req(input$fileToRead) # a file is required
    #df <- get_data_table(input$fileToRead, input$header, input$sep, input$quote)
    
    SS_TABLE <<- build_start_stop_table(DF,
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
    updateCheckboxGroupInput(session, "selected.observations", choices = OBSERVATIONS, selected = OBSERVATIONS)
  })
  
  observeEvent(input$selected.observations, {
    SUBJECTS <<- get_all_subjects_labels(SS_TABLE, input$selected.observations)
    updateCheckboxGroupInput(session, "selected.subjects", choices = SUBJECTS, selected = SUBJECTS)
  })
  
  observeEvent(input$selected.subjects, {
    BEHAVIORS <<- get_all_behaviors_labels(SS_TABLE, input$selected.observations, input$selected.subjects)
    updateCheckboxGroupInput(session, "selected.behaviors", choices = BEHAVIORS, selected = BEHAVIORS)
  })
  
  #when click on conversion button
  observeEvent(input$runOno, {

    for(o in input$selected.observations){
      for(s in input$selected.subjects){
        possibleError <- tryCatch({
          
          result <- SS_TABLE %>%
            filter(observation == o) %>%
            filter(subject == s) %>%
            filter(behavior %in% input$selected.behaviors)%>%
            build_ono_data() %>%
            write.csv(., paste0("ono-",
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
