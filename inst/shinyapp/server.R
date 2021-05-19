library(dplyr)
library(tidyr)

### Define the shiny app server
server <- function(input, output, session) {
  
  options(DT.options = list(pageLength = 5))
  
  ### Function which update columns names inputs
  updateColNamesInput <- function() {
    updateSelectInput(session = session,
                      inputId = "Event_Type",
                      choices = colnames(dataImported),
                      selected = "Event_Type")
    updateSelectInput(session = session,
                      inputId = "Time_Relative_sf",
                      choices = colnames(dataImported),
                      selected = "Time_Relative_sf")
    updateSelectInput(session = session,
                      inputId = "Observation",
                      choices = colnames(dataImported),
                      selected = "Observation")
    updateSelectInput(session = session,
                      inputId = "Subject",
                      choices = colnames(dataImported),
                      selected = "Subject")
    updateSelectInput(session = session,
                      inputId = "Behavior",
                      choices = colnames(dataImported),
                      selected = "Behavior")
  }
  
  
  
  ### Check if the file is in csv format
  output$isCsvFile <- reactive({
    # A file is required
    req(input$fileToRead)
    
    # Extract file extension (4 characters)
    fileExtension = tolower(substr(input$fileToRead$name,
                                   nchar(input$fileToRead$name) - 3,
                                   nchar(input$fileToRead$name)))
    
    # Check the extension
    (fileExtension == ".csv") 
  })
  
  # Keep the boolean value
  outputOptions(output, "isCsvFile", suspendWhenHidden = FALSE) 

  
  
  ### When a file is inputted
  observe(output$dataImportedView <- renderDataTable({
    # A file is required
    req(input$fileToRead)
   
    # Creation of the data file
    dataImported <<- importDataTable(inFile = input$fileToRead, 
                                    header = input$header,
                                    sep = input$sep, 
                                    quote = input$quote)
    
    # Updating the columns names input for the future configuration
    updateColNamesInput()
    
    # Update the view of the data file
    datatable(data = dataImported)
  }))


  
  ### When click on a config csv input
  observeEvent(c(input$header, input$sep, input$quote), {
    # A file is required
    req(input$fileToRead)
    
    # Updating the columns names input for the future configuration
    updateColNamesInput()
  })


  
  ## When click on the build start stop table button
  observeEvent(input$buildStartStop, {
    # A file is required
    req(input$fileToRead)
    
    # Creation of the start/stop table
    ssTable <<- buildStartStopTable(data = dataImported,
                                        Event_Type = input$Event_Type,
                                        Time_Relative_sf = input$Time_Relative_sf,
                                        Observation = input$Observation,
                                        Subject = input$Subject,
                                        Behavior = input$Behavior)
    
    # Update the view of the start/stop table
    output$startStopView <- renderDataTable({
      datatable(data = ssTable)
    })
    
    # Finding all observations in the start/stop table
    OBSERVATIONS <<- getObservationsLabels(data = ssTable)
    
    # Update the observations input
    updateCheckboxGroupInput(session = session,
                             inputId = "selected.observations",
                             choices = OBSERVATIONS,
                             selected = OBSERVATIONS)
  })

  
  
  ### When click on an observation in the items list
  observeEvent(input$selected.observations, {
    # Finding all subjects in the start/stop tablea ccording to the selected observations
    SUBJECTS <<- getSubjectsLabels(data = ssTable,
                                         observation_column = input$selected.observations)
    
    # Update the subjects input
    updateCheckboxGroupInput(session = session,
                             inputId = "selected.subjects",
                             choices = SUBJECTS,
                             selected = SUBJECTS)
  })

  
  
  ### When click on a subject in the items list
  observeEvent(input$selected.subjects, {
    # Finding all behaviors in the start/stop table according to the selected observations and subjects
    BEHAVIORS <<- getBehaviorsLabels(data = ssTable,
                                           observation_column = input$selected.observations,
                                           subject_column = input$selected.subjects)
    
    # Update the behaviors input
    updateCheckboxGroupInput(session,
                             "selected.behaviors",
                             choices = BEHAVIORS,
                             selected = BEHAVIORS)
  })

  
  ### When click on the downloadOno button
  output$downloadOno <- downloadHandler(
    # Name of the zip file exported
    filename = function() {
      # Format date and time of now
      time <- format(Sys.time(), "%Y%m%d-%H%M%S")
      # Naming the zip file
      paste0("All-ono-", time, ".zip")
    },
    
    # Content of the zip file exported
    content = function(con) {
      # Save csv files in the temp directory
      tmpdir <- tempdir()
      setwd(tempdir())
      filestosave = c()
      
      # Browsing all observations and all subjects in the start/stop table
      for (o in input$selected.observations) {
        for (s in input$selected.subjects) {
          
          possibleError <- tryCatch({
            
            # Building the ono table for the Observation 'o' and the Subject 's'
            onoTable <- ssTable %>%
              filter(observation == o) %>%
              filter(subject == s) %>%
              filter(behavior %in% input$selected.behaviors) %>%
              buildOnoTable()
            
            # Creating the name of the ono table
            onoName <- paste0("ono-", o, s, ".csv")
            
            # Writing the csv file
            write.csv(onoTable, onoName)
            
            # Adding the path of the csv file to the zip
            filestosave <- c(filestosave, onoName)
          },
          error = function(e) {
            print(e)
          },
          warning = function(e) {
            print(e)
          })
          if (inherits(possibleError, "error"))
            next
          if (inherits(possibleError, "warning"))
            next
        }
      }
      # Creating the zip file to download
      zip(zipfile = con, files = filestosave)
    },
    contentType = "application/zip"
  )
}

shinyServer(server)
