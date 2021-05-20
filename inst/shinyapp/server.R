library(dplyr)
library(tidyr)

### Define the shiny app server
server <- function(input, output, session) {
 
  showErrorPopup <- function(text){
    shinyalert(
      title = "Sorry, something went wrong ... ",
      text = text,
      size = "s", 
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      type = "error",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#F27474",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    ) 
  }
  
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

  output$isFileUploaded <- reactive({
    req(input$fileToRead)
  })
  outputOptions(output, "isFileUploaded", suspendWhenHidden = FALSE) 
  
  
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
    
    tryCatch({
      # Creation of the start/stop table
      ssTable <- buildStartStopTable(data = dataImported,
                                          Event_Type = input$Event_Type,
                                          Time_Relative_sf = input$Time_Relative_sf,
                                          Observation = input$Observation,
                                          Subject = input$Subject,
                                          Behavior = input$Behavior)
      
      # Update the view of the start/stop table
      output$ssTable <- renderDataTable({
        datatable(data = ssTable)
      })
      
      # Finding all observations in the start/stop table
      observations <- sort(unique(ssTable$observation))
      
      # Update the observations input
      updateCheckboxGroupInput(session = session,
                               inputId = "selected.observations",
                               choices = observations,
                               selected = observations)
      
      # Finding all subjects in the start/stop tablea ccording to the selected observations
      subjects <- sort(unique(ssTable$subject))
      
      # Update the subjects input
      updateCheckboxGroupInput(session = session,
                               inputId = "selected.subjects",
                               choices = subjects,
                               selected = subjects)
      
      # Finding all behaviors in the start/stop table according to the selected observations and subjects
      behaviors <- sort(unique(ssTable$behavior))
      
      # Update the behaviors input
      updateCheckboxGroupInput(session,
                               "selected.behaviors",
                               choices = behaviors,
                               selected = behaviors)
    },error = function(e){
      showErrorPopup("Your column names are probably incorrect.")
    })
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
      
      i<-0
      
      # A file is required
      req(input$fileToRead)
      
      # Creation of the start/stop table
      ssTable <- buildStartStopTable(data = dataImported,
                                     Event_Type = input$Event_Type,
                                     Time_Relative_sf = input$Time_Relative_sf,
                                     Observation = input$Observation,
                                     Subject = input$Subject,
                                     Behavior = input$Behavior)
      
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
            
            i <- i + 1 
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
      tryCatch({
        # Creating the zip file to download
        zip(zipfile = con, files = filestosave)
        
        shinyalert(
          title = paste0(i, " file(s) saved"),
          size = "s", 
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE,
          html = FALSE,
          type = "success",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = "#00A659",
          timer = 0,
          imageUrl = "",
          animation = TRUE
        )
      },error = function(e) {
        showErrorPopup("Check filters and try again.")
      })
      
    },
    contentType = "application/zip"
  )
}

shinyServer(server)
