library(shiny)
library(shinydashboard)
library(DT)

## Define the shiny app ui
ui <- dashboardPage(
  # Skin of the page
  skin = "yellow",
  
  # Header of the ui
  dashboardHeader(title = "The Observer Converter",
                  titleWidth = 300),
  
  # Sidebar of the ui (hidden)
  dashboardSidebar(disable = TRUE),
  
  # Content of the page
  dashboardBody(
    
    # Widget for building ono files
    conditionalPanel(condition = "input.buildStartStop",
                     fluidRow(
                       box(
                         title = "Choose your items to build your ono files",
                         width=12,
                         status = "warning",
                         solidHeader = TRUE,
                         column(width = 3,
                                checkboxGroupInput("selected.observations", "Observations:", c())),
                         column(width = 3,
                                checkboxGroupInput("selected.subjects", "Subjects:", c())),
                         column(width = 3,
                                checkboxGroupInput("selected.behaviors", "Behaviors:", c())),
                         column(width = 3,
                                downloadButton("downloadOno", "Convert & Download"))
                         )
                       )
                     ),
    fluidRow(
      column(
        width = 4,
        
        # Widget for import a file
        box(
          width = NULL,
          title = "Import your data",
          status = "primary",
          solidHeader = TRUE,
          
          # Choose the file to import
          fileInput(
            "fileToRead",
            "Select your file",
            multiple = FALSE,
            accept = c(".csv", ".xlsx", "xls")
          ),
          
          # Inputs for config csv import showed only for csv files
          conditionalPanel(
            condition = "output.isCsvFile",
            
            hr(),
            
            # Choose if the file imported has a header
            checkboxInput("header", "Header", TRUE),
            
            # Choose the separator in the file imported
            radioButtons(
              inputId = "sep",
              label = "Separator",
              choices = c(
                Comma = ",",
                Semicolon = ";",
                Tab = "\t"
              ),
              selected = ","
            ),
            
            # Choose the quote in the file imported
            radioButtons(
              "quote",
              "Quote",
              choices = c(
                None = "",
                "Double Quote" = '"',
                "Single Quote" = "'"
              ),
              selected = '"'
            ),
          )
        ),
        
        
        # Widget of start/stop table builder
        box(
          width = NULL,
          title = "Build the start/stop table",
          solidHeader = TRUE,
          status = "success",
          selectInput("Event_Type", label = "Event Type:", choices = NULL),
          selectInput("Time_Relative_sf", label = "Time Relative:", choices = NULL),
          hr(),
          selectInput("Observation", label = "Observation:", choices = NULL),
          selectInput("Subject", label = "Subject:", choices = NULL),
          selectInput("Behavior", label = "Behavior:", choices = NULL),
          actionButton("buildStartStop", label = "Create Start Stop Table"),
        ),
        
      ),
      column(
        width = 8,
        
        ## Widget for the view of the imported file
        box(
          width = NULL,
          title = "Your data imported",
          solidHeader = TRUE,
          status = "primary",
          div(style = 'overflow-x: scroll', dataTableOutput('dataImportedView'))
        ),
        
        ## Widget for the view of the start/stop table
        box(
          width = NULL,
          status = "success",
          solidHeader = TRUE,
          title = "The Start/Stop table",
          div(style = 'overflow-x: scroll', dataTableOutput('startStopView'))
        )
      ),
      title = "The Observer Converter"
    )
  )
)

shinyUI(ui)
