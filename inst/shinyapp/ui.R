library(shiny)
library(shinydashboard)
library(DT)

## Define the shiny app ui
ui <- dashboardPage(
                    
  # Skin of the page
  skin = "yellow",
  
  # Header of the ui
  dashboardHeader(title = tagList(icon("sunglasses", lib="glyphicon"), "onoBuilder"),
                  titleWidth = 300),
  
  # Sidebar of the ui (hidden)
  dashboardSidebar(disable = TRUE),
  
  # Content of the page
  dashboardBody(
    
    # Widget for building ono files
    conditionalPanel(condition = "input.buildStartStop",
                     fluidRow(
                       box(
                         title = tagList(icon("screenshot", lib="glyphicon"), "Filter your items before building your ono tables"),
                         width=12,
                         status = "warning",
                         solidHeader = FALSE,
                         column(width = 3,
                                box(title=tagList(icon("sunglasses", lib="glyphicon"), "Observations:"),
                                    solidHeader = TRUE,
                                    width=NULL, 
                                    status="warning", 
                                    collapsible = TRUE, 
                                    collapsed = TRUE,
                                    checkboxGroupInput("selected.observations", "", c())
                                )),
                         column(width = 3,
                                box(title=tagList(icon("sunglasses", lib="glyphicon"), "Subjects:"),
                                    solidHeader = TRUE,
                                    width=NULL, 
                                    status="warning", 
                                    collapsible = TRUE,
                                    collapsed = TRUE,
                                    checkboxGroupInput("selected.subjects", "", c())
                                )),
                         column(width = 3,
                                box(title=tagList(icon("sunglasses", lib="glyphicon"), "Behaviors:"),
                                    solidHeader = TRUE,
                                    width=NULL, 
                                    status="warning", 
                                    collapsible = TRUE,
                                    collapsed = TRUE,
                                    checkboxGroupInput("selected.behaviors", "", c())
                                )),
                         column(width = 3,
                                fluidRow(
                                  valueBoxOutput("progressBox", width = NULL),
                                ),
                                fluidRow(
                                  downloadButton("downloadOno", "Convert & Download")
                                )
                         )
                         )
                       )
                     ),
    fluidRow(
        
        ## Widget for the view of the start/stop table
        conditionalPanel(
          condition = "input.buildStartStop",
          box(
            width = 12,
            status = "success",
            solidHeader = TRUE,
            title = tagList(icon("eye-open", lib="glyphicon"), "The Start/Stop table created"),
            div(style = 'overflow-x: scroll', dataTableOutput('startStopView'))
          )
      )
    ),
    fluidRow(
           # Widget for import a file
           box(
             width = 3,
             title = tagList(icon("circle-arrow-up", lib="glyphicon"), "Import your data"),
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
           
           ## Widget for the view of the imported file
           conditionalPanel(
             condition = "output.isFileUploaded",
             box(
               width = 6,
               title = tagList(icon("eye-open", lib="glyphicon"), "This is your file imported"),
               solidHeader = TRUE,
               status = "primary",
               div(style = 'overflow-x: scroll', dataTableOutput('dataImportedView'))
             )
           ) ,
           
           # Widget of start/stop table builder
           conditionalPanel(
             condition = "output.isFileUploaded",
             box(
               width = 3,
               title = tagList(icon("ok", lib="glyphicon"), "Check column names"),
               solidHeader = TRUE,
               status = "success",
               selectInput("Event_Type", label = "Event Type:", choices = NULL),
               selectInput("Time_Relative_sf", label = "Time Relative:", choices = NULL),
               hr(),
               selectInput("Observation", label = "Observation:", choices = NULL),
               selectInput("Subject", label = "Subject:", choices = NULL),
               selectInput("Behavior", label = "Behavior:", choices = NULL),
               actionButton(inputId = "buildStartStop", 
                            label = tagList("Next step", icon("chevron-right", lib="glyphicon")),
                            style="float : right;
                                font-weight : bold"), 
             )
           )
           
    )
  ),
  title = "onoBuilder"
)

shinyUI(ui)
