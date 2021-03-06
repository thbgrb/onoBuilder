# Importing libraries
library(shiny)
library(shinydashboard)
library(DT)
library(shinyalert)

## Define the shiny app ui
ui <- dashboardPage(
  
  # Skin of the page
  skin = "yellow",
  
  # Header of the ui
  dashboardHeader(title = tagList(icon("sunglasses",  lib = "glyphicon"),"onoBuilder"),
                  titleWidth = 300),
  
  # Sidebar of the ui (hidden)
  dashboardSidebar(disable = TRUE),
  
  # Content of the page
  dashboardBody(
    
    # To use notification in this page
    useShinyalert(),
    
    # First line of boxes
    fluidRow(
      box(
        width = 12,
        solidHeader = TRUE,
        collapsible = TRUE,
        status = "primary",
        title = tagList(icon("arrow-up", lib = "glyphicon"),
                        "STEP 1: Import your data"),
        column(
          width = 3,
          
          # Box for importing a file
          box(
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            
            # Choose the file to import
            fileInput(
              "fileToRead",
              "Choose a CSV or an EXCEL file:",
              multiple = FALSE,
              accept = c(".csv", ".xlsx", "xls")
            ),
          ),
          
          # Inputs for config csv import showed only for csv files
          conditionalPanel(
            condition = "output.isCsvFile",
            box(
              width = NULL,
              status = "primary",
              solidHeader = TRUE,
              
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
          )
        ),
        
        # Box for viewing of the imported file raw
        conditionalPanel(
          condition = "output.isFileUploaded",
          box(
            width = 9,
            status = "primary",
            solidHeader = TRUE,
            div(style = 'overflow-x: scroll', dataTableOutput('dataImportedView'))
          )
        ) ,
      )
    ),
    
    # Second line of boxes
    fluidRow(conditionalPanel(
      condition = "output.isFileUploaded",
      box(
        width = 12,
        title = tagList(
          icon("ok", lib = "glyphicon"),
          "STEP 2: Check column names to create the start/stop table"
        ),
        solidHeader = TRUE,
        collapsible = TRUE,
        status = "success",
        
        # Box for choosing columns name
        box(
          width = 3,
          solidHeader = TRUE,
          status = "success",
          selectInput("Event_Type", label = "Event Type:", choices = NULL),
          selectInput("Time_Relative_sf", label = "Time Relative:", choices = NULL),
          selectInput("Observation", label = "Observation:", choices = NULL),
          selectInput("Subject", label = "Subject:", choices = NULL),
          selectInput("Behavior", label = "Behavior:", choices = NULL),
          actionButton(
            inputId = "buildStartStop",
            label = tagList("Next step", icon("chevron-right", lib =
                                                "glyphicon")),
            style = "float : right;
                                font-weight : bold"
          ),
        ),
        
        
        # Box for viewing the start/stop table created
        conditionalPanel(
          condition = "output.isSsTableCreated",
          box(
            width = 9,
            status = "success",
            solidHeader = TRUE,
            div(style = 'overflow-x: scroll', dataTableOutput('ssTable'))
          )
        ),
      ),
    )),
    
    # Third line of boxes
    fluidRow(
      conditionalPanel(
        condition = "output.isSsTableCreated",
        
        # Box for applying filters on start/stop table and download ono files
        box(
          collapsible = TRUE,
          width = 12,
          title = tagList(
            icon("arrow-down", lib = "glyphicon"),
            "STEP 3: Filter your items and get your ono tables"
          ),
          status = "warning",
          solidHeader = TRUE,
          
          box(
            solidHeader = TRUE,
            width = 3,
            status = "warning",
            checkboxGroupInput(inputId = "selected.observations", 
                               label = "Observations:", 
                               choices = c())
          ),
          box(
            solidHeader = TRUE,
            width = 3,
            status = "warning",
            checkboxGroupInput(inputId = "selected.subjects", 
                               label = "Subjects:", 
                               choices = c())
          ),
          box(
            solidHeader = TRUE,
            width = 3,
            status = "warning",
            checkboxGroupInput(inputId = "selected.behaviors", 
                               label = "Behaviors:", 
                              choices = c())
          ),
          box(
            width = 3,
            solidHeader = TRUE,
            status = "warning",
            background = "yellow",
            h5("Make sure you have selected all the filters your want, and click 
              on the button below to save the ono files on your computer"),
            hr(),
            downloadButton(outputId = "downloadOno",
                           icon = NULL,
                           label = tagList("Save your files", " ", icon("ok", lib="glyphicon")), 
                           style = "font-weight:bold; float:right")
          )
        )
        
      ))
  ),
  title = "onoBuilder"
)

shinyUI(ui)
