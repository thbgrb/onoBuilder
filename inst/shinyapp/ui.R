## ui of the shiny app

library(shiny)
library(shinydashboard)
library(DT)

ui <- dashboardPage(
  
  #header of the page
  dashboardHeader(title = "The Observer Converter", titleWidth = 300),
  
  #sidebar of the bar (hidden)
  dashboardSidebar(disable = TRUE),
  
  #content of the page
  dashboardBody(fluidRow(
    column(width = 4,
      ## box for the import
      box(width = NULL,
          title = "Import your data",
          status = "primary",
        # choose the file to import
        fileInput("fileToRead", 
                  "Select your file",
                  multiple = FALSE,accept = c(".csv", ".xlsx", "xls")
                  ),
        conditionalPanel(
          condition = "output.cond == true",
          
          hr(),
          
          # select if the file imported has a header
          checkboxInput("header", "Header", TRUE),
          
          # select separator in the file imported
          radioButtons(
            "sep",
            "Separator",
            choices = c(
              Comma = ",",
              Semicolon = ";",
              Tab = "\t"
            ),
            selected = ","
          ),
          
          # select quote in the file imported
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
        
      )),
      
      ## box for the conversion
      box(width = NULL,
        title = "Convert your data",
        selectInput("Event_Type", label = "Event Type:", choices = NULL),
        selectInput("Time_Relative_sf", label = "Time Relative:", choices = NULL),
        hr(),
        selectInput("Observation", label = "Observation:", choices = NULL),
        selectInput("Subject", label = "Subject:", choices = NULL),
        selectInput("Behavior", label = "Behavior:", choices = NULL),
        actionButton("buildStartStop", label = "Create Start Stop Table"),
      ),
      conditionalPanel(
        condition = "input.buildStartStop",
        box(width = NULL,
          checkboxGroupInput("selected.observations", "Observations:", c()),
          checkboxGroupInput("selected.subjects", "Subjects:", c()),
          checkboxGroupInput("selected.behaviors", "Behaviors:", c()),
          actionButton("runOno", label = "Convert")
        )
      )
    ),
    column(width = 8,
      ## box for the view of the imported file
      box(width = NULL,
        title = "Your data imported",
        div(style = 'overflow-x: scroll', dataTableOutput('dataImportedView'))
      ),
      ## box for the view of the start/stop table
      box(width = NULL,
          title = "The Start/Stop table",
          div(style = 'overflow-x: scroll', dataTableOutput('startStopView'))
      )
    ),
  title = "The Observer Converter"
  ))
)

shinyUI(ui)
