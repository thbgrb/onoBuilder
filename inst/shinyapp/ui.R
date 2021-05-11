## ui of the shiny app

library(shiny)
library(shinydashboard)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title = "The Observer Converter", titleWidth = 300),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    
    ## box for the import
    box(
      title = "Import your data",
      status = "primary",
      
      # choose the file to import
      fileInput("fileToRead", "Select your file", multiple = FALSE),
      
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
      )
    ),
    
    ## box for the view
    box(title = "Your data imported",
        div(style = 'overflow-x: scroll', dataTableOutput('dataImported'))),
    
    ## box for the conversion
    conditionalPanel(
      condition = "output.dataImported",
      box(
        title = "Convert your data",
        selectInput("Event_Type", label = "Event Type:", choices = NULL),
        selectInput("Time_Relative_sf", label = "Time Relative:", choices = NULL),
        hr(),
        selectInput("Observation", label = "Observation:", choices = NULL),
        selectInput("Subject", label = "Subject:", choices = NULL),
        selectInput("Behavior", label = "Behavior:", choices = NULL),
        hr(),
        actionButton("runConvert", label = "Convert !")
      )
    )
  ),
  title = "The Observer Converter"
)

shinyUI(ui)
