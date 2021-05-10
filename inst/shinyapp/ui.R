## ui of the shiny app

library(shiny)
library(shinydashboard)
library(DT)

ui <- dashboardPage(
  
  dashboardHeader(title = "The Observer Converter", titleWidth = 300),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    
    ## box for the import
    box(title = "Import your data", status = "primary",
        
        # choose the file to import
        fileInput("fileToRead", "Select your file", multiple = FALSE),
        
        hr(),
        
        # select if the file imported has a header
        checkboxInput("header", "Header", TRUE),
        
        # select separator in the file imported
        radioButtons("sep", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = ","),
        
        # select quote in the file imported
        radioButtons("quote", "Quote",
                     choices = c(None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"),
                     selected = '"')
        ),
    
    ## box for the view
    box(title = "Your data imported",
        div(style = 'overflow-x: scroll', dataTableOutput('dataImported'))
    ),
    
    ## box for the conversion
    conditionalPanel(
      condition = "output.dataImported",
      box(title = "Convert your data",
        selectInput("select_time", label = "Time:", choices = NULL),
        selectInput("select_event_type", label = "Event type:", choices = NULL),
        selectInput("select_group", label = "Group:", choices = NULL),
        selectInput("select_subject", label = "Subject:", choices = NULL),
        selectInput("select_behavior", label = "Behavior:", choices = NULL),
        actionButton("runConvert", label = "Convert !")
        )
    )
  ),
  title = "The Observer Converter"
)

shinyUI(ui)
