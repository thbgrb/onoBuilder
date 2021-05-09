## ui of the shiny app

library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "The Observer Converter"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    box(title = "Import your data", status = "primary",
        fileInput("fileToRead", "Select your file", multiple = FALSE)
        ),
    box(title = "Your data imported",
        tableOutput("dataImported")),
    box(title = "Convert your data")
  )
)

shinyUI(ui)
