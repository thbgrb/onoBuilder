## ui of the shiny app

ui <- dashboardPage(
  dashboardHeader(title = "The Observer Converter", titleWidth = 300),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    box(title = "Import your data", status = "primary",
        fileInput("fileToRead", "Select your file", multiple = FALSE)
        ),
    box(title = "Your data imported",
        div(style = 'overflow-x: scroll', DT::dataTableOutput('dataImported'))
        ),
    box(title = "Convert your data")
  ),
  title = "The Observer Converter"
)

shinyUI(ui)
