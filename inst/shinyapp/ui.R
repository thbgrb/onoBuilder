## ui of the shiny app

ui <- dashboardPage(
  dashboardHeader(title = "The Observer Converter"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    box(title = "Import your data", status = "primary"),
    box(title = "Your data imported"),
    box(title = "Convert your data")
  )
)

shinyUI(ui)
