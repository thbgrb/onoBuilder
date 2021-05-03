library(shiny)

ui <- fluidPage(titlePanel("Frequency Profile"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Convert into csv quantitative",
           tags$h4("Convert qualitative csv into quantiative csv"),
           tags$p("Here you can import a csv file with behaviors in the form of qualitative variables"),         
        ),
        tabPanel("Create FreqProf plot",
                 tags$h4("Create frequency profile plot"),
                 tags$p("Here you can import a csv file in the form of quantitative variables to analysis data."),
          fileInput('file', 'Choose File',
                    accept = c('.csv', '.bin', '.fpw')),
          tags$hr(),
          checkboxGroupInput("selected.behaviors", "Behaviors:",
                             c()),
          tags$hr(),
          checkboxInput('ggplot', 'ggplot', FALSE),
          helpText('Note: packages "ggplot2", "reshape2", and "grid" are required.'),
          checkboxInput('panel.in', 'Show left panel', TRUE),
          checkboxInput('panel.out', 'Show right panel', TRUE),
          checkboxInput('multiplot', 'Multiplots', FALSE),
          tags$hr(),
          radioButtons(
            'which',
            'Moving function',
            c('Sum' = 'sum',
              'Proportion' = 'proportion'),
            'proportion'
          ),
          numericInput(
            "window",
            "Window length:",
            min = 1,
            max = 100,
            value = 25,
            step = 1
          ),
          radioButtons('unit_length', 'length unit:',
                       c('%' = 'percent',
                         'bins' = 'bins')),
          numericInput(
            "step",
            "Step:",
            min = 1,
            max = 100,
            value = 1,
            step = 1
          ),
          numericInput(
            "resolution",
            "Resolution:",
            min = 1,
            max = 10,
            value = 1,
            step = 1
          ),
          textInput("units", "Time units:", value = "sec", width = NULL),
          numericInput(
            "tick.every",
            "Tick every:",
            min = 1,
            max = 10,
            value = 1,
            step = 1
          ),
          numericInput(
            "label.every",
            "Label every:",
            min = 1,
            max = 10,
            value = 1,
            step = 1
          ),
          tags$hr(),
          downloadButton('downloadData', 'Download Data'),
          tags$hr(),
          downloadButton('downloadPlotPDF', 'Download Plot as PDF'),
          downloadButton('downloadPlotPNG', 'Download Plot as PNG'),
          numericInput(
            "graphWidth",
            "Width (inches):",
            min = 1,
            max = 20,
            value = 10,
            step = 1
          ),
          numericInput(
            "graphHeight",
            "Height (inches):",
            min = 1,
            max = 20,
            value = 8,
            step = 1
          )
        )
      )
    ),
    
    mainPanel(plotOutput("distPlot", height = "500px"))
    
  ))

server <- function(input, output, session) {
  getDataFromShiny = function(inFile) {
    if (is.null(inFile))
      return(NULL)
    
    # reading a file, whose extension is either csv, bin or fpw,
    # and importing it as a data.frame
    filename = inFile$name
    
    file.extension = tolower(substr(filename, nchar(filename) - 2, nchar(filename)))
    
    data.behavior = switch(
      file.extension,
      csv = read.csv(inFile$datapath),
      bin = read.bin(inFile$datapath),
      fpw = read.fpw(inFile$datapath)
    )
    
    if (is.null(data.behavior))
      stop("file extension must be either csv, fpw, or bin")
    
    # update selected behaviors
    updateCheckboxGroupInput(
      session,
      "selected.behaviors",
      choices = names(data.behavior),
      selected = input$selected.behaviors
    )
    
    data.behavior = data.behavior[, names(data.behavior) %in% input$selected.behaviors]
    
    if (is.null(ncol(data.behavior))) {
      # this means that only one behavior is selected
      dat = as.data.frame(data.behavior)
      names(dat) = input$selected.behaviors
      return(dat)
    }
    
    if (ncol(data.behavior) > 1)
      return(data.behavior)
    
    return(NULL)
  }
  
  getWindowLength = function(unit, window, data) {
    if (unit == "bins")
      return(window)
    else
      return(round(window / 100 * nrow(data)))
  }
  
  output$distPlot <- renderPlot({
    data.behavior = getDataFromShiny(input$file)
    if (is.null(data.behavior))
      return(NULL)
    
    data.freqprof = freqprof(
      data.behavior,
      window = getWindowLength(input$unit_length, input$window, data.behavior),
      step = input$step,
      resolution = input$resolution,
      which = input$which
    )
    
    # plotting
    plot_freqprof(
      data.freqprof,
      gg = input$ggplot,
      panel.in = input$panel.in,
      panel.out = input$panel.out,
      multiPlot = input$multiplot,
      xAxisUnits = input$units,
      tick.every = input$tick.every,
      label.every = input$label.every
    )
  })
  
  observe({
    data.behavior = getDataFromShiny(input$file)
    if (is.null(data.behavior))
      return(NULL)
    
    # update range for window length
    if (input$unit_length == "bins") {
      win = round(.25 * nrow(data.behavior))
      updateSliderInput(
        session,
        "window",
        value = win,
        min = 1,
        max = 4 * win,
        step = 1
      )
    }
    if (input$unit_length == "percent") {
      updateSliderInput(
        session,
        "window",
        value = 25,
        min = 1,
        max = 100,
        step = 1
      )
    }
    
    # update tick.every and label.every
    t.every = round(nrow(data.behavior) / 31)
    updateSliderInput(
      session,
      "tick.every",
      value = t.every,
      min = 1,
      max = nrow(data.behavior),
      step = 1
    )
    updateSliderInput(
      session,
      "label.every",
      value = 3,
      min = 1,
      max = 100,
      step = 1
    )
  })
  
  output$downloadData <- downloadHandler(
    filename = "freqprof.csv",
    content = function(file) {
      data.behavior = getDataFromShiny(input$file)
      if (is.null(data.behavior))
        return(NULL)
      
      data.freqprof = freqprof(
        data.behavior,
        window = input$window,
        step = input$step,
        resolution = input$resolution,
        which = input$which
      )
      
      # which panels will be downloaded?
      panels = c(2)
      if (input$panel.in)
        panels = c(1, panels)
      if (input$panel.out)
        panels = c(panels, 3)
      
      write.csv(data.freqprof$data[data.freqprof$data$panels %in% panels,], file, row.names =
                  F)
    }
  )
  
  output$downloadPlotPDF <- downloadHandler(
    filename = function() {
      paste0("ShinyPlot.pdf")
    },
    content = function(file) {
      pdf(file,
          width = input$graphWidth,
          height = input$graphHeight)
      data.behavior = getDataFromShiny(input$file)
      data.freqprof = freqprof(
        data.behavior,
        getWindowLength(input$unit_length, input$window, data.behavior),
        step = input$step,
        resolution = input$resolution,
        which = input$which
      )
      plot.freqprof(
        data.freqprof,
        gg = input$ggplot,
        panel.in = input$panel.in,
        panel.out = input$panel.out,
        multiPlot = input$multiplot,
        xAxisUnits = input$units
      )
      dev.off()
      
      if (file.exists(paste0(file, ".pdf")))
        file.rename(paste0(file, ".pdf"), file)
    }
  )
  
  output$downloadPlotPNG <- downloadHandler(
    filename = function() {
      paste0("ShinyPlot.png")
    },
    content = function(file) {
      png(
        file,
        width = input$graphWidth,
        height = input$graphHeight,
        units = 'in',
        res = 100
      )
      data.behavior = getDataFromShiny(input$file)
      data.freqprof = freqprof(
        data.behavior,
        window = getWindowLength(input$unit_length, input$window, data.behavior),
        step = input$step,
        resolution = input$resolution,
        which = input$which
      )
      plot.freqprof(
        data.freqprof,
        gg = input$ggplot,
        panel.in = input$panel.in,
        panel.out = input$panel.out,
        multiPlot = input$multiplot,
        xAxisUnits = input$units
      )
      dev.off()
      
      if (file.exists(paste0(file, ".png")))
        file.rename(paste0(file, ".png"), file)
    }
  )
  
}

shinyApp(ui = ui, server = server)
