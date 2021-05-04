library(shiny)
library(tidyverse)

ui <- fluidPage(titlePanel("Frequency Profile"),
  sidebarLayout(sidebarPanel(tabsetPanel(
    tabPanel(
      "Convert into csv quantitative",
      tags$h4("Convert qualitative csv into quantiative csv"),
      tags$p(
        "Here you can import a csv file with behaviors in the form of qualitative variables"
      ),
      tags$hr(),
      fileInput('fileToConvert', 'Choose File',
                accept = c('.csv')),
      actionButton('downloadDataConverted', 'Convert')
    ),
    tabPanel(
      "Create FreqProf plot",
      tags$h4("Create frequency profile plot"),
      tags$p(
        "Here you can import a csv file in the form of quantitative variables to analysis data."
      ),
      tags$hr(),
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
  )),
  
  mainPanel(plotOutput("distPlot", height = "500px"))))

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
      
      write.csv(data.freqprof$data[data.freqprof$data$panels %in% panels, ], file, row.names =
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
  
  getDataToConvert = function(inFile) {
    if (is.null(inFile))
      return(NULL)
    
    ## Importing data
    dataToConvert <- read_csv(
      file = inFile$datapath,
      col_types =
        cols(
          Date_Time_Absolute_dmy_hmsf = col_datetime(format = "%d-%m-%Y %H:%M:%OS"),
          Date_dmy = col_date(format = "%d-%m-%Y"),
          Time_Absolute_hms = col_time(format = "%H:%M:%S"),
          Time_Absolute_f = col_integer(),
          Time_Relative_hmsf = col_time(format = "%H:%M:%OS"),
          Time_Relative_hms = col_time(format = "%H:%M:%S"),
          Time_Relative_f = col_integer(),
          Time_Relative_sf = col_character(),
          Duration_sf = col_double(),
          Result_Container = col_character(),
          Observation = col_factor(),
          Event_Log = col_character(),
          Subject = col_factor(),
          Behavior = col_factor(),
          #Modifier_1 = col_character(),
          #Modifier_2 = col_character(),
          #Modifier_3 = col_character(),
          #Modifier_4 = col_character(),
          #Modifier_5 = col_character(),
          #Modifier_6 = col_character(),
          #Modifier_7 = col_character(),
          Event_Type = col_factor(),
          Comment = col_character()
        )
    )
    
    return(dataToConvert)
  }
  
  
  observeEvent(input$downloadDataConverted, {
    dataToConvert <- getDataToConvert(input$fileToConvert)
    if (is.null(dataToConvert))
      return(NULL)
    
    ## Transforming data into start - stop data
    result <- dataToConvert %>%
      arrange(Observation, Subject, Behavior) %>%
      group_by(row = ceiling(row_number() / 2)) %>%
      pivot_wider(
        names_from = Event_Type,
        values_from = c(
          Date_Time_Absolute_dmy_hmsf,
          Date_dmy,
          Time_Absolute_hms,
          Time_Absolute_f,
          Time_Relative_hmsf,
          Time_Relative_hms,
          Time_Relative_f,
          Time_Relative_sf,
          Duration_sf,
          #Result_Container,
          #Observation,
          #Event_Log,
          #Subject,
          #Behavior,
          #Modifier_1,
          #Modifier_2,
          #Modifier_3,
          #Modifier_4,
          #Modifier_5,
          #Modifier_6,
          #Modifier_7,
          #Event_Type,
          Comment
        )
      ) %>%
      ungroup() %>%
      select(
        starts_with("Date_Time_Absolute_dmy_hmsf"),
        starts_with("Date_dmy"),
        starts_with("Time_Absolute_hms "),
        starts_with("Time_Absolute_f"),
        starts_with("Time_Relative_hmsf"),
        starts_with("Time_Relative_hms Time_Relative_f"),
        starts_with("Time_Relative_sf"),
        starts_with("Duration_sf"),
        Result_Container,
        Observation,
        Event_Log,
        Subject,
        Behavior,
        Modifier_1,
        #Modifier_2,
        #Modifier_3,
        #Modifier_4,
        #Modifier_5,
        #Modifier_6,
        #Modifier_7,
        starts_with("Comment"),-row
      )
    
    ## Transforming column type
    result$`Time_Relative_sf_State start` <-
      as.integer(result$`Time_Relative_sf_State start`)
    
    result$`Time_Relative_sf_State stop` <-
      as.integer(result$`Time_Relative_sf_State stop`)
    
    result$Behavior <- as.character(result$Behavior)
    
    ## finding all groups to create a vector
    groups <- c()
    for (b in dataToConvert$Observation) {
      if ((b %in% groups) == 0) {
        groups <- c(groups, b)
      }
    }
    
    ## finding all subjects to create a vector
    subjects <- c()
    for (s in dataToConvert$Subject) {
      if ((s %in% subjects) == 0) {
        subjects <- c(subjects, s)
      }
    }
    
    ### for each pupils in each groups generate a .csv file
    withProgress(message = 'Making plot', value = 0, {
      n <- 0
      i <- (length(groups) * length(subjects))
      
      for (group in groups) {
        for (subject in subjects) {
          possibleError <- tryCatch({
            result %>%
              filter(Observation == group) %>%
              filter(Subject == subject) %>%
              build_ono_data(
                df = as.data.frame(.),
                start = "Time_Relative_sf_State start",
                end = "Time_Relative_sf_State stop",
                behavior_column_name = "Behavior",
                select_behavior = c("all")
              ) %>%
              write.csv(., paste0("csv/data-",
                                  group,
                                  subject,
                                  ".csv"))
            n <- n + 1
          },
          error = function(e) {
            print(e)
          },
          warning = function(e) {
            print(e)
          })
          if (inherits(possibleError, "error"))
            next
          if (inherits(possibleError, "warning"))
            next
          i <- i - 1
          incProgress(1 / i, detail = paste0(i, " observation(s) left."))
        }
      }
      showNotification(type="message", paste0(n, " file(s) saved in the csv directory"))
    })
  })
  
}

shinyApp(ui = ui, server = server)

