#Required libraries:
library(shiny)
library(prophet)
library(anytime)
library(shinycssloaders)
library(ggplot2)
library(dplyr)
library(shinyjs)
library(lubridate)
library(reshape2)

#Define UI for data upload

ui <- fluidPage(
  useShinyjs(),
  titlePanel(
    span(style = "color: rgb(0,94,184)", "NHS Demand and Capacity Forecasting Tool"),
    windowTitle = "NHS Demand and Capacity Forecasting Tool"
  ),
  
  sidebarLayout(
    #Sidebar panel for user interaction
    sidebarPanel(
      h4("Step 1: Data Upload"),
      
      fileInput(
        inputId = 'file1',
        label = "Upload model export here:",
        multiple = FALSE,
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")
      ),
      h5("What timescale is the data in?"),
      radioButtons("time", "Choose:", choices = c(
        "Hourly" = 1,
        "Daily" = 2,
        "Weekly" = 3
      )),
      tags$hr(style = "border-color: black;"),
      h4("Step 2: Parameters"),
      h6(
        "Set the opening times for the department using the two sliders. For example, if the department is open from 8am to 8pm, set the left slider to 8 and the right slider to 20."
      ),
      sliderInput(
        "slider",
        label = "Department Open (24hr) :",
        min = 0,
        max = 24,
        value = c(0, 24)
      ),
      h5("Do you want to forecast or test predictions?"),
      radioButtons("type", "Choose:", choices = c(
        "Forecast" = 1, "Train/Test" = 2
      )),
      conditionalPanel(
        condition = ("input.type == 1"),
        sliderInput(
          "windowslider",
          label = "Forecast window (hours):",
          min = 1,
          max = 52,
          value = 52
        )
      ),
      conditionalPanel(
        condition = ("input.type == 2"),
        sliderInput(
          "testdatasize",
          label = "Enter the number of weeks for the train dataset",
          min = 1,
          max = 52,
          value = 1
        )
      ),
      h5("Choose the lower bound percentile:"),
      selectInput(
        "percentileselection",
        "Select the lower percentile bound",
        choices = c(
          '65percentile',
          '70percentile',
          '75percentile',
          '80percentile'
        )
      ),
      tags$hr(style = "border-color: black;"),
      h4("Step 3: Prediction"),
      h6('Use the "Predict" button below to begin the forecast.'),
      actionButton("go", "Predict"),
      tags$hr(style = "border-color: black;"),
      h4("Step 4: Download"),
      h6("Download the completed forecast to be brought back in to the model."),
      downloadButton("downloadData", "Download"),
      br(),
      br(),
      em(
        "Please do not click the download button until the graph on the right has been generated"
      )
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      img(
        src = 'NHS.png',
        align = "right",
        height = "15%",
        width = "15%"
      ),
      
      uiOutput("tabs"),
      withSpinner(plotOutput("forecast")),
      uiOutput("dates")
      
    )
    
  )
)


#Define server logic to process uploaded file
server <- function(input, output, session) {
  
  #UI GENERATION
  #Slider labels
  observeEvent(input$time, {
    val <- input$time
    text <-
      case_when(
        val == "1" ~ "Forecast window (hours):",
        val == "2" ~ "Forecast window (days):",
        val == "3" ~ "Forecast window (weeks):"
      )
    min <- 1
    max <-
      case_when(val == "1" ~ 168, val == "2" ~ 30, val == "3" ~ 52)
    updateSliderInput(
      session,
      "windowslider",
      label = text,
      min = min,
      max = max,
      value = max
    )
  })
  
  observeEvent(input$time, {
    val <- input$time
    text <-
      case_when(
        val == "1" ~ "Enter the number of hours for the train dataset:",
        val == "2" ~ "Enter the number of days for the train dataset:",
        val == "3" ~ "Enter the number of weeks for the train dataset"
      )
    min <- 1
    max <-
      case_when(val == "1" ~ 168, val == "2" ~ 30, val == "3" ~ 52)
    updateSliderInput(
      session,
      "testdatasize",
      label = text,
      min = min,
      max = max,
      value = 1
    )
  })
  
  output$tabs <- renderUI({
    counter <- counter()
    if (is.null(fcst_short())) {
      return(NULL)
    } else {
      data <- df()
      data <- data[1:counter + 1]
      tabs <- lapply(names(data), tabPanel)
      do.call(tabsetPanel, c(tabs, id = "streamtab"))
    }
  })
  
  output$dates <- renderUI({
    if (is.null(fcst_short())) {
      return(NULL)
    } else {
      data <- fcst_short()
      data <- data$ds
      mini <- min(as_date(data))
      maxi <- max(as_date(data))
      fluidRow(column(12,
                      wellPanel(
                        sliderInput(
                          "dates",
                          label = "Show dates:",
                          min = mini,
                          max = maxi,
                          value = c(mini, maxi)
                        )
                      )))
    }})
  
  #Prep Prophet
  m <-
    prophet(
      yearly.seasonality = TRUE,
      monthly.seasonality = TRUE,
      daily.seasonality = TRUE,
      interval.width = 0.85
    )
  m <- add_country_holidays(m, 'England')
  
  #Read datafile
  df <- reactive({
    df <- read.csv(input$file1$datapath,
                   header = TRUE,
                   sep = ",")
    
    df$ds <- anytime(df$ds, tz = "GMT")
    return(df)
  })
  
  #Get stream count
  counter <- reactive({
    if (is.null(input$file1)) {
      return(NULL)
    }
    data_set <- df()
    counter <- ncol(data_set) - 1
    return(counter)
  })
  
  #Get file name
  file_name <- eventReactive(input$go, {
    file_name <- input$file1$name
    return(file_name)
  })
  
  #Process data through prophet
  fcst_short <- eventReactive(input$go, {
    
    df <- df()
    
    if (input$type == 2) {
      train <- tail(df, input$testdatasize)
      df <- head(df, nrow(df) - input$testdatasize)
      periods <- input$testdatasize
    }
    else
    {
      periods <- input$windowslider
    }
    
    rows <- nrow(df)
    
    x <- NULL
    for (i in 2:ncol(df)) {
      x <- append(x, colnames(df[i]))
    }
    
    if (input$type == 2) {
      for (i in 1:length(x)) {
        assign(paste("df", x[i], sep = ""), data.frame(df[1], df[i + 1]))
        d <- get(paste("df", x[i], sep = ""))
        names(d) <- c("ds", "y")
        assign(paste("df", x[i], sep = ""), d)
        d <- NULL
        assign(paste("m", x[i], sep = ""), fit.prophet(m, get(paste("df", x[i], sep = ""))))
        assign(paste("train", x[i], sep = ""), data.frame(train[1], train[i + 1]))}
    } else {
    for (i in 1:length(x)) {
      assign(paste("df", x[i], sep = ""), data.frame(df[1], df[i + 1]))
      d <- get(paste("df", x[i], sep = ""))
      names(d) <- c("ds", "y")
      assign(paste("df", x[i], sep = ""), d)
      d <- NULL
      assign(paste("m", x[i], sep = ""), fit.prophet(m, get(paste("df", x[i], sep = ""))))
    }}
    
    val <- input$time
    
    freq <- if(val == "1") {60*60} else { 
      case_when(val == "2" ~ "d",
                val == "3" ~ "w")}
    
    future <- make_future_dataframe(
      get(paste("m", x[1], sep = "")),
      periods = periods,
      freq = freq,
      include_history = FALSE
    )
    
    future_base <- future
    
    future <- future %>%
      filter(as.numeric(format(ds, "%H")) >= input$slider[1]) %>%
      filter(as.numeric(format(ds, "%H")) < input$slider[2])
    
    df <- data.frame()
    
    for (i in 1:length(x)) {
      assign(paste("forecast", x[i], sep = ""), predict(get(paste("m", x[i], sep =
                                                                    "")), future))}
      
      for (i in 1:length(x)) {
        assign(paste("samples", x[i], sep = ""),
               predictive_samples(get(paste(
                 "m", x[i], sep = ""
               )),
               future))
      }
      
      
      for (i in 1:length(x)) {
        assign(paste("centile_calc", x[i], sep = ""),
               melt(get(paste("samples",x[i], sep = ""))$yhat))
        assign(paste("centile_calc", x[i], sep = ""),
               get(paste("centile_calc", x[i], sep = "")) %>%   
               select(Var1, value) %>% 
                 group_by(Var1) %>% 
                 summarise('65percentile' = quantile(value, 0.65),
                           '70percentile' = quantile(value, 0.7),
                           '75percentile' = quantile(value, 0.75),
                           '80percentile' = quantile(value, 0.8),
                           '85percentile' = quantile(value, 0.85)
                 )
        )
        assign(paste("forecast",x[i], sep = ""),
               cbind(get(paste("forecast",x[i], sep = "")),get(paste("centile_calc", x[i], sep = ""))))}
      
    for (i in 1:length(x)) {
      if (input$type == 2) {
        d <- get(paste("forecast", x[i], sep = ""))
        d <- left_join(d, get(paste("train", x[i], sep = "")))
        d <- d %>% 
          select('ds',paste0(x[i]),'yhat','yhat_lower','yhat_upper','65percentile','70percentile','75percentile','80percentile','85percentile')
        names(d) <- c( "ds",
                       paste("y", x[i], sep = ""),
                       paste("yhat", x[i], sep = ""),
                       paste("yhat_lower", x[i], sep = ""),
                       paste("yhat_upper", x[i], sep = ""),
                       paste("65percentile", x[i], sep = ""),
                       paste("70percentile",x[i], sep = ""),
                       paste("75percentile",x[i], sep = ""),
                       paste("80percentile", x[i], sep = ""),
                       paste("85percentile", x[i], sep = "")) 
        assign(paste("forecast", x[i], sep = ""), d)
        if (is_empty(df)) {
          df <-
            get(paste("forecast", x[i], sep = ""))
        } else {
          df <- cbind(df, get(paste("forecast", x[i], sep = "")))}
      } 
      else {
      d <- get(paste("forecast", x[i], sep = ""))
      d <- d %>%
        select("ds", "yhat", "yhat_lower", "yhat_upper")
      names(d) <-
        c(
          "ds",
          paste("yhat", x[i], sep = ""),
          paste("yhat_lower", x[i], sep = ""),
          paste("yhat_upper", x[i], sep = "")
        )
      assign(paste("forecast", x[i], sep = ""), d)
      if (is_empty(df)) {
        df <-
          get(paste("forecast", x[i], sep = ""))
      } else {
        df <- cbind(df, get(paste("forecast", x[i], sep = "")))
      } }} 
    
    combined <- data.frame()
    
    for (i in 1:length(x)) {
      if (is_empty(combined)) {
        combined <-
          left_join(future_base, get(paste("forecast", x[i], sep = "")))
      } else {
        (combined <-
           left_join(combined, get(paste(
             "forecast", x[i], sep = ""
           ))))
      }
    }
      combined[is.na(combined)] <- 0
    fcst_short <- tail(combined, (168 * 6))
    
    return(fcst_short)
  })
  
  #Present outputs in line and area chart
  output$forecast <- renderPlot({
    if (is.null(fcst_short())) {
      return(NULL)
    }
    
    myData <- fcst_short()
    
    #fcst_m <- max(myData[paste0("yhat_upper",input$streamtab)])
    
    if (is.null(input$streamtab)) {
      return(NULL)
    }
    
    myData <- myData %>%
      filter(ds >= input$dates[1] & ds <= input$dates[2])
    fcst_m <- max(myData[paste0("yhat_upper", input$streamtab)])
    
    fcst <- myData
    percentile_text <- paste("fcst$\'",input$percentileselection, "\'", sep="")
    percentile_text <- eval(parse(text = percentile_text))
      
      if (input$type == 2) {
        
        ggplot(data = myData, aes(x=ds, y=get(paste0("yhat",input$streamtab))))+
          geom_ribbon(aes(ymin = get(paste0("yhat_lower",input$streamtab)), ymax = get(paste0("yhat_upper",input$streamtab))), fill = "blue", alpha=0.3, show.legend = TRUE)+
          #geom_ribbon(aes(ymin = percentile_text, ymax = fcst()$'85percentile'), fill = 'blue', alpha = 0.7, show.legend = TRUE)+ 
          geom_point(aes(x = ds, y = get(paste0("y",input$streamtab)))) +
          ggtitle(file_name())+
          xlab("Date")+
          ylab("Attendances")+
          #scale_x_datetime(date_breaks = "6 months")+
          theme(plot.title = element_text(size = 22), axis.text.x = element_text(angle = 90, hjust = 1))+
          coord_cartesian(ylim = c(0,fcst_m))
        
      }
      else {
        
        ggplot(data = myData, aes(x=ds,y=get(paste0("yhat",input$streamtab)), group = 1))+
          geom_ribbon(aes(ymin = get(paste0("yhat_lower",input$streamtab)), ymax = get(paste0("yhat_upper",input$streamtab))),fill = "blue", alpha = 0.3)+
          geom_line()+
          ggtitle(paste0(input$streamtab))+
          xlab("Date")+
          ylab("Attendances")+
          #scale_x_datetime(date_breaks = "12 hours",expand = c(0,0))+
          theme(plot.title = element_text(size = 22), axis.text.x = element_text(angle = 90, hjust = 1))+
          coord_cartesian(ylim = c(0,fcst_m), expand = FALSE)
      }
  }) 
  
  #Define download parameters for outputs
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Import", Sys.Date(), ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(fcst_short(), file)
    }
    
  )
  
}
# Run the app ----
shinyApp(ui, server)
