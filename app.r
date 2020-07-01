#Required libraries:
library(shiny)
library(prophet)
library(anytime)
library(shinycssloaders)
library(ggplot2)
library(dplyr)
library(shinyjs)
library(reshape2)

#Define UI for data upload

ui <- fluidPage(
  useShinyjs(),
  titlePanel(span(style = "color: rgb(0,94,184)","NHS Demand and Capacity Forecasting Tool"), windowTitle = "NHS Demand and Capacity Forecasting Tool"),
  
  sidebarLayout(
    
    #Sidebar panel for user interaction
    sidebarPanel(h2("Data Upload"),
                 
                 fileInput(inputId = 'file1', label = "Upload model export here:",multiple = FALSE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 tags$hr(style="border-color: black;"),
                 h6("Do you want to forecast or test predictions?"),
                 radioButtons("type","Choose:", choices = c("Forecast" = 1,"Train/Test" = 2)),
                 conditionalPanel(
                   condition = ("input.type == 1"),
                   sliderInput("windowslider", label = "Forecast window (Weeks):", min = 1, max = 52, value = 52)
                 ),
                 conditionalPanel(
                   condition = ("input.type == 2"),
                   sliderInput("testdatasize", label = "Enter the number of weeks for the train dataset", min = 1, max = 52, value = 1)
                 ),
                 tags$hr(style = "border-color: black;"),
                 h6("Choose the lower bound percentile:"),
                 selectInput("percentileselection", "Select the lower percentile bound", choices = c('65percentile','70percentile','75percentile','80percentile')),
                 h6('Use the "Predict" button below to begin the forecast.'),
                 actionButton("go","Predict"),
                 tags$hr(style="border-color: black;"),
                 downloadButton("downloadData", "Download"),
                 br(),
                 br(),
                 em("Please do not click the download button until the graph on the right has been generated")
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      withSpinner(plotOutput("forecast"), type = 8)
      
    )
    
  ))


#Define server logic to process uploaded file
server <- function(input, output) {
  
  #Process data through prophet
  fcst <- eventReactive(input$go, {
    
    df <- read.csv(input$file1$datapath,
                   header = TRUE,
                   sep = ","
    )
    
    df$ds <- as.POSIXct(anydate(df$ds), format='%Y-%m-%d')
    
    if (input$type == 2) {
      train <- tail(df, input$testdatasize)
      df <- head(df,nrow(df)-input$testdatasize)
      periods <- input$testdatasize
    }
    else
    {
      periods <- input$windowslider
    }
    
    m <- prophet(seasonality.mode = 'multiplicative', weekly.seasonality = TRUE, interval.width = 0.95)
    m <- add_country_holidays(m,'England')
    m <- fit.prophet(m, df)
    
    future_short <- make_future_dataframe(m, periods = periods, freq = "w")
    fcst <- predict(m, future_short)
    
    samples <- predictive_samples(m, future_short)
    
    centile_calc <- melt(samples$yhat) %>%
      select(Var1, value) %>% 
      group_by(Var1) %>% 
      summarise('65percentile' = quantile(value, 0.65), 
                '70percentile' = quantile(value, 0.7), 
                '75percentile' = quantile(value, 0.75), 
                '80percentile' = quantile(value, 0.8), 
                '85percentile' = quantile(value, 0.85)) 
    
    fcst <- cbind(fcst, centile_calc)
    
    if (input$type == 2) {
      fcst <- left_join(fcst,train)
      fcst <- fcst[c('ds','y','yhat','yhat_lower','yhat_upper','65percentile','70percentile','75percentile','80percentile','85percentile')]
    } else {
      fcst <- fcst[c('ds','yhat','yhat_lower','yhat_upper','65percentile','70percentile','75percentile','80percentile','85percentile')]
    }
    
    return(fcst)
  })
  
  #Process data to match capacity timescales
  # fcst_cut <- reactive({
  #   fcst_cut <- fcst_short() %>% mutate(yhat_lower = replace(yhat_lower, yhat_lower < 0, 0), yhat_upper = replace(yhat_upper, yhat_upper < 0, 0), yhat = replace(yhat, yhat < 0, 0)) 
  #   return(fcst_cut)
  # })
  
  #Process outputs to help chart display
  fcst_m <- reactive({
    fcst_m <- max(fcst()$yhat_upper)
    return(fcst_m)
  })
  
  #Process filename for chart
  
  file_name <- eventReactive(input$go, {
    file_name <- input$file1$name
    return(file_name)
  })
  
  #Present outputs in line and area chart
  output$forecast <- renderPlot({
    
    req(input$file1)
    
    fcst <- fcst()
    percentile_text <- paste("fcst$\'",input$percentileselection, "\'", sep="")
    percentile_text <- eval(parse(text = percentile_text))
    
    if (input$file1$name == file_name()) {
      
      if (input$type == 2) {
        
        ggplot(data = fcst(), aes(x=ds, y=yhat))+
          geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper), fill = "blue", alpha=0.3, show.legend = TRUE)+
          geom_ribbon(aes(ymin = percentile_text, ymax = fcst()$'85percentile'), fill = 'blue', alpha = 0.7, show.legend = TRUE)+ 
          geom_point(aes(x = ds, y = y)) +
          ggtitle(file_name())+
          xlab("Date")+
          ylab("Attendances")+
          scale_x_datetime(date_breaks = "6 months")+
          theme(plot.title = element_text(size = 22), axis.text.x = element_text(angle = 90, hjust = 1))+
          coord_cartesian(ylim = c(0,fcst_m()))
        
      }
      else {
        ggplot(data = fcst(), aes(x=ds, y=yhat))+
          geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper), fill = "blue", alpha=0.3, show.legend = TRUE)+
          geom_ribbon(aes(ymin = percentile_text, ymax = fcst()$'85percentile'), fill = 'blue', alpha = 0.7, show.legend = TRUE)+ 
          ggtitle(file_name())+
          xlab("Date")+
          ylab("Attendances")+
          scale_x_datetime(date_breaks = "6 months")+
          theme(plot.title = element_text(size = 22), axis.text.x = element_text(angle = 90, hjust = 1))+
          coord_cartesian(ylim = c(0,fcst_m()))
      }
    }
  })
  
  #Define download parameters for outputs
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("output ",file_name(), sep = "")
    },
    content = function(file) {
      write.csv(fcst(), file)
    }
    
  )
  
}
# Run the app ----
shinyApp(ui, server)

