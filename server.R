library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(stringr)
library(lubridate)



shinyServer(function(input, output, session) {
  
  
  output$file_input <- renderUI({
    
    
    if (input$dataType == "Markus test data") {
      opts <- tags$h4("Using Markus extreme training data")
    } else {
      opts <- fileInput('file1', 'Choose CSV File', 
                        accept=c('text/csv',
                                 'text/comma-separated-values,text/plain',
                                 '.csv'))
    } 
    list(opts)
  })
  
  data_import <- reactive({
    
    if (input$dataType == "Markus test data") {
      load("data/log-8425-2010-06-06-2015-06-06.rda") 
    } else {
      inFile <- input$file1
      dat <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
    } 
    # date into dates
    dat$date <- as.character(dat$date)
    dat$date <- as.Date(dat$date)
    # extract month
    dat$month <- format(dat$date, "%B")
    dat$month_num <- format(dat$date, "%m")
    # ectract Year
    dat$year <- format(dat$date, "%Y")
    # order months 
    df <- dat[c("month","month_num")]
    df <- arrange(df[!duplicated(df[c("month")]),],month_num)
    dat$month <- factor(dat$month, labels=df$month)
    
    # time into hours:minutes:second
    dat$time <- as.character(dat$time)
    dat$time <- lubridate::period_to_seconds(hms(dat$time))/60
    dat
  })
  
  ### --------------------------------------------------------------------- ###
  # -- Which domain within group
  
  output$select_cols <- renderUI({
    
    dat <- data_import()
    cols <- names(dat)
    opts <- selectInput("cols", "Pick a columns for table:", 
                        selectize = TRUE, multiple = TRUE,
                        choices = cols, selected= c("date","activity","time","distance.km."))
    list(opts)
  })
  
  output$activity_list <- renderUI({
    
    dat <- data_import()
    activity_freq <- dat %>% group_by(activity) %>% dplyr::summarise(count = n())
    activities <- as.character(arrange(activity_freq, -count)$activity)
    opts <- selectInput("activity", "Pick activities:", 
                        selectize = TRUE, multiple = TRUE,
                        choices = activities, selected= c(activities[1],activities[2]))
    list(opts)
  })
  
  output$time_frame <- renderUI({

    dat <- data_import()
    opts <- dateRangeInput("timeFrame", "Date range:",
                   start = min(dat$date),
                   end   = max(dat$date))
    list(opts)
  })
  
  
    output$mytable= renderDataTable({
      
      dat <- data_import()
      # subset time
      dat <- dat[dat$date >= input$timeFrame[1] & dat$date <= input$timeFrame[2],]
      # subset activities
      dat <- dat[dat$activity %in% input$activity,]
      # subset cols
      dat[c(input$cols)]
      
    },options = list(pageLength = 10))
    
    output$scatter <- renderPlot({
      
      dat <- data_import()
      dat <- dat[dat$date >= input$timeFrame[1] & dat$date <= input$timeFrame[2],]
      dat <- dat[dat$activity %in% input$activity,]
      
      ggplot(dat, aes(x=time,y=distance.km.,color=month,group=1)) +
        geom_point(size=3) + 
        facet_grid(year~activity) +
        geom_smooth(method="loess") +
        labs(x="Exercise duration",y="Exercise distance") +
        theme(axis.text.x = element_text(angle = 45)) +
        scale_color_manual(values=c("#0868ac","#7bccc4","#feb24c","#fd8d3c","#f03b20","#bd0026",
                                    "#bd0026","#f03b20","#fd8d3c","#feb24c","#7bccc4","#0868ac")) +
        theme_bw()
      
    })
    
    
    output$pace_bar <- renderPlot({
      
      dat <- data_import()
      dat <- dat[dat$date >= input$timeFrame[1] & dat$date <= input$timeFrame[2],]
      dat <- dat[dat$activity %in% input$activity,]
      
        dat$pace <- dat$time / dat$distance.km.         
        df <- dat %>% group_by(year,month_num,month,activity) %>% dplyr::summarise(mean_pace = mean(pace))
        df <- df[!is.na(df$mean_pace),]
        df$date <- paste(df$year,df$month_num,"01",sep="-")
        df$date <- as.Date(df$date)
        if (input$paceType == "km/h" ) df$mean_pace <- 60 / df$mean_pace
        ylab <- "pace min/km"
        if (input$paceType == "km/h" ) ylab <- "speed in km/h"
        
      
      
      
      ggplot(df, aes(x=date,y=mean_pace,color=month,group=activity)) +
        geom_point(size=4) +
        facet_grid(~activity) +
        geom_smooth(method="loess") +
        labs(x="date",y=ylab) +
        scale_color_manual(values=c("#0868ac","#7bccc4","#feb24c","#fd8d3c","#f03b20","#bd0026",
                                    "#bd0026","#f03b20","#fd8d3c","#feb24c","#7bccc4","#0868ac")) +
        theme_bw()
      
    })
    
    output$minutes_bar <- renderPlot({
      
      dat <- data_import()
      dat <- dat[dat$date >= input$timeFrame[1] & dat$date <= input$timeFrame[2],]
      
      
      df <- dat %>% group_by(year,month_num,month,activity) %>% dplyr::summarise(sum_time = sum(time))
      df <- df[!is.na(df$sum_time),]
      df$date <- paste(df$year,df$month_num,"01",sep="-")
      df$date <- as.Date(df$date)
      
      ggplot(df, aes(x=date,y=sum_time,fill=activity)) +
        geom_bar(stat="identity", position="stack") +
        theme_bw()
      
    })
    
    output$distance_bar <- renderPlot({
      
      dat <- data_import()
      dat <- dat[dat$date >= input$timeFrame[1] & dat$date <= input$timeFrame[2],]
      
      
      df <- dat %>% group_by(year,month_num,month,activity) %>% dplyr::summarise(sum_distance = sum(distance.km.))
      df <- df[!is.na(df$sum_distance),]
      df$date <- paste(df$year,df$month_num,"01",sep="-")
      df$date <- as.Date(df$date)
      
      ggplot(df, aes(x=date,y=sum_distance,fill=activity)) +
        geom_bar(stat="identity", position="stack") +
        theme_bw()
      
    })
    
})



