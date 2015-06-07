library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(stringr)
library(lubridate)
library(tm)
library(wordcloud)


col24 <-  c("#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7", 
            "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", 
            "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D", 
            "#8A7C64", "#599861","#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7", 
            "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", 
            "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D", 
            "#8A7C64", "#599861")

function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
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
    # ectract week number
    dat$week <- format(dat$date, "%W")
    # order months 
    df <- dat[c("month","month_num")]
    df <- arrange(df[!duplicated(df[c("month")]),],month_num)
    dat$month <- factor(dat$month, labels=df$month)
    
    # time into hours:minutes:second
    dat$time <- as.character(dat$time)
    dat$time <- lubridate::period_to_seconds(hms(dat$time))/60
    dat
  })
  
  output$time_frame <- renderUI({
    
    dat <- data_import()
    opts <- dateRangeInput("timeFrame", "Date range:",
                           start = min(dat$date),
                           end   = max(dat$date))
    list(opts)
  })
  
  output$mytable <- renderDataTable({
    
    dat <- data_import()
    # subset time
    dat <- dat[dat$date >= input$timeFrame[1] & dat$date <= input$timeFrame[2],]
    # subset cols
    dat[c(input$cols)]
    
  },options = list(pageLength = 10))
  
  output$select_cols <- renderUI({
    
    dat <- data_import()
    cols <- names(dat)
    opts <- selectInput("cols", "Pick a columns for table:", 
                        selectize = TRUE, multiple = TRUE,
                        choices = cols, selected= c("date","activity","time","distance.km."))
    list(opts)
  })
  
  
  
  
  
  output$activity_list_monthly <- renderUI({
    
    dat <- data_import()
    dat <- dat[dat$date >= input$timeFrame[1] & dat$date <= input$timeFrame[2],]
    activity_freq <- dat %>% group_by(activity) %>% dplyr::summarise(count = n())
    activities <- as.character(arrange(activity_freq, -count)$activity)
    opts <- selectInput("activity_monthly", "Pick activities:", 
                        selectize = TRUE, multiple = TRUE,
                        choices = activities, selected= c(activities[1]))
    list(opts)
  })
  
  output$activity_list_weekly <- renderUI({
    
    dat <- data_import()
    dat <- dat[dat$date >= input$timeFrame[1] & dat$date <= input$timeFrame[2],]
    activity_freq <- dat %>% group_by(activity) %>% dplyr::summarise(count = n())
    activities <- as.character(arrange(activity_freq, -count)$activity)
    opts <- selectInput("activity_weekly", "Pick activities:", 
                        selectize = TRUE, multiple = TRUE,
                        choices = activities, selected= c(activities[1]))
    list(opts)
  })
  
  
  ## Outputs
  
  
  output$pace_point_monthly <- renderPlot({
    
    dat <- data_import()
    dat <- dat[dat$date >= input$timeFrame[1] & dat$date <= input$timeFrame[2],]
    dat <- dat[dat$activity %in% input$activity_monthly,]
    
    dat$pace <- dat$time / dat$distance.km.         
    df <- dat %>% group_by(year,month_num,month,activity) %>% dplyr::summarise(mean_pace = mean(pace))
    df <- df[!is.na(df$mean_pace),]
    df$date <- paste(df$year,df$month_num,"01",sep="-")
    df$date <- as.Date(df$date)
    if (input$paceTypeMonthly == "km/h" ) df$mean_pace <- 60 / df$mean_pace
    ylab <- "pace min/km"
    if (input$paceTypeMonthly == "km/h" ) ylab <- "speed in km/h"
    
    ggplot(df, aes(x=date,y=mean_pace,color=month,group=activity)) +
        geom_point(size=4) +
        facet_grid(~activity, scale="free") +
        geom_smooth(method="loess") +
        labs(x="date",y=ylab) +
        scale_color_manual(values=c("#0868ac","#7bccc4","#feb24c","#fd8d3c","#f03b20","#bd0026",
                                    "#bd0026","#f03b20","#fd8d3c","#feb24c","#7bccc4","#0868ac")) +
        theme_bw()
    
  })
  
  output$pace_point_weekly <- renderPlot({
    
    dat <- data_import()
    dat <- dat[dat$date >= input$timeFrame[1] & dat$date <= input$timeFrame[2],]
    dat <- dat[dat$activity %in% input$activity_weekly,]
    
    dat$pace <- dat$time / dat$distance.km.         
    df <- dat %>% group_by(year,week,activity) %>% dplyr::summarise(mean_pace = mean(pace))
    df <- df[!is.na(df$mean_pace),]
    if (input$paceTypeWeekly == "km/h" ) df$mean_pace <- 60 / df$mean_pace
    ylab <- "pace min/km"
    if (input$paceTypeWeekly == "km/h" ) ylab <- "speed in km/h"
    
    ggplot(df, aes(x=week,y=mean_pace,group=activity)) +
        geom_point(size=4) +
        facet_grid(year~activity, scales = "free") +
        geom_smooth(method="loess") +
        labs(x="date",y=ylab) +
        scale_color_manual(values=c("#0868ac","#7bccc4","#feb24c","#fd8d3c","#f03b20","#bd0026",
                                    "#bd0026","#f03b20","#fd8d3c","#feb24c","#7bccc4","#0868ac")) +
        theme_bw()
    
  })
  
  output$minutes_bar_monthly <- renderPlot({
    
    dat <- data_import()
    dat <- dat[dat$date >= input$timeFrame[1] & dat$date <= input$timeFrame[2],]
    
    
    df <- dat %>% group_by(year,month_num,month,activity) %>% dplyr::summarise(sum_time = sum(time))
    df <- df[!is.na(df$sum_time),]
    df$date <- paste(df$year,df$month_num,"01",sep="-")
    df$date <- as.Date(df$date)
    
    ggplot(df, aes(x=date,y=sum_time,fill=activity)) +
      geom_bar(stat="identity", position="stack") +
      theme_bw() +
      scale_fill_manual(values = col24)
    
  })
  
  output$distance_bar_weekly <- renderPlot({
    
    dat <- data_import()
    dat <- dat[dat$date >= input$timeFrame[1] & dat$date <= input$timeFrame[2],]
    
    
    df <- dat %>% group_by(year,week,activity) %>% dplyr::summarise(sum_distance = sum(distance.km.))
    df <- df[!is.na(df$sum_distance),]
    
    ggplot(df, aes(x=week,y=sum_distance,fill=activity)) +
      geom_bar(stat="identity", position="stack") +
      theme_bw() +
      facet_grid(year~.) +
      scale_fill_manual(values = col24)
    
  })
  
  output$minutes_bar_weekly <- renderPlot({
    
    dat <- data_import()
    dat <- dat[dat$date >= input$timeFrame[1] & dat$date <= input$timeFrame[2],]
    
    
    df <- dat %>% group_by(year,week,activity) %>% dplyr::summarise(sum_time = sum(time))
    df <- df[!is.na(df$sum_time),]
    
    ggplot(df, aes(x=week,y=sum_time,fill=activity)) +
      geom_bar(stat="identity", position="stack") +
      theme_bw() +
      facet_grid(year~.) +
      scale_fill_manual(values = col24)
    
  })
  
  output$distance_bar_monthly <- renderPlot({
    
    dat <- data_import()
    dat <- dat[dat$date >= input$timeFrame[1] & dat$date <= input$timeFrame[2],]
    
    
    df <- dat %>% group_by(year,month_num,month,activity) %>% dplyr::summarise(sum_distance = sum(distance.km.))
    df <- df[!is.na(df$sum_distance),]
    df$date <- paste(df$year,df$month_num,"01",sep="-")
    df$date <- as.Date(df$date)
    
    ggplot(df, aes(x=date,y=sum_distance,fill=activity)) +
      geom_bar(stat="identity", position="stack") +
      theme_bw() +
      scale_fill_manual(values = col24)
    
  })
  
  
  #   output$wordcloud <- renderPlot({
  #     
  #     dat <- data_import()
  #     dat <- dat[dat$date >= input$timeFrame[1] & dat$date <= input$timeFrame[2],]
  #     
  #     myCorpus <- Corpus(VectorSource(dat[["description"]]))
  #     myCorpus = tm_map(myCorpus, content_transformer(tolower))
  #     myCorpus = tm_map(myCorpus, removePunctuation)
  #     myCorpus = tm_map(myCorpus, removeNumbers)
  #     myCorpus = tm_map(myCorpus, removeWords,
  #                       c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
  #   
  #     myDTM = TermDocumentMatrix(myCorpus,
  #                                control = list(minWordLength = 1))
  #     m = as.matrix(myDTM)
  #     sort(rowSums(m), decreasing = TRUE)
  #     wordcloud(m, scale=c(4,0.5), 
  #                   colors=brewer.pal(8, "Dark2"))  
  #   })
  
  
}