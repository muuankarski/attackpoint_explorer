library(shinydashboard)
library(shiny)
library(DT)

dashboardPage(skin = "black",
              dashboardHeader(title = "Attackpoint explorer"),
              dashboardSidebar(
                sidebarMenu(
                  menuItem("Upload and View data", tabName = "upload", icon = icon("file-text")),
                  uiOutput("time_frame"),
                  menuItem("Monthly summaries", tabName = "monthly", icon = icon("bar-chart")),
                  menuItem("Weekly summaries", tabName = "weekly", icon = icon("bar-chart")),
                  #menuItem("Sentiment analysis of comments", tabName = "sentiment", icon = icon("user-md")),
                  #tag$hr(),
                  menuItem("Source code", icon = icon("github"), 
                           href = "https://github.com/muuankarski/attackpoint_explorer"),
                  menuItem("Attackpoint", icon = icon("bicycle"), 
                           href = "http://attackpoint.org/")
                )
                
                
              ),
              dashboardBody(
                tabItems(
                  # First tab content
                  tabItem(tabName = "upload",
                          fluidRow(
                            box(title = NULL, width = 4, 
                                radioButtons("dataType", "" ,inline = FALSE, 
                                             choices = list("Markus test data","Upload Own")
                                )
                            ),
                            
                            box(
                              title = NULL, width=4,
                              uiOutput("file_input")
                            ),
                            box(title=NULL,width=4,
                                uiOutput("select_cols")
                            )
                          ),
                          fluidRow(
                            box(title = "Browse the data", width = 12, 
                                DT::dataTableOutput("mytable")
                            )
                          )
                  ),
                  
                  # Second tab content
                  tabItem(tabName = "monthly",
                          fluidRow(
                            box(title=NULL, width=3,
                                radioButtons("paceTypeMonthly", "Show pace/speed",
                                             inline = FALSE, 
                                             choices = list("min/km","km/h"))
                            ),
                            box(title=NULL, width=6,
                                uiOutput("activity_list_monthly")
                            )
                          ),
                          fluidRow(  
                            tabBox(title=NULL, width=12,
                                   side = "left", height = "250px",
                                   tabPanel("Mean pace", plotOutput("pace_point_monthly")),
                                   tabPanel("Monthly training minutes per activity", plotOutput("minutes_bar_monthly")),
                                   tabPanel("Monthly training distance per activity", plotOutput("distance_bar_monthly"))
                            )
                          )
                  ),
                  # Second tab content
                  tabItem(tabName = "weekly",
                          fluidRow(
                            box(title=NULL, width=3,
                                radioButtons("paceTypeWeekly", "Show pace/speed",
                                             inline = FALSE, 
                                             choices = list("min/km","km/h"))),
                            box(title=NULL, width=6,
                                uiOutput("activity_list_weekly")
                            )
                          ),
                          fluidRow(  
                            tabBox(title=NULL, width=12,
                                   side = "left", height = "250px",
                                   tabPanel("Mean pace", plotOutput("pace_point_weekly")),
                                   tabPanel("Weekly training minutes per activity", plotOutput("minutes_bar_weekly")),
                                   tabPanel("Weekly training distance per activity", plotOutput("distance_bar_weekly"))
                            )
                          )
                          
                  )#,
                  #       # Sentiment analysis
                  #       tabItem(tabName = "sentiment",
                  #               fluidRow(
                  #                 box(title=NULL, width=7,
                  #                     tags$img(src = "http://content.gallup.com/origin/gallupinc/GallupSpaces/Production/Cms/POLL/fuvzeznhr0ar7mtpmyh2xg.jpg", width = "350", height = "350")
                  #               )
                  #               )
                  #               
                  #       )
                )
              )
)