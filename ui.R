library(shiny)
#library(shinyBS)
library(DT)

shinyUI(fluidPage(
  title = "Attackpoint explorer",
  
#   tags$head(
#     # Include our custom CSS
#     #includeCSS("styles.css"),
#     # Hide the red error messages!!!
#     tags$style(type="text/css",
#               ".shiny-output-error { visibility: hidden; }",
#                ".shiny-output-error:before { visibility: hidden; }"
#     )
#   ), 
  tags$h2("Attackpoint explorer"),
  tags$a(href="https://github.com/muuankarski/attackpoint_explorer","Source code in Github"),
  tags$hr(),
    fluidRow(
    shiny::column(4, radioButtons("dataType", "Own or test data",inline = FALSE, choices = list("Markus test data","Upload Own"))),
    shiny::column(4, uiOutput("file_input")),
    shiny::column(4, radioButtons("paceType", "Show pace/speed",inline = FALSE, choices = list("min/km","km/h")))
    ),
  fluidRow(
    shiny::column(4, uiOutput("activity_list")),
    shiny::column(4, uiOutput("time_frame")),
    shiny::column(4, uiOutput("select_cols"))
  ),
  tags$hr(),
  
  tabsetPanel(tabPanel("Table", DT::dataTableOutput("mytable")),
              tabPanel("Distance vs. duration Scatterplots", 
                      plotOutput("scatter",
                                 height="600px", width="auto")),
              tabPanel("Monthly mean pace/speed", plotOutput("pace_bar",height="600px", width="auto")),
              tabPanel("Monthly training minutes per activity", plotOutput("minutes_bar_monthly",height="600px", width="auto")),
              tabPanel("Monthly training distance per activity", plotOutput("distance_bar_monthly",height="600px", width="auto")),
              tabPanel("Weekly training minutes per activity", plotOutput("minutes_bar_weekly",height="600px", width="auto")),
              tabPanel("Weekly training distance per activity", plotOutput("distance_bar_weekly",height="600px", width="auto"))
)
)
)