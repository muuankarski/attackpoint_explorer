library(shiny)
library(shinyBS)
library(DT)

shinyUI(fluidPage(
  title = "Attackpoint explorer",
  
  tags$head(
    # Include our custom CSS
    #includeCSS("styles.css"),
    # Hide the red error messages!!!
    tags$style(type="text/css",
              ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    )
  ), 
  tags$h2("Attackpoint explorer"),
  tags$a(href="https://github.com/muuankarski/attackpoint_explorer","Source code in Github"),
  tags$hr(),
    fluidRow(
    shiny::column(6, radioButtons("dataType", "Which dataset",inline = FALSE, choices = list("Use Markus","Upload Own"))),
    shiny::column(6, uiOutput("file_input"))
    ),
  fluidRow(
    shiny::column(4, uiOutput("activity_list")),
    shiny::column(4, uiOutput("time_frame")),
    shiny::column(4, uiOutput("select_cols"))
  ),
  tags$hr(),
  
  tabsetPanel(tabPanel("Table", DT::dataTableOutput("mytable")),
              tabPanel("Scatterplots", 
                      plotOutput("scatter",
                                 height="600px", width="auto"))
              #tabPanel("Top 30 items", plotOutput("export_Barchart",height="600px", width="auto"))
)
)
)