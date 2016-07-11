#ui.R

library(shiny)

shinyUI(fluidPage(
  titlePanel("Facebook Page Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Please enter a Facebook Page name to examine. 
        Information will be collected through Facebook API. Note:If name has space than replace space with '_'"),
    
      textInput("pg", "Page", "shopyourway"),
    
      dateRangeInput("dates", 
        "Date range",
        start = "2015-01-01", 
        end = as.character(Sys.Date())),
      numericInput("num", "Num of Posts", 200),
      radioButtons("grpby", "Group By:",
                   c("Month" = "month",
                     "Week" = "week",
                     "Year" = "year",
                     "Date" = "date")),
      actionButton("button", "Run"),
      br(),
      br()
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot by Time", plotOutput("plot")),
        tabPanel("Plot by Post Type", plotOutput("plot1")),
        tabPanel("Users with > 5 posts", tableOutput("table1"))
      )
    )
  )
))