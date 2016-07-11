
# server.R

#install.packages("devtools")
#install.packages("sqldf")
#install_github("Rfacebook", "pablobarbera", subdir="Rfacebook")
library(devtools)
require("Rfacebook")
#fb_oauth <- fbOAuth(app_id="297636413916478", app_secret="c7f2bb2ea2234a607ae2dd5e99eda370",extended_permissions = TRUE)
#save(fb_oauth, file="fb_oauth")
load("fb_oauth")
library(Rfacebook)
library(ggplot2)
library(scales)
library(sqldf)

format.facebook.date <- function(datestring) {
  date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
}

shinyServer(function(input, output) {
  observeEvent(input$button, {
    cat("getting", input$num, "posts\n")
  })
  
  finalInput <-eventReactive(input$button,{ 
    dataInput <- getPage(input$pg, token=fb_oauth, n = input$num, since = input$dates[1], until = input$dates[2], feed = TRUE)
    dataInput$datetime <- format.facebook.date(dataInput$created_time)
    dataInput$month <- format(dataInput$datetime, "%Y-%b")
    dataInput$year <- format(dataInput$datetime, "%Y")
    dataInput$week <- format(dataInput$datetime, "%Y-%w")
    dataInput$date <- format(dataInput$datetime, "%m/%d")
    
    #aggregate data at month level
    aggregate.month <- function(metric) {
      m <- aggregate(dataInput[[paste0(metric, "_count")]], list(month = dataInput$month), mean)
      m$month <- m$month
      m$metric <- metric
      return(m)
    }
    
    #aggregate data at week level
    aggregate.week <- function(metric) {
      m <- aggregate(dataInput[[paste0(metric, "_count")]], list(week = dataInput$week), mean)
      #m$month <- as.Date(paste0(m$month, "-15"))
      m$week <- m$week
      m$metric <- metric
      return(m)
    }
    
    #aggregate data at year level
    aggregate.year <- function(metric) {
      m <- aggregate(dataInput[[paste0(metric, "_count")]], list(year = dataInput$year), mean)
      #m$month <- as.Date(paste0(m$month, "-15"))
      m$year <- m$year
      m$metric <- metric
      return(m)
    }
    
    #aggregate data at date level
    aggregate.date <- function(metric) {
      m <- aggregate(dataInput[[paste0(metric, "_count")]], list(date = dataInput$date), mean)
      #m$month <- as.Date(paste0(m$month, "-15"))
      m$date <- m$date
      m$metric <- metric
      return(m)
    }
    
    dateInputMonth <- lapply(c("likes", "comments", "shares"), aggregate.month)
    dateInputWeek <- lapply(c("likes", "comments", "shares"), aggregate.week)
    dateInputYear <- lapply(c("likes", "comments", "shares"), aggregate.year)
    dateInputDate <- lapply(c("likes", "comments", "shares"), aggregate.date)
    
    if (input$grpby == "month") {
      do.call(rbind, dateInputMonth)
    } else if (input$grpby == "week") {
      do.call(rbind, dateInputWeek)
    } else if (input$grpby == "year") {
      do.call(rbind, dateInputYear)
    } else {
      do.call(rbind, dateInputDate)
    }
  })
  
  typeInput <- eventReactive(input$button,{ 
    page <- getPage(input$pg, token=fb_oauth, n = input$num, since = input$dates[1], until = input$dates[2], feed = TRUE)
    page$type <- format(page$type, trim=TRUE)
   
     #aggreagate data at type level
    aggregate.type <- function(metric) {
      m <- aggregate(page[[paste0(metric, "_count")]], list(type = page$type), mean)
      m$type <- m$type
      m$metric <- metric
      return(m)
    }
    dataInputType <- lapply(c("likes", "comments", "shares"), aggregate.type)
    do.call(rbind, dataInputType)
    })
  
  #mbrInut step requires to get post data again which will slow down the application
  mbrInput<-eventReactive(input$button,{ 
    feedInput <- getPage(input$pg, token=fb_oauth, n = input$num, since = input$dates[1], until = input$dates[2], feed = TRUE)
    mbrnm<-sqldf("SELECT from_name name,COUNT(id) ct FROM feedInput group by from_name having ct>=5 ")
    as.data.frame(mbrnm$name)
  })
  
  output$plot <-renderPlot({
    if (input$grpby == "month") {
      ggplot(finalInput(), aes(month, y = x, group = metric)) + geom_line(aes(color = metric)) +
        scale_x_discrete("Time Group By") + 
        scale_y_log10("Average count per post",breaks = c(10, 100, 1000, 10000, 50000)) +
        theme_bw() + theme(axis.title.x = element_blank())
    } else if (input$grpby == "week") {
      ggplot(finalInput(), aes(week, y = x, group = metric)) + geom_line(aes(color = metric)) +
        scale_x_discrete("Time Group By") + 
        scale_y_log10("Average count per post",breaks = c(10, 100, 1000, 10000, 50000)) +
        theme_bw() + theme(axis.title.x = element_blank())
    } else if (input$grpby == "year") {
      ggplot(finalInput(), aes(year, y = x, group = metric)) + geom_line(aes(color = metric)) +
        scale_x_discrete("Time Group By") + 
        scale_y_log10("Average count per post",breaks = c(10, 100, 1000, 10000, 50000)) +
        theme_bw() + theme(axis.title.x = element_blank())
    } else {
      ggplot(finalInput(), aes(date, y = x, group = metric)) + geom_line(aes(color = metric)) +
        scale_x_discrete("Time Group By") + 
        scale_y_log10("Average count per post",breaks = c(10, 100, 1000, 10000, 50000)) +
        theme_bw() + theme(axis.title.x = element_blank())
    }
  })
  
  output$plot1 <-renderPlot({
    ggplot(typeInput(), aes(type, y = x, group = metric)) + geom_line(aes(color = metric)) +
      scale_x_discrete("Type") + 
      scale_y_log10("Average count per post",breaks = c(10, 100, 1000, 10000, 50000)) +
      theme_bw() + theme(axis.title.x = element_blank())
  })
  
  output$table1 <- renderTable({
    mbrInput()
  })
  
}
)