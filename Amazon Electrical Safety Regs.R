This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



#To open excel in R
library(openxlsx)
# To plot using ggplot2
library(ggplot2)
#To plot side by side or on top of each other
library(gridExtra)
#To use date_break functinoallity
library(scales)
library(lubridate)
library(rkt)
library(EnvStats)
library(zoo)
library(dplyr)
library(Kendall)
library(boot)
library(magrittr)
library(ggpubr)
library(ggthemes)
library(plotly)
library(psych)
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(rsconnect)

dbPath1 <- "SalmonWatcher.xlsx"

# load the workbook

wb <- loadWorkbook(dbPath1)

#load the worksheets
Counts2018 <-  read.xlsx(dbPath1, "2018", detectDates = T)
Counts2019 <-  read.xlsx(dbPath1, "2019", detectDates = T)

# Combine Worksheets
AllCountsWatcher <- rbind(Counts2018, Counts2019)
#Creat Year and Month Vectors




f <- list(
  family = "helvetica",
  size = 18,
  color = "Black"
)
y <- list(
  title = "Summer Dissolved Oxygen (mg/L)",
  titlefont = f
)
x <- list(
  title = "Temperature (Degrees Celcius)",
  titlefont = f
)

nms3 <- names(AllCountsWatcher)

ui <- fluidPage(
  
  headerPanel("Bothell Salmon Watcher"),
  sidebarPanel(
    #sliderInput('sampleSize', 'Sample Size', min = 1, max = nrow(AllCountsAmbient3),
    #value = 1000, step = 50, round = 0),
    selectInput('x', 'X', choices = c("Month", "Year", "Species", "Stream"), selected = "Month"),
    selectInput('y', 'Y', choices = "Salmon.Count", selected = "Salmon.Count"),
    selectInput('color', 'Color', choices = c("Stream", "Species", "Month"), selected = "Month"),
    
    dateRangeInput("Date", "Date Range:",
                   start = min(AllCountsWatcher$Date),
                   end = max(AllCountsWatcher$Date)),
    
    # selectInput('facet_row', 'Facet Row', c(None = '.', nms2), selected = "Stream"), to choose all categories as facet type
    selectInput('facet_row', 'Facet Row', c(None = '.', "Year", "Species", "Month")),
    selectInput('facet_col', 'Facet Column', c(None = '.', "Year", "Species", "Month")),
    sliderInput('plotHeight', 'Height of plot (in pixels)', 
                min = 100, max = 1000, value = 700)
  ),
  
  
  mainPanel(
    plotlyOutput('trendPlot', height = "900px")
  )
)

server <- function(input, output) {
  
  #add reactive data information. Dataset = built in diamonds data
  #dataset <- reactive({
  # AllCountsAmbient3[sample(nrow(AllCountsAmbient3), input$sampleSize),]
  #})
  
  dataset <- reactive({
    filter(AllCountsWatcher, between(Date, input$Date[1], input$Date[2]))
  })
  
  output$trendPlot <- renderPlotly({
    
    # build graph with ggplot syntax
    p <- ggplot(dataset(), aes_string(x = input$x, y = input$y, color = input$color)) + 
      geom_col() +theme_bw() + theme(axis.title.y = element_text(size=18), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                                     axis.text.y = element_text(size = 15), legend.title = element_text(size=15), legend.text = element_text(size=15), legend.position = "none")
    
    # if at least one facet column/row is specified, add it
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .') p <- p + facet_grid(facets)
    
    ggplotly(p) %>% 
      layout(height = input$plotHeight, autosize=TRUE)
    
  })
  
}

shinyApp(ui, server)
