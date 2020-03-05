#This is a Shiny web application. You can run the application by clicking
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

dbPath1 <- "Amazon Electrical Safety Regs.xlsx"

# load the workbook

wb <- loadWorkbook(dbPath1)

#load the worksheets
Servicing_Activity_1 <-  read.xlsx(dbPath1, "Servicing Activity 1", detectDates = T)
Servicing_Activity_2 <-  read.xlsx(dbPath1, "Servicing Activity 2", detectDates = T)
Servicing_Activity_3 <-  read.xlsx(dbPath1, "Servicing Activity 3", detectDates = T)

# Combine Worksheets
AllServices <- rbind(Servicing_Activity_1, Servicing_Activity_2, Servicing_Activity_3)
#Creat Year and Month Vectors

#try this?

#how to create plotly graph first?
#ggplot(Servicing_Activity_2, aes(x = Last.Ammended, y = Regulation)) + 
 # geom_point(aes(color=Compliance)) +theme_bw() + theme(axis.title.y = element_text(size=18), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                                #axis.text.y = element_text(size = 15), legend.title = element_text(size=15), legend.text = element_text(size=15))



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
#
#
nms3 <- names(AllServices)

ui <- fluidPage(
  
  headerPanel("Data"),
  sidebarPanel(
    #sliderInput('sampleSize', 'Sample Size', min = 1, max = nrow(AllCountsAmbient3),
    #value = 1000, step = 50, round = 0),
    selectInput('x', 'X', choices = c("Last.Ammended", "Country"), selected = "Country"),
    selectInput('y', 'Y', choices = "Regulation", selected = "Regulation"),
    selectInput('color', 'Color', choices = c("Compliance", "Region", "Country"), selected = "Compliance"),
    
    dateRangeInput("Date", "Date Range:",
                   start = min(AllServices$Date),
                   end = max(AllServices$Date)),
    
    # selectInput('facet_row', 'Facet Row', c(None = '.', nms2), selected = "Stream"), to choose all categories as facet type
    selectInput('facet_row', 'Facet Row', c(None = '.',"Region"), selected = "none"),
    selectInput('facet_col', 'Facet Column',"Servicing.Activity", selected = "Servicing.Activity"),
    sliderInput('plotHeight', 'Height of plot (in pixels)', 
                min = 100, max = 1000, value = 500)
  ),
  
  
  mainPanel(
    plotOutput('trendPlot', height = "600px", width = "1000px", brush = "plot_brush" ),
    verbatimTextOutput("info")
    
  )
)

server <- function(input, output) {
  
  #add reactive data information. Dataset = built in diamonds data
  #dataset <- reactive({
  # AllCountsAmbient3[sample(nrow(AllCountsAmbient3), input$sampleSize),]
  #})
  
  dataset <- reactive({
    filter(AllServices, between(Date, input$Date[1], input$Date[2]))
  })
  
  output$trendPlot <- renderPlot({
    
    # build graph with ggplot syntax
    p <- ggplot(dataset(), aes_string(x = input$x, y = input$y, color = input$color)) + 
      geom_point() + theme_bw() + theme(axis.title.y = element_blank(), axis.title.x = element_blank(), title = element_text(size = 18), axis.text.x = element_text(size = 8, angle = 65, vjust = 0.6),
                                        axis.text.y = element_text(size = 8), legend.title = element_text(size=10), legend.text = element_text(size=10))
    
    # if at least one facet column/row is specified, add it
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .') p <- p + facet_grid(facets)
    
    
    p 
    
    #ggplotly(p) %>% 
    #layout(height = input$plotHeight, autosize=TRUE)
    
  })
  output$info <- renderPrint({
    brushedPoints(AllServices, input$plot_brush, xvar = "Country", yvar = "Regulation")
  }) 
  
}

shinyApp(ui, server)