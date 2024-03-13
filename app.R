library(shiny)
library(shinythemes)
library(dplyr)
library(tidyverse)
data(airquality)

# Define UI for app that draws a histogram ----
ui <- fluidPage(theme = shinytheme("cerulean"),
                
  # navbar
  navbarPage(
    "Air Quality",
    tabPanel("Histograms",
        titlePanel("Ozone"),
        p(HTML("Daily readings of the following air quality values for May 1, 1973 (a Tuesday) to September 30, 1973. <br>
        <ul>
          <li> <b>Ozone:</b> Mean ozone in parts per billion from 1300 to 1500 hours at Roosevelt Island </li>
          <li> <b>Solar.R</b>: Solar radiation in Langleys in the frequency band 4000–7700 Angstroms from 0800 to 1200 hours at Central Park </li>
          <li> <b>Wind</b>: Average wind speed in miles per hour at 0700 and 1000 hours at LaGuardia Airport </li>
          <li> <b>Temp</b>: Maximum daily temperature in degrees Fahrenheit at La Guardia Airport. </li>")),
        hr(),
       
       # Nested Layouts for side-by-side elements
       fluidRow(
           column(
             width = 6,
             # Month Chooser: Radio buttons
             radioButtons("radio", label = h4("Select a month"),
                          choices = list("May" = 5, "June" = 6, "July" = 7, "August" = 8, "September" = 9), 
                          selected = 5,),
           ),
           
           fluidRow(
               column(
                 width = 6,
                 # Month Chooser: Radio buttons
                 selectInput("select", label = h4("Select constraints"), 
                             choices = list("Ozone" = 1, "Solar Radiation" = 2, "Wind" = 3, "Temperature" = 4), 
                             selected = 1),
               ),
          )
       ),
    
       
       hr(),
       
       # Bins: Slider
       sidebarLayout(
         sidebarPanel(
           sliderInput(inputId = "bins",
                       label = "Number of bins:",
                       min = 1,
                       max = 50,
                       value = 30)
           
         ),
         
         # Main panel for displaying outputs ----
         mainPanel(
           plotOutput(outputId = "distPlot")
         )
       )
    ), # Ozone, tabPanel
  ),
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    month <- input$radio
    if(month == 5){
      month.name = "May"
    } else if(month == 6){
      month.name = "June"
    } else if(month == 7){
      month.name = "July"
    } else if(month == 8){
      month.name = "August"
    } else if(month == 9){
      month.name = "September"
    } 
    
    if(month >=5 && month <= 9){
      x <- airquality |>
        filter(Month == month)
      
      
      constraint <- input$select
      if(constraint == 1){
        x <- x |>
          select(Ozone) |>
          na.omit(x)
        
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        hist(x$Ozone, breaks = bins, col="skyblue", xlab="Ozone quantity (in ppb)",main = month.name)
      } 
      else if(constraint == 2){
        x <- x |>
          select(Solar.R) |>
          na.omit(x)
        
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        hist(x$Solar.R, breaks = bins, col="skyblue", xlab="Solar Radiation",main = month.name)
      } 
      else if(constraint == 3){
        x <- x |>
          select(Wind) |>
          na.omit(x)
        
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        hist(x$Wind, breaks = bins, col="skyblue", xlab="Wind speed",main = month.name)
      } 
      else {
        x <- x |>
          select(Temp) |>
          na.omit(x)
        
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        hist(x$Temp, breaks = bins, col="skyblue", xlab="Ozone quantity (in ppb)",main = month.name)
      }
    } 
    
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
