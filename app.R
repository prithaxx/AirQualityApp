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
       
     # Layouts for side-by-side elements
     fluidRow(
       column(
         width = 6,
         # Month Chooser: Radio buttons
         radioButtons("radio", label = h4("Select a month"),
                      choices = list("May" = 5, "June" = 6, "July" = 7, "August" = 8, "September" = 9), 
                      selected = 5),
       ),
       
       column(
         width = 6,
         # Select Constraint
         selectInput("select", label = h4("Select constraints"), 
                     choices = list("Ozone" = 1, "Solar Radiation" = 2, "Wind" = 3, "Temperature" = 4),
                     selected = 1),
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
                     value = 30),
         
         actionButton("save_btn", "Save Histogram"),
         hr(),
         h4("Saved Histograms"),
         uiOutput("saved_plots"),
       ),
       
       # Main panel for displaying outputs ----
       mainPanel(
         plotOutput(outputId = "distPlot")
       )
     )
    ) 
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  # Load saved histograms from file if it exists
  saved_histograms <- reactiveVal({
    if (file.exists("saved_histograms.rds")) {
      readRDS("saved_histograms.rds")
    } else {
      list()
    }
  })
  
  # Create a histogram
  output$distPlot <- renderPlot({
    month <- input$radio
    month.name <- switch(as.character(month),
                         "5" = "May",
                         "6" = "June",
                         "7" = "July",
                         "8" = "August",
                         "9" = "September")
    
    constraint <- input$select
    arg <- switch(as.character(constraint),
                  "1" = "Ozone",
                  "2" = "Solar.R",
                  "3" = "Wind",
                  "4" = "Temp")
    
    if (month >= 5 && month <= 9) {
      x <- airquality %>%
        filter(Month == month) %>%
        select(all_of(arg)) %>%
        na.omit()
    }
    
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x[,1], breaks = bins, col = "skyblue", xlab = arg, main = month.name)
  })
  
  # Save histogram
  observeEvent(input$save_btn, {
    # Show a modal to ask for histogram name
    showModal(modalDialog(
      title = "Save Histogram",
      textInput("histogram_name", "Enter a name for the histogram", value = "Histogram name"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_save", "Save")
      )
    ))
  })
  
  # When the user clicks save in the modal
  observeEvent(input$confirm_save, {
    removeModal()  # Remove the modal
    
    # Save the histogram details
    current_hist <- list(
      name = input$histogram_name,
      month = input$radio,
      constraint = input$select,
      bins = input$bins
    )
    
    hist_list <- saved_histograms()
    hist_list[[length(hist_list) + 1]] <- current_hist
    saved_histograms(hist_list)
    
    # Save to file
    saveRDS(hist_list, "saved_histograms.rds")
  })
  
  # Create buttons for saved histograms
  output$saved_plots <- renderUI({
    hist_list <- saved_histograms()
    if (length(hist_list) == 0) {
      return(h4("No saved histograms."))
    }
    
    buttons <- lapply(1:length(hist_list), function(i) {
      hist_data <- hist_list[[i]]
      actionButton(paste0("load_hist_", i), hist_data$name)
    })
    
    do.call(tagList, buttons)
  })
  
  # Load and display saved histogram
  observe({
    hist_list <- saved_histograms()
    lapply(1:length(hist_list), function(i) {
      observeEvent(input[[paste0("load_hist_", i)]], {
        hist_data <- hist_list[[i]]
        
        updateRadioButtons(session, "radio", selected = hist_data$month)
        updateSelectInput(session, "select", selected = hist_data$constraint)
        updateSliderInput(session, "bins", value = hist_data$bins)
      })
    })
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
