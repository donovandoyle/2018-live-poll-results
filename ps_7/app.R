#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(dplyr)
library(plyr)
library(tidyverse)
library(foreign)
library(readxl)
library(stringr)
library(fs)
library(kableExtra)
library(formattable)
library(lubridate)
library(knitr)
library(janitor)
library(stringr)
library(plotly)


joined_data <- read_rds("joined_data.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("How Well Republican Advantage Predicts Winning Party"),
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "x",
                  label = "Winning Party:",
                  choices = unique(joined_data$win_party),
                  selected = "Republican")),
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
     
      
      tabsetPanel(type = "tabs",
                  tabPanel("About", htmlOutput("about")),
                  tabPanel("Linear Regression", plotOutput("myPlot")),
                  tabPanel("Summary", plotOutput("summary")))
      
                 
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  

    
    
  output$myPlot <- renderPlot({
    
    joined_data %>%
      filter(win_party == input$x) %>%
      ggplot(aes(x = Rep_per, y = rep_true)) + geom_point(response = "dodge") + geom_smooth(method = "lm") + geom_abline(linetype = "dashed")
    
    
  })
    
  
}

# Run the application 
shinyApp(ui = ui, server = server)

