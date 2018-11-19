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

data <- read_rds("ca_polls.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("How Well Republican Advantage Predicts Winning Party"),
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "y",
                  label = "Educ",
                  choices = unique(data$educ),
                  selected = "Some college or trade school"),
    selectInput(inputId = "d",
                label = "Age:",
                choices = unique(data$ager),
                selected = "65 and older"),
    selectInput(inputId = "k",
                label = "Gender:",
                choices = unique(data$gender),
                selected = "Male"),
    selectInput(inputId = "c",
                label = "Race/Ethnicity",
                choices = unique(data$race_eth),
                selected = "Hispanic")),
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput(outputId = "myPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$myPlot <- renderPlot({ 
    
    data%>%
      filter(educ == input$y, ager == input$d, gender == input$k, race_eth == input$c) %>%
      ggplot(aes(x = response)) + geom_bar(response = "dodge") + ggtitle("Predicted Republican Advantage is not Always Right ") +
      xlab("Republican Advantage Prediction") + ylab("Republican Advantage Results")})
}

# Run the application 
shinyApp(ui = ui, server = server)

