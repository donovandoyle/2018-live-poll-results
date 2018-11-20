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

data <- read_rds("polls_clean.rds")
joined_data <- read_rds("joined_data.rds")

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
     
      
      tabsetPanel(type = "tabs",
                  tabPanel("About this app", htmlOutput("about")),
                  tabPanel("Scatterplot", plotlyOutput("myPlot")),
                  tabPanel("Linear regression plot", plotOutput("myPlot2")))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$myPlot <- renderPlotly({ 
    
   data%>%
      filter(educ == input$y, ager == input$d, gender == input$k, race_eth == input$c) %>%
     ggplot(aes(x = response)) + geom_bar(response = "dodge") + ggtitle("Predicted Republican Advantage is not Always Right ") +
     xlab("Republican Advantage Prediction") + ylab("Republican Advantage Results")})
  
  output$myPlot2 <- renderPlot({
    
    joined_data %>%
      mutate(dem_diff = dem_true - Dem_per) %>%
      mutate(rep_diff = rep_true - Rep_per) %>%
      ggplot(aes(x = Dem_per, y = dem_true)) + geom_point(response = "dodge")
    
    
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

