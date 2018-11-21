#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
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
ui <- fluidPage(fluidPage(theme = shinytheme("cerulean")),
                
                # Application title
                titlePanel("Republican Prediction Error and Winning Party"),
                
                
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
                                tabPanel("Linear Regression", plotlyOutput("myPlot")),
                                tabPanel("Statistics", htmlOutput("stats")))
                    
                    
                  )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  #Creating a scatter plot
  output$myPlot <- renderPlotly({
    
    joined_data %>%
      filter(win_party == input$x) %>%
      ggplot(aes(x = Rep_per, y = rep_true)) + geom_point(response = "dodge") + geom_smooth(method = "lm") + geom_abline(linetype = "dashed") + ggtitle("Republican Prediction Error and Winning Party") +
      xlab("Proportion of Republicans in Polling") + ylab("True Proportion that Voted Republican")
    
    
  })
  
  output$about <- renderUI({
    
    # Provide users with a summary of the application and instructions
    # Provide users with information on the data source
    
    str1 <- paste("Summary")
    str2 <- paste("This application uses polling data and election results data to observe prediction error.")
    str3 <- paste("Instructions") 
    str4 <- paste("Click through the above tabs to see the plot and the statistical analysis. On this plot, you can see the polled Republican percentage compared
                  to the true percentage of Republican votes after the election. Toggle between the winning party to see how prediction error came into effect in close races.")
    str5 <- paste("Data Source")
    str6 <- paste("The New York Times Upshot/Sienna Poll and The New York Times Election Results Coverage")
    
    HTML(paste(h3(str1), p(str2), h3(str3), p(str4), h3(str5), p(str6)))})
  
  output$stats <- renderUI({
    
    #Provide statistical data for the visual seen in the other tab. 
    
    
    str1 <- paste("Republican")
    str2 <- paste("Insert Republican stats here")
    str3 <- paste("Democrat") 
    str4 <- paste("Insert Democrat stats here")
    str5 <- paste("Undecided")
    str6 <- paste("Insert Undecided stats here")
    
    HTML(paste(h3(str1), p(str2), h3(str3), p(str4), h3(str5), p(str6)))})
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)

