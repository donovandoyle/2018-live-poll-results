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
                  tabPanel("Linear Regression", plotOutput("myPlot")),
                  tabPanel("Statistics", plotOutput("statistics")))
      
                 
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  

    
    
  output$myPlot <- renderPlot({
    
    joined_data %>%
      filter(win_party == input$x) %>%
      ggplot(aes(x = Rep_per, y = rep_true)) + geom_point(response = "dodge") + geom_smooth(method = "lm") + geom_abline(linetype = "dashed") + ggtitle("Republican Prediction Error and Winning Party") +
      xlab("Republican Advantage Prediction") + ylab("Republican Advantage Results")
    
    
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
  
  output$stats <- renderPrint({
    my_formula <- paste0("accuracy ~ ", input$variable)
    m0 <- (lm(my_formula, data = app_data))
    m1 <- summary(m0)
    fstat <- m1$fstatistic 
    pval <- pf(fstat[1], fstat[2], fstat[3], lower.tail = F)
    
    
    HTML(paste(tags$ul(
      tags$li("The correlation coefficient is about ", strong(round(m1$coefficients[2], digits = 2)), 
              ". This is the slope of the regression line and means that the variables have a ", weak_strong(), " relationship."),
      tags$li("The multiple r-squared is appoximately ", strong(round(m1$r.squared, digits = 2)), 
              ". This means that roughly ", round((m1$r.squared)*100, digits = 2), "percent of the variation is explained by this variable." ),
      tags$li("The p-value is appoximately ", strong(round(pval, digits = 2)), ". This means that the result ", is_sig(), " statistically significant
              with respect to a significance level of 0.05."))))})
    
  
  }


# Run the application 
shinyApp(ui = ui, server = server)

