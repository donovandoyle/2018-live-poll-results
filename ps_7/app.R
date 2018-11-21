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
    str2 <- paste("This application uses polling data and election results data from U.S. congressional districts to observe prediction error.")
    str3 <- paste("Instructions") 
    str4 <- paste("Click through the above tabs to see the plot and the statistical analysis. On this plot, you can see the polled Republican percentage compared
                  to the true percentage of Republican votes after the election. Toggle between the winning party to see how prediction error came into effect in close races.")
    str5 <- paste("Data Source")
    str6 <- paste("The New York Times Upshot/Sienna Poll and The New York Times Election Results Coverage")
    
    HTML(paste(h3(str1), p(str2), h3(str3), p(str4), h3(str5), p(str6)))})
  
  output$stats <- renderUI({
    
    #Provide statistical data for the visual seen in the other tab. 
    
    
    str1 <- paste("Republican")
    str2 <- paste("The multiple r-squared is appoximately 0.0148, a very weak correlation. This means that around 1.48% of the variation is explained by this variable.
                  The p-value is appoximately 0.64 . This means that the result is not statistically significant since it is greater than the significance level of 0.05.")
    str3 <- paste("Democrat") 
    str4 <- paste("The multiple r-squared is appoximately 0.5749, a moderately strong correlation. This means that around 57.49% of the variation is explained by this variable.
                  The p-value is appoximately 6.61e-6. This means that the result is statistically significant since it is greater than the significance level of 0.05.")
    str5 <- paste("Undecided")
    str6 <- paste("The multiple r-squared is appoximately 0.9245, a very strong correlation. This means that around 92.45% of the variation is explained by this variable.
                  The p-value is appoximately 0.0022. This means that the result is statistically significant since it is greater than the significance level of 0.05.")
    str7 <- paste("More Details")
    str8 <- paste("The dashed line found in the visual represents the if the predicted republican percentage were a perfect guess on the true, so
                  you can visualize the difference. It is interesting to note that upshot was most wrong with their predictions when it came to republican-won districts, 
                  as that relationship was not statistically significant. However, for Democrats and Undecided as winning party, the prediction was very accurate and
                  often predicted that the Republican would lose in that district, which turned out to be true. This visual could have been made with Democrat predicted and 
                  true percentage, but would have yielded similar results, just visualized differently. ")
    
    HTML(paste(h3(str1), p(str2), h3(str3), p(str4), h3(str5), p(str6), h4(str7), p(str8)))})
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)

