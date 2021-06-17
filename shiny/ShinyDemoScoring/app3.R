#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinydashboard)
library(ggplot2) # added
library(dplyr) # added
library(magrittr) # pipe

# HERE WE ADD GGPLOT

ui <- dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(
            box(plotOutput("plot1", height = 250)),
            
            box(
                title = "Controls",
                sliderInput("slider", "Number of observations:", 1, 100, 50)
            )
        )
    )
)

server <- function(input, output) {
    set.seed(111)
    
    histdata <- data.frame(value = rnorm(500)) # rewrited to data.frame
   
    output$plot1 <- renderPlot({
        data <- histdata %>% top_n(input$slider) # selected top_n from data.frame by slider
        ggplot(data, aes(x=value)) + 
            geom_histogram(color="darkblue", bins=10, fill="lightblue") # render histogram
        
    })
}

shinyApp(ui, server)