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
library(DT)

# HERE WE ADDED NAVIGATION TABS AND BOXES for ITEMS

ui <- dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(
        # MENU
        sidebarMenu(
            menuItem("Demo", tabName = "demo", icon = icon("th")),
            menuItem("Data", tabName = "dataset", icon = icon("database"))
        )
    ),
    dashboardBody(
        
        tabItems(
            # First tab content
            tabItem(tabName = "demo",
                    fluidRow(
                        
                        box(title = "Histogram", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE,
                            plotOutput("plot1", height = 250)),
                        
                        box(
                            title = "Inputs", status = "warning",  solidHeader = TRUE,
                            sliderInput("slider", "Slider input:", 1, 100, 50)
                        )
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "dataset",
                    box(title = "Data loading:", status = "primary", solidHeader = TRUE,
                        width = 12,
                    DT::dataTableOutput("data_table"))
                    
            )
        )
    )
)

server <- function(input, output) {
    
    set.seed(111)
    
    histdata <- data.frame(value = rnorm(500)) # rewrited to data.frame
    
    my_data <- read.csv("data/scoring.csv")
    
    output$plot1 <- renderPlot({
        data <- histdata %>% top_n(input$slider) # selected top_n from data.frame by slider
        ggplot(data, aes(x=value)) + 
            geom_histogram(color="darkblue", bins=10, fill="lightblue") # render histogram
        
    })
    
    output$data_table = DT::renderDataTable({
        my_data
    })
}

shinyApp(ui, server)