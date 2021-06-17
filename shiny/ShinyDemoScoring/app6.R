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

# HERE WE ADDED Data preview

ui <- dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(
        # MENU
        sidebarMenu(
            menuItem("Demo", tabName = "demo", icon = icon("th")),
            menuItem("Data", tabName = "dataset", icon = icon("database")),
            menuItem("EDA", tabName = "charts", icon = icon("bar-chart"))
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
                        width =12,
                        DT::dataTableOutput("data_table"))),
            
            # third tab content
            tabItem(tabName = "charts",
                    box(title = "DataPreview", status = "primary", solidHeader = TRUE,
                        width =12,
                        
                        fluidRow(column(width = 4, selectInput("targetSelectInput","Target variable",
                                                               choices = c(), selected="",multiple = F)),
                                 column(width = 4, selectInput("factorSelectInput","Factor variable",
                                                                choices = c(), selected="",multiple = F)),
                                 column(width = 4, selectInput("typeSelectInput","Chart type",
                                                               choices = c("bar", "histogram", "boxplot"), selected = "bar",multiple = F))),
                        
                        hr(),
                        
                        plotOutput("edaPlot", height = 250)
                        
                       ))
        )
    )
)

server <- function(input, output, session) {
    
    set.seed(111)
    
    histdata <- data.frame(value = rnorm(500)) # rewrited to data.frame
    
    my_data <- read.csv("data/scoring.csv")
    
    target_variable = "deposit" # its fixed!

    output$plot1 <- renderPlot({
        
        data <- histdata %>% top_n(input$slider) # selected top_n from data.frame by slider
        ggplot(data, aes(x=value)) + 
            geom_histogram(color="darkblue", bins=10, fill="lightblue") # render histogram
        
    })
    
    output$edaPlot <- renderPlot({
        
        xplot <- NA
        
        if(input$typeSelectInput == "bar") {
           xplot <- ggplot(my_data, aes_string(x=input$factorSelectInput, fill=input$targetSelectInput)) +  
                geom_bar(position = "stack") 
        }
        
        if(input$typeSelectInput == "histogram") {
            xplot <- ggplot(my_data, aes_string(x=input$factorSelectInput)) + 
                geom_histogram(color="darkblue", bins=10, fill="lightblue") 
        }
        
        if(input$typeSelectInput == "boxplot") {
            xplot <- ggplot(my_data, aes_string(x=input$factorSelectInput, fill=input$targetSelectInput)) +
                geom_boxplot(fill='lightblue', color="darkblue")
        }
        
        xplot <- xplot + theme_bw() 
        
        xplot
        
    })
    
    output$data_table = DT::renderDataTable({
        my_data
    })
    
  
    
    observe({
        
        updateSelectInput(session = session,
                          inputId = "targetSelectInput", 
                          choices =  my_data %>% colnames(),
                          selected = target_variable)
        
        updateSelectInput(session = session,
                          inputId = "factorSelectInput", 
                          choices =  setdiff(my_data %>% colnames(), target_variable))

    })
}

shinyApp(ui, server)