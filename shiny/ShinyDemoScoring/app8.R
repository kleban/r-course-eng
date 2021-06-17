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
library(rhandsontable)
library(caret)
library(InformationValue)

# HERE WE ADDED train test split

ui <- dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(
        # MENU
        sidebarMenu(
            menuItem("Demo", tabName = "demo", icon = icon("th")),
            menuItem("Data", tabName = "dataset", icon = icon("database")),
            menuItem("EDA", tabName = "charts", icon = icon("bar-chart")),            
            menuItem("Train/test split", tabName = "split", icon = icon("tasks")),
            menuItem("Build models", tabName = "models", icon = icon("random"))
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
                        
                       )),
            tabItem(tabName = "split",
                    box(title = "Select split ration", status = "primary", solidHeader = TRUE,
                        width =12, 
                        fluidRow(                        column(8 ,
                               sliderInput("sliderSplit", "Dataset split:", 1, 80, 65)),
                        column(4, 
                               br(),br(),
                               actionButton("splitButton", "Split it!"))),
                        hr(),
                        fluidRow(
                          column(width = 6, 
                                 h2("Train set"),
                                 rHandsontableOutput("split_train_table")),
                          column(width = 6, 
                                 h2("Test set"),
                                 rHandsontableOutput("split_test_table"))))),
            
            tabItem(tabName = "models",
                    box(title = "Click to billd models: logistic regression and neural netowrk:", 
                        width =12,
                        fluidRow(
                          column(12, actionButton("modelIt", "Build models"))
                        ),
                        hr(),
                        fluidRow(
                          column(width = 6,
                                 h2("Variables importance LR:"),
                                 plotOutput("impGlmPlot", height = 450)),
                          column(width = 6,
                                 h2("Variables importance NN:"),
                                 plotOutput("impNnPlot", height = 450))
                        )))
            )
        )
    )

server <- function(input, output, session) {
    
    set.seed(111)
    
    histdata <- data.frame(value = rnorm(500)) # rewrited to data.frame
    
    my_data <- read.csv("data/scoring.csv")
    my_data <- my_data %>% mutate(Status = ifelse(Status == "bad", 1 , 0)) # replace good/bad with 0/1
    
    target_variable = "Status" # its fixed!
    
    train_set <- reactiveValues(data = NULL)
    test_set <- reactiveValues(data = NULL, 
                               glm = NULL, 
                               prediction_glm = NULL)

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
    
    observeEvent(input$splitButton, {
      
      trainIndex <- createDataPartition(my_data[, target_variable], p = input$sliderSplit/100, 
                                        list = FALSE, 
                                        times = 1)
      train_set$data <- my_data[trainIndex, ]
      test_set$data <- my_data[-trainIndex, ]
    })

    observeEvent(input$modelIt, {
      
      #GLM
      trainctrl <- trainControl(verboseIter = TRUE) 
      
      test_set$glm <- train(as.formula(paste(target_variable, "~ .")), 
                     data = train_set$data, method = "glm", 
                     trControl = trainctrl)
      
      test_set$prediction_glm <- predict(test_set$glm, newdata = test_set$data)
      
      test_set$nn <- train(as.formula(paste(target_variable, "~ .")), 
                    data = train_set$data, 
                    method = "nnet") 
      
     test_set$prediction_nn <- predict(test_set$nn, newdata = test_set$data)
    })
    
    output$split_test_table = renderRHandsontable({
      
      if(!is.null(test_set$data)) {
        ct <- gmodels::CrossTable(test_set$data[,target_variable])
        rhandsontable(data.frame(ct$t) %>% bind_rows(data.frame(round(ct$prop.row*100, 2))))
      }
    })
    
    output$split_train_table = renderRHandsontable({
      
      if(!is.null(train_set$data)) {
        ct <- gmodels::CrossTable(train_set$data[,target_variable])
        rhandsontable(data.frame(ct$t) %>% bind_rows(data.frame(round(ct$prop.row*100, 2))))
      }
    })
    
    output$impGlmPlot <- renderPlot({
      
      imp <- varImp(test_set$glm)
      plot(imp)
      
    })
    
    output$impNnPlot <- renderPlot({
      
      imp <- varImp(test_set$nn)
      plot(imp)
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