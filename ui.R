#--------------------------------------------------
# R UI Code for the Capstone Project Shiny App
#--------------------------------------------------

suppressWarnings(library(shiny))

shinyUI(fluidPage(
    
    # Application title
    navbarPage("Data Science Capstone: Next-word Predictor"),
    
    # Sidebar layout
    sidebarLayout(
        
        sidebarPanel(
            textInput("sentence", "Enter a partial sentence below", value = "when will "),
            sliderInput("obs", "Max Predictions:",
                        min = 0, max = 10, value = 3)),
        
        mainPanel(
            h4("Sentence"),
            verbatimTextOutput("text"),
            
            h4("Prediction"),
            verbatimTextOutput("prediction"),
            
            mainPanel(plotOutput('dataset')),
            br(),
            br(),
            dataTableOutput("output_dataset")
        )
    )
))