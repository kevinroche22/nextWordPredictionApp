#################
# Load Packages #
#################

library(shiny)
library(shinydashboard)
library(tidyverse)
library(sbo)

########################
# Load Data and Models #
########################

## Load Data
load("/Users/kevinroche22/RData/SwiftkeyTextMiningAndAnalytics/models/sboShiny3.rda") 
load("/Users/kevinroche22/RData/SwiftkeyTextMiningAndAnalytics/models/sboShiny4.rda") 
load("/Users/kevinroche22/RData/SwiftkeyTextMiningAndAnalytics/models/sboShiny5.rda") 

## Load models
sboPredictor3 <- sbo_predictor(sboShiny3) ## 3-gram model
sboPredictor4 <- sbo_predictor(sboShiny4) ## 4-gram model
sboPredictor5 <- sbo_predictor(sboShiny5) ## 5-gram model

#############################
# Build Dashboard Interface #
#############################

ui <- dashboardPage(
        
        ## Build header
        dashboardHeader(title = "Next Word Prediction"),
        
        ## Build sidebar
        dashboardSidebar(
                tags$style(HTML(".main-sidebar{width: 220px;}")),
                sidebarMenu(
                        menuItem("Instructions", ## Dropdown menu name
                                 icon = icon("arrow-down"))),
                h5("Enter a sentence in the text box on the right, and the next-word prediction results of each of the three models will appear below the text box, ranked by their probability.",
                   align = "center")
        ),
        
        ## Build body
        dashboardBody(h1(div(style="height:60px"),
                         tags$b('Enter a sentence below'), ## Main title, tags make it bold
                         align = "center",
                         div(style="height:20px")),
                      textInput("text", 
                                label = NULL, 
                                placeholder = "Enter your sentence...", 
                                width = "100%"),
                      h1(div(style="height:20px")),
                      fluidRow(
                              column("", ## For spacing
                                     align = "center",
                                     width = 3,
                                     div(style = "height:60px")),
                              column(tags$b(tags$i("Three-gram model prediction:")),
                                     align = "center",
                                     width = 2,
                                     textOutput("sbo3prediction1"),
                                     textOutput("sbo3prediction2"),
                                     textOutput("sbo3prediction3"),
                                     textOutput("sbo3prediction4"),
                                     textOutput("sbo3prediction5"),
                                     div(style = "height:60px")),
                              column(tags$b(tags$i("Four-gram model prediction:")),
                                     align = "center",
                                     width = 2,
                                     textOutput("sbo4prediction1"),
                                     textOutput("sbo4prediction2"),
                                     textOutput("sbo4prediction3"),
                                     textOutput("sbo4prediction4"),
                                     textOutput("sbo4prediction5"),
                                     div(style = "height:60px")),
                              column(tags$b(tags$i("Five-gram model prediction:")),
                                     align = "center",
                                     width = 2,
                                     textOutput("sbo5prediction1"),
                                     textOutput("sbo5prediction2"),
                                     textOutput("sbo5prediction3"),
                                     textOutput("sbo5prediction4"),
                                     textOutput("sbo5prediction5"),
                                     div(style = "height:60px")),
                              column("", ## For spacing
                                     align = "center",
                                     width = 3,
                                     div(style = "height:60px"))
                      )
        ))

###########################
# Define Dashboard Server #
###########################

server <- function(input, output) {   
        
        
        ## 3-gram model
        output$sbo3prediction1 <- renderText({
                sbo3prediction1 <- predict(sboPredictor3, input$text)[1]
        })
        
        output$sbo3prediction2 <- renderText({
                sbo3prediction2 <- predict(sboPredictor3, input$text)[2]
        })
        
        output$sbo3prediction3 <- renderText({
                sbo3prediction3 <- predict(sboPredictor3, input$text)[3]
        })
        
        output$sbo3prediction4 <- renderText({
                sbo3prediction4 <- predict(sboPredictor3, input$text)[4]
        })
        
        output$sbo3prediction5 <- renderText({
                sbo3prediction5 <- predict(sboPredictor3, input$text)[5]
        })
        
        
        ## 4-gram model
        output$sbo4prediction1 <- renderText({
                sbo4prediction1 <- predict(sboPredictor4, input$text)[1]
        })
        
        output$sbo4prediction2 <- renderText({
                sbo4prediction2 <- predict(sboPredictor4, input$text)[2]
        })
        
        output$sbo4prediction3 <- renderText({
                sbo4prediction3 <- predict(sboPredictor4, input$text)[3]
        })
        
        output$sbo4prediction4 <- renderText({
                sbo4prediction4 <- predict(sboPredictor4, input$text)[4]
        })
        
        output$sbo4prediction5 <- renderText({
                sbo4prediction5 <- predict(sboPredictor4, input$text)[5]
        })
        
        
        ## 5-gram model
        output$sbo5prediction1 <- renderText({
                sbo5prediction1 <- predict(sboPredictor5, input$text)[1]
        })
        
        output$sbo5prediction2 <- renderText({
                sbo5prediction2 <- predict(sboPredictor5, input$text)[2]
        })
        
        output$sbo5prediction3 <- renderText({
                sbo5prediction3 <- predict(sboPredictor5, input$text)[3]
        })
        
        output$sbo5prediction4 <- renderText({
                sbo5prediction4 <- predict(sboPredictor5, input$text)[4]
        })
        
        output$sbo5prediction5 <- renderText({
                sbo5prediction5 <- predict(sboPredictor5, input$text)[5]
        })
        
}

###########
# Run App #
###########

shinyApp(ui = ui, server = server)
