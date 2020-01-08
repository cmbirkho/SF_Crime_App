
library(shiny)
library(shinythemes)
library(leaflet)

shinyServer(navbarPage(
    "San Francisco Criminal Offenses",
    
    theme = shinytheme("sandstone"),
    
    tags$head(tags$style(HTML("
        .selectize-input, .selectize-dropdown, .main-sidebar {
          font-size: 100%;
        }
        "))),
    
    tabPanel("Overview",
            mainPanel(
                tabsetPanel(
                    tabPanel("Summary Statistics", verbatimTextOutput("yarrr")),
                    tabPanel("Interactive Map", leafletOutput("overviewMap", 
                                                  height = 850,
                                                  width = 850)))
            )
    ),
    
    tabPanel("Inferential Stats",
             
    ),
    
    tabPanel("Predictions",
    
    ),
    
    navbarMenu("More",
               tabPanel("Data Explorer",
               
               ),
               tabPanel("Your Data",
                        
               ),
               tabPanel("About",
                        
               ))
))