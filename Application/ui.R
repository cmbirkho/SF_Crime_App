
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
        sidebarLayout(
            sidebarPanel(
                selectInput("districtInput","Select your district:", c("Bayview", 
                                                                       "Central",
                                                                       "Ingleside", 
                                                                       "Mission",
                                                                       "Northern", 
                                                                       "Park", 
                                                                       "Richmond",
                                                                       "Southern", 
                                                                       "Taraval",
                                                                       "Tenderloin")),
                
                submitButton("Submit"),
                width = 3
            ),
            
            mainPanel(
                tabsetPanel(
                    tabPanel("Map", plotOutput("plot1")),
                    tabPanel("Chart", verbatimTextOutput("yarrr")),
                    tabPanel("Graph", verbatimTextOutput("charrr"))),
            )
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