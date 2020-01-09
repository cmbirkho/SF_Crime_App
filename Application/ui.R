
library(shiny)
library(shinythemes)
library(leaflet)
library(plotly)

shinyServer(navbarPage(
    "San Francisco Criminal Offenses",
    
    theme = shinytheme("superhero"),
    
    tabPanel("Overview",
             
             sidebarLayout(
                 
                 sidebarPanel(width = 3,
                              
                              uiOutput("ui_districtList"),
                              
                              uiOutput("ui_dateslider"),
                              
                              uiOutput("ui_offensetypeList")
                              
                              
                 ),
                 mainPanel(
                     
                     tabsetPanel(id = "overviewTabs",
                         
                         tabPanel(title = "Summary Statistics", value = 'overviewTab1',
                                  
                                  fluidRow(
                                      
                                      splitLayout(cellWidths = c("50%", "50%"),
                                                  plotlyOutput("ui_pctDistrictChart",
                                                               width = "auto", height = "auto"),
                                                  plotlyOutput("ui_pctDayofweek",
                                                               width = "auto", height = "auto"))
                                  )
                         ),
                         
                         tabPanel(title = "Interactive Map", value = 'overviewTab2',
                                  
                                  
                                  
                                  # include custom CSS
                                  div(class='outer',
                                      
                                      tags$head(
                                          
                                          includeCSS("styles.css"),
                                      ),
                                      
                                  ),
                                  
                                  leafletOutput("overviewMap", height = 1000, width = 1500),
                                  
                                  absolutePanel(id = 'controls', class = "panel panel-default",
                                                fixed = TRUE, draggable = TRUE, top = 'auto',
                                                left = 555, right = 'auto', bottom = 150,
                                                width = 400, height = 'auto',

                                                plotOutput("barChartMap", height = 400)
                                  )
                                  
                                  
                         )
                     )
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