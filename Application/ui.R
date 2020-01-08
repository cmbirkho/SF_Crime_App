
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
                              
                              selectInput("districtInput", 'District:',
                                          list('All',
                                               'Central',
                                               'Richmond',
                                               'Mission',
                                               'Taraval',
                                               'Ingleside',
                                               'Southern',
                                               'Bayview',
                                               'Northern',
                                               'Tenderloin',
                                               'Park')),
                              
                              sliderInput("dateSlider", "Date:",
                                          min = as.Date("2018-01-01", "%Y-%m-%d"),
                                          max = as.Date("2020-01-01", "%Y-%m-%d"),
                                          value = as.Date("2018-01-01"))
                 ),
                 
                 mainPanel(
                     tabsetPanel(type = 'tabs', id = "overviewTabs",
                                 
                                 tabPanel(title = "Summary Statistics", value = 'overviewTab1',
                                          
                                          
                                 ),
                                 
                                 tabPanel(title = "Interactive Map", value = 'overviewTab2',
                                          
                                          # include custom CSS
                                          div(class='outer',
                                              
                                              tags$head(
                                                  
                                                  includeCSS("styles.css"),
                                              ),
                                              
                                          ),
                                          
                                          leafletOutput("overviewMap", height = 1000, width = 1800),
                                          
                                          absolutePanel(id = 'controls', class = "panel panel-default",
                                                        fixed = TRUE, draggable = TRUE, top = 'auto',
                                                        left = 555, right = 'auto', bottom = 150,
                                                        width = 330, height = 'auto',
                                                        
                                                        plotOutput("barChartMap", height = 400)
                                          ),
                                          
                                          # tags$div(id="cite",
                                          #          'Data was refreshed on ', tags$em('2020-01-07'))
                                 )
                     )
                 )
             ),
             
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