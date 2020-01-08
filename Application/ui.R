
library(shiny)
library(shinythemes)
library(leaflet)
library(plotly)

shinyServer(navbarPage(
    "San Francisco Criminal Offenses",
    
    theme = shinytheme("sandstone"),
    
    tabPanel("Overview",
            mainPanel(
                tabsetPanel(
                    tabPanel("Summary Statistics", 
                             
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
                                 fluidRow(
                                     column(8, ),
                                     column(12, plotlyOutput("categoryChart"))
                                 )
                             )
                    ),
                    
                    
                    tabPanel("Interactive Map", 
                             
                             div(class='outer',
                                 
                                 tags$head(
                                     # include custom CSS
                                     includeCSS("styles.css"),
                                 ),
                                 
                                 ),
                             
                             leafletOutput("overviewMap", height = 850, width = 1900),
                             
                             absolutePanel(id = 'controls', class = "panel panel-default",
                                           fixed = TRUE, draggable = TRUE, top = 150,
                                           left = 'auto', right = 20, bottom = 'auto',
                                           width = 330, height = 'auto',
                                           
                                           h2("District Explorer"),
                                           
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
                                           
                                           plotOutput("barChartMap", height = 400)
                             ),
                             
                             # tags$div(id="cite",
                             #          'Data was refreshed on ', tags$em('2020-01-07'))
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