
library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
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
                                              column(width = 6, plotlyOutput("ui_pctDistrictChart",
                                                                             width = "auto", height = "auto")),
                                              column(width = 6, plotlyOutput("ui_pctDayofweek",
                                                                             width = "auto", height = "auto"))
                                          ),
                                          br(),
                                          br(),
                                          fluidRow(
                                              column(width = 4, valueBoxOutput("nbrIncidents")),
                                              column(width = 4, valueBoxOutput("pctCriminal")),
                                          ),
                                          br(),
                                          br(),
                                          fluidRow(
                                              column(width = 4, valueBoxOutput("avgNbrIncidents")),
                                              column(width = 4, valueBoxOutput("value4"))
                                          )
                                          
                                 ),
                                 
                                 tabPanel(title = "Interactive Map", value = 'overviewTab2',
                                          
                                          leafletOutput("overviewMap", height = 1000, width = 1500),
                                          
                                          absolutePanel(id = 'controls', class = "panel panel-default",
                                                        fixed = TRUE, draggable = TRUE, top = 'auto',
                                                        left = 555, right = 'auto', bottom = 'auto',
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