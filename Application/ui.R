
library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
library(leaflet)
library(plotly)

shinyServer(navbarPage(
    
    "SF Police Incident Reports", id = 'navbarPages',
    
    theme = shinytheme("darkly"),
    
    tabPanel("Overview",
             
             sidebarLayout(
                 
                 sidebarPanel(width = 3,
                              
                              uiOutput("ui_districtList"),
                              
                              uiOutput("ui_dateslider"),
                              
                              uiOutput("ui_offensetypeList")
                              
                              
                 ),
                 mainPanel(
                     
                     tabsetPanel(id = "overviewTabs",
                                 
                                 tabPanel(title = "Summary Statistics", value = 'summaryStats',
                                          
                                          # suppress red output error messages
                                          tags$style(type="text/css",
                                                     ".shiny-output-error { visibility: hidden; }",
                                                     ".shiny-output-error:before { visibility: hidden; }"
                                          ),
                                          
                                          fluidRow(
                                              column(width = 3,  valueBoxOutput("nbrIncidents")),
                                              column(width = 3,  valueBoxOutput("avgNbrIncidents")),
                                              column(width = 3,  valueBoxOutput("pctCriminal")),
                                              column(width = 3,  valueBoxOutput("incidentCatMetric"))
                                          ),
                                          
                                          br(),
                                          
                                          fluidRow(
                                              column(width = 6, plotlyOutput("ui_pctDistrictChart",
                                                                             width = "auto", height = "auto")),
                                              column(width = 6, plotlyOutput("ui_pctDayofweek",
                                                                             width = "auto", height = "auto"))
                                          ),
                                          br(),
                                          
                                          fluidRow(
                                              column(width = 6, plotlyOutput("ui_incidentCat",
                                                                             width = "auto", height = "auto")),
                                              column(width = 6, plotlyOutput("ui_theftValue",
                                                                             width = "auto", height = "auto"))
                                          )
                                          
                                          
                                 ),
                                 
                                 tabPanel(title = "Interactive Map", value = 'interactiveMap',
                                          
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
    
    tabPanel("Inferential Stats", value = "inferentialStats",
             
             fluidRow(
                 
               column(width = 12, 
                      h4("Hypothesis: Does the day of the week significantly impact the time between incidents?"),
                      br(),
                      h5("HO: The mean under Friday and Saturday is less than or equal to the mean under Sunday-Thursday"),
                      h5("HA: The mean under Friday and Saturday is greater than the mean under Sunday-Thursday"))
             ),
             
             fluidRow(
                 
                 column(width = 12,
                        
                        mainPanel(
                            
                            tabsetPanel(id = 'inferenceTabs',
                                        
                                        tabPanel(title = "Distributions",
                                                 
                                                 fluidRow(
                                                     
                                                     h5("Conclusions: ")
                                                     
                                                 ),
                                                 
                                                 br(),
                                                 
                                                 fluidRow(
                                                     
                                                     h5("Raw data: "),
                                                     
                                                     column(width = 6,
                                                            plotlyOutput("isHistogram",
                                                                         width = 'auto', height = '400px')),
                                                     br(),
                                                     
                                                     column(width = 6,
                                                            plotlyOutput("isBoxPlot",
                                                                       width = 'auto', height = '400px'))
                                                 ),
                                                 
                                                 fluidRow(
                                                     
                                                     h5("Normalized data: "),
                                                     
                                                     column(width = 6,
                                                            plotlyOutput("isHistogramNormalized",
                                                                         width = 'auto', height = '400px')),
                                                     
                                                     br(),
                                                     
                                                     column(width = 6,
                                                            plotlyOutput("isBoxPlotNormalized",
                                                                         width = 'auto', height = '400px'))
                                                 )
                                        ),
                                        
                                        tabPanel(title = "Tests",
                                                 
                                                 # t-tests and results
                                        ),
                                        
                                        tabPanel(title = "Conclusions",
                                                 
                                                 # explanation of results
                                        )
                            )
                        )

                 )
                 
             )
             
    ),
    
    tabPanel("Machine Learning", value = "machineLearning",
             
             h4("Goal: Predict the distance from and the time until the next incident"),
             
             br(),
             
             sidebarLayout(
                 
                 sidebarPanel(width = 3,
                     
                     selectInput("pick_model",
                                 label = "Choose the model:",
                                 choices = c(" ",
                                             "Linear Regression",
                                             "Logistic Regression",
                                             "Random Forest"),
                                 selected = NULL, multiple = FALSE),
                     
                     uiOutput("ml_ui_districtList"),
                     
                     uiOutput("ml_ui_dayOfweekList")
                     
                     
                 ),
                 
                 mainPanel(
                     
                     tabsetPanel(
                         
                         tabPanel("Results",
                                  
                                  ),
                         
                         tabPanel("Model Validation",
                             
                             
                         )
                     )
                     
                 )
             )
             
    ),
    
    tabPanel("Optimization", value = "optimize",
             
             mainPanel(
                 
                 tabsetPanel(
                     
                     tabPanel("Problem Definition",
                              
                              h3("How do we minimize the number of officers needed on duty without sacrificing public safety?"),
                              br(),
                              h3("Assumptions:")
                              
                     ),
                     
                     tabPanel("Model Construction",
                              
                              
                     ),
                     
                     tabPanel("Model Solution",
                              sidebarLayout(
                                  
                                  sidebarPanel(
                                      
                                      h4("Model Inputs")
                                      
                                  ),
                                  
                                  mainPanel(
                                      
                                      h4("test")
                                      
                                  )
                              )
                              
                     )
                     
                 )
                 
             )
             
    ),
    
    tabPanel(title = "Data Download", value = 'dataExplorer',
             
             fluidRow(
                 column(width = 12, downloadButton("downloadData", "Download"))
             ),
             
             br(),
             br(),
             
             fluidRow(
                 
                 tags$style(HTML("
                        .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, 
                        .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, 
                        .dataTables_wrapper .dataTables_paginate {
                        
                        color: #ffffff;
                        }
        
                        thead {
                        color: #ffffff;
                        }
        
                        tbody {
                        color: #000000;
                         }
                        
                        table th {
                        text-align: center;
                        }
                        
                        table td {
                        text-align: center;
                        }
                        
                        table td {
                        font-size: 90%;
                        }
                        "
                 )),
                 
                 
                 column(width = 12, dataTableOutput("downloadTable"))
             )
    ),
    
    tabPanel("About", value = "aboutTab",
             
             fluidRow(
                 
                 column(width = 12, 
                        img(src = "seal.png", align = 'center'))
             ),
             
             br(),
             
             fluidRow(
                 
                 column(width = 12,
                        helpText("Data source:",a("click here", 
                                                  href = "https://data.sfgov.org/Public-Safety/Police-Department-Incident-Reports-2018-to-Present/wg3w-h783")))
                 
             ),
             
             br(),
             
             fluidRow(
                 
                 column(width = 6,
                        h4("Due to inconsistencies in the data a sample of the data was taken in 
                            an attempt to more accurately mirror a real life scenario. 
                            The data used in this project was filtered to incidents during the week of 2019-09-29 to 2019-10-05. 
                            This accounts for ~60% percent of the data from this source."))
             ),
    )
))