
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
                                 
                                 tabPanel(title = "Summary Statistics", value = 'overviewTab1',
                                          
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
             
             fluidRow(
                 
               column(width = 12, 
                      h1("Hypothesis Testing"),
                      h3("Hypothesis:"),
                      h4("Does the day of the week significantly impact the number of incidents?"),
                      h5("HO: The mean under Friday and Saturday is less than or equal to the mean under Sunday-Thursday"),
                      h5("HA: The mean under Friday and Saturday is greater than the mean under Sunday-Thursday"))
             ),
             
             fluidRow(
                 
                 column(width = 12,
                        
                        mainPanel(
                            
                            tabsetPanel(id = 'inferenceTabs',
                                        
                                        tabPanel(title = "Distributions",
                                                 
                                                 # box plots
                                                 # histogram 
                                                 # density plot
                                                 
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
    
    tabPanel("Predictions",
             
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
    
    tabPanel("About",
             
             fluidRow(
                 
                 column(width = 12, 
                        img(src = "seal.png", align = 'center'))
             ),
             
             br(),
             
             fluidRow(
                 
                 column(width = 12,
                        h4("Data source: ", uiOutput("sourceLink")))
             )
             
    )
))