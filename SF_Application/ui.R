
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
                 
                 column(width = 8, 
                        h3("Does the frequency of incidents impact the distance between them?"),
                        br(),
                        h4("Hypothesis: The more incidents there are the shorter the distance between them."),
                        h5("HO: The mean distance under Friday is greater than or equal to the mean under Wednesday."),
                        h5("HA: The mean distance under Friday is less than the mean under Wednesday."),
                        br(),
                        tags$div(tags$ul(
                            tags$li(tags$span("~35% of incidents occur on Friday and ~10% of incidents occur on Wednesday.")),
                            tags$li(tags$span(textOutput("isTextSampFriday"))),
                            tags$li(tags$span(textOutput("isTextSampWednesday"))),
                            tags$li(tags$span("To calculate the distance between incidents we order them by when they occured using date and time. 
                                                We then use the Haversine distance formula to measure the distance from
                                                the first incident to the second, the second to the third, the third to the fourth and so on."))
                        ))
                 )
             ),
             
             fluidRow(
                 
                 column(width = 12,
                        
                        mainPanel(
                            
                            tabsetPanel(id = 'inferenceTabs',
                                        
                                        tabPanel(title = "Distributions",
                                                 
                                                 br(),
                                                 
                                                 fluidRow(
                                                     
                                                     column(width = 6,
                                                            plotlyOutput("isHistogram",
                                                                         width = 'auto', height = '600px')),
                                                     
                                                     column(width = 6,
                                                            plotlyOutput("isBoxPlot",
                                                                         width = 'auto', height = '600px')),
                                                     
                                                     br(),
                                                     
                                                     h4("The data appears skewed.")
                                                     
                                                 ),
                                                 
                                                 fluidRow(
                                                     
                                                     tags$div(tags$ul(
                                                         tags$li(tags$span(textOutput("shapiroTest")))
                                                     )),
                                                     
                                                     h4("Is the deviation from normality going to materially impact results?"),
                                                     
                                                     tags$div(tags$ul(
                                                         tags$li(tags$span("It will not because we intend to perform a t-test and although not valid
                                                         for small sample sizes with non-normal distributions a t-test is valid
                                                         for large sample sizes with non-normal distributions."))
                                                     ))

                                                 ),
                                                 
                                                 br(),
                                                 br(),
                                                 
                                                 fluidRow(
                                                     
                                                     h4("Let's take a look at a sampling distribution of the data."),
                                                     
                                                     column(width = 6,
                                                            plotlyOutput("isHistogramSampling",
                                                                         width = 'auto', height = '600px'))
                                                     
                                                 )
                                        ),
                                        
                                        tabPanel(title = "Tests & Conclusions",
                                                 
                                                 br(),
                                                 
                                                 fluidRow(
                                                     
                                                     h4("To test our hypothesis we will use an independent samples t-test."),
                                                     br(),
                                                     h4("T-Test Metrics: "),
                                                     column(width = 4, valueBoxOutput("isTpvalue")),
                                                     column(width = 4, valueBoxOutput("isTmuFriday")),
                                                     column(width = 4, valueBoxOutput("isTmuSunday"))
                                                     
                                                 ),
                                                 
                                                 fluidRow(
                                                     
                                                     column(width = 4, valueBoxOutput("isTlwBnd")),
                                                     column(width = 4, valueBoxOutput("isTupBnd")),
                                                     column(width = 4, valueBoxOutput("isTdf"))
                                                     
                                                 ),
                                                 
                                                 br(),
                                                 
                                                 fluidRow(
                                                     h4("Conclusions:"),
                                                     tags$div(tags$ul(
                                                         tags$li(tags$span("The mean under Friday is greater than the mean under Wednesday.")),
                                                         tags$li(tags$span("The p-value is greater than the significance level of 0.05.")),
                                                         tags$li(tags$span("The confidence interval contains zero.")),
                                                         tags$li(tags$span("Based on these results we can infer that the number of incidents
                                                         does not have a statistically significant effect on the distance between them. 
                                                         Thus we fail to reject the null hypothesis.")),
                                                         tags$li(tags$span("Applying these findings to real life we can infer that on days when incident counts
                                                        are high officers will still be traveling the same distance between incidents and thus
                                                        taking the same time to respond from one incident to the next. This logic would support 
                                                        the need for having more officers on duty during days with a higher frequency of incidents."))
                                                     ))
                                                 )
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
                              
                              h5("How do we minimize the number of officers needed on duty without sacrificing public safety?"),
                              br(),
                              h5("Assumptions:")
                              
                     ),
                     
                     tabPanel("Model Construction",
                              
                              
                     ),
                     
                     tabPanel("Model Solution",
                              sidebarLayout(
                                  
                                  sidebarPanel(
                                      
                                      h5("Model Inputs")
                                      
                                  ),
                                  
                                  mainPanel(
                                      
                                      h5("test")
                                      
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