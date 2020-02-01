
library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
library(leaflet)
library(plotly)
library(wordcloud2)

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
                                 
                                 tabPanel(title = 'Word Cloud', value = 'wordCloud',
                                          
                                          wordcloud2Output("theWordCloud", height = '500px', width = '1000px')
                                          
                                 ),
                                 
                                 tabPanel(title = "Interactive Map", value = 'interactiveMap',
                                          
                                          leafletOutput("overviewMap", height = 1000, width = 1000),
                                          
                                          
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
                        h5("HO: The mean under Friday is equal to or greater than the mean under Wednesday."),
                        h5("HA: The mean under Friday is less than the mean under Wednesday."),
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
                                                     
                                                     tags$div(tags$ul(
                                                         tags$li(tags$span("From the histogram we can see the distribution of distance between incidents does not appear to be normal.")),
                                                         tags$li(tags$span(textOutput("shapiroTest")))
                                                     )),
                                                     
                                                     h4("Is the deviation from normality going to materially impact results?"),
                                                     
                                                     tags$div(tags$ul(
                                                         tags$li(tags$span("Our intention is to complete a t-test to compare the means of
                                                                            the two groups. The t-test is a robust test with the assumption
                                                                            to normality. Meaning that some deviation from normality will not 
                                                                            have a large impact on our Type 1 error rate. The exception to this 
                                                                            is if the ratio of the smallest to largest group is greater than
                                                                            1.5. Since our data meets this exception we should further validate 
                                                                            that a t-test is appropriate."))
                                                     ))
                                                 ),
                                              
                                                 br(),
                                                 
                                                 fluidRow(
                                                     
                                                     h4("Let's take a look at a sampling distribution of the data."),
                                                     
                                                     column(width = 6,
                                                            plotlyOutput("isHistogramSampling",
                                                                         width = 'auto', height = '600px'))
                                                     
                                                 ),
                                                 
                                                 fluidRow(
                                                     
                                                     tags$div(tags$ul(
                                                         tags$li(tags$span("This suggests that a t-test would be ok with a sample size of 30.")),
                                                         tags$li(tags$span("Since the sample sizes from both groups are large enough (n > 30) 
                                                                            we can implement a Welch's Two Sample t-test to account for the non-normal distribution."))
                                                     ))
                                                     
                                                 ),
                                                 
                                                 br(),
                                                 br()
                                        ),
                                        
                                        tabPanel(title = "Tests & Conclusions",
                                                 
                                                 br(),
                                                 
                                                 fluidRow(
                                                     
                                                     h4("To test our hypothesis we will use a Welch's Two Sample t-test."),
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
                                                         tags$li(tags$span("The p-value is greater than 0.05 indicating weak evidence against the null hypothesis.")),
                                                         tags$li(tags$span("The p-value is saying that assuming the incident count (aka day of week) has no effect, you'd 
                                                                           obtain the observed difference or more in 74% of studies due to random sampling error.")),
                                                         tags$li(tags$span("Further supporting the lack of evidence against the null hypothesis the confidence interval contains zero.")),
                                                         tags$li(tags$span("Based on these results we can infer that the number of incidents
                                                                            does not have a statistically significant effect on mean distance between them. 
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
    
    # tabPanel("Machine Learning", value = "machineLearning",
    #          
    #          h4(""),
    #          
    #          br(),
    #          
    #          sidebarLayout(
    #              
    #              sidebarPanel(width = 3,
    #                           
    #                           
    #              ),
    #              
    #              mainPanel(
    #                  
    #                  tabsetPanel(
    #                      
    #                      tabPanel("Results",
    #                               
    #                      ),
    #                      
    #                      tabPanel("Model Validation",
    #                               
    #                               
    #                      )
    #                  )
    #                  
    #              )
    #          )
    #          
    # ),
    
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
                        h5("Due to inconsistencies in the data, a sample of the data from the above source was used for this project. 
                            The data was filtered to incident reports during the week of 2019-09-29 to 2019-10-05. 
                            Which accounts for ~60% percent of the incident reports from this source at the time the data was collected."))
             ),
    )
))