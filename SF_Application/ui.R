
library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
library(leaflet)
library(plotly)
library(wordcloud2)
library(shinycssloaders)

shinyServer(navbarPage(
    
    "SF Police Incident Reports", id = 'navbarPages',
    
    theme = shinytheme("darkly"),
    
    tabPanel("Overview",
             
             tags$head(includeHTML("google-analytics.html")),
             
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
                                              column(width = 6, withSpinner(plotlyOutput("ui_pctDistrictChart",
                                                                             width = "auto", height = "auto"))),
                                              column(width = 6, withSpinner(plotlyOutput("ui_pctDayofweek",
                                                                             width = "auto", height = "auto")))
                                          ),
                                          br(),
                                          
                                          fluidRow(
                                              column(width = 6, withSpinner(plotlyOutput("ui_incidentCat",
                                                                             width = "auto", height = "auto"))),
                                              column(width = 6, withSpinner(plotlyOutput("ui_theftValue",
                                                                             width = "auto", height = "auto")))
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
    
    tabPanel("Inference", value = "inferentialStats",
             
             fluidRow(
                 
                 column(width = 8, 
                        h3("Does the frequency of incidents impact the distance between them?"),
                        br(),
                        h4("We will test this by comparing days with a high count of incidents and days with a low count of incidents."),
                        h4("Hypothesis: The more incidents there are the shorter the distance between them."),
                        h5("HO: The frequency of incidents does NOT impact the distance between them."),
                        h5("HA: The frequency of incidents does impact the distance between them."),
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
                                                     
                                                     br(),
                                                     
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
                                              
                                                 hr(),
                                                 
                                                 fluidRow(
                                                     
                                                     h4("Let's take a look at a sampling distribution of the data."),
                                                     
                                                     br(),
                                                     
                                                     column(width = 6,
                                                            plotlyOutput("FridayisHistogramSampling",
                                                                         width = 'auto', height = '600px')),
                                                     column(width = 6,
                                                            plotlyOutput("WednesdayisHistogramSampling",
                                                                         width = 'auto', height = '600px'))
                                                     
                                                 ),
                                                 
                                                 fluidRow(
                                                     
                                                     tags$div(tags$ul(
                                                         tags$li(tags$span("This suggests that a t-test would be ok with a sample size of 30.")),
                                                         tags$li(tags$span("Since the sample sizes from both groups are large enough (n > 30) 
                                                                            we can implement a Welch's Two Sample t-test to account for the sample's non-normal distribution."))
                                                     ))
                                                     
                                                 ),
                                                 
                                                 br(),
                                                 br()
                                        ),
                                        
                                        tabPanel(title = "Tests & Conclusions",
                                                 
                                                 br(),
                                                 
                                                 fluidRow(
    
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
                                                 
                                                 hr(),
                                                 
                                                 fluidRow(
                                                     h4("Interpretation of Results:"),
                                                     tags$div(tags$ul(
                                                         tags$li(tags$span("The mean under Friday is greater than the mean under Wednesday.")),
                                                         tags$li(tags$span("The p-value is greater than 0.05 indicating weak evidence against the null hypothesis.")),
                                                         tags$li(tags$span("The p-value is saying that assuming the incident count (aka day of week) has no effect, you'd 
                                                                           obtain the observed difference or more in 74% of studies due to random sampling error.")),
                                                         tags$li(tags$span("Further supporting the lack of evidence against the null hypothesis the confidence interval contains zero.")),
                                                     ))
                                                 ),
                                                 
                                                 br(),
                                                 
                                                 fluidRow(
                                                     h4("Inferences:"),
                                                     tags$div(tags$ul(
                                                         tags$li(tags$span("Based on these results we can infer that the frequency of incidents
                                                                            is not a statistically significant determinent of the average distance between incidents.
                                                                            Thus we fail to reject the null hypothesis.")),
                                                         tags$li(tags$span("Applying these findings to real life we can infer that on days when incident counts
                                                                            are high officers will still be traveling the same distance between incidents and thus
                                                                            taking the same time to respond from one incident to the next. This logic would support 
                                                                            the need for having more officers on duty during days with a higher frequency of incidents."))
                                                     ))
                                                 ),
                                                 
                                                 br(),
                                                 
                                                 fluidRow(
                                                     h4("Final Thoughts:"),
                                                     tags$div(tags$ul(
                                                         tags$li(tags$span("The results of the test are possibly skewed due to how the distance was calculated. In the
                                                                            next iteration of this test it would make sense to calculate distance between the next
                                                                            incident for each individual police district versus all of San Francisco. To be continued..."))
                                                     ))
                                                 )
                                        )
                                        
                            )
                        )
                        
                 )
                 
             )
             
    ),
    
    tabPanel("Classification", value = "machineLearning",

             h4("Can we use the text data from the report descriptions to classify the crimes?"),
             br(),
             
             tags$div(tags$ul(
                 tags$li(tags$span("Mine the text data from the incident descriptions filed by the officers.")),
                 tags$li(tags$span("Create a model that takes in text inputs and assigns the incident category."))
             )),

             br(),

             tabsetPanel(id = "machineLearningTabs",

                 tabPanel("Text Overview",

                          fluidRow(
                              column(width = 6, withSpinner(plotlyOutput("ui_top20barchart",
                                                                         width = "auto", height = "auto"))),
                              column(width = 6, withSpinner(plotlyOutput("ui_reportDescTextLength",
                                                                         width = "auto", height = "auto")))
                          )
                 ),

                 tabPanel("Classifier Tool", value = "classifierTool",

                          br(),

                          fluidRow(
                              column(width = 12, h4("Describe the crime using the dropdown and description boxes:"))
                          ),
                          
                          br(),

                          fluidRow(
                              column(width = 3, uiOutput("top10wordsList")),
                              column(width = 3, uiOutput("top20wordsList")),
                              column(width = 6, textInput("classTextInput",
                                                          label = "Text Input:",
                                                          value = "Enter text..."))
                          ),

                          br(),
                         
                          fluidRow(
                              column(width = 3, h4("Predicted category:")),
                              
                              ),
                          fluidRow(
                              column(width = 6,
                                     h2(tags$b(tags$span(textOutput("predictedClass", 
                                                                    inline = TRUE), style = "color:green"))))
                          )
                 )
             )
             
    ),
    
    tabPanel(title = "Data", value = 'dataExplorer',
             
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
                        helpText("For documentation and code:", a("click here",
                                                                  href = "https://github.com/cmbirkho/SF_Crime_App")))
             ),
             
             br(),
             
             fluidRow(
                 
                 column(width = 6,
                        h5("Due to inconsistencies in the data, a sample of the data from the above source was used for this project. 
                            The data was filtered to incident reports during the week of 2019-09-29 to 2019-10-05. 
                            Which accounts for ~60% percent of the incident reports from this source at the time the data was collected."))
             )
    )
))