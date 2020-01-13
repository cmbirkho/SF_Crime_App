
library(shiny)
library(DT)
library(shinydashboard)
library(tidyverse)
library(data.table)
library(lubridate)
library(RSQLite)
library(DBI)
library(leaflet)
library(plotly)

shinyServer(function(input, output, session){
    
#-------------------------------------------------------------------------------
    # create districtList for select inputs
    districtList <- reactive({
        
        dbPath <- "./sf_crime_db.sqlite"
        db <- dbConnect(RSQLite::SQLite(), dbname = dbPath)
        
        districtList <- dbGetQuery(db,  "SELECT
                                                distinct(police_district)
                                            FROM incident_reports
                                            ORDER BY police_district;")
        
        dbDisconnect(db)
        
        districtList <- districtList$police_district
        districtList
    })
    
    # ui output for districtList
    output$ui_districtList <- renderUI({
        selectInput("pick_district",
                    label = "Police District:",
                    choices = c("All", districtList()),
                    selected = NULL, multiple = FALSE)
    })
    
    # create offensetypeList for select inputs
    offensetypeList <- reactive({
        
        dbPath <- "./sf_crime_db.sqlite"
        db <- dbConnect(RSQLite::SQLite(), dbname = dbPath)
        
        offensetypeList <- dbGetQuery(db,  "SELECT
                                                distinct(incident_category)
                                            FROM incident_reports
                                            ORDER BY incident_category;")
        
        dbDisconnect(db)
        
        offensetypeList <- offensetypeList$incident_category
        offensetypeList
    })
    
    # ui output for offensetypeList
    output$ui_offensetypeList <- renderUI({
        selectInput("pick_offensetype",
                    label = "Incident Category:",
                    choices = c("All", offensetypeList()),
                    selected = NULL, multiple = FALSE)
    })
    
    # get maxdate for slider
    maxDate <- reactive({
        
        dbPath <- "./sf_crime_db.sqlite"
        db <- dbConnect(RSQLite::SQLite(), dbname = dbPath)
        
        maxDate <- dbGetQuery(db,  "SELECT
                                        MAX(DATE(incident_date)) as dt
                                    FROM incident_reports;")
        
        dbDisconnect(db)
        
        maxDate <- maxDate$dt
        maxDate
    })
    
    # get mindate for slider 
    minDate <- reactive({
        
        dbPath <- "./sf_crime_db.sqlite"
        db <- dbConnect(RSQLite::SQLite(), dbname = dbPath)
        
        minDate <- dbGetQuery(db,  "SELECT
                                        MIN(DATE(incident_date)) as dt
                                    FROM incident_reports;")
        
        dbDisconnect(db)
        
        minDate <- minDate$dt
        minDate
    })
    
    # ui output for dates
    output$ui_dateslider <- renderUI({
        sliderInput("dateSlider", "Date:",
                    min = as.Date(minDate(), "%Y-%m-%d"),
                    max = as.Date(maxDate(), "%Y-%m-%d"),
                    value = as.Date(maxDate(), "%Y-%m-%d"))
    })
    
#-------------------------------------------------------------------------------
    # observe event expression for OVERVIEW TABS
    observeEvent(input$overviewTabs, {
        
        # pull the data used on overview tabs
        dbPath <- "./sf_crime_db.sqlite"
        db <- dbConnect(RSQLite::SQLite(), dbname = dbPath)
        
        sfCrimeData <- dbGetQuery(db, "SELECT 
                                            sum(incident_cnt) as cnt,
                                            latitude,
                                            longitude,
                                            police_district,
                                            incident_category,
                                            incident_day_of_week,
                                            incident_date,
                                            incident_value
                                        FROM incident_reports
                                        GROUP BY
                                            latitude,
                                            longitude,
                                            police_district,
                                            incident_category,
                                            incident_day_of_week,
                                            incident_date,
                                            incident_value;")
        
        dbDisconnect(db)
        setDT(sfCrimeData) # convert to data.table
        sfCrimeData[, incident_date := ymd(incident_date)] # convert to date
        
        #-----------------------------------------------------------------------
        # summary statistics tab
        if(input$overviewTabs == "overviewTab1"){
            
            # reactive expression for total count of incidents
            totalIncidents <- reactive({
                
                totalIncidents <- sfCrimeData[incident_date <= input$dateSlider,]
                totalIncidents <- sum(totalIncidents$cnt)
                totalIncidents
            })
            
            
            # data for percentage charts
            pctData <- reactive({
                
                if(input$pick_district == "All" &
                   input$pick_offensetype == "All"){
                    
                    pctData <- sfCrimeData[incident_date <= input$dateSlider,]
                    
                } else if(input$pick_district != "All" &
                          input$pick_offensetype == "All"){
                    
                    pctData <- sfCrimeData[incident_date <= input$dateSlider &
                                               police_district == input$pick_district,]
                    
                } else if(input$pick_district == "All" &
                          input$pick_offensetype != "All"){
                    
                    pctData <- sfCrimeData[incident_date <= input$dateSlider &
                                               incident_category == input$pick_offensetype,]
                    
                } else if(input$pick_district != "All" &
                          input$pick_offensetype != "All"){
                    
                    pctData <- sfCrimeData[incident_date <= input$dateSlider &
                                               police_district == input$pick_district &
                                               incident_category == input$pick_offensetype,]
                    
                } 
                
                pctData
            })
            
            # percentage by district chart
            output$ui_pctDistrictChart <- renderPlotly({
                
                pctDistrict <- pctData()[, .(pct = (sum(cnt)/totalIncidents()) * 100),
                                         by = police_district]
                
                pctDistrict$police_district <- factor(pctDistrict$police_district, 
                                                      levels = unique(pctDistrict$police_district)[order(pctDistrict$pct, 
                                                                                                         decreasing = TRUE)])
                
                plot_ly(x = pctDistrict$police_district,
                        y = pctDistrict$pct,
                        type = 'bar') %>% 
                    layout(title = "Percent of Incidents by Police District",
                           yaxis = list(ticksuffix = "%"),
                           margin = list(t = 90,
                                         size = 14),
                           paper_bgcolor = 'transparent',
                           plot_bgcolor = 'transparent',
                           font = list(color = '#ffffff'))
            })
            
            # percentage by day of week
            output$ui_pctDayofweek <- renderPlotly({
                
                pctDay <- pctData()[, .(pct = sum(cnt)/totalIncidents()),
                                    by = incident_day_of_week]
                
                
                colors <- c('rgb(119,136,153)', 
                            'rgb(105,105,105)', 
                            'rgb(128,128,128)', 
                            'rgb(169,169,169)', 
                            'rgb(192,192,192)',
                            'rgb(211,211,211)',
                            'rgb(220,220,220)')
                
                
                plot_ly(labels = ~ pctDay$incident_day_of_week,
                        values = ~ pctDay$pct,
                        marker = list(colors = colors)) %>% 
                    add_pie(hole = 0.6) %>% 
                    layout(title = "Percent of Incidents by Day of Week",  showlegend = TRUE,
                           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                           margin = list(t = 90,
                                         size = 14),
                           paper_bgcolor = 'transparent',
                           plot_bgcolor = 'transparent',
                           font = list(color = '#ffffff'))
                
            })
            
            # larceny theft incident value
            output$ui_theftValue <- renderPlotly({
                
                cntTheft <- pctData()[incident_category == 'Larceny Theft', 
                                      .(cnt = sum(cnt)),
                                      by = incident_value]
                
                cntTheft$incident_value <- factor(cntTheft$incident_value, 
                                                      levels = unique(cntTheft$incident_value)[order(cntTheft$cnt, 
                                                                                                         decreasing = TRUE)])
                
                plot_ly(cntTheft,
                        y = cntTheft$cnt,
                        x = cntTheft$incident_value,
                        type = 'bar') %>% 
                    layout(title = "Larceny Theft Incidents by Value",
                           yaxis = list(title = "Count"),
                           margin = list(t = 90,
                                         size = 14),
                           paper_bgcolor = 'transparent',
                           plot_bgcolor = 'transparent',
                           font = list(color = '#ffffff'))
                
            })
            
            # incidents by category
            output$ui_incidentCat <- renderPlotly({
                
                countIncid <- pctData()[, .(cnt = sum(cnt)),
                                        by = incident_category]
                
                countIncid$incident_category <- factor(countIncid$incident_category, 
                                                  levels = unique(countIncid$incident_category)[order(countIncid$cnt, 
                                                                                                 decreasing = FALSE)])
                
                
                plot_ly(x = countIncid$cnt,
                        y = countIncid$incident_category,
                        type = 'bar',
                        orientation = 'h') %>% 
                    layout(title = "Count of Incidents by Category",
                           margin = list(t = 90,
                                         size = 14),
                           paper_bgcolor = 'transparent',
                           plot_bgcolor = 'transparent',
                           font = list(color = '#ffffff'))
            })
            
            # output values below charts
            output$nbrIncidents <- renderValueBox({
               
                totCnt <- sum(pctData()$cnt)
                
                 valueBox(
                    tags$p(formatC(totCnt, format="d"), style = "font-size: 90%; color: #00CC66"),
                    paste('Total incidents'),
                    width = NULL)
                
            })
            
            output$avgNbrIncidents <- renderValueBox({
                
                avgInc <- sum(pctData()$cnt)/uniqueN(pctData()$incident_date)
                
                valueBox(
                    tags$p(formatC(avgInc, format="f", digits = 1), style = "font-size: 90%; color: #00CC66"),
                    paste('Incidents Per Day'),
                    width = NULL)
            })
            
            output$pctCriminal <- renderValueBox({
                
                crimInc <- pctData()[incident_category != 'Non-Criminal',]
                crimInc <- sum(crimInc$cnt) / sum(pctData()$cnt)
                crimInc <- paste(round(crimInc * 100, 0), "%")
                
                valueBox(
                    tags$p(crimInc, style = "font-size: 90%; color: #00CC66"),
                    paste('Criminal Incidents'),
                    width = NULL)
            })
            
            output$incidentCatMetric <- renderValueBox({
                
                incCat <- uniqueN(pctData()$incident_category)
                
                valueBox(
                    tags$p(incCat, style = "font-size: 90%; color: #00CC66"),
                    paste('Incident Categories'),
                    width = NULL)
            })
            
            #-------------------------------------------------------------------
            # interactive map tab
        } else if(input$overviewTabs == 'overviewTab2'){
            
            # reactive expression to get and change data
            sfCrimeFiltMap <- reactive({
                
                sfCrimeData <- sfCrimeData[, incident_date := ymd(incident_date)]
                
                
                if(input$pick_district == "All" &
                   input$pick_offensetype == "All"){
                    
                    sfCrimeFiltMap <- sfCrimeData[incident_date <= input$dateSlider,]
                    
                } else if(input$pick_district != "All" &
                          input$pick_offensetype == "All"){
                    
                    sfCrimeFiltMap <- sfCrimeData[incident_date <= input$dateSlider &
                                                 police_district == input$pick_district,]
                    
                } else if(input$pick_district == "All" &
                          input$pick_offensetype != "All"){
                    
                    sfCrimeFiltMap <- sfCrimeData[incident_date <= input$dateSlider &
                                                 incident_category == input$pick_offensetype,]
                    
                } else if(input$pick_district != "All" &
                          input$pick_offensetype != "All"){
                    
                    sfCrimeFiltMap <- sfCrimeData[incident_date <= input$dateSlider &
                                                 police_district == input$pick_district &
                                                 incident_category == input$pick_offensetype,]
                    
                } 
                
                sfCrimeFiltMap
            })
            
            # create overviewMap
            output$overviewMap <- renderLeaflet({
                
                pal <- colorFactor("viridis", sfCrimeFiltMap()$police_district)
                
                sfCrimeFiltMap() %>%
                    leaflet() %>%
                    addTiles() %>%
                    addCircles(weight = 12,
                               radius = sqrt(sfCrimeFiltMap()$cnt) * 30,
                               color = ~ pal(police_district),
                               popup = ~ incident_category) %>%
                    addMarkers(clusterOptions = markerClusterOptions()) %>%
                    addLegend(pal = pal, values = ~ police_district,
                              position = "topleft", title = "District") %>% 
                    setView(lng = -122.431297, lat = 37.773972, zoom = 12)
            })
            
            
            
        }
    })
#-------------------------------------------------------------------------------
    
    
})