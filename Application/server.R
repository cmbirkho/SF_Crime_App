
library(shiny)
library(tidyverse)
library(data.table)
library(lubridate)
library(RSQLite)
library(DBI)
library(leaflet)
library(plotly)

shinyServer(function(input, output, session){
    
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
    
    # create dayofweekList for select inputs
    dayofweekList <- reactive({
        
        dbPath <- "./sf_crime_db.sqlite"
        db <- dbConnect(RSQLite::SQLite(), dbname = dbPath)
        
        dayofweekList <- dbGetQuery(db,  "SELECT
                                                distinct(incident_day_of_week)
                                            FROM incident_reports
                                            ORDER BY incident_day_of_week;")
        
        dbDisconnect(db)
        
        dayofweekList <- dayofweekList$incident_day_of_week
        dayofweekList
    })
    
    # ui output for dayofweekList
    output$ui_dayofweekList <- renderUI({
        selectInput("pick_dayofweek",
                    label = "Day of week:",
                    choices = c("All", dayofweekList()),
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
                    label = "Offense Type:",
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
                                            incident_date
                                        FROM incident_reports
                                        GROUP BY
                                            latitude,
                                            longitude,
                                            police_district,
                                            incident_category,
                                            incident_day_of_week,
                                            incident_date;")
        
        dbDisconnect(db)
        setDT(sfCrimeData)
        sfCrimeData[, incident_date := ymd(incident_date)]
        totalIncidents <- sum(sfCrimeData$cnt)
        
        # summary statistics tab
        if(input$overviewTabs == "overviewTab1"){
            
            # data for percentage by district chart
            pctDistrictChart <- reactive({
                
                if(input$pick_district == "All" &
                   input$pick_offensetype == "All"){
                    
                    pctDistrictChart <- sfCrimeData[incident_date <= input$dateSlider,
                                                    .(pct = sum(cnt)/totalIncidents),
                                                    by = police_district]
                    
                } else if(input$pick_district != "All" &
                          input$pick_offensetype == "All"){
                    
                    pctDistrictChart <- sfCrimeData[incident_date <= input$dateSlider &
                                                        police_district == input$pick_district,
                                                    .(pct = sum(cnt)/totalIncidents),
                                                    by = police_district]
                    
                } else if(input$pick_district == "All" &
                          input$pick_offensetype != "All"){
                    
                    pctDistrictChart <- sfCrimeData[incident_date <= input$dateSlider &
                                                        incident_category == input$pick_offensetype,
                                                    .(pct = sum(cnt)/totalIncidents),
                                                    by = police_district]
                    
                } else if(input$pick_district != "All" &
                          input$pick_offensetype != "All"){
                    
                    pctDistrictChart <- sfCrimeData[incident_date <= input$dateSlider &
                                                        police_district == input$pick_district &
                                                        incident_category == input$pick_offensetype,
                                                    .(pct = sum(cnt)/totalIncidents),
                                                    by = police_district]
                    
                } 
                
                pctDistrictChart
            })
            
            # data for percentage day of week chart
            pctDayofweek <- reactive({
                
                if(input$pick_district == "All" &
                   input$pick_offensetype == "All"){
                    
                    pctDayofweek <- sfCrimeData[incident_date <= input$dateSlider,
                                                .(pct = sum(cnt)/totalIncidents),
                                                by = incident_day_of_week]
                    
                } else if(input$pick_district != "All" &
                          input$pick_offensetype == "All"){
                    
                    pctDayofweek <- sfCrimeData[incident_date <= input$dateSlider &
                                                    police_district == input$pick_district,
                                                .(pct = sum(cnt)/totalIncidents),
                                                by = incident_day_of_week]
                    
                } else if(input$pick_district == "All" &
                          input$pick_offensetype != "All"){
                    
                    pctDayofweek <- sfCrimeData[incident_date <= input$dateSlider &
                                                    incident_category == input$pick_offensetype,
                                                .(pct = sum(cnt)/totalIncidents),
                                                by = incident_day_of_week]
                    
                } else if(input$pick_district != "All" &
                          input$pick_offensetype != "All"){
                    
                    pctDayofweek <- sfCrimeData[incident_date <= input$dateSlider &
                                                    police_district == input$pick_district &
                                                    incident_category == input$pick_offensetype,
                                                .(pct = sum(cnt)/totalIncidents),
                                                by = incident_day_of_week]
                    
                } 
                
                pctDayofweek
            })
            
            # percentage by district chart
            output$ui_pctDistrictChart <- renderPlotly({
                
                plot_ly(x = pctDistrictChart()$police_district,
                        y = pctDistrictChart()$pct,
                        type = 'bar') %>% 
                    layout(title = "Pct of Incidents by Police District",
                           margin = list(t = 90,
                                         size = 14))
            })
            
            # percentage by day of week
            output$ui_pctDayofweek <- renderPlotly({
                
                plot_ly(labels = ~ pctDayofweek()$incident_day_of_week,
                        values = ~ pctDayofweek()$pct) %>% 
                    add_pie(hole = 0.6) %>% 
                    layout(title = "Percent of Incidents by Day of Week",  showlegend = TRUE,
                           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                           margin = list(t = 90,
                                         size = 14))
                
            })
            
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
                
                pal <- colorFactor(topo.colors(11), sfCrimeFiltMap()$police_district)
                
                sfCrimeFiltMap() %>%
                    leaflet() %>%
                    addTiles() %>%
                    addCircles(weight = 12,
                               radius = sqrt(sfCrimeFiltMap()$cnt) * 30,
                               color = ~ pal(police_district),
                               popup = ~ incident_category) %>%
                    addMarkers(clusterOptions = markerClusterOptions()) %>%
                    addLegend(pal = pal, values = ~ police_district,
                              position = "topleft", title = "District")
            })
            
            # create histMap
            output$barChartMap <- renderPlot({
                
                ggplot(sfCrimeFiltMap(), aes(x = police_district, y = cnt)) +
                    geom_bar(stat = 'identity', fill = 'green', alpha = 0.5) +
                    theme(axis.text.x = element_text(size = 14, angle = 90),
                          axis.text.y = element_text(size = 10),
                          plot.title = element_text(hjust = 0.5)) +
                    ggtitle("Incidents") +
                    xlab(element_blank()) +
                    ylab("Count")
                
            })
            
        }
    })
    
    
})