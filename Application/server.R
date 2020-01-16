
library(shiny)
library(DT)
library(shinydashboard)
library(tidyverse)
library(data.table)
library(lubridate)
library(geosphere)
library(RSQLite)
library(DBI)
library(gridExtra)
library(leaflet)
library(plotly)


shinyServer(function(input, output, session){
    
#-------------------------------------------------------------------------------
# this section of server code is reserved for non observeEvent code
    
    
#-------------------------------------------------------------------------------
    # observe event expression for OVERVIEW TABS
    observeEvent(input$overviewTabs, {
        
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
        
        # pull the data used on overview tabs
        dbPath <- "./sf_crime_db.sqlite"
        db <- dbConnect(RSQLite::SQLite(), dbname = dbPath)
        
        sfCrimeData <- dbGetQuery(db, "SELECT 
                                            incident_id_nbr_cd,
                                            latitude,
                                            longitude,
                                            police_district,
                                            incident_category,
                                            incident_day_of_week,
                                            incident_date,
                                            incident_datetime,
                                            incident_value,
                                            incident_cnt
                                        FROM incident_reports;")
        
        dbDisconnect(db)
        setDT(sfCrimeData) # convert to data.table
        # convert to date and date time
        sfCrimeData[, 
                    `:=`(incident_date = ymd(incident_date),
                         incident_datetime = as.POSIXct(incident_datetime,
                                                        format = "%Y-%m-%d %H:%M:%S"))] 
        #-----------------------------------------------------------------------
        # summary statistics tab
        if(input$overviewTabs == "summaryStats"){
            
            # reactive expression for total count of incidents
            totalIncidents <- reactive({
                
                totalIncidents <- sfCrimeData[incident_date <= input$dateSlider,]
                totalIncidents <- sum(totalIncidents$incident_cnt)
                totalIncidents
            })
            
            
            # reactive expression to change data based on inputs
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
                
                pctDistrict <- pctData()[, .(pct = (sum(incident_cnt)/totalIncidents()) * 100),
                                         by = police_district]
                
                pctDistrict$police_district <- factor(pctDistrict$police_district, 
                                                      levels = unique(pctDistrict$police_district)[order(pctDistrict$pct, 
                                                                                                         decreasing = TRUE)])
                
                plot_ly(x = pctDistrict$police_district,
                        y = pctDistrict$pct,
                        type = 'bar',
                        color = I('#1287A8')) %>% 
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
                
                pctDay <- pctData()[, .(pct = sum(incident_cnt)/totalIncidents()),
                                    by = incident_day_of_week]
                
                
                colors <- c('#004c6d', 
                            '#256885', 
                            '#42849d', 
                            '#5fa2b5', 
                            '#7ec0cd',
                            '#9fdfe6',
                            '#c1ffff')
                
                
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
                                      .(cnt = sum(incident_cnt)),
                                      by = incident_value]
                
                cntTheft$incident_value <- factor(cntTheft$incident_value, 
                                                      levels = unique(cntTheft$incident_value)[order(cntTheft$cnt, 
                                                                                                         decreasing = TRUE)])
                
                plot_ly(cntTheft,
                        y = cntTheft$cnt,
                        x = cntTheft$incident_value,
                        type = 'bar',
                        color = I('#1287A8')) %>% 
                    layout(title = "Larceny Theft | Incidents by Value",
                           yaxis = list(title = "Count"),
                           margin = list(t = 90,
                                         size = 14),
                           paper_bgcolor = 'transparent',
                           plot_bgcolor = 'transparent',
                           font = list(color = '#ffffff'))
                
            })
            
            # incidents by category
            output$ui_incidentCat <- renderPlotly({
                
                countIncid <- pctData()[, .(cnt = sum(incident_cnt)),
                                        by = incident_category]
                # there's a bug somewhere in this chart, not reacting properly when inputs change
                countIncid$incident_category <- factor(countIncid$incident_category, 
                                                  levels = unique(countIncid$incident_category)[order(countIncid$cnt, 
                                                                                                 decreasing = FALSE)])
                countIncid <- countIncid[order(-rank(cnt)),]
                countIncid <- countIncid[1:10, ]
        
                
                plot_ly(x = countIncid$cnt,
                        y = countIncid$incident_category,
                        type = 'bar',
                        orientation = 'h',
                        color = I('#1287A8')) %>% 
                    layout(title = "Incidents by Category | Top 10",
                           xaxis = list(title = "Count"),
                           margin = list(t = 90,
                                         size = 14),
                           paper_bgcolor = 'transparent',
                           plot_bgcolor = 'transparent',
                           font = list(color = '#ffffff'))
            })
            
            # output values above charts
            output$nbrIncidents <- renderValueBox({
               
                totCnt <- sum(pctData()$incident_cnt)
                
                 valueBox(
                    tags$p(formatC(totCnt, format="d"), style = "font-size: 90%; color: #EBC944"),
                    paste('Total incidents'),
                    width = NULL)
                
            })
            
            output$avgNbrIncidents <- renderValueBox({
                
                avgInc <- sum(pctData()$incident_cnt)/uniqueN(pctData()$incident_date)
                
                valueBox(
                    tags$p(formatC(avgInc, format="f", digits = 1), style = "font-size: 90%; color: #EBC944"),
                    paste('Incidents Per Day'),
                    width = NULL)
            })
            
            output$pctCriminal <- renderValueBox({
                
                crimInc <- pctData()[incident_category != 'Non-Criminal',]
                crimInc <- sum(crimInc$incident_cnt) / sum(pctData()$incident_cnt)
                crimInc <- paste(round(crimInc * 100, 0), "%")
                
                valueBox(
                    tags$p(crimInc, style = "font-size: 90%; color: #EBC944"),
                    paste('Criminal Incidents'),
                    width = NULL)
            })
            
            output$incidentCatMetric <- renderValueBox({
                
                timeBw <- pctData()[, c("incident_id_nbr_cd",
                                        "incident_datetime")]
                
                timeBw <- timeBw[order(rank(incident_datetime)),]
                timeBw$nxt_incident_datetime <- lead(timeBw$incident_datetime, n = 1)
                timeBw <- na.omit(timeBw)
                timeBw$time_bw_nxt_incident <- difftime(timeBw$nxt_incident_datetime, timeBw$incident_datetime,
                                                        units = "mins")
                timeBw$time_bw_nxt_incident <- as.numeric(timeBw$time_bw_nxt_incident)
                timeBw <- round(mean(timeBw$time_bw_nxt_incident),0) # using median to exclude outliers
                timeBw <- paste(timeBw, "min", sep = " ")
                
                valueBox(
                    tags$p(timeBw, style = "font-size: 90%; color: #EBC944"),
                    paste('Incident Frequency'),
                    width = NULL)
            })
            
            #-------------------------------------------------------------------
            
            # interactive map tab
        } else if(input$overviewTabs == 'interactiveMap'){
            
            # reactive expression change data based on inputs
            sfCrimeFilt <- reactive({
                
                if(input$pick_district == "All" &
                   input$pick_offensetype == "All"){
                    
                    sfCrimeFilt <- sfCrimeData[incident_date <= input$dateSlider,]
                    
                } else if(input$pick_district != "All" &
                          input$pick_offensetype == "All"){
                    
                    sfCrimeFilt <- sfCrimeData[incident_date <= input$dateSlider &
                                                 police_district == input$pick_district,]
                    
                } else if(input$pick_district == "All" &
                          input$pick_offensetype != "All"){
                    
                    sfCrimeFilt <- sfCrimeData[incident_date <= input$dateSlider &
                                                 incident_category == input$pick_offensetype,]
                    
                } else if(input$pick_district != "All" &
                          input$pick_offensetype != "All"){
                    
                    sfCrimeFilt <- sfCrimeData[incident_date <= input$dateSlider &
                                                 police_district == input$pick_district &
                                                 incident_category == input$pick_offensetype,]
                    
                } 
                
                sfCrimeFilt
            })
            
            # create overviewMap
            output$overviewMap <- renderLeaflet({
                
                sfCrimeFiltMap <- sfCrimeFilt()[, .(cnt = sum(incident_cnt)),
                                              by = .(latitude,
                                                     longitude,
                                                     police_district,
                                                     incident_category)]
                
                
                pal <- colorFactor("viridis", sfCrimeFiltMap$police_district)
                
                sfCrimeFiltMap %>%
                    leaflet() %>%
                    addTiles() %>%
                    addCircles(weight = 12,
                               radius = sqrt(sfCrimeFiltMap$cnt) * 30,
                               color = ~ pal(police_district),
                               popup = ~ incident_category) %>%
                    addMarkers(clusterOptions = markerClusterOptions()) %>%
                    addLegend(pal = pal, values = ~ police_district,
                              position = "topleft", title = "District") %>% 
                    setView(lng = -122.431297, lat = 37.6879, zoom = 12)
            })
            
            
            
        }
    })
#-------------------------------------------------------------------------------
    # observe event for INFERENTIAL STATS tab
    observeEvent(input$navbarPages == "inferentialStats", {
        
       
        # pull the data used for this tab
        isData <- reactive({
            
            dbPath <- "./sf_crime_db.sqlite"
            db <- dbConnect(RSQLite::SQLite(), dbname = dbPath)
            
            isData <- dbGetQuery(db,  "SELECT *
                                            FROM ml_data_incidents;")
            
            dbDisconnect(db)
            
            setDT(isData)
            
            isData
            
        })
        
        # histogram with outliers
        output$isHistogram <- renderPlotly({
            
            g1 <- ggplot(isData(),
                         aes(x = min_to_nxt_incident)) +
                geom_histogram(aes(fill = ..count..)) +
                theme(
                    # panel.background = element_rect(fill = "transparent"), 
                    plot.background = element_rect(fill = "transparent", color = NA),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(), 
                    legend.background = element_rect(fill = "transparent"), 
                    legend.box.background = element_rect(fill = "transparent"),
                    title = element_text(colour = "#ffffff"),
                    axis.text = element_text(colour = '#ffffff'),
                    legend.text = element_text(colour = '#ffffff')) +
                labs(x = 'Minutes', y = 'Frequency', fill = 'Count') +
                ggtitle("Sample Distribution | Minutes Between Incidents")
            
            ggplotly(g1)
            
        })
        
        # boxplot with outliers
        output$isBoxPlot <- renderPlotly({
            
            g2 <- ggplot(isData(),
                         aes(as.factor(incident_day_of_week), min_to_nxt_incident,
                             fill = as.factor(incident_day_of_week))) +
                theme(
                    # panel.background = element_rect(fill = "transparent"), 
                    plot.background = element_rect(fill = "transparent", color = NA), 
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(), 
                    legend.background = element_rect(fill = "transparent"), 
                    legend.box.background = element_rect(fill = "transparent"),
                    title = element_text(colour = "#ffffff"),
                    axis.text = element_text(colour = '#ffffff'),
                    legend.text = element_text(colour = '#ffffff')) +
                labs(y = "Minutes", x = 'Day of Week', fill = ' ') +
                ggtitle("Minutes Between Incidents by Day of Week")
              
            
            ggplotly(g2)
            
        })
        
        # histogram outliers removed
        output$isHistogramNormalized <- renderPlotly({
            
            mu <- mean(isData()$min_to_nxt_incident)
            sdv <- sd(isData()$min_to_nxt_incident)
            
            isDataSub <- isData()[, min_to_nxt_incident := (min_to_nxt_incident - mu) / sdv]
            
            g3 <- ggplot(isDataSub,
                         aes(x = min_to_nxt_incident)) +
                geom_histogram(aes(fill = ..count..)) +
                theme(
                    # panel.background = element_rect(fill = "transparent"), 
                    plot.background = element_rect(fill = "transparent", color = NA),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(), 
                    legend.background = element_rect(fill = "transparent"), 
                    legend.box.background = element_rect(fill = "transparent"),
                    title = element_text(colour = "#ffffff"),
                    axis.text = element_text(colour = '#ffffff'),
                    legend.text = element_text(colour = '#ffffff')) +
                labs(x = 'Minutes', y = 'Frequency', fill = 'Count') +
                ggtitle("Sample Distribution | Minutes Between Incidents")
            
            ggplotly(g3)
            
        })
        
        # boxplot with outliers
        output$isBoxPlotNormalized <- renderPlotly({
            
            mu <- mean(isData()$min_to_nxt_incident)
            sdv <- sd(isData()$min_to_nxt_incident)
            
            isDataSub <- isData()[, min_to_nxt_incident := (min_to_nxt_incident - mu) / sdv]
            
            g4 <- ggplot(isDataSub,
                         aes(as.factor(incident_day_of_week), min_to_nxt_incident,
                             fill = as.factor(incident_day_of_week))) +
                theme(
                    # panel.background = element_rect(fill = "transparent"), 
                    plot.background = element_rect(fill = "transparent", color = NA), 
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(), 
                    legend.background = element_rect(fill = "transparent"), 
                    legend.box.background = element_rect(fill = "transparent"),
                    title = element_text(colour = "#ffffff"),
                    axis.text = element_text(colour = '#ffffff'),
                    legend.text = element_text(colour = '#ffffff')) +
                labs(y = "Minutes", x = 'Day of Week', fill = ' ') +
                ggtitle("Minutes Between Incidents by Day of Week")
            
            
            ggplotly(g4)
            
        })
        
    })
    
#-------------------------------------------------------------------------------
    # observe event for MACHINE LEARNING tab
    observeEvent(input$navbarPages == "machineLearning", {
        
        # create districtList for select inputs
        mlDistrictList <- reactive({
            
            dbPath <- "./sf_crime_db.sqlite"
            db <- dbConnect(RSQLite::SQLite(), dbname = dbPath)
            
            mlDistrictList <- dbGetQuery(db,  "SELECT
                                                distinct(police_district)
                                            FROM incident_reports
                                            ORDER BY police_district;")
            
            dbDisconnect(db)
            
            mlDistrictList <- mlDistrictList$police_district
            mlDistrictList
        })
        
        # ui output for districtList
        output$ml_ui_districtList <- renderUI({
            selectInput("ml_pick_district",
                        label = "Police District:",
                        choices = c(mlDistrictList()),
                        selected = NULL, multiple = FALSE)
        })
        
        # create dayOfWeekList for select inputs
        mlDayOfweekList <- reactive({
            
            dbPath <- "./sf_crime_db.sqlite"
            db <- dbConnect(RSQLite::SQLite(), dbname = dbPath)
            
            mlDayOfweekList <- dbGetQuery(db,  "SELECT
                                                distinct(incident_day_of_week)
                                            FROM incident_reports
                                            ORDER BY police_district;")
            
            dbDisconnect(db)
            
            mlDayOfweekList <- mlDayOfweekList$incident_day_of_week
            mlDayOfweekList
        })
        
        # ui output for districtList
        output$ml_ui_dayOfweekList <- renderUI({
            selectInput("ml_day_of_week",
                        label = "Day of week:",
                        choices = c(mlDayOfweekList()),
                        selected = NULL, multiple = FALSE)
        })
        
    })
    
#-------------------------------------------------------------------------------
    # observe event for OPTIMIZATION tab
    observeEvent(input$navbarPages == "optimize", {
        
        
        
        
    })
    
#-------------------------------------------------------------------------------
    # observe event for DATA DOWNLOAD tab
    observeEvent(input$navbarPages == 'dataExplorer' ,{
        
        # pull the data 
        dbPath <- "./sf_crime_db.sqlite"
        db <- dbConnect(RSQLite::SQLite(), dbname = dbPath)
        
        sfCrimeDataDld <- dbGetQuery(db, "SELECT 
                                            incident_id_nbr_cd,
                                            latitude,
                                            longitude,
                                            police_district,
                                            incident_category,
                                            incident_day_of_week,
                                            incident_date,
                                            incident_datetime,
                                            incident_value,
                                            report_date,
                                            report_datetime,
                                            incident_cnt
                                        FROM incident_reports;")
        
        dbDisconnect(db)
        setDT(sfCrimeDataDld) # convert to data.table
        
        # render table for ui
        output$downloadTable <- renderDataTable({
            
            datatable(sfCrimeDataDld, rownames = FALSE)
        })
        
        # output for download button
        output$downloadData <- downloadHandler(
            
            filename = function(){
                paste("sf_crime_data", ".csv", sep = "")
            },
            
            content = function(file){
                
                write.csv(sfCrimeDataDld, file, row.names = FALSE)
            }
        )
        
    })
    
#-------------------------------------------------------------------------------
    
    # observe event for ABOUT tab
    observeEvent(input$navbarPages == "aboutTab", {
        
        
    })
#===============================================================================
})