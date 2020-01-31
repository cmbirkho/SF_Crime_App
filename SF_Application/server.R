
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
                                            incident_time_of_day,
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
            
            # reactive expression to change data based on inputs
            pctData <- reactive({
                
                # wrapped in req to prevent logical warninng message
                if(req(input$pick_district) == "All" & 
                   req(input$pick_offensetype) == "All"){

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
              
                incidentCountDistrict <- sum(pctData()$incident_cnt)
                
                pctDistrict <- pctData()[, .(pct = (sum(incident_cnt)/incidentCountDistrict) * 100),
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
                
                incidentCountWeek <- sum(pctData()$incident_cnt)

                pctDay <- pctData()[, .(pct = sum(incident_cnt)/incidentCountWeek),
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

            # incidents by time of day
            output$ui_theftValue <- renderPlotly({
                
                incidentCountDay <- sum(pctData()$incident_cnt)
                
                cntTheft <- pctData()[,
                                      .(pct = (sum(incident_cnt)/incidentCountDay) * 100),
                                      by = incident_time_of_day]

                cntTheft$incident_time_of_day <- factor(cntTheft$incident_time_of_day,
                                                      levels = unique(cntTheft$incident_time_of_day)[order(cntTheft$pct,
                                                                                                         decreasing = TRUE)])

                plot_ly(cntTheft,
                        y = cntTheft$pct,
                        x = cntTheft$incident_time_of_day,
                        type = 'bar',
                        color = I('#1287A8')) %>%
                    layout(title = "Percent of Incidents by Time of Day",
                           yaxis = list(ticksuffix = "%"),
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

                countIncid <- countIncid[order(-rank(cnt)),]
                countIncid <- countIncid[1:10, ]

                countIncid$incident_category <- factor(countIncid$incident_category,
                                                       levels = unique(countIncid$incident_category)[order(countIncid$cnt,
                                                                                                           decreasing = FALSE)])

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
                    tags$p(formatC(totCnt, format="d"), style = "font-size: 70%; color: #EBC944"),
                    paste('Total incidents'),
                    width = NULL)

            })

            output$avgNbrIncidents <- renderValueBox({

                avgInc <- sum(pctData()$incident_cnt)/uniqueN(pctData()$incident_date)

                valueBox(
                    tags$p(formatC(avgInc, format="f", digits = 1), style = "font-size: 70%; color: #EBC944"),
                    paste('Incidents Per Day'),
                    width = NULL)
            })

            output$pctCriminal <- renderValueBox({

                crimInc <- pctData()[incident_category != 'Non-Criminal',]
                crimInc <- sum(crimInc$incident_cnt) / sum(pctData()$incident_cnt)
                crimInc <- paste(round(crimInc * 100, 0), "%")

                valueBox(
                    tags$p(crimInc, style = "font-size: 70%; color: #EBC944"),
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
                    tags$p(timeBw, style = "font-size: 70%; color: #EBC944"),
                    paste('Incident Frequency'),
                    width = NULL)
            })
            
            #-------------------------------------------------------------------
            
            # interactive map tab
        } else if(input$overviewTabs == 'interactiveMap'){
            
            # reactive expression change data based on inputs
            sfCrimeFilt <- reactive({
                
                if(req(input$pick_district) == "All" &
                   req(input$pick_offensetype) == "All"){
                    
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
                                        FROM ml_data_incidents
                                        WHERE incident_day_of_week IN ('Friday', 'Wednesday');")
            
            dbDisconnect(db)
            
            setDT(isData)
            
            isData <- isData[, incident_day_of_week := as.factor(incident_day_of_week)]
            
            isData
            
        })
        
        
        # text output for sample for Friday
        output$isTextSampFriday <- renderText({
            
            nbrRowFri <- nrow(isData()[incident_day_of_week == 'Friday',])
            meanFtFri <- isData()[incident_day_of_week == 'Friday',]
            meanFtFri <- round(mean(meanFtFri$ft_to_nxt_incident),0)
            
            paste("The sample size for Friday is ", nbrRowFri, ".", " With a mean distance to next incident ", meanFtFri, " feet.")
        })
        
        # text output for sample for Wednesday
        output$isTextSampWednesday <- renderText({
            
            nbrRowWed <- nrow(isData()[incident_day_of_week == 'Wednesday',])
            meanFtWed <- isData()[incident_day_of_week == 'Wednesday',]
            meanFtWed <- round(mean(meanFtWed$ft_to_nxt_incident),0)
            
            paste("The sample size for Wednesday is ", nbrRowWed, ".", " With a mean distance to next incident ", meanFtWed, " feet.")
        })
    
        # boxplot - sample distribution
        output$isBoxPlot <- renderPlotly({
            
            p2 <- ggplot(isData(),
                         aes(incident_day_of_week, ft_to_nxt_incident,
                             fill = incident_day_of_week)) +
                geom_boxplot() +
                scale_fill_brewer() +
                theme(
                    # panel.background = element_rect(fill = "transparent"),
                    plot.background = element_rect(fill = "transparent", color = NA),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    legend.position = "none",
                    legend.background = element_rect(fill = "transparent"),
                    legend.box.background = element_rect(fill = "transparent"),
                    plot.title = element_text(colour = "#ffffff", size = 12, hjust = 0.5),
                    title = element_text(colour = "#ffffff"),
                    axis.text = element_text(colour = '#ffffff'),
                    axis.text.x = element_text(angle = 0, size = 10, hjust = 0),
                    legend.text = element_text(colour = '#ffffff')) +
                labs(y = "Feet", x = 'Day of Week', fill = ' ') +
                ggtitle("Sample Distribution | Feet Between Incidents")

            ggplotly(p2)
        })
        
        # histogram - sample distribution
        output$isHistogram <- renderPlotly({
            
            p1 <- ggplot(isData(),
                         aes(x = ft_to_nxt_incident)) +
                geom_histogram(aes(fill = ..count..), bins = 30) +
                geom_vline(aes(xintercept = mean(ft_to_nxt_incident), 
                               color = 'mean'),
                           linetype = 'dashed', size = 1) +
                geom_vline(aes(xintercept = median(ft_to_nxt_incident), 
                               color = 'median'),
                           linetype = 'dashed', size = 1) + 
                theme(
                    # panel.background = element_rect(fill = "transparent"), 
                    plot.background = element_rect(fill = "transparent", color = NA),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(), 
                    legend.background = element_rect(fill = "transparent"), 
                    legend.box.background = element_rect(fill = "transparent"),
                    plot.title = element_text(colour = "#ffffff", size = 12, hjust = 0.5),
                    title = element_text(colour = "#ffffff"),
                    axis.text = element_text(colour = '#ffffff'),
                    legend.text = element_text(colour = '#ffffff')) +
                scale_color_manual(name = '',
                                   values = c(mean = 'hotpink', median = 'green')) +
                labs(x = 'Feet', y = 'Frequency', fill = 'Count') +
                ggtitle("Sample Distribution | Feet Between Incidents")
            
            ggplotly(p1)
            
        })
        
        # shapiro test
        output$shapiroTest <- renderText({
            
            sTest <- shapiro.test(isData()$ft_to_nxt_incident)
            paste("Shapiro Wilk's test: p-value = ", sTest$p.value, ".  Since the p-value is less 
                  than 0.05 it confirmsdoes  our variable is not normally distributed.")
        })
        
        
        # histogram - sampling distribution 
        output$isHistogramSampling <- renderPlotly({

            x <- isData()$ft_to_nxt_incident
            nbrSamples <- 1000
            sampleMeans <- rep(NA, nbrSamples)

            for (i in 1:nbrSamples) {
                sample <- sample(x, size = 30, replace = TRUE)
                sampleMeans[i] <- mean(sample)
            }

            sampleMeans <- as.data.frame(sampleMeans)
            names(sampleMeans) <- "mu"

            p3 <- ggplot(sampleMeans,
                         aes(x = mu)) +
                geom_histogram(aes(fill = ..count..), bins = 30) +
                geom_vline(aes(xintercept = mean(mu), color = 'mean'),
                           linetype = 'dashed', size = 1) +
                geom_vline(aes(xintercept = median(mu), color = 'median'),
                           linetype = 'dashed', size = 1) +
                theme(
                    # panel.background = element_rect(fill = "transparent"),
                    plot.background = element_rect(fill = "transparent", color = NA),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    legend.background = element_rect(fill = "transparent"),
                    legend.box.background = element_rect(fill = "transparent"),
                    plot.title = element_text(colour = "#ffffff", size = 12, hjust = 0.5),
                    plot.subtitle = element_text(colour = "#ffffff", size = 8, hjust = 0.5),
                    title = element_text(colour = "#ffffff"),
                    axis.text = element_text(colour = '#ffffff'),
                    legend.text = element_text(colour = '#ffffff')) +
                scale_color_manual(name = '',
                                   values = c(mean = 'hotpink', median = 'green')) +
                labs(x = 'Feet', y = 'Frequency', fill = 'Count',
                     title = "Distribution of Sample Mean (n = 30)")

            ggplotly(p3)

        })
        
        # t-test
        tTestValue <- t.test(ft_to_nxt_incident ~ incident_day_of_week, 
                             isData(), paired = FALSE)
        
        # t-test metric output for UI
        output$isTpvalue <- renderValueBox({
            
            valueBox(
                tags$p(round(tTestValue$p.value, 2), style = "font-size: 85%; color: #EBC944"),
                paste('p-value'),
                width = NULL)
        })
        
        output$isTmuFriday <- renderValueBox({
            
            valueBox(
                tags$p(round(tTestValue$estimate[1], 0), style = "font-size: 85%; color: #EBC944"),
                paste('Mean Under Friday'),
                width = NULL)
        })
        
        output$isTmuSunday <- renderValueBox({
            
            
            valueBox(
                tags$p(round(tTestValue$estimate[2], 0), style = "font-size: 85%; color: #EBC944"),
                paste('Mean Under Wednesday'),
                width = NULL)
        })
        
        output$isTlwBnd <- renderValueBox({
            
            
            valueBox(
                tags$p(round(tTestValue$conf.int[1], 1), style = "font-size: 85%; color: #EBC944"),
                paste('95% CI Lower Bound'),
                width = NULL)
        })
        
        output$isTupBnd <- renderValueBox({
            
            
            valueBox(
                tags$p(round(tTestValue$conf.int[2], 1), style = "font-size: 85%; color: #EBC944"),
                paste('95% CI Upper Bound'),
                width = NULL)
        })
        
        output$isTdf <- renderValueBox({
            
            
            valueBox(
                tags$p(round(tTestValue$parameter, 1), style = "font-size: 85%; color: #EBC944"),
                paste('Degrees of Freedom'),
                width = NULL)
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
    # observe event for DATA DOWNLOAD tab
    observeEvent(input$navbarPages == 'dataExplorer' ,{
        
        # pull the data 
        dbPath <- "./sf_crime_db.sqlite"
        db <- dbConnect(RSQLite::SQLite(), dbname = dbPath)
        
        sfCrimeDataDld <- dbGetQuery(db, "SELECT 
                                            ir.*
                                            ,ml.min_to_nxt_incident
                                            ,ft_to_nxt_incident
                                            ,min_bw_report
                                          FROM incident_reports ir
                                          JOIN ml_data_incidents ml
                                          ON ir.incident_id_nbr_cd = ml.incident_id_nbr_cd;")
        
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
    
    
#===============================================================================
})