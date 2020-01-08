
library(shiny)
library(tidyverse)
library(RSQLite)
library(DBI)
library(leaflet)

shinyServer(function(input, output, session){
    
    observeEvent(input$overviewTabs, {
        
        if(input$overviewTabs == "overviewTab2"){
            # Code for Interactive Map tab
            
            # reactive expression to get/change data
            sfCrimeFiltMap <- reactive({
                
                dbPath <- "./sf_crime_db.sqlite"
                db <- dbConnect(RSQLite::SQLite(), dbname = dbPath)
                
                sfCrimeMap <- dbGetQuery(db, "SELECT 
                                                sum(incident_cnt) as cnt,
                                                latitude,
                                                longitude,
                                                police_district,
                                                incident_category
                                            FROM incident_reports
                                            GROUP BY
                                                latitude,
                                                longitude,
                                                police_district,
                                                incident_category;")
                
                dbDisconnect(db)
                
                if(input$districtInput == "All"){
                    sfCrimeFiltMap <- sfCrimeMap
                } else {
                    sfCrimeFiltMap <- filter(sfCrimeMap, police_district == input$districtInput)
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
                    theme_classic() +
                    theme(axis.text.x = element_text(size = 14, angle = 90),
                          axis.text.y = element_text(size = 10),
                          plot.title = element_text(hjust = 0.5)) +
                    ggtitle("Criminal Offenses") +
                    xlab(element_blank()) +
                    ylab("Count")
                
            })
        } 
    })
    
    
})