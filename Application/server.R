
library(shiny)
library(tidyverse)
library(RSQLite)
library(DBI)
library(leaflet)

shinyServer(function(input, output){
    
    # Query the data from the database
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
    
    dbDisconnect(db)  # disconnect from db
    
    # create categoryChart
    output$categoryChart <- renderPlotly({
        
    })
    
    
    # create overviewMap
    output$overviewMap <- renderLeaflet({
        
        if(input$districtInput == "All"){
            sfCrimeFiltMap <- sfCrimeMap
        } else {
            sfCrimeFiltMap <- filter(sfCrimeMap, police_district == input$districtInput)
        }
        
        pal <- colorFactor(topo.colors(11), sfCrimeMap$police_district)
        
        sfCrimeFiltMap %>% 
            leaflet() %>% 
            addTiles() %>% 
            addCircles(weight = 12, 
                       radius = sqrt(sfCrimeFiltMap$cnt) * 30,
                       color = ~ pal(police_district),
                       popup = ~ incident_category) %>% 
            addMarkers(clusterOptions = markerClusterOptions()) %>% 
            addLegend(pal = pal, values = ~ police_district, 
                      position = "topleft", title = "District")
    })
    
    # create histMap
    output$barChartMap <- renderPlot({
        
        if(input$districtInput == "All"){
            sfCrimeFiltMap <- sfCrimeMap
        } else {
            sfCrimeFiltMap <- filter(sfCrimeMap, police_district == input$districtInput)
        }
        
        ggplot(sfCrimeFiltMap, aes(x = police_district, y = cnt)) +
            geom_bar(stat = 'identity', fill = 'green', alpha = 0.5) +
            theme_classic() +
            theme(axis.text.x = element_text(size = 14, angle = 90),
                  axis.text.y = element_text(size = 10),
                  plot.title = element_text(hjust = 0.5)) +
            ggtitle("Criminal Offenses") +
            xlab(element_blank()) +
            ylab(element_blank())
        
    })
    
})