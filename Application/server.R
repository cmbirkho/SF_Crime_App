
library(shiny)
library(tidyverse)
library(RSQLite)
library(DBI)
library(leaflet)

shinyServer(function(input, output){
    
    
    # Query the data from the database
    dbPath <- "./sf_crime_db.sqlite"
    db <- dbConnect(RSQLite::SQLite(), dbname = dbPath)
    
    sfCrime <- dbGetQuery(db, "SELECT 
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
    # disconnect from db
    dbDisconnect(db)
    
    # create overviewMap
    output$overviewMap <- renderLeaflet({
        
        pal <- colorFactor(topo.colors(11), sfCrime$police_district)
        
        sfCrime %>% 
            leaflet() %>% 
            addTiles() %>% 
            addCircles(weight = 12, 
                       radius = sqrt(sfCrime$cnt) * 30,
                       color = ~ pal(police_district),
                       popup = ~ incident_category) %>% 
            addMarkers(clusterOptions = markerClusterOptions()) %>% 
            addLegend(pal = pal, values = ~ police_district, 
                      position = "bottomleft")
    })
    
})