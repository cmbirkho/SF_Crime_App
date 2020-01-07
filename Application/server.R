
library(shiny)
library(tidyverse)
library(RSQLite)
library(DBI)
library(leaflet)
library(lubridate)
library(caret)
library(e1071)

shinyServer(function(input, output){
    
    
    # Query the data from the database
    dbPath <- "./sf_crime_db.sqlite"
    db <- dbConnect(RSQLite::SQLite(), dbname = dbPath)
    
    # pull CPI actuals data
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
    
    
    # create the main plot
    output$plot1 <- renderLeaflet({
        sfCrimeFilt <- filter(sfCrime, police_district == input$districtInput)
        
        sfCrimeFilt <- na.omit(sfCrimeFilt)
        
        pal <- colorFactor(topo.colors(11), sfCrime$police_district)
        
        sfCrimeFilt %>% 
            leaflet() %>% 
            addTiles() %>% 
            addCircles(weight = 12, 
                       radius = sqrt(sfCrimeFilt$cnt) * 30,
                       color = ~ pal(police_district),
                       popup = ~ incident_category) %>% 
            addMarkers(clusterOptions = markerClusterOptions())
    })
    
})