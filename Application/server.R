
library(shiny)
library(tidyverse)
library(leaflet)
library(lubridate)
library(caret)
library(e1071)

shinyServer(function(input, output){
    
    # get and format data
    url <- "https://data.sfgov.org/resource/wg3w-h783.csv"
    sfCrime <- read.csv(url, encoding = 'UTF-8') %>% 
        mutate(cnt = 1,
               incident_date = str_sub(incident_date, 1, 10)) %>%
        mutate(incident_date = ymd(incident_date)) %>% 
        mutate(month = month(incident_date, label = TRUE, abbr = FALSE)) %>% 
        group_by(latitude, longitude, police_district, 
                 incident_category, month) %>% 
        summarise(cnt = sum(cnt))
    
    # check if there is data available
    output$check1 <- reactive({
        sfCrimeFilt <- filter(sfCrime, police_district == input$districtInput &
                                  month == input$monthInput)
        
        sfCrimeFilt <- na.omit(sfCrimeFilt)
        
        if(nrow(sfCrimeFilt) == 0){
            check1 <- 'No'
        } else {
            check1 <- 'Yes'
        }
    })
    
    # create the main plot
    output$plot1 <- renderLeaflet({
        sfCrimeFilt <- filter(sfCrime, police_district == input$districtInput &
                                  month == input$monthInput)
        
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
    
    # predict the crime
    subSfCrime <- sfCrime[,c(3:5)]
    modFit <- train(incident_category ~ ., data = subSfCrime,
                    method = 'rpart')
    
    output$predCrime <- reactive({
        
        sfCrimeFilt <- filter(sfCrime, police_district == input$districtInput &
                                  month == input$monthInput)
        
        sfCrimeFilt <- na.omit(sfCrimeFilt)
        
        if(nrow(sfCrimeFilt) == 0){
            predCrime <- 'Zoltar cant see the future without data'
        } else {
            predCrime <- predict(modFit, 
                                 newdata = data.frame(police_district = input$districtInput,
                                                      month = input$monthInput))
        }
    })
    
})