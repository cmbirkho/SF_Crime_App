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
                                               "
))



# ui output for summary tab metrics
output$ui_sumtab <- renderDataTable({
    
    sumTabMetrics <- sumTabData()[,
                                  .(`Total Incidents` = sum(cnt),
                                    `Avg Incidents Per Day` = round(sum(cnt)/uniqueN(incident_date), 1),
                                    `Types of Incidents Per Day` = uniqueN(incident_category)),
                                  by = police_district]
    
    names(sumTabMetrics) <- c("Police District", "Total Incidents",
                              "Avg Incidents Per Day", "Types of Incidents Per Day")
    
    datatable(sumTabMetrics, rownames = FALSE) 
    
})


# pie chart
plot_ly(labels = ~ pctDay$incident_day_of_week,
        values = ~ pctDay$pct) %>% 
    add_pie(hole = 0.6) %>% 
    layout(title = "Percent of Incidents by Day of Week",  showlegend = TRUE,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           margin = list(t = 90,
                         size = 14))

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


output$value4 <- renderValueBox({
    
    topInc <- pctData()[, .(cnt = sum(cnt)),
                        by = incident_category] %>% 
        arrange(desc(cnt))
    
    topInc <- topInc$incident_category[[1]]
    
    valueBox(
        topInc,
        paste('Top Incident Type'),
        width = NULL)
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



# ABOUT tab plot
# pull the data 
dbPath <- "./sf_crime_db.sqlite"
db <- dbConnect(RSQLite::SQLite(), dbname = dbPath)

aboutData <- dbGetQuery(db,   "SELECT 
                                            100 * SUM(incident_cnt) / (SELECT COUNT(*) FROM incident_reports) AS pct,
                                            incident_month,
                                            incident_year
                                        FROM incident_reports
                                        GROUP BY 
                                            incident_month,
                                            incident_year;")

dbDisconnect(db)
setDT(aboutData)

aboutData <- dcast(aboutData, incident_month ~ incident_year,
                   value.var = "pct")

output$aboutChart <- renderPlotly({
    
    plot_ly(x = aboutData$incident_month,
            y = aboutData$`2018`,
            type = 'bar',
            name = "2018",
            color = I('#004c6d')) %>%
        add_trace(y = aboutData$`2019`, 
                  name = "2019",
                  color = I('#c1ffff')) %>%
        layout(title = "Percent of Incidents by Month & Year",
               yaxis = list(ticksuffix = "%"),
               margin = list(t = 90,
                             size = 8),
               paper_bgcolor = 'transparent',
               plot_bgcolor = 'transparent',
               font = list(color = '#ffffff'))