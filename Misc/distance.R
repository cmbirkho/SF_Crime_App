
library(geosphere)
# pull the data 
dbPath <- "./Application/sf_crime_db.sqlite"
db <- dbConnect(RSQLite::SQLite(), dbname = dbPath)

df <- dbGetQuery(db, "SELECT 
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
setDT(df) # convert to data.table

timeBw <- df[, c("incident_id_nbr_cd",
                 "incident_datetime",
                 "latitude",
                 "longitude")]

timeBw <- timeBw[order(rank(incident_datetime)),]
timeBw$nxt_lat <- lead(timeBw$latitude, n = 1)
timeBw$nxt_lon <- lead(timeBw$longitude, n = 1)

timeBw <- na.omit(timeBw)

dist.Meters <- function(data){
    
    for(i in 1:nrow(data)){
        data$distance_meters[i] <- distm(c(timeBw$longitude[i], timeBw$latitude[i]),
                                         c(timeBw$nxt_lon[i], timeBw$nxt_lat[i]),
                                         fun = distHaversine)
    }
    
    return(data)
}

test <- dist.Meters(data = timeBw)


median(test$distance_meters)
