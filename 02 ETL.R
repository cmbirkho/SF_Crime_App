# Clear Workspace
rm(list = ls())

# Load packages
library(RSQLite)
library(DBI)
library(data.table)
library(tidyverse)
library(lubridate)
library(geosphere)

# Load data from db
# create a connection to the database
dbPath <- "C:/Users/Cbirkho/Documents/SF_Crime_App/Application/sf_crime_db.sqlite"
db <- dbConnect(RSQLite::SQLite(), dbname = dbPath)

sfCrime <- dbGetQuery(db, "SELECT * FROM incident_reports;")

dbDisconnect(db)

# convert to data.table
setDT(sfCrime)

# format columns
sfCrime <- sfCrime[,
                   `:=`(incident_datetime = as.POSIXct(incident_datetime,
                                                       format = "%Y-%m-%d %H:%M:%S"))]

#===============================================================================
# Create a new table containing more numerical information for modeling

# incident_time_of_day - dawn (midnight to 6am), morning (6am-12pm), afternoon (12pm-6pm), evening (6pm-12am)

# Initial column selection
sfCrime <- sfCrime[, c("incident_id_nbr_cd",
                       "incident_day_of_week",
                       "police_district",
                       "incident_datetime",
                       "latitude",
                       "longitude")]

# Make tables by police_district
richmond <- sfCrime[police_district == "Richmond", ]
mission <- sfCrime[police_district == "Mission", ]
central <- sfCrime[police_district == "Central", ]
park <- sfCrime[police_district == "Park", ]
taraval <- sfCrime[police_district == "Taraval", ]
ingleside <- sfCrime[police_district == "Ingleside", ]
southern <- sfCrime[police_district == "Southern", ]
tenderloin <- sfCrime[police_district == "Tenderloin", ]
bayview <- sfCrime[police_district == "Bayview", ]
northern <- sfCrime[police_district == "Northern", ]

# min_to_nxt_incident by police_district
time.till <- function(data){
    
    data <- data[order(rank(incident_datetime)),]
    
    data$nxt_incident_datetime <- lead(data$incident_datetime, n = 1)
    
    data$min_to_nxt_incident <- difftime(data$nxt_incident, data$incident_datetime,
                                            units = "mins")
    
    data$min_to_nxt_incident <- as.numeric(data$min_to_nxt_incident)
    
    data <- data[, - "nxt_incident_datetime"]
    
    return(data)
}

richmond <- time.till(data = richmond)
mission <- time.till(data = mission)
central <- time.till(data = central)
park <- time.till(data = park)
taraval <- time.till(data = taraval)
ingleside <- time.till(data = ingleside)
southern <- time.till(data = southern)
tenderloin <- time.till(data = tenderloin)
bayview <- time.till(data = bayview)
northern <- time.till(data = northern)

# miles_to_nxt_incident
dist.Miles <- function(data){
    
    data$nxt_lat <- lead(data$latitude, n = 1)
    data$nxt_lon <- lead(data$longitude, n = 1)
    
    for(i in 1:nrow(data)){
        data$distance_meters[i] <- distm(c(data$longitude[i], data$latitude[i]),
                                         c(data$nxt_lon[i], data$nxt_lat[i]),
                                         fun = distHaversine)
    }
    
    data$miles_to_nxt_incident <- round(data$distance_meters / 1609.34, 2)
    
    data <- data[, - c("nxt_lat",
                       "nxt_lon",
                       "distance_meters")]
    return(data)
}

richmond <- dist.Miles(data = richmond)
mission <- dist.Miles(data = mission)
central <- dist.Miles(data = central)
park <- dist.Miles(data = park)
taraval <- dist.Miles(data = taraval)
ingleside <- dist.Miles(data = ingleside)
southern <- dist.Miles(data = southern)
tenderloin <- dist.Miles(data = tenderloin)
bayview <- dist.Miles(data = bayview)
northern <- dist.Miles(data = northern)

# stack tables
sfCrimeMet <- rbind(richmond, mission, central, park, taraval,
                    ingleside, southern, tenderloin, bayview, northern)

# remove NA at end of tables caused by lead functions 
sfCrimeMet <- na.omit(sfCrimeMet)

# trim outliers
maxMin <- quantile(sfCrimeMet$min_to_nxt_incident, 0.80)
minMin <- quantile(sfCrimeMet$min_to_nxt_incident, 0.10)
sfCrimeMet <- sfCrimeMet[min_to_nxt_incident < maxMin &
                             min_to_nxt_incident > minMin]

# format columns for sqlite
sfCrimeMet <- sfCrimeMet[, incident_datetime := as.character(incident_datetime)]

sfCrimeMet <- sfCrimeMet[, - c("latitude",
                               "longitude",
                               "incident_datetime")]


#===============================================================================
# Load the new table into sqlite

# create a connection to the database
dbPath <- "C:/Users/Cbirkho/Documents/SF_Crime_App/Application/sf_crime_db.sqlite"
db <- dbConnect(RSQLite::SQLite(), dbname = dbPath)

# Set up incident_reports table
# This was the code used to initially set up the table
#-------------------------------------------------------------------------------
# dbExecute(db, "CREATE TABLE ml_data_incidents
#                 (incident_id_nbr_cd TEXT NOT NULL,
#                 incident_day_of_week TEXT,
#                 police_district TEXT,
#                 min_to_nxt_incident REAL,
#                 miles_to_nxt_incident REAL,
#                 UNIQUE (incident_id_nbr_cd));")
#-------------------------------------------------------------------------------

load_data <- function(df) {
    
    print("=====Uploading data to ml_data_incidents table=====")
    # we want to add only the new combinations
    insertnew <- dbSendQuery(db, "INSERT OR IGNORE INTO ml_data_incidents VALUES 
                                    (:incident_id_nbr_cd, 
                                     :incident_day_of_week,
                                     :police_district,
                                     :min_to_nxt_incident,
                                     :miles_to_nxt_incident);")
    dbBind(insertnew, params = df)  # execute
    dbClearResult(insertnew) # release the prepared statement
    
    # disconnecting
    dbDisconnect(db)
    
    print("=====Data upload complete=====")
}

load_data(df = sfCrimeMet)

