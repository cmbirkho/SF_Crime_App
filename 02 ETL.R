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
dbPath <- "C:/Users/Cbirkho/Documents/SF_Crime_App/SF_Application/sf_crime_db.sqlite"
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
                       "report_datetime",
                       "latitude",
                       "longitude")]

# put in order by datetime
sfCrime <- sfCrime[order(rank(incident_datetime)), ]


# min_to_nxt_incident by police_district
time.till <- function(data){
    
    data <- data[order(rank(incident_datetime)),]
    
    data$nxt_incident_datetime <- lead(data$incident_datetime, n = 1)
    
    data$min_to_nxt_incident <- difftime(data$nxt_incident_datetime, data$incident_datetime,
                                            units = "secs")
    
    data$min_to_nxt_incident <- as.numeric(data$min_to_nxt_incident)
    
    data <- data[, - "nxt_incident_datetime"]
    
    return(data)
}

sfCrime <- time.till(data = sfCrime)

# ft_to_nxt_incident
dist.ft <- function(data){
    
    data$nxt_lat <- lead(data$latitude, n = 1)
    data$nxt_lon <- lead(data$longitude, n = 1)
    
    for(i in 1:nrow(data)){
        data$distance_meters[i] <- distm(c(data$longitude[i], data$latitude[i]),
                                         c(data$nxt_lon[i], data$nxt_lat[i]),
                                         fun = distHaversine)
    }
    
    data$ft_to_nxt_incident <- round(data$distance_meters / 3.28084, 2)
    
    data <- data[, - c("nxt_lat",
                       "nxt_lon",
                       "distance_meters")]
    return(data)
}

sfCrime <- dist.ft(data = sfCrime)

# min_bw_report - minutes b/w incident and report
sfCrime <- sfCrime[, min_bw_report := difftime(report_datetime, 
                                               incident_datetime, 
                                               units = c("mins"))]

sfCrime$min_bw_report <- as.numeric(sfCrime$min_bw_report)

# remove NA at end of tables caused by lead functions 
sfCrime <- na.omit(sfCrime)

# format columns for sqlite
sfCrime <- sfCrime[, incident_datetime := as.character(incident_datetime)]

sfCrime <- sfCrime[, - c("latitude",
                         "longitude",
                         "report_datetime",
                         "incident_datetime")]


#===============================================================================
# Load the new table into sqlite

# create a connection to the database
dbPath <- "C:/Users/Cbirkho/Documents/SF_Crime_App/SF_Application/sf_crime_db.sqlite"
db <- dbConnect(RSQLite::SQLite(), dbname = dbPath)

# Set up incident_reports table
# This was the code used to initially set up the table
#-------------------------------------------------------------------------------
dbExecute(db, "CREATE TABLE ml_data_incidents
                (incident_id_nbr_cd TEXT NOT NULL,
                incident_day_of_week TEXT,
                police_district TEXT,
                min_to_nxt_incident REAL,
                ft_to_nxt_incident REAL,
                min_bw_report,
                UNIQUE (incident_id_nbr_cd));")
#-------------------------------------------------------------------------------

load_data <- function(df) {
    
    print("=====Uploading data to ml_data_incidents table=====")
    # we want to add only the new combinations
    insertnew <- dbSendQuery(db, "INSERT OR IGNORE INTO ml_data_incidents VALUES 
                                    (:incident_id_nbr_cd, 
                                     :incident_day_of_week,
                                     :police_district,
                                     :min_to_nxt_incident,
                                     :ft_to_nxt_incident,
                                     :min_bw_report);")
    dbBind(insertnew, params = df)  # execute
    dbClearResult(insertnew) # release the prepared statement
    
    # disconnecting
    dbDisconnect(db)
    
    print("=====Data upload complete=====")
}

load_data(df = sfCrime)

