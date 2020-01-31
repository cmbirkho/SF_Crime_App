
# Clear Workspace
rm(list = ls())

# Load packages
library(RSQLite)
library(DBI)
library(data.table)
library(tidyverse)
library(lubridate)

#===============================================================================
# EXTRACT
url <- "https://data.sfgov.org/resource/wg3w-h783.csv"

sfCrime <- fread(url)

#===============================================================================
# CLEAN & TRANSFORM

# Set up a unique column with the combination of incident_id, incident_number, and incident_code
sfCrime <- sfCrime[, incident_id_nbr_cd := paste(sfCrime$incident_id, 
                                                 sfCrime$incident_number, 
                                                 sfCrime$incident_code, sep = "-")]

# Clean up dates
sfCrime <- sfCrime[, 
                   `:=`(incident_date = str_sub(incident_date, 1, 10),
                        report_date = str_sub(report_datetime, 1, 10),
                        report_time = str_sub(report_datetime, 12, 16))] %>% 
    .[,
      `:=`(incident_date = ymd(incident_date),
           report_date = ymd(report_date),
           incident_datetime = paste(incident_date, incident_time, sep = " "),
           report_datetime = paste(report_date, report_time, sep = " "))] %>% 
  .[, incident_month := month(incident_date, abbr = TRUE, label = TRUE)] %>% 
  .[,
    `:=`(incident_month = as.character(incident_month),
         incident_date = as.character(incident_date),
         report_date = as.character(report_date),
         incident_datetime = as.POSIXct(incident_datetime, format = "%Y-%m-%d %H:%M"),
         report_datetime = as.POSIXct(report_datetime, format = "%Y-%m-%d %H:%M"),
         incident_time = as.POSIXct(incident_time, format = "%H:%M"))] %>%
  # date filter (one week of data, contains the most incident reports)
  .[incident_date >= '2019-09-29' & incident_date <= '2019-10-05', ] %>%
  # convert to character for sqlite
  .[,
    `:=`(incident_datetime = as.character(incident_datetime),
         report_datetime = as.character(report_datetime))]

# add incident_time_of_day feature
dawnFilt <- as.POSIXct(paste(Sys.Date(), "06:00:00"), format = "%Y-%m-%d %H:%M")
morningFilt <-  as.POSIXct(paste(Sys.Date(), "12:00:00"), format = "%Y-%m-%d %H:%M")
afternoonFilt <-  as.POSIXct(paste(Sys.Date(), "18:00:00"), format = "%Y-%m-%d %H:%M")
nightFilt <- as.POSIXct(paste(Sys.Date(), "24:00:00"), format = "%Y-%m-%d %H:%M")

sfCrime <- sfCrime[,
                   incident_time_of_day := ifelse(incident_time <= dawnFilt, 'dawn',
                                                  ifelse(incident_time > dawnFilt &
                                                           incident_time <= morningFilt, 'morning',
                                                         ifelse(incident_time > morningFilt &
                                                                  incident_time <= afternoonFilt, 'afternoon',
                                                                ifelse(incident_time > afternoonFilt &
                                                                         incident_time <= nightFilt, 'night', NA))))]


# Remove all "@computed_region" columns
colNm <- grep("@computed_region", names(sfCrime), value = TRUE)
sfCrime <- sfCrime[, !colNm, with = FALSE]

# Exclude data with police_district equal to "Out of SF"
sfCrime <- sfCrime[police_district != "Out of SF", ]

# Exclude data with lat and long is not available
sfCrime <- sfCrime[!is.na(latitude) & !is.na(longitude), ]

# Create an incident_value column
sfCrime$incident_value <- sub("*\\(.*", "", sfCrime$incident_description) # extract the whole string before "("
sfCrime$incident_value <- sub(".*,\\s*", "", sfCrime$incident_value) 
sfCrime$incident_value <- str_replace_all(sfCrime$incident_value, " ", "")

sfCrime <- sfCrime[, incident_value := ifelse(incident_value %in% c("$200-$950",
                                                                    "$50-$200",
                                                                    "<$50",
                                                                    ">$950"),
                                              incident_value,
                                              NA)]

# Add an incident_cnt column
sfCrime$incident_cnt <- 1

# Create a vehicle_flag indicating that a vehicle was involved
sfCrime <- sfCrime[, vehicle_flag := ifelse(grepl("vehicle", incident_subcategory,
                                                  ignore.case = TRUE) == TRUE |
                                              grepl("vehicle", incident_description,
                                                    ignore.case = TRUE) == TRUE |
                                              grepl("carjacking", incident_subcategory,
                                                    ignore.case = TRUE) == TRUE |
                                              grepl("Hit & Run", incident_description,
                                                    ignore.case = TRUE) == TRUE,
                   1, 0)]


# Create a weapon_flag indicating that a weapon was involved
sfCrime <- sfCrime[, weapon_flag := ifelse(grepl("knife", incident_subcategory,
                                                 ignore.case = TRUE) == TRUE |
                                             grepl("knife", incident_description,
                                                   ignore.case = TRUE) == TRUE |
                                             grepl("gun", incident_subcategory,
                                                   ignore.case = TRUE) == TRUE |
                                             grepl("gun", incident_description,
                                                   ignore.case = TRUE) == TRUE |
                                             grepl("weapon", incident_subcategory,
                                                  ignore.case = TRUE) == TRUE |
                                             grepl("weapon", incident_description,
                                                   ignore.case = TRUE) == TRUE,
                                           1, 0)]


# Remove columns
sfCrime <- sfCrime[, -c("filed_online",
                        "cad_number",
                        "incident_time",
                        "incident_id",
                        "incident_number",
                        "incident_code",
                        "row_id",
                        "point",
                        "supervisor_district",
                        "intersection",
                        "cnn",
                        "report_type_code",
                        "resolution")]


# Re-order columns
sfCrime <- sfCrime[, c("incident_id_nbr_cd",
                       "incident_date",
                       "incident_datetime",
                       "incident_day_of_week",
                       "incident_time_of_day",
                       "incident_year",
                       "incident_month",
                       "report_date",
                       "report_datetime",
                       "police_district",
                       "analysis_neighborhood",
                       "latitude",
                       "longitude",
                       "report_type_description",
                       "incident_category",
                       "incident_subcategory",
                       "incident_description",
                       "incident_value",
                       "vehicle_flag",
                       "weapon_flag",
                       "incident_cnt")]

#===============================================================================
# LOAD THE DATA INTO SQLITE
# create a connection to the database
dbPath <- "C:/Users/Cbirkho/Documents/SF_Crime_App/SF_Application/sf_crime_db.sqlite"
db <- dbConnect(RSQLite::SQLite(), dbname = dbPath)

# Set up incident_reports table
# This was the code used to initially set up the table
#-------------------------------------------------------------------------------
# dbExecute(db, "CREATE TABLE incident_reports
#                 (incident_id_nbr_cd TEXT NOT NULL,
#                 incident_date TEXT NOT NULL,
#                 incident_datetime TEXT,
#                 incident_day_of_week TEXT,
#                 incident_time_of_day TEXT,
#                 incident_year TEXT,
#                 incident_month TEXT,
#                 report_date TEXT,
#                 report_datetime TEXT,
#                 police_district TEXT,
#                 analysis_neighborhood TEXT,
#                 latitude REAL,
#                 longitude REAL,
#                 report_type_description TEXT,
#                 incident_category TEXT,
#                 incident_subcategory TEXT,
#                 incident_description TEXT,
#                 incident_value TEXT,
#                 vehicle_flag INTEGER,
#                 weapon_flag INTEGER,
#                 incident_cnt INTEGER,
#                 UNIQUE (incident_id_nbr_cd, incident_date));")
#-------------------------------------------------------------------------------

load_data <- function(df) {
    
    print("=====Uploading data to incident_reports table=====")
    # we want to add only the new combinations
    insertnew <- dbSendQuery(db, "INSERT OR IGNORE INTO incident_reports VALUES 
                                    (:incident_id_nbr_cd, 
                                    :incident_date,
                                    :incident_datetime,
                                    :incident_day_of_week, 
                                    :incident_time_of_day,
                                    :incident_year,
                                    :incident_month,
                                    :report_date, 
                                    :report_datetime,
                                    :police_district,
                                    :analysis_neighborhood, 
                                    :latitude, 
                                    :longitude,
                                    :report_type_description, 
                                    :incident_category,
                                    :incident_subcategory, 
                                    :incident_description,
                                    :incident_value,
                                    :vehicle_flag,
                                    :weapon_flag,
                                    :incident_cnt);")
    dbBind(insertnew, params = df)  # execute
    dbClearResult(insertnew) # release the prepared statement
    
    # disconnecting
    dbDisconnect(db)
    
    print("=====Data upload complete=====")
}

load_data(df = sfCrime)
