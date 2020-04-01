rm(list = ls())

library(data.table)
library(tidyverse)
library(lubridate)
library(RSQLite)
library(DBI)
library(caret)
library(stopwords)
library(tm)

#===============================================================================
# load data
dbPath <- "C:/Users/Cbirkho/Documents/SF_Crime_App/SF_Application/sf_crime_db.sqlite"

db <- dbConnect(SQLite(), dbname = dbPath)

df <- dbGetQuery(db,  "SELECT *
                       FROM incident_reports;")

dbDisconnect(db)

setDT(df)

# filter out low frequency incident_category
df$cnt <- 1

filt <- df[, .(sum = sum(cnt)),
           by = incident_category] %>% 
    .[sum >= 3,]

df <- df[incident_category %in% filt$incident_category, ]

# make a corpus
setnames(df, "incident_id_nbr_cd", "doc_id")
setnames(df, "incident_description", "text")

dfSource <- DataframeSource(df)

dfCorpus <- VCorpus(dfSource)

# clean the corpus text
clean.Text <- function(corpus){
    
    corpus <- tm_map(corpus, content_transformer(removePunctuation))
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, content_transformer(removeWords), stopwords("english"))
    # corpus <- tm_map(corpus, stemDocument)
    corpus <- tm_map(corpus, stripWhitespace)
    
    return(corpus)
}

dfCorpus <- clean.Text(dfCorpus)

# convert corpus to bag of words (tokenization)
word.Bag <- function(corpus, data){
    
    docMatrix <- DocumentTermMatrix(corpus)
    dfMatrix <- as.matrix(docMatrix)
    dfMatrix <- cbind(dfMatrix, data$incident_category)
    colnames(dfMatrix)[ncol(dfMatrix)] <- "incident_category"
    dfNew <- as.data.frame(dfMatrix)
    
    return(dfNew)
}

dfNew <- word.Bag(dfCorpus, df)

#-------------------------------------------------------------------------------
# Formatting for sqlite

# switch incident_category
setDT(dfNew)
dfNew <- dfNew[, incident_category := as.character(incident_category)]

# switch factor columns to numeric
incident_category <- dfNew$incident_category

num <- select_if(dfNew, is.factor)
num <- as.data.frame(apply(num, 2, as.numeric))

dfNew <- cbind(incident_category, num)

# switch incident_category
setDT(dfNew)
dfNew <- dfNew[, incident_category := as.character(incident_category)]

# gather data into tall format
dfNew <- melt(dfNew, id.vars = "incident_category")

# convert to character for sqlite
dfNew$variable <- as.character(dfNew$variable)

#===============================================================================
# Load the new table into sqlite

# create a connection to the database
dbPath <- "C:/Users/Cbirkho/Documents/SF_Crime_App/SF_Application/sf_crime_db.sqlite"
db <- dbConnect(RSQLite::SQLite(), dbname = dbPath)

# Set up incident_reports table
# This was the code used to initially set up the table
#-------------------------------------------------------------------------------
# dbExecute(db, "CREATE TABLE bag_of_words
#                 (incident_category TEXT NOT NULL,
#                  variable TEXT NOT NULL,
#                  value INTEGER NOT NULL);")
#-------------------------------------------------------------------------------

load_data <- function(df) {
    
    print("=====Uploading data to bag_of_words table=====")
    # we want to add only the new combinations
    insertnew <- dbSendQuery(db, "INSERT OR IGNORE INTO bag_of_words VALUES 
                                    (:incident_category,
                                     :variable,
                                     :value);")
    dbBind(insertnew, params = df)  # execute
    dbClearResult(insertnew) # release the prepared statement
    
    # disconnecting
    dbDisconnect(db)
    
    print("=====Data upload complete=====")
}

load_data(df = dfNew)











