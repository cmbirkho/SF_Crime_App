dbPath <- "./Application/sf_crime_db.sqlite"
db <- dbConnect(RSQLite::SQLite(), dbname = dbPath)

isData <- dbGetQuery(db,  "SELECT *
                                            FROM ml_data_incidents;")

dbDisconnect(db)

setDT(isData)

mu <- mean(isData$min_to_nxt_incident)
sdv <- sd(isData$min_to_nxt_incident)

isData <- isData[, min_to_nxt_incident := log10(min_to_nxt_incident + 1)]
# isData <- isData[, min_to_nxt_incident := ifelse(min_to_nxt_incident == 0, NA, min_to_nxt_incident)]
# x <- isData$min_to_nxt_incident
# x <- na.omit(x)

nbrSamples <- 10000
sampleMeans <- rep(NA, nbrSamples)

for (i in 1:nbrSamples) {
    sample <- sample(x, size = 90, replace = TRUE)
    sampleMeans[i] <- mean(sample)
}

hist(sampleMeans)
mean(sampleMeans, na.rm = TRUE)
median(sampleMeans, na.rm = TRUE)
