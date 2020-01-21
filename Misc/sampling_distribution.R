dbPath <- "./Application/sf_crime_db.sqlite"
db <- dbConnect(RSQLite::SQLite(), dbname = dbPath)

isData <- dbGetQuery(db,  "SELECT *
                                            FROM ml_data_incidents;")

dbDisconnect(db)

setDT(isData)

# mu <- mean(isData$min_to_nxt_incident)
# sdv <- sd(isData$min_to_nxt_incident)
# 
# isData <- isData[, min_to_nxt_incident := log10(min_to_nxt_incident + 1)]
# isData <- isData[, min_to_nxt_incident := ifelse(min_to_nxt_incident == 0, NA, min_to_nxt_incident)]
# x <- isData$min_to_nxt_incident
# x <- na.omit(x)

x <- isData$miles_to_nxt_incident
nbrSamples <- 1000
sampleMeans <- rep(NA, nbrSamples)

for (i in 1:nbrSamples) {
    sample <- sample(x, size = 10, replace = TRUE)
    sampleMeans[i] <- mean(sample)
}

hist(sampleMeans)
mean(sampleMeans, na.rm = TRUE)
median(sampleMeans, na.rm = TRUE)
sd(sampleMeans)


mu <- mean(isData$miles_to_nxt_incident)
sd <- sd(isData$miles_to_nxt_incident)
cutOff <- sd * 3

lowBnd <- mu - cutOff
upBnd <- mu + cutOff

x <- shapiro.test(isData$miles_to_nxt_incident)
x$p.value

isData <- isData[incident_day_of_week %in% c('Friday', 'Wednesday'),]
isData <- isData[,
                 `:=`(incident_day_of_week = as.factor(incident_day_of_week))]

x <- t.test(miles_to_nxt_incident ~ incident_day_of_week, isData, paired = FALSE)
df <- data.frame(x$statistic, x$p.value, x$estimate[1], x$estimate[2])
x$conf.int[2]
df <- x$estimate
