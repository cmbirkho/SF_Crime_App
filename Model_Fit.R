library(tidyverse)
library(lubridate)
library(caret)


# get and format data
url <- "https://data.sfgov.org/resource/wg3w-h783.csv"
sfCrime <- read.csv(url) %>% 
    mutate(cnt = 1,
           incident_date = str_sub(incident_date, 1, 10)) %>%
    mutate(incident_date = ymd(incident_date)) %>% 
    mutate(month = month(incident_date, label = TRUE, abbr = FALSE)) %>% 
    group_by(latitude, longitude, police_district, 
             incident_category, month) %>% 
    summarise(cnt = sum(cnt))

# data subset for model
subSfCrime <- sfCrime[,c(3:5)]

# model selection
inTrain <- createDataPartition(y = subSfCrime$incident_category,
                               p = 0.7, list = FALSE)

training <- subSfCrime[inTrain,]
testing <- subSfCrime[-inTrain, ]

# 1
fit1 <- train(incident_category ~ ., data = training,
              method = 'gbm', verbose = FALSE)

test_prediction1 <- predict(fit1, newdata = testing)

confusionMatrix(test_prediction1, testing$incident_category)


# 2
fit2 <- train(incident_category ~ ., data = training,
              method = 'rpart')

test_prediction2 <- predict(fit2, newdata = testing)

confusionMatrix(test_prediction2, testing$incident_category)
