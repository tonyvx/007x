library(dplyr)
library(ggplot2)
library(mice)
#Load csv
pollingData <- read.csv("./PollingData.csv", header = TRUE)

#Analyse Data
str(pollingData)
#Do we have data for all years for all states
table(pollingData$Year)
# indicates for year 2012 5 states are missing data

#lets analyze NA data
summary(pollingData)
# 46 rows are having NA 's for Rasmussen & 71 has NA's for SurveyUSA

# populate missing data using mice

simple <- pollingData[c("Rasmussen","SurveyUSA","PropR","DiffCount")]
summary(simple)
set.seed(144)

imputed <- complete(mice(simple))
summary(imputed)

# Now update polling data

pollingData$Rasmussen <- imputed$Rasmussen
pollingData$SurveyUSA <- imputed$SurveyUSA

#lets analyze NA data
summary(pollingData)

train <- subset(pollingData, Year == 2004 | Year == 2008)
test <- subset(pollingData, Year == 2012)

table(train$Republican)

republicanProbability <- table(train$Republican)[2]/(table(train$Republican)[1]+table(train$Republican)[2])
republicanProbability

# using Rasmussen
rasmussen <- table(sign(train$Rasmussen))

#smart baseline data
table(train$Republican, sign(train$Rasmussen))

cor(pollingData[c("Rasmussen","SurveyUSA","PropR","DiffCount","Republican")])

mod1 <- glm(Republican ~ SurveyUSA + DiffCount, data = train , family = "binomial")
summary(mod1)

pred1 <- predict(mod1, data = train, type = "response")
summary(pred1)

table(test$Republican, sign(test$Rasmussen))

testPredication <- predict(mod1, newdata = test, type = "response")

table(test$Republican, testPredication >= 0.5)

#1 false 
subset(test,testPredication >= 0.5 & Republican == 0)