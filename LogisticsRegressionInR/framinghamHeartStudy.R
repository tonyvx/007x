library(dplyr)
library(ggplot2)

#Load csv
framinghamHeartStudy <- read.csv("./framingham.csv", header = TRUE)

#Analyse Data
str(framinghamHeartStudy)

framinghamHeartStudy$male <- as.factor(framinghamHeartStudy$male)
framinghamHeartStudy$education <- as.factor(framinghamHeartStudy$education)
framinghamHeartStudy$currentSmoker <- as.factor(framinghamHeartStudy$currentSmoker)
framinghamHeartStudy$BPMeds <- as.factor(framinghamHeartStudy$BPMeds)
framinghamHeartStudy$prevalentStroke <- as.factor(framinghamHeartStudy$prevalentStroke)
framinghamHeartStudy$prevalentHyp <- as.factor(framinghamHeartStudy$prevalentHyp)
framinghamHeartStudy$diabetes <- as.factor(framinghamHeartStudy$diabetes)
framinghamHeartStudy$TenYearCHD <- as.factor(framinghamHeartStudy$TenYearCHD)

#split data as Test and Train
library(caTools)

set.seed(1000)

split = sample.split(framinghamHeartStudy$TenYearCHD, SplitRatio = 0.65)

train = subset(framinghamHeartStudy, split == TRUE)
nrow(train)

test = subset(framinghamHeartStudy, split == FALSE)
nrow(test)

# Model

framingham_glm = glm(TenYearCHD ~ .,
                 data = train,
                 family = binomial)

summary(framingham_glm)

predicTest = predict(framingham_glm, type = "response", newdata = test)

summary(predicTest)

#threshold t>0.5
threshold <- 0.15
threshold
predictivity <- table(test$TenYearCHD, predicTest > threshold)
predictivity

library(ROCR)

ROCRPred <- prediction(predicTest,test$TenYearCHD)
ROCRPerf <- performance(ROCRPred,"tpr","fpr")
plot(ROCRPerf)

 as.numeric(performance(ROCRPred,"auc")@y.values)


#Sensitivity / true positive rate
sensitivityVal <- predictivity[3]/(predictivity[3] + predictivity[2])
sensitivityVal

#specificity (SPC) or true negative rate
specifityVal <- predictivity[1]/(predictivity[1] + predictivity[4])
specifityVal

