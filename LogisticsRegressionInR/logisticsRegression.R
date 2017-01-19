library(dplyr)
library(ggplot2)

#Load csv
patientCareQuality <- read.csv("./quality.csv", header = TRUE)

#Analyse Data
str(patientCareQuality)

#plot "Number of Narcotics prescribed" by "Number of Office Visits"
patientCareQuality %>% mutate(`Patient Care Quality` = if_else(PoorCare == 1, "Poor", "Good")) %>%
  ggplot(aes(x = OfficeVisits,
             y = Narcotics,
             col = `Patient Care Quality`)) +
  geom_point() +
  ggtitle("Healthcare Quality") +
  labs(x = "Number of Office Visits", y = "Number of Narcotics prescribed")

#PoorCare Ratio
table(patientCareQuality$PoorCare)

#PoorCare Ratio as initial Baseline
goodCarePercentBaseline = 98 / 131

goodCarePercentBaseline

#Logistics Regression

#split data as Test and Train
library(caTools)

set.seed(88)

split = sample.split(patientCareQuality$PoorCare, SplitRatio = 0.7)

qualityTrain = subset(patientCareQuality, split == TRUE)
nrow(qualityTrain)

qualityTest = subset(patientCareQuality, split == FALSE)
nrow(qualityTest)

# Model

QualityLog = glm(PoorCare ~ Narcotics + OfficeVisits,
                 data = qualityTrain,
                 family = binomial)

summary(QualityLog)

predictTrain = predict(QualityLog, type = "response")

summary(predictTrain)

tapply(predictTrain, qualityTrain$PoorCare, mean)

#threshold t>0.5
table(qualityTrain$PoorCare, predictTrain > 0.5)

sensitivityVal <- 9 / (9 + 14)
sensitivityVal

specifityVal <- 65 / (65 + 4)
specifityVal

#threshold t>0.5
table(qualityTrain$PoorCare, predictTrain > 0.7)
sensitivityVal <- 5 / (5 + 18)
sensitivityVal

specifityVal <- 68 / (68 + 1)
specifityVal

#threshold t>0.2
table(qualityTrain$PoorCare, predictTrain > 0.2)
sensitivityVal <- 17 / (17 + 6)
sensitivityVal

specifityVal <- 47 / (47 + 22)
specifityVal

library(ROCR)

ROCRPred <- prediction(predictTrain,qualityTrain$PoorCare)
ROCRPerf <- performance(ROCRPred,"tpr","fpr")
plot(ROCRPerf)

