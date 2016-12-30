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
