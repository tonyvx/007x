library(ggplot2)
library(dplyr)
library(tidyr)

stevens <- read.csv("./stevens.csv")

str(stevens)

#Independent Variable
#Term
#Circuit
#Issue
#Petitioner
#Respondent
#LowerCourt
#Unconst

#Dependent Variable
#Reverese

#Sample data for training data
library(caTools)

set.seed(3000)

split = sample.split(stevens$Reverse, SplitRatio = 0.7)
Train = subset(stevens, split = TRUE)
Test = subset(stevens, split = FALSE)

library(rpart)
library(rpart.plot)

StevensTree = rpart(
  Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
  method = "class",
  data = Train ,
  control=rpart.control(minbucket = 25)
)

prp(StevensTree)

#prediction using type class

predictCART=predict(StevensTree, newdata = Test, type="class")

table(Test$Reverse, predictCART)

#prediction using ROC

library(ROCR)

predictROC=predict(StevensTree, newdata = Test)

predictROC

#ROC curve for predict performance

pred = prediction(predictROC[,2], Test$Reverse)
perf= performance(pred, "tpr", "fpr")
