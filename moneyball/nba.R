library(dplyr)
library(ggplot)

analysis <- function(lmodel, data) {
  print(summary(lmodel))
  sse = sum(lmodel$residuals ^ 2)
  rmse = sqrt(sse / nrow(data))
  print("SSE :")
  print(sse)
  print("RMSE :")
  rmse
}

analysisPredict <- function(predicted, train, test, test_data) {
  sse = sum((predicted - test) ^ 2)
  sst = sum((mean(train) - test) ^ 2)
  R2 = 1 - sse / sst
  rmse = sqrt(sse / nrow(test_data))
  print("sse")
  print(sse)
  print("rmse")
  print(rmse)
  print("R2")
  R2
}

nba_train = subset(read.csv("./nba_train.csv"))

str(nba_train)

#Playoffs vs Wins stats
nba_train %>% select(Playoffs, W) %>% filter(Playoffs > 0) %>% summary()

nba_train %>% select(Playoffs, W) %>% filter(W > 30 &
                                               W < 72) %>% summary()

nba_train$PTSdiff = nba_train$PTS - nba_train$oppPTS

nba_train %>% ggplot(aes(PTSdiff, W)) + geom_point()

#Linear Models

WinsReg = lm(W ~ PTSdiff, data = nba_train)

PointsReg1 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL +
                  BLK, data = nba_train)
analysis(PointsReg1, nba_train)

PointsReg2 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK, data =
                  nba_train)
analysis(PointsReg2, nba_train)

PointsReg3 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL + BLK, data =
                  nba_train)
analysis(PointsReg3, nba_train)

PointsReg4 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data = nba_train)
analysis(PointsReg4, nba_train)

#predictions

nba_test = subset(read.csv("./nba_test.csv"))

predictNBATest=predict(PointsReg4, newdata = nba_test)

analysisPredict(predictNBATest,nba_train$PTS, nba_test$PTS,nba_test)



