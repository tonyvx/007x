library(ggplot2)
library(dplyr)
library(tidyr)
library(GGally)

#Loading dataset from csv
wine <- read.csv("./wine.csv")
wine_test <- read.csv("./wine_test.csv")

summary(wine)

model1 <- lm(Price ~ AGST, data = wine)

#Residual standard error: 0.4993 on 23 degrees of freedom
#Multiple R-squared:  0.435,	Adjusted R-squared:  0.4105
#F-statistic: 17.71 on 1 and 23 DF,  p-value: 0.000335
#SSE1
#[1] 5.734875
summary(model1)

model1$residuals

SSE1 = sum(model1$residuals ^ 2)

model2 <- lm(Price ~ AGST + HarvestRain, data = wine)

#Residual standard error: 0.3674 on 22 degrees of freedom
#Multiple R-squared:  0.7074,	Adjusted R-squared:  0.6808
#F-statistic: 26.59 on 2 and 22 DF,  p-value: 1.347e-06
#SSE2
#[1] 2.970373
summary(model2)

model2$residuals

SSE2 = sum(model2$residuals ^ 2)

model3 <-
  lm(Price ~ AGST + HarvestRain + WinterRain + FrancePop + Age, data = wine)

# Residual standard error: 0.3019 on 19 degrees of freedom
# Multiple R-squared:  0.8294,	Adjusted R-squared:  0.7845
# F-statistic: 18.47 on 5 and 19 DF,  p-value: 1.044e-06
# SSE3
# [1] 1.732113
summary(model3)

model3$residuals

SSE3 = sum(model3$residuals ^ 2)

model4 <-
  lm(Price ~ AGST + HarvestRain + WinterRain + Age, data = wine)

# Residual standard error: 0.295 on 20 degrees of freedom
# Multiple R-squared:  0.8286,	Adjusted R-squared:  0.7943
# F-statistic: 24.17 on 4 and 20 DF,  p-value: 2.036e-07
# SSE4
# [1] 1.740162
summary(model4)

model4$residuals

SSE4 = sum(model4$residuals ^ 2)

#corr - 0.14 (+ve close to 1)
wine %>% select(WinterRain, Price) %>% ggplot(aes(WinterRain, Price)) + geom_point()
cor(wine$WinterRain, wine$Price)

#Corr - -0.06 (close 0 no value)
wine %>% select(HarvestRain, AGST) %>% ggplot(aes(HarvestRain, AGST)) + geom_point()
cor(wine$HarvestRain, wine$AGST)

#Corr - -0.99 (close -1 lowest it could go)
wine %>% select(Age, FrancePop) %>% ggplot(aes(Age, FrancePop)) + geom_point()
cor(wine$Age, wine$FrancePop)

cor(wine)

str(wine_test)
predictTest = predict(model4, newdata = wine_test)

predictTest

SSE = sum((wine_test$Price - predictTest) ^ 2)
SST = sum((wine_test$Price - mean(wine$Price)) ^ 2)

1 - SSE / SST