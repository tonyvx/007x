# HR Efficiency Analysis
`r format(Sys.Date())`  



## Problem
###Factors important to retain performing employees
* Why are our best and most experienced employees leaving prematurely? 
We will analysing the data available in hand to identify avenues to improving hr efficiency.
* Can we predict which valuable employees will leave next?
We will be building a predictive model to determine how long employee would stay and their probability in leaving.

## Data Source
https://www.kaggle.com/ludobenistant/hr-analytics

https://www.kaggle.com/ludobenistant/hr-analytics/downloads/human-resources-analytics.zip

Lets load the dataset

```r
  hr <- read.csv("./HR_comma_sep.csv", header = TRUE, stringsAsFactors = FALSE)
```

#####Lets look at the fields in the dataset

* _satisfaction_level_ - Level of satisfaction (0-1)
* _last_evaluation_ - Evaluation of employee performance (0-1)
* _number_project_ - Number of projects completed while at work
* _average_montly_hours_ - Average monthly hours at workplace
* _time_spend_company_ - Number of years spent in the company
* _Work_accident_ - Whether the employee had a workplace accident
* _left_ - Whether the employee left the workplace or not (1 or 0) 
* _promotion_last_5years_ - Whether the employee was promoted in the last five years
* _sales_ - Department in which they work for
* _salary_ - Salary (High, medium, Low)

Lets analyze the structure of dataset

```r
str(hr)
```

```
## 'data.frame':	14999 obs. of  10 variables:
##  $ satisfaction_level   : num  0.38 0.8 0.11 0.72 0.37 0.41 0.1 0.92 0.89 0.42 ...
##  $ last_evaluation      : num  0.53 0.86 0.88 0.87 0.52 0.5 0.77 0.85 1 0.53 ...
##  $ number_project       : int  2 5 7 5 2 2 6 5 5 2 ...
##  $ average_montly_hours : int  157 262 272 223 159 153 247 259 224 142 ...
##  $ time_spend_company   : int  3 6 4 5 3 3 4 5 5 3 ...
##  $ Work_accident        : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ left                 : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ promotion_last_5years: int  0 0 0 0 0 0 0 0 0 0 ...
##  $ sales                : chr  "sales" "sales" "sales" "sales" ...
##  $ salary               : chr  "low" "medium" "medium" "low" ...
```

####Observations
* Fields _number_project_, _promotion_last_5years_, _left_, _Work_accident_, _sales_ have discrete values.


```r
str(as.factor(hr$salary))
```

```
##  Factor w/ 3 levels "high","low","medium": 2 3 3 2 2 2 2 2 2 2 ...
```

####Observations
* Field _salary_ is also discrete with "high", "medium" & "low". Also looks like "low" is assigned 2, "medium" is assigned 3 and "high" is assigned 1.

Lets look at all unique values for field 'sales'.

```r
unique(hr$sales)
```

```
##  [1] "sales"       "accounting"  "hr"          "technical"   "support"    
##  [6] "management"  "IT"          "product_mng" "marketing"   "RandD"
```

####Observations
* Field _sales_ does not seems to have sales figures but departments that employee belongs to.


Lets look at the summary of the data set to see if there are no invalid data

```r
  summary(hr)
```

```
##  satisfaction_level last_evaluation  number_project  average_montly_hours
##  Min.   :0.0900     Min.   :0.3600   Min.   :2.000   Min.   : 96.0       
##  1st Qu.:0.4400     1st Qu.:0.5600   1st Qu.:3.000   1st Qu.:156.0       
##  Median :0.6400     Median :0.7200   Median :4.000   Median :200.0       
##  Mean   :0.6128     Mean   :0.7161   Mean   :3.803   Mean   :201.1       
##  3rd Qu.:0.8200     3rd Qu.:0.8700   3rd Qu.:5.000   3rd Qu.:245.0       
##  Max.   :1.0000     Max.   :1.0000   Max.   :7.000   Max.   :310.0       
##  time_spend_company Work_accident         left       
##  Min.   : 2.000     Min.   :0.0000   Min.   :0.0000  
##  1st Qu.: 3.000     1st Qu.:0.0000   1st Qu.:0.0000  
##  Median : 3.000     Median :0.0000   Median :0.0000  
##  Mean   : 3.498     Mean   :0.1446   Mean   :0.2381  
##  3rd Qu.: 4.000     3rd Qu.:0.0000   3rd Qu.:0.0000  
##  Max.   :10.000     Max.   :1.0000   Max.   :1.0000  
##  promotion_last_5years    sales              salary         
##  Min.   :0.00000       Length:14999       Length:14999      
##  1st Qu.:0.00000       Class :character   Class :character  
##  Median :0.00000       Mode  :character   Mode  :character  
##  Mean   :0.02127                                            
##  3rd Qu.:0.00000                                            
##  Max.   :1.00000
```
####Observations
* All fields in the dataset have non-NA values. 


## Data Wrangling

Rename the _sales_ field to _dept_ and create seperate field salary_level 3: High, 2: Medium & 1: low

```r
names(hr)[9] <- "dept"
hr <- hr %>% mutate(salary_level = case_when(
  .$salary == "high" ~ 3, 
  .$salary == "medium" ~ 2, 
  .$salary == "low" ~ 1))
```

Lets look are structure once again

```r
  str(hr)
```

```
## 'data.frame':	14999 obs. of  11 variables:
##  $ satisfaction_level   : num  0.38 0.8 0.11 0.72 0.37 0.41 0.1 0.92 0.89 0.42 ...
##  $ last_evaluation      : num  0.53 0.86 0.88 0.87 0.52 0.5 0.77 0.85 1 0.53 ...
##  $ number_project       : int  2 5 7 5 2 2 6 5 5 2 ...
##  $ average_montly_hours : int  157 262 272 223 159 153 247 259 224 142 ...
##  $ time_spend_company   : int  3 6 4 5 3 3 4 5 5 3 ...
##  $ Work_accident        : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ left                 : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ promotion_last_5years: int  0 0 0 0 0 0 0 0 0 0 ...
##  $ dept                 : chr  "sales" "sales" "sales" "sales" ...
##  $ salary               : chr  "low" "medium" "medium" "low" ...
##  $ salary_level         : num  1 2 2 1 1 1 1 1 1 1 ...
```

## Data Exploration

Lets analyze _satisfaction_level_, _time_spend_company_, _last_evaluation_, _average_monthly_hours_, _work_accident_, _salary_ and _number_project_



```r
hr_left <- hr %>% filter(left == 1)

satis_l <- hr_left %>% ggplot(aes(satisfaction_level)) +
  geom_histogram( binwidth = 0.05, aes(fill = ..count..)) +
  scale_fill_gradient("Count", low = "green", high = "red") +
  labs(x = "satisfaction_level", y = "employees", title = "satisfaction level") + myTheme

tm_spnd <- hr_left %>% ggplot(aes(time_spend_company)) +
  geom_histogram( binwidth = 0.05, aes(fill = ..count..)) +
  scale_fill_gradient("Count", low = "green", high = "red") +
  labs(x = "time_spend_company", y = "employees", title = "Time Spend in Company") + myTheme

lst_eval <- hr_left %>% ggplot(aes(last_evaluation)) +
  geom_histogram( binwidth = 0.05, aes(fill = ..count..)) +
  scale_fill_gradient("Count", low = "green", high = "red") +
  labs(x = "last_evaluation", y = "employees", title = "Last evaluation") + myTheme

mnthly_hrs <- hr_left %>% ggplot(aes(average_montly_hours)) +
  geom_histogram( binwidth = 0.05, aes(fill = ..count..)) +
  scale_fill_gradient("Count", low = "green", high = "red") +
  labs(x = "average_montly_hours", y = "employees", title = "Average montly hours") + myTheme

wrk_accdnt <- hr_left %>% ggplot(aes(Work_accident)) +
  geom_histogram( binwidth = 0.05, aes(fill = ..count..)) +
  scale_fill_gradient("Count", low = "green", high = "red") +
  labs(x = "Work_accident", y = "employees", title = "Work accident") + myTheme

sal <- hr_left %>% ggplot(aes(salary_level)) +
  geom_histogram( binwidth = 0.05, aes(fill = ..count..)) +
  scale_fill_gradient("Count", low = "green", high = "red") +
  labs(x = "salary", y = "employees", title = "Salary") + myTheme

nmbr_prj <- hr_left %>% ggplot(aes(as.numeric(number_project))) +
  geom_histogram( binwidth = 0.05, aes(fill = ..count..)) +
  scale_fill_gradient("Count", low = "green", high = "red") +
  labs(x = "number_project", y = "employees", title = "Number of projects") + myTheme

grid.arrange(satis_l, tm_spnd, lst_eval,mnthly_hrs,wrk_accdnt,sal,nmbr_prj, nrow = 4)
```

![](HR_Analysis_files/figure-html/plot_data_exp_analysis-1.png)<!-- -->

####Notice high number of employees leaving the company 
* had been with company for less than 3 years
* had an evaluation rating less than 0.5
* had an average monthly hours in work at less than 170 hrs
* had 2 projects or less
* Overall lower performing employees are leaving more. This warrants improvement in hiring process to avoid low performers
* Aside from low performers, we can notice number of employees leaving creeeping up among mid to high performing. This is an area that needs to be also looked into for reduction in rate of attrition.  



##Regression Model

###Linear Regression
Lets build a model to determine how long an employee will stay

####Train Data

```r
set.seed(3456)
trainIndex <- createDataPartition(hr$time_spend_company, p = .8, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)
```

```
##      Resample1
## [1,]         1
## [2,]         2
## [3,]         3
## [4,]         4
## [5,]         6
## [6,]         7
```

```r
hrTrain <- hr[ trainIndex,]
```
###Test Data

```r
hrTest  <- hr[-trainIndex,]
```

###Models


```r
lm_time_spend <- lm(time_spend_company~left+number_project+last_evaluation+salary+Work_accident+satisfaction_level, data = hrTrain)
lm_time_spend_summary <- summary(lm_time_spend)
lm_time_spend_summary
```

```
## 
## Call:
## lm(formula = time_spend_company ~ left + number_project + last_evaluation + 
##     salary + Work_accident + satisfaction_level, data = hrTrain)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.4365 -0.7382 -0.3421  0.5142  7.0906 
## 
## Coefficients:
##                    Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         2.60685    0.08132  32.057  < 2e-16 ***
## left                0.49930    0.03365  14.838  < 2e-16 ***
## number_project      0.19559    0.01132  17.278  < 2e-16 ***
## last_evaluation     0.65850    0.08141   8.089 6.59e-16 ***
## salarylow          -0.42034    0.04875  -8.622  < 2e-16 ***
## salarymedium       -0.30038    0.04895  -6.136 8.72e-10 ***
## Work_accident       0.11860    0.03689   3.215  0.00131 ** 
## satisfaction_level -0.19898    0.05757  -3.456  0.00055 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.407 on 11992 degrees of freedom
## Multiple R-squared:  0.07186,	Adjusted R-squared:  0.07132 
## F-statistic: 132.6 on 7 and 11992 DF,  p-value: < 2.2e-16
```


```r
lm_time_spend_mthly_hrs <- lm(time_spend_company~left+number_project+last_evaluation+average_montly_hours+salary+Work_accident+satisfaction_level, data = hrTrain)
lm_time_spend_mthly_hrs_summary <- summary(lm_time_spend_mthly_hrs)
lm_time_spend_mthly_hrs_summary
```

```
## 
## Call:
## lm(formula = time_spend_company ~ left + number_project + last_evaluation + 
##     average_montly_hours + salary + Work_accident + satisfaction_level, 
##     data = hrTrain)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.4708 -0.7318 -0.3306  0.5118  7.1295 
## 
## Coefficients:
##                        Estimate Std. Error t value Pr(>|t|)    
## (Intercept)           2.5222750  0.0860600  29.308  < 2e-16 ***
## left                  0.4909424  0.0337536  14.545  < 2e-16 ***
## number_project        0.1836446  0.0119988  15.305  < 2e-16 ***
## last_evaluation       0.6038130  0.0834059   7.239 4.78e-13 ***
## average_montly_hours  0.0008680  0.0002899   2.994 0.002761 ** 
## salarylow            -0.4185636  0.0487411  -8.587  < 2e-16 ***
## salarymedium         -0.2991606  0.0489370  -6.113 1.01e-09 ***
## Work_accident         0.1178218  0.0368783   3.195 0.001402 ** 
## satisfaction_level   -0.2059424  0.0575975  -3.576 0.000351 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.406 on 11991 degrees of freedom
## Multiple R-squared:  0.07256,	Adjusted R-squared:  0.07194 
## F-statistic: 117.3 on 8 and 11991 DF,  p-value: < 2.2e-16
```

###Lets compare the two models

```r
lm_model_anova <- anova(lm_time_spend,lm_time_spend_mthly_hrs)
lm_model_anova
```

```
## Analysis of Variance Table
## 
## Model 1: time_spend_company ~ left + number_project + last_evaluation + 
##     salary + Work_accident + satisfaction_level
## Model 2: time_spend_company ~ left + number_project + last_evaluation + 
##     average_montly_hours + salary + Work_accident + satisfaction_level
##   Res.Df   RSS Df Sum of Sq      F   Pr(>F)   
## 1  11992 23738                                
## 2  11991 23720  1     17.73 8.9629 0.002761 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

_lm_time_spend_ has an R Squared of 0.0718649 and adjusted R-Squared of 0.0713232

_lm_time_spend_mthly_hrs_ has an R Squared of 0.0725582 and adjusted R-Squared of 0.0719394

#####Based on anova and both R Squared and Adjusted R Squared model _lm_time_spend_mthly_hrs_ seems to be a better fit


###Predict using the model _lm_time_spend_mthly_hrs_

```r
predict_lm_emp_leaving <- predict(lm_time_spend_mthly_hrs, newdata = hrTest)
#summary of prediction
summary(predict_lm_emp_leaving)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   2.662   3.259   3.419   3.513   3.650   5.020
```

```r
#summary of actuals
summary(hrTest$time_spend_company)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   2.000   3.000   3.000   3.491   4.000  10.000
```

```r
#plot actual vs predicted
p <- data.frame(predict_lm_emp_leaving)
p$ID <- as.numeric(rownames(p))
p$Type <- 'Predicted'
names(p)[1] <- 'time_spend_company'

a <- data.frame(hrTest$time_spend_company)
a$ID <- as.numeric(rownames(hrTest))
a$Type <- 'Actual'
names(a)[1] <- 'time_spend_company'

m <- data.frame(rbind(a,p))

m %>% ggplot(aes(x = ID, y = time_spend_company, col = Type)) + 
  geom_line() + 
  geom_smooth() +
  labs(x = "Observation", y = "Number of years spent in the company", title = "Plot Predicted & Actual Time spend vs Observation") +
  scale_colour_brewer(palette = brewer.pal(11,"Spectral")) +
  myTheme
```

![](HR_Analysis_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
#correlation data
plot_data <- data.frame(predict_lm_emp_leaving, hrTest$time_spend_company)
names(plot_data)[1] <- "predicted"
names(plot_data)[2] <- "actual"

correlation <- cor(plot_data)
correlation
```

```
##           predicted    actual
## predicted 1.0000000 0.2539589
## actual    0.2539589 1.0000000
```

```r
#correlation
correlation[1,2]
```

```
## [1] 0.2539589
```

```r
#lets see how good the model prediction was

#Sum of squared errors(SSE)
sse = sum((hrTest$time_spend_company - predict_lm_emp_leaving ) ^ 2)
round(sse, digits = 2)
```

```
## [1] 5990.23
```

```r
#Root mean squared errors (RMSE)
rmse = sqrt(sse / nrow(hrTest))
round(rmse, digits = 2)
```

```
## [1] 1.41
```
####_lm_time_spend_mthly_hrs_ prediction has an 'Root mean squared errors' 1.41 and correlation coefficient of 0.25

###Logistic Regression
Lets build a model to predict if the employee will leave

####Train Data

```r
set.seed(3456)
trainIndex <- createDataPartition(hr$left, p = .8, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)
```

```
##      Resample1
## [1,]         1
## [2,]         2
## [3,]         3
## [4,]         4
## [5,]         5
## [6,]         6
```

```r
hrTrain <- hr[ trainIndex,]
```
###Test Data

```r
hrTest  <- hr[-trainIndex,]
```

###Models


```r
mod1 <- glm(left ~ time_spend_company + last_evaluation + salary + satisfaction_level, data = hrTrain , family = "binomial")
summary(mod1)
```

```
## 
## Call:
## glm(formula = left ~ time_spend_company + last_evaluation + salary + 
##     satisfaction_level, family = "binomial", data = hrTrain)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.8469  -0.6784  -0.4705  -0.1697   2.8076  
## 
## Coefficients:
##                    Estimate Std. Error z value Pr(>|z|)    
## (Intercept)        -1.75302    0.18380  -9.538   <2e-16 ***
## time_spend_company  0.20840    0.01614  12.909   <2e-16 ***
## last_evaluation     0.13613    0.13887   0.980    0.327    
## salarylow           2.12131    0.14456  14.674   <2e-16 ***
## salarymedium        1.61215    0.14545  11.084   <2e-16 ***
## satisfaction_level -3.69298    0.09926 -37.205   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 13161  on 11999  degrees of freedom
## Residual deviance: 10892  on 11994  degrees of freedom
## AIC: 10904
## 
## Number of Fisher Scoring iterations: 5
```


```r
#Lets test prediction using test data 
testPredication <- predict(mod1, newdata = hrTest, type = "response")
table(hrTest$left, testPredication >= 0.18)
```

```
##    
##     FALSE TRUE
##   0  1396  884
##   1   138  581
```


