# HR Efficiency Analysis
`r format(Sys.Date())`  



## Problem
###Factors important to retain performing employees
* We will be analysing the data available in hand to identify avenues to improving hr efficiency.
* We will be building a predictive model to determine how long employee would stay and their probability in leaving.

## Data Source
https://www.kaggle.com/ludobenistant/hr-analytics

https://www.kaggle.com/ludobenistant/hr-analytics/downloads/human-resources-analytics.zip

#####Lets load the dataset

```r
hr <- read.csv("./HR_comma_sep.csv", 
               header = TRUE, 
               stringsAsFactors = FALSE)

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

* _satisfaction_level_ - Employee Level of satisfaction. It ranges from __0 (low satisfaction)__ to __1 (high satisfaction)__
* _last_evaluation_ - Latest performance evaluation rating of employee. It ranges from __0 (low rating)__ to __1 (high rating)__
* _number_project_ - Number of projects completed while at work. It varies from __2__ to __7__ projects, on an average an employee has worked on __4__ projects.
* _average_montly_hours_ - Average monthly work hours at workplace. It varies from __96 hrs__ to __310 hrs__ with a mean of __201 hrs__ per month.
* _time_spend_company_ - Number of years spent in the company. It ranges from __2__ years to __10__ years, with an average of around __4__ years until now or until they left.
* _Work_accident_ - Whether the employee had a workplace accident
* _left_ - Whether the employee left the workplace or not. __1__ means __left__ and __0__ means still with company. Probability of leaving is __0.23__.
* _promotion_last_5years_ - Whether the employee was promoted in the last five years. __1__ means promoted and __0__ means no promotion in last 5 years. Probability of promotion in last 5 years is __0.02__.


* _sales_ - Department in which they work for. _accounting_, _hr_, _IT_, _management_,  _marketing_, _product_mng_, _RandD_, _sales_, _support_, _technical_ are various departments employees belong to.

```r
summary(as.factor(hr$sales))
```

```
##  accounting          hr          IT  management   marketing product_mng 
##         767         739        1227         630         858         902 
##       RandD       sales     support   technical 
##         787        4140        2229        2720
```

* _salary_ - Salary as high, medium & low.

```r
summary(as.factor(hr$salary))
```

```
##   high    low medium 
##   1237   7316   6446
```

* Fields _number_project_, _promotion_last_5years_, _left_, _Work_accident_, _sales_,_salary_ have discrete values.
* Looking at the summary of the data set there are no bad data , 'blanks', 'NA', null etc.

## Data Wrangling

Rename the _sales_ column to _dept_ and make columns _number_project_, _promotion_last_5years_, _left_, _Work_accident_, _sales_,_salary_ as factor

```r
#hr$number_project <- as.factor(hr$number_project)
hr$promotion_last_5years <- as.factor(hr$promotion_last_5years)
hr$left <- as.factor(hr$left)
hr$Work_accident <- as.factor(hr$Work_accident)
hr$sales <- as.factor(hr$sales)
hr$salary <- as.factor(hr$salary)

names(hr)[9] <- "dept"
```

Lets look are structure

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
##  $ Work_accident        : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
##  $ left                 : Factor w/ 2 levels "0","1": 2 2 2 2 2 2 2 2 2 2 ...
##  $ promotion_last_5years: Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
##  $ dept                 : Factor w/ 10 levels "accounting","hr",..: 8 8 8 8 8 8 8 8 8 8 ...
##  $ salary               : Factor w/ 3 levels "high","low","medium": 2 3 3 2 2 2 2 2 2 2 ...
```

## Data Exploration

Lets look at correlation between these various columns


```r
hr_correl <- hr
hr_correl$number_project <- as.factor(hr_correl$number_project)
hr_correl$time_spend_company <- as.factor(hr_correl$time_spend_company)
hr_correl %>%  dplyr::select(
  satisfaction_level,
  last_evaluation,
  average_montly_hours,
  number_project,
  time_spend_company,
  left
  ) %>% ggpairs() 
```

![](HR_Analysis_files/figure-html/ggpairs_continous_attibutes-1.png)<!-- -->

###number_project (Number of projects)
* employees involved in 3-5 projects have high satisfaction level, less than 3 projects or more than 5 projects satisfaction is low
* last_evaluation increases with number_project
* average_monthly_hours also increases with number_project
* number of employees leaving (left=1) is high at 2 or less improves for 3 projects and then slowly creeps up until 5 projects and goes down
* Overall Looks like 3-5 projects would mean less employees quiting and good employee satisfaction.

###average_monthly_hours (Average monthly hours)
* relation to satisfaction or last_evaluation is not very obvious
* average_monthly_hours seems to increase with number_project
* employees leaving (left=1)
  + 150 hours +/- 25 shows high number of employees leaving
	+ Again 225 hours and more employees leaving creaps to peak at around 250 hours and dips gradually to 300 hours
	+ 175 hours - 225 hours for average_monthly_hours seems to have low rate of employees leaving

###last_evaluation (Latest evaluation rating for the employee, ranges 0=low to 1=high)
* Between 0.4 to 0.6 evaluation rating, employees leaving (left=1) is high.
* It goes down at mid level and then picks up at around 0.75 and peaks around 0.85
* So low and higher end of evaluation rating employees leaving is high.
* Though one's leaving at lower rating is good for the company but the employees leaving having high rating is a concern for the company. 	
	
###satisfaction_level (Measure of employee statisfaction level, ranges 0=low to 1=high)
* number projects between 3-5 show high satisfaction, less projects or too many projects tend to lower satisfaction
* employees leaving have lower satisfaction than employees not leaving the company

###time_spend_company
* Employees who have been with the company 4-6 years seems to be having evaluation rating higher as well as higher average monthly hours.
* Employees who have been with comapny for 3 years shows higher rate of leaving company
* with respect to satisfaction level, intial years its high and its dips by 4 years and then picks up and stabilizes.
* Between 3 - 4 years period we see satisfaction level dipping and a peak in employees leaving. This is an area to be looked into.  

###overall
Looks like employees involved in 3 -5 projects and putting in 175 hours - 225 hours average monthly hours have lower rate of leaving company and have high satisfaction level.
Assuming 22 working days a month, 175 hours - 225 hours average monthly hours translates to 8 - 10 hrs per day.


```r
hr %>%  dplyr::select(Work_accident, promotion_last_5years, salary, left) %>% ggpairs()
```

![](HR_Analysis_files/figure-html/ggpairs_discrete_attributes-1.png)<!-- -->

###Work_accident ( Accidents during work - 1 indicates accident )
* A small percent of employees involved in Work_accident are actually leaving company. Work_accident does not seem very significant.
* Work_accident is higher at medium and lower salary level than in higher salary level

###promotion_last_5years (Promotion in last 5 years - 1 indicates promotion)
* Very few employees are getting promoted
* number of employees getting promoted are even at different salary level.

###salary ( Salary category for employees as high, medium and low)
* Higher number of employees leaving are in the medium & low salary level

###overall
Work_accident, promotion_last_5years & salary seems to have lower impact on employees leaving company.


####Lets analyze _satisfaction_level_, _time_spend_company_, _last_evaluation_, _average_monthly_hours_ and _number_project_



```r
satis_l <- hr %>% ggplot(aes(satisfaction_level)) +
  geom_histogram( binwidth = 0.05, aes(fill = left)) +
  labs(x = "satisfaction_level", y = "employees", title = "satisfaction level") + myTheme

nmbr_prj <- hr %>% ggplot(aes(as.numeric(number_project))) +
  geom_histogram( binwidth = 1, aes(fill = left)) +
  labs(x = "number_project", y = "employees", title = "Number of projects") + myTheme

lst_eval <- hr %>% ggplot(aes(last_evaluation)) +
  geom_histogram( binwidth = 0.05, aes(fill = left)) +
  labs(x = "last_evaluation", y = "employees", title = "Last evaluation") + myTheme

mnthly_hrs <- hr %>% ggplot(aes(average_montly_hours)) +
  geom_histogram( binwidth = 1, aes(fill = left)) +
  labs(x = "average_montly_hours", y = "employees", title = "Average montly hours") + myTheme

tm_spnd <- hr %>% ggplot(aes(time_spend_company)) +
  geom_histogram( binwidth = 1, aes(fill = left)) +
  labs(x = "time_spend_company", y = "employees", title = "Time Spend in Company") + myTheme


grid.arrange(satis_l, nmbr_prj, lst_eval, mnthly_hrs,tm_spnd,  nrow = 3)
```

![](HR_Analysis_files/figure-html/plot_data_exp_analysis-1.png)<!-- -->

* Using plots for __satisfaction_level__, __avaerage_monthly_hours__, __last_evaluation__ & __number_projects__:
  + Overall lower performing employees are leaving more. This warrants improvement in hiring process to avoid low performers
  + Aside from low performers, we can notice number of employees leaving creeeping up among mid to high performing. This is an area that needs to be also looked into for reduction in rate of attrition.  
  + Overall employees involved in 3 -5 projects and putting in 175 hours - 225 hours average monthly hours have lower rate of leaving company and have high satisfaction level.


###Cluster Analysis 
Here we will be using cluster analysis on __employees who left__ to explore various correlations. We will be using k-Means clustering.

To decide how many clusters for analysis we will plot total within-groups sums of squares against the number of clusters in a K-means. A bend in the graph can suggest the appropriate number of clusters. 


```r
#lets identify optimum number of clusters
wssplot <- function(data, nc = 9, seed = 1234) {
  wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$withinss)
  }
  plot(1:nc,
       wss,
       type = "b",
       xlab = "Number of Clusters",
       ylab = "Within groups sum of squares")   
}

#lets consider employees who left the company
hr_clust <- hr %>% filter(left == 1)

#remove attribute left
hr_clust <- hr_clust[-7]

#lets make all fields numeric
hr_clust$dept <- as.numeric( as.factor(hr_clust$dept))
hr_clust$salary <- as.numeric( as.factor(hr_clust$salary))
hr_clust$number_project <- as.numeric( hr_clust$number_project)
hr_clust$Work_accident <- as.numeric( hr_clust$Work_accident)
hr_clust$promotion_last_5years <- as.numeric(hr_clust$promotion_last_5years)

#lets identify optimum number of clusters
wssplot(scale(hr_clust))
```

![](HR_Analysis_files/figure-html/cluster1-1.png)<!-- -->
Lets consider 3 clusters for analysis.

```r
# lets consider 3 clusters 

fit.km <- kmeans(scale(hr_clust), 3)
clusplot(hr_clust,fit.km$cluster, color = TRUE)
```

![](HR_Analysis_files/figure-html/clustering-1.png)<!-- -->


```r
cluster_analysis <- cbind(hr_clust,fit.km$cluster)
names(cluster_analysis)[10] <- 'cluster'

satis_2 <- cluster_analysis %>% 
  ggplot(aes(satisfaction_level)) + 
  geom_histogram(fill = "red") + 
  facet_grid(.~cluster)

num_prj2 <- cluster_analysis %>% 
  ggplot(aes(number_project)) + 
  geom_histogram(fill = "red") + 
  facet_grid(.~cluster)

last_eval2 <- cluster_analysis %>% 
  ggplot(aes(last_evaluation)) + 
  geom_histogram(fill = "red") + 
  facet_grid(.~cluster)

avg_hrs2 <- cluster_analysis %>% 
  ggplot(aes(average_montly_hours)) + 
  geom_histogram(fill = "red") + 
  facet_grid(.~cluster)

time_spend <- cluster_analysis %>% 
  ggplot(aes(time_spend_company)) + 
  geom_histogram(fill = "red") + 
  facet_grid(.~cluster)

grid.arrange(satis_2, num_prj2, last_eval2, avg_hrs2,time_spend, nrow = 5)
```

![](HR_Analysis_files/figure-html/clustering_analysis_signifanct-1.png)<!-- -->

###cluster 3
* Employees having overall higher satisfaction level, 
* involved 3-5 projects, 
* spend 5 - 6 years.
* higher evaluation rating (0.8 -1) and 
* average monthly hours of 225  - 275 hours. Assuming 22 working days a month, this translates to 10 - 13 hrs per day.

###cluster 1
* Employees having overall medium satisfaction level, 
* involved in less than 2 projects, 
* spend 3 - 4 years
* low to medium evaluation rating (<0.6) and 
* low average monthly hours of less than 175 hours.

###cluster 2
* Employees having overall low satisfaction level, 
* involved in more than 4 projects, 
* spend 4 -5 years
* high evaluation rating (0.8-1) and 
* high average monthly hours of 225 - 350 hours. Assuming 22 working days a month, this translates to 10 - 16 hrs per day.

###overall
* cluster 3 are employees that comapany need to find ways to retain.
* cluster 2 are over worked employees, company need to find ways to optimise work load and improve satisfaction level
* cluster 1 are under under utilized employees and company need to find means to have them share the work load of cluster 2.


```r
promotion2 <- cluster_analysis %>% 
  ggplot(aes(promotion_last_5years)) + 
  geom_histogram(fill = "red") + 
  facet_grid(.~cluster)

dept2 <- cluster_analysis %>% 
  ggplot(aes(dept)) + 
  geom_histogram(fill = "red") + 
  facet_grid(.~cluster)

wrk_accdnt2 <- cluster_analysis %>% 
  ggplot(aes(Work_accident)) + 
  geom_histogram(fill = "red") + 
  facet_grid(.~cluster)

sal2 <- cluster_analysis %>% 
  ggplot(aes(salary)) + 
  geom_histogram(fill = "red") + 
  facet_grid(.~cluster)

grid.arrange(promotion2,dept2, wrk_accdnt2, sal2, nrow = 4)
```

![](HR_Analysis_files/figure-html/cluster_analysis_not_imp-1.png)<!-- -->

###overall
All clusters have even distribution for promotion, department, work accident and salary. Not much significant observation that can be made.

##Regression Analysis

###Linear Regression Analysis
Lets build a model to determine how long an employee will stay using data from __employees who already left__


####Data

```r
set.seed(3456)
hr_left <- hr %>% filter(left == 1)
trainIndex <- createDataPartition(hr_left$time_spend_company, p = .8, 
                                  list = FALSE, 
                                  times = 1)

#Training data
hrTrain <- hr_left[ trainIndex,]

#Test data
hrTest  <- hr_left[-trainIndex,]
```

####Lets use caret package to anlyze significant fields for linear model

```r
plot(varImp(train(time_spend_company ~ ., data = hrTrain, method = "lm")))
```

![](HR_Analysis_files/figure-html/fields_lm-1.png)<!-- -->
Attributes satisfaction_level,number_project, avaerage_monthly_hours, dept seems to be significant for linear regression

####Linear Regression Model

```r
#Model
lm_time_spend <- lm(time_spend_company~satisfaction_level+number_project+ average_montly_hours+promotion_last_5years+ dept, data = hrTrain)

#Summary of the model
lm_time_spend_summary <- summary(lm_time_spend)
lm_time_spend_summary
```

```
## 
## Call:
## lm(formula = time_spend_company ~ satisfaction_level + number_project + 
##     average_montly_hours + promotion_last_5years + dept, data = hrTrain)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.3409 -0.1412 -0.0354  0.0768  2.6006 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             0.9747470  0.0544620  17.898   <2e-16 ***
## satisfaction_level      2.1215638  0.0374686  56.622   <2e-16 ***
## number_project          0.2539249  0.0109960  23.092   <2e-16 ***
## average_montly_hours    0.0047166  0.0003178  14.843   <2e-16 ***
## promotion_last_5years1 -0.2933485  0.1224198  -2.396   0.0166 *  
## depthr                  0.0104099  0.0554611   0.188   0.8511    
## deptIT                 -0.0227746  0.0527948  -0.431   0.6662    
## deptmanagement         -0.1637822  0.0726322  -2.255   0.0242 *  
## deptmarketing           0.0678844  0.0568452   1.194   0.2325    
## deptproduct_mng         0.0234342  0.0559649   0.419   0.6754    
## deptRandD               0.0939470  0.0649179   1.447   0.1480    
## deptsales              -0.0211977  0.0436452  -0.486   0.6272    
## deptsupport             0.0462420  0.0466821   0.991   0.3220    
## depttechnical           0.0315632  0.0451115   0.700   0.4842    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.5008 on 2845 degrees of freedom
## Multiple R-squared:  0.739,	Adjusted R-squared:  0.7378 
## F-statistic: 619.6 on 13 and 2845 DF,  p-value: < 2.2e-16
```

From model summary we can see following attributes impacting time spend in company 
* satisfaction_level      
* number_project        - 3 - 7  
* average_montly_hours    
* promotion_last_5years - 1 (promoted)
* dept                  - management           

Model _lm_time_spend_ has an R Squared of 0.7389789 and adjusted R-Squared of 0.7377862

####Lets predict using the model _lm_time_spend_

```r
predict_time_spend <- predict(lm_time_spend, newdata = hrTest)
```

####Lets measure the correlation

```r
#correlation data
plot_data <- data.frame(predict_time_spend, hrTest$time_spend_company)
names(plot_data)[1] <- "predicted"
names(plot_data)[2] <- "actual"

# plot predicted time spend vs actual time spend in conpany
plot_data %>% ggplot(aes(x = actual, y = predicted)) + 
  geom_point() + 
  geom_smooth(method = "lm")
```

![](HR_Analysis_files/figure-html/compute_correlation_coef-1.png)<!-- -->

```r
correlation <- cor(plot_data)
correlation
```

```
##           predicted    actual
## predicted 1.0000000 0.8726757
## actual    0.8726757 1.0000000
```

```r
#correlation
correlation[1,2]
```

```
## [1] 0.8726757
```

The correlation coefficient is excellent __0.8726757__

####Lets compute Root Mean Square Error for the model

```r
#lets see how good the model prediction was

#Sum of squared errors(SSE)
sse = sum((hrTest$time_spend_company - predict_time_spend ) ^ 2)
round(sse, digits = 2)
```

```
## [1] 162.26
```

```r
#Root mean squared errors (RMSE)
rmse = sqrt(sse / nrow(hrTest))
round(rmse, digits = 2)
```

```
## [1] 0.48
```
####Linear model _lm_time_spend_ prediction with a correlation coefficent of 0.87 is providing a good prediction of time spent in company by employees with an RMSE of 0.48 years.


###Logistic Regression Analysis

Lets build a model to __predict if the employee will leave__

#### Data

```r
set.seed(3456)
trainIndex <- createDataPartition(hr$left, p = .6, 
                                  list = FALSE, 
                                  times = 1)

validationIndex <- createDataPartition(hr[-trainIndex,]$left, p = .5, 
                                  list = FALSE, 
                                  times = 1)
#Training data 
hrTrain <- hr[ trainIndex,]

#validation data
hrValidation <- hr[-trainIndex,][validationIndex,]

#Test data
hrTest  <- hr[-trainIndex,][-validationIndex,]
```

####Lets identify significant attributes for logistics regression


```r
plot(varImp(train(left ~ ., data = hrTrain, method = "glm")))
```

![](HR_Analysis_files/figure-html/log_reg_model_signi_fields-1.png)<!-- -->
* Attributes - _number_project_, _time_spend_company_, _satisfaction_level_, _Work_accident_, _salary_, _last_evaluation, _average_montly_hours_, _promotion_last_5years_ & _dept_ are significant for the logistics regression model to predict left (employees leaving left=1)

####Logistics Regression Model

```r
#Logistics regression model
log_model <- train( left ~ time_spend_company + 
                      average_montly_hours + 
                      dept+
                      last_evaluation +
                      promotion_last_5years +
                      number_project +
                      salary + 
                      Work_accident  + 
                      satisfaction_level ,
                    data = hrTrain, 
                    method = "glm",
                    trControl = trainControl(method = "cv",number = 5))
#Summary of the model
summary(log_model)
```

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.2309  -0.6563  -0.3924  -0.1117   2.9803  
## 
## Coefficients:
##                          Estimate Std. Error z value Pr(>|z|)    
## (Intercept)            -1.1453918  0.2464842  -4.647 3.37e-06 ***
## time_spend_company      0.2727637  0.0204546  13.335  < 2e-16 ***
## average_montly_hours    0.0041072  0.0006728   6.105 1.03e-09 ***
## depthr                  0.1017119  0.1683504   0.604  0.54573    
## deptIT                 -0.3589811  0.1582059  -2.269  0.02326 *  
## deptmanagement         -0.4250604  0.2086675  -2.037  0.04165 *  
## deptmarketing           0.1618021  0.1654741   0.978  0.32817    
## deptproduct_mng        -0.1553435  0.1658179  -0.937  0.34885    
## deptRandD              -0.5963897  0.1849382  -3.225  0.00126 ** 
## deptsales              -0.1409267  0.1297452  -1.086  0.27740    
## deptsupport            -0.0915149  0.1393595  -0.657  0.51139    
## depttechnical           0.0347376  0.1348545   0.258  0.79672    
## last_evaluation         0.4602353  0.1946712   2.364  0.01807 *  
## promotion_last_5years1 -1.9142072  0.3862661  -4.956 7.21e-07 ***
## number_project         -0.2882122  0.0274228 -10.510  < 2e-16 ***
## salarylow               1.8605034  0.1630711  11.409  < 2e-16 ***
## salarymedium            1.3038860  0.1641910   7.941 2.00e-15 ***
## Work_accident1         -1.5884899  0.1165772 -13.626  < 2e-16 ***
## satisfaction_level     -4.1935402  0.1269494 -33.033  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 9880.1  on 8999  degrees of freedom
## Residual deviance: 7629.7  on 8981  degrees of freedom
## AIC: 7667.7
## 
## Number of Fisher Scoring iterations: 6
```

```r
#predict against the training data                           
glm_prediction_train <- predict(log_model,  hrTrain)

#Confusion matrix for prediction against training data
glm_confustion_matrix_train <- confusionMatrix(glm_prediction_train,
                                               hrTrain$left,
                                               dnn = c("Predicted", "actual"))

glm_confustion_matrix_train
```

```
## Confusion Matrix and Statistics
## 
##          actual
## Predicted    0    1
##         0 6344 1314
##         1  513  829
##                                           
##                Accuracy : 0.797           
##                  95% CI : (0.7885, 0.8053)
##     No Information Rate : 0.7619          
##     P-Value [Acc > NIR] : 9.454e-16       
##                                           
##                   Kappa : 0.358           
##  Mcnemar's Test P-Value : < 2.2e-16       
##                                           
##             Sensitivity : 0.9252          
##             Specificity : 0.3868          
##          Pos Pred Value : 0.8284          
##          Neg Pred Value : 0.6177          
##              Prevalence : 0.7619          
##          Detection Rate : 0.7049          
##    Detection Prevalence : 0.8509          
##       Balanced Accuracy : 0.6560          
##                                           
##        'Positive' Class : 0               
## 
```

####Validation

```r
#Lets test prediction using test data 
validationPredication <- predict(log_model,hrValidation)
glm_confustion_matrix_validation <- confusionMatrix(validationPredication,hrValidation$left, dnn = c("Predicted","actual"))

glm_confustion_matrix_validation
```

```
## Confusion Matrix and Statistics
## 
##          actual
## Predicted    0    1
##         0 2140  459
##         1  146  255
##                                           
##                Accuracy : 0.7983          
##                  95% CI : (0.7835, 0.8126)
##     No Information Rate : 0.762           
##     P-Value [Acc > NIR] : 1.092e-06       
##                                           
##                   Kappa : 0.3453          
##  Mcnemar's Test P-Value : < 2.2e-16       
##                                           
##             Sensitivity : 0.9361          
##             Specificity : 0.3571          
##          Pos Pred Value : 0.8234          
##          Neg Pred Value : 0.6359          
##              Prevalence : 0.7620          
##          Detection Rate : 0.7133          
##    Detection Prevalence : 0.8663          
##       Balanced Accuracy : 0.6466          
##                                           
##        'Positive' Class : 0               
## 
```

####Prediction

```r
#Lets test prediction using test data 
testPredication <- predict(log_model,hrTest)
glm_confustion_matrix_test <- confusionMatrix(testPredication,hrTest$left, dnn = c("Predicted","actual"))

glm_confustion_matrix_test
```

```
## Confusion Matrix and Statistics
## 
##          actual
## Predicted    0    1
##         0 2113  481
##         1  172  233
##                                           
##                Accuracy : 0.7823          
##                  95% CI : (0.7671, 0.7969)
##     No Information Rate : 0.7619          
##     P-Value [Acc > NIR] : 0.004444        
##                                           
##                   Kappa : 0.2949          
##  Mcnemar's Test P-Value : < 2.2e-16       
##                                           
##             Sensitivity : 0.9247          
##             Specificity : 0.3263          
##          Pos Pred Value : 0.8146          
##          Neg Pred Value : 0.5753          
##              Prevalence : 0.7619          
##          Detection Rate : 0.7046          
##    Detection Prevalence : 0.8650          
##       Balanced Accuracy : 0.6255          
##                                           
##        'Positive' Class : 0               
## 
```

####Logistics regression model to predict employees leaving the company is predicting with an 
  * overall Accuracy of 0.7822608,
  * predicting employees leaving company with 0.3263305 accuracy and
  * predicting employess staying with the company with 0.9247265 accuracy.

###Random Forest Analysis

```r
library(doMC)
registerDoMC(5)
```

####Data

```r
set.seed(3456)
trainIndex <- createDataPartition(hr$left, p = .6, 
                                  list = FALSE, 
                                  times = 1)

validationIndex <- createDataPartition(hr[-trainIndex,]$left, p = .5, 
                                  list = FALSE, 
                                  times = 1)
#Training data 
hrTrain <- hr[ trainIndex,]

#validation data
hrValidation <- hr[-trainIndex,][validationIndex,]

#Test data
hrTest  <- hr[-trainIndex,][-validationIndex,]
```

####Random Forest Model

```r
#Create smaller Train data for Random forest
hrTrain_1 <- hrTrain[createDataPartition(y = hrTrain$left,p = 0.3,list = FALSE),]

#Random Forest Model
rf_model <- train(left~.,
                  data = hrTrain_1,
                  method = "rf",
                trControl = trainControl(method = "cv",number = 5),
                pallowParallel = TRUE)

print(rf_model)
```

```
## Random Forest 
## 
## 2701 samples
##    9 predictor
##    2 classes: '0', '1' 
## 
## No pre-processing
## Resampling: Cross-Validated (5 fold) 
## Summary of sample sizes: 2162, 2160, 2160, 2160, 2162 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa    
##    2    0.9533524  0.8638864
##   10    0.9777859  0.9373590
##   18    0.9722352  0.9223973
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 10.
```

```r
#prediction on train data
rf_prediction_train = predict(rf_model, newdata = hrTrain)

rf_conf_matrix_train <- confusionMatrix(rf_prediction_train, hrTrain$left)

rf_conf_matrix_train
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    0    1
##          0 6845  103
##          1   12 2040
##                                           
##                Accuracy : 0.9872          
##                  95% CI : (0.9847, 0.9894)
##     No Information Rate : 0.7619          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9643          
##  Mcnemar's Test P-Value : < 2.2e-16       
##                                           
##             Sensitivity : 0.9982          
##             Specificity : 0.9519          
##          Pos Pred Value : 0.9852          
##          Neg Pred Value : 0.9942          
##              Prevalence : 0.7619          
##          Detection Rate : 0.7606          
##    Detection Prevalence : 0.7720          
##       Balanced Accuracy : 0.9751          
##                                           
##        'Positive' Class : 0               
## 
```


####Validation

```r
#Lets test prediction using test data 
rf_validationPredication <- predict(rf_model,hrValidation)
rf_confustion_matrix_validation <- confusionMatrix(rf_validationPredication,hrValidation$left, dnn = c("Predicted","actual"))

rf_confustion_matrix_validation
```

```
## Confusion Matrix and Statistics
## 
##          actual
## Predicted    0    1
##         0 2283   72
##         1    3  642
##                                           
##                Accuracy : 0.975           
##                  95% CI : (0.9688, 0.9803)
##     No Information Rate : 0.762           
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9287          
##  Mcnemar's Test P-Value : 4.096e-15       
##                                           
##             Sensitivity : 0.9987          
##             Specificity : 0.8992          
##          Pos Pred Value : 0.9694          
##          Neg Pred Value : 0.9953          
##              Prevalence : 0.7620          
##          Detection Rate : 0.7610          
##    Detection Prevalence : 0.7850          
##       Balanced Accuracy : 0.9489          
##                                           
##        'Positive' Class : 0               
## 
```

####Prediction


```r
#prediction on test data
rf_prediction = predict(rf_model, newdata = hrTest)

rf_conf_matrix <- confusionMatrix(rf_prediction, hrTest$left)
rf_conf_matrix
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    0    1
##          0 2279   40
##          1    6  674
##                                           
##                Accuracy : 0.9847          
##                  95% CI : (0.9796, 0.9887)
##     No Information Rate : 0.7619          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.957           
##  Mcnemar's Test P-Value : 1.141e-06       
##                                           
##             Sensitivity : 0.9974          
##             Specificity : 0.9440          
##          Pos Pred Value : 0.9828          
##          Neg Pred Value : 0.9912          
##              Prevalence : 0.7619          
##          Detection Rate : 0.7599          
##    Detection Prevalence : 0.7733          
##       Balanced Accuracy : 0.9707          
##                                           
##        'Positive' Class : 0               
## 
```
####Random forest regression model to predict employees leaving the company is predicting with an 
  * overall Accuracy of 0.9846616,
  * predicting employees leaving company with 0.9439776 accuracy and 
  * predicting employess staying with the company with 0.9973742 accuracy.

###Summarize Logistics and Random Forest Regression results

```r
overall <- cbind(
  glm_confustion_matrix_train$overall,
  glm_confustion_matrix_validation$overall,
  glm_confustion_matrix_test$overall,
  rf_conf_matrix_train$overall,
  rf_confustion_matrix_validation$overall,
  rf_conf_matrix$overall
  )
  
byClass <- cbind(
  glm_confustion_matrix_train$byClass,
  glm_confustion_matrix_validation$byClass,
  glm_confustion_matrix_test$byClass,
  rf_conf_matrix_train$byClass,
  rf_confustion_matrix_validation$byClass,
  rf_conf_matrix$byClass
  )
  
all_conf_matrix <- rbind(overall, byClass)
all_conf_matrix <- data.frame(all_conf_matrix)
names(all_conf_matrix) <- c("glm_train","glm_validation", "glm_test", "rf_train","rf_validation", "rf_test")

all_conf_matrix$glm_train <- round(all_conf_matrix$glm_train, digits = 4)
all_conf_matrix$glm_validation <- round(all_conf_matrix$glm_validation, digits = 4)
all_conf_matrix$glm_test <- round(all_conf_matrix$glm_test, digits = 4)
all_conf_matrix$rf_train <- round(all_conf_matrix$rf_train, digits = 4)
all_conf_matrix$rf_validation <- round(all_conf_matrix$rf_validation, digits = 4)
all_conf_matrix$rf_test <- round(all_conf_matrix$rf_test, digits = 4)

rownames(all_conf_matrix)[3] <- "95% CI (Upper)"
rownames(all_conf_matrix)[4] <- "95% CI (Lower)"
rownames(all_conf_matrix)[5] <- "No Information Rate"
rownames(all_conf_matrix)[6] <- "P-Value [Acc > NIR]"
rownames(all_conf_matrix)[7] <- "Mcnemar's Test P-Value"

all_conf_matrix
```

```
##                        glm_train glm_validation glm_test rf_train
## Accuracy                  0.7970         0.7983   0.7823   0.9872
## Kappa                     0.3580         0.3453   0.2949   0.9643
## 95% CI (Upper)            0.7885         0.7835   0.7671   0.9847
## 95% CI (Lower)            0.8053         0.8126   0.7969   0.9894
## No Information Rate       0.7619         0.7620   0.7619   0.7619
## P-Value [Acc > NIR]       0.0000         0.0000   0.0044   0.0000
## Mcnemar's Test P-Value    0.0000         0.0000   0.0000   0.0000
## Sensitivity               0.9252         0.9361   0.9247   0.9982
## Specificity               0.3868         0.3571   0.3263   0.9519
## Pos Pred Value            0.8284         0.8234   0.8146   0.9852
## Neg Pred Value            0.6177         0.6359   0.5753   0.9942
## Precision                 0.8284         0.8234   0.8146   0.9852
## Recall                    0.9252         0.9361   0.9247   0.9982
## F1                        0.8741         0.8762   0.8662   0.9917
## Prevalence                0.7619         0.7620   0.7619   0.7619
## Detection Rate            0.7049         0.7133   0.7046   0.7606
## Detection Prevalence      0.8509         0.8663   0.8650   0.7720
## Balanced Accuracy         0.6560         0.6466   0.6255   0.9751
##                        rf_validation rf_test
## Accuracy                      0.9750  0.9847
## Kappa                         0.9287  0.9570
## 95% CI (Upper)                0.9688  0.9796
## 95% CI (Lower)                0.9803  0.9887
## No Information Rate           0.7620  0.7619
## P-Value [Acc > NIR]           0.0000  0.0000
## Mcnemar's Test P-Value        0.0000  0.0000
## Sensitivity                   0.9987  0.9974
## Specificity                   0.8992  0.9440
## Pos Pred Value                0.9694  0.9828
## Neg Pred Value                0.9953  0.9912
## Precision                     0.9694  0.9828
## Recall                        0.9987  0.9974
## F1                            0.9838  0.9900
## Prevalence                    0.7620  0.7619
## Detection Rate                0.7610  0.7599
## Detection Prevalence          0.7850  0.7733
## Balanced Accuracy             0.9489  0.9707
```




