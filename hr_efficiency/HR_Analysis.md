# HR Efficiency Analysis
`r format(Sys.Date())`  




##About Client and problem
The client is the HR department for company ABC.

HR Department wants to know:

* The factors that are important to the employees.
* Why are best and most experienced employees of ABC leaving prematurely.
* Provide recommendation to HR department to lower attrition rate and save on hiring costs.
* Build a predictive model to determine their probability in leaving.

## Data Source
https://www.kaggle.com/ludobenistant/hr-analytics

https://www.kaggle.com/ludobenistant/hr-analytics/downloads/human-resources-analytics.zip

#####Lets load the dataset and analyze

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

Lets look at the structure

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

###Quick exploratory analysis with ggpairs

Lets look at correlation between various attributes of the dataset


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

* We can see attributes _number_project_, _average_monthly_hours_, _last_evaluation_, _time_spend_company_ having correlation to _left_ and _satisfaction_level_.
* Looks like employees involved in 3 -5 projects and putting in 175 - 225 average monthly hours have lower rate of leaving company and have high satisfaction level.
* Assuming 22 working days a month, 175 - 225 average monthly hours translates to 8 - 10 hrs per day. 


```r
hr %>%  dplyr::select(Work_accident, promotion_last_5years, salary, left) %>% ggpairs()
```

![](HR_Analysis_files/figure-html/ggpairs_discrete_attributes-1.png)<!-- -->


* Work_accident, promotion_last_5years & salary seems to have lower impact on employees leaving company.


###Exploratory Analysis using ggplots , geom_histogram & facet_grids
Lets plot satisfaction_level against facets number_project, average_montly_hours, time_spend_company, promotion_last_5years, Work_accident, salary & dept. Each plot correlates satisfaction_level(histogram), left (fill ) with one of the other attribute as facets. 

#### number_project

```r
hr_explore <- hr

hr_explore %>% ggplot(aes(satisfaction_level)) + 
  geom_histogram(binwidth = 0.05, aes(fill = left)) + labs(x = "satisfaction_level", y = "employees", title = "number_project") + 
  facet_grid(. ~ number_project) + 
  myTheme
```

![](HR_Analysis_files/figure-html/plot_data_exp_analysis_number_project-1.png)<!-- -->
  
  * Most of employees having 2 projects are showing a high rate of leaving most of them seem to be mid level satisfaction_level. 
  * Most of the employees having 3 - 5 projects are having high satisfaction level. We can also see employees leaving creeping between 3 - 5 projects and most of employees leaving are having high satisfaction level too.
  * 6 - 7 projects however we see satisfaction level being very low and a very high rate employees leaving.

####average_montly_hours

```r
hr_explore$monthly_hrs_range <- cut(hr_explore$average_montly_hours,
                                    breaks = c(0, 174, 225, 250, 275, 300, 325, 350),
                                    labels = c("174", "225", "250", "275", "300", "325", "350"),
                                    right = FALSE
                                    )
hr_explore %>% ggplot(aes(satisfaction_level)) + 
  geom_histogram(binwidth = 0.05, aes(fill = left)) + labs(x = "satisfaction_level", y = "employees", title = "monthly_hrs_range") + 
  facet_grid(. ~monthly_hrs_range) + 
  myTheme
```

![](HR_Analysis_files/figure-html/plot_data_exp_analysis_monthly_hrs_range-1.png)<!-- -->
  
  * Since average_montly_hours is continuous data, in order for being able to use as a facet lets make it a discrete by creating ranges / monthly_hrs_range. Starting at 174 hours as that is __normal__ average monthly hours.
  * Employees putting in 174 and less average monthly hours show medium satisfaction and very high rate of leaving.
  * Employees putting in 174 - 225 average monthly hours seem to have higher satisfaction level. Though we can see a small peak in employees leaving in this category. (8-10 hrs a day)
  * Employees putting in  225 - 275 average monthly hours show mostly higher satisfaction level and we can see rate of leaving in high satisfaction level creeping. (10 - 13 hours a day)
  * Employees putting in 275 hours and more average monthly hours are having a very low satisfaction level and are mostly leaving.

#### last_evaluation

```r
hr_explore$evaluation_range <- as.factor(round(hr_explore$last_evaluation,1))

hr_explore %>% ggplot(aes(satisfaction_level)) + 
  geom_histogram(binwidth = 0.05, aes(fill = left)) + labs(x = "satisfaction_level", y = "employees", title = "evaluation_range") + 
  facet_grid(. ~ evaluation_range) + 
  myTheme
```

![](HR_Analysis_files/figure-html/plot_data_exp_analysis_last_evaluation-1.png)<!-- -->

* Since last_evaluation is continuous  data, first lets round the last evaluation to 1 place and then make it into a factor
* Employees having evaluation rating 0.4 - 0.6, show satisfaction levels improving with rating and employees leaving have mostly medium satisfaction level.
* Employees at 0.7 rating show higher satisfaction level and low rate of quitting.
* Employees having higher rating of 0.8 - 1 show folks leaving with low satisfaction level high and gradually reducing but then employees with satisfaction and leaving is slowly creeping up.

#### time_spend_company

```r
hr_explore %>% ggplot(aes(satisfaction_level)) + 
  geom_histogram(binwidth = 0.05, aes(fill = left)) + labs(x = "satisfaction_level", y = "employees", title = "time_spend_company") + 
  facet_grid(. ~ time_spend_company) + 
  myTheme
```

![](HR_Analysis_files/figure-html/plot_data_exp_analysis_time_spend_company-1.png)<!-- -->

* Employees who have spent 2 years show very low rate of leaving and seem to have high satisfaction
* Employees who have been with the company 3 years show high satisfaction level among folks who remain but mostly there is a segment with medium satisfaction who are leaving with high rate.
* Employees who have spent 4 years continue to have high satisfaction level among who remain, but has a segment which has low satisfaction level and are leaving at high rate.
* Employees who have been with company 5- 6 years however show a higher rate of leaving in higher satisfaction level.
* Employees who have been longer than 6 are very low and most of them stay.


#### promotion_last_5years

```r
hr_explore %>% ggplot(aes(satisfaction_level)) + 
  geom_histogram(binwidth = 0.05, aes(fill = left)) + labs(x = "satisfaction_level", y = "employees", title = "promotion_last_5years") +
  facet_grid(. ~ promotion_last_5years) + 
  myTheme
```

![](HR_Analysis_files/figure-html/plot_data_exp_analysis_promotion_last_5years-1.png)<!-- -->

* Very few employees have got promoted in last 5 years
* Among employees who did not get promoted we can see employees quitting at low, medium and high satisfaction levels. This trend follows among folks who got promoted.
* promotion_last_5years is not very a concern for employees leaving.

#### Work_accident

```r
hr_explore %>% ggplot(aes(satisfaction_level)) + 
  geom_histogram(binwidth = 0.05, aes(fill = left)) + labs(x = "satisfaction_level", y = "employees", title = "Work_accident") + 
  facet_grid(. ~ Work_accident) + 
  myTheme
```

![](HR_Analysis_files/figure-html/plot_data_exp_analysis_Work_accident-1.png)<!-- -->

* Among employees who did not have work accident, we can see employees quitting at low, medium and high satisfaction levels. This trend follows among folks who had accident.
* Also, very small percent of employees who had accident are leaving, which means accidents are not impacting there work long term.
* Work_accident does not seem to high of a concern for employees to leave.

#### salary

```r
hr_explore %>% ggplot(aes(satisfaction_level)) + 
  geom_histogram(binwidth = 0.05, aes(fill = left)) + labs(x = "satisfaction_level", y = "employees", title = "salary") + 
  facet_grid(. ~ salary) + 
  myTheme
```

![](HR_Analysis_files/figure-html/plot_data_exp_analysis_salary-1.png)<!-- -->

* More people are in medium and low salary level than high salary level.
* At all salary level, we can see similar pattern of peaks in employees leaving at low, medium and high satisfaction levels.
* Looks like employees are paid well for their role and salary is not driving employees to leave.

#### dept

```r
hr_explore %>% ggplot(aes(satisfaction_level)) + 
  geom_histogram(binwidth = 0.05, aes(fill = left)) + labs(x = "satisfaction_level", y = "employees", title = "dept") + 
  facet_grid(. ~ dept) +
  myTheme
```

![](HR_Analysis_files/figure-html/plot_data_exp_analysis_dept-1.png)<!-- -->

* sales department is very large, followed by technical and support departments.
* All departments show a similar pattern of peaks in employees leaving at low, medium and high satisfaction levels.
* dept data does not show any unusual pattern that can be correlated, all departments are showing similar pattern.

#####Overall
* number_project, average_montly_hours, last_evaluation & time_spend_company are of significant interest for analyzing reasons for employees leaving
* We can see 3 categories/patterns of employees who are leaving
  
  + high satisfaction level 
    * Employees involved in 3 - 5 projects 
    * Employees putting in 225 - 275 average monthly hours. (10 - 13 hrs)
    * Employees with a evaluation rating of 0.8 - 1.
    * Employees who have been with the company 5 - 6 years

  + medium satisfaction level 
    * Employees involved in 2 projects
    * Employees putting in 174 and less average monthly hours. (less than 8 hrs a day)
    * Employees having evaluation rating 0.4 - 0.6
    * Employees who have been with the company for around 3 years

  + low satisfaction level
    * Employees involved in 6 - 7 projects
    * Employees putting in 275 hours and more average monthly hours  (More than 13 hrs a day)
    * Employees having evaluation rating 0.8 - 1 (this group also has folks leaving at higher satisfaction level too)
    * Employees who have been with the company for around 4 years

* It seems low satisfaction is driven by over work and medium satisfaction is due to less work (does less work means lower remuneration?). 
* Employees putting in 8-10 hours a day seems to be having higher satisfaction level and low rate of leaving.
* Overall right sizing work across the workforce could improve overall satisfaction and company will be able to retain employees and improve overall employee utilization.


###Exploratory analysis using Clustering
Here we will be using cluster analysis on __employees who left__ to explore various correlations. We will be using k-Means clustering.

####Identify number of clusters for kmeans cluster analysis
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


####Cluster Plot 
Let's consider 3 clusters for analysis. Also in exploratory analysis, we could identify 3 groups of employees who are leaving (high, medium & low satisfaction level)


```r
# let's consider 3 clusters 

fit.km <- kmeans(scale(hr_clust), 3)
clusplot(hr_clust,fit.km$cluster, color = TRUE)
```

![](HR_Analysis_files/figure-html/clustering-1.png)<!-- -->

####Lets plot the 3 clusters with key attributes 
We will use attributes identified earlier as showing correlation to employee satisfaction and employees leaving - 
satisfaction_level, number_projects, last_evalaluation, average_monthly_hours,time_spend_company

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

####Observation
#####cluster 3
* Employees having overall higher satisfaction level, 
* involved 3-5 projects, 
* spend 5 - 6 years.
* higher evaluation rating (0.8 -1) and 
* average monthly hours of 225  - 275 hours. Assuming 22 working days a month, this translates to 10 - 13 hrs per day.

#####cluster 1
* Employees having overall medium satisfaction level, 
* involved in less than 2 projects, 
* spend 3 - 4 years
* low to medium evaluation rating (<0.6) and 
* low average monthly hours of less than 175 hours.

#####cluster 2
* Employees having overall low satisfaction level, 
* involved in mainly 6 - 7 projects, 
* spend 4 -5 years
* high evaluation rating (0.8-1) and 
* high average monthly hours of 225 - 350 hours. Assuming 22 working days a month, this translates to 10 - 16 hrs per day.

#####overall
* cluster 3 are employees that company need to find ways to retain. Based on earlier exploratory analysis we know keeping average monthly hours to 174 - 225 hrs or 8-10 hrs a day helps to maintain retention and employee satisfaction.
* cluster 2 are over worked employees, company need to find ways to optimize work load and improve satisfaction level
* cluster 1 are under-utilized employees and company need to find means to have them share the work load of cluster 2.
* Overall right sizing work load and ensuring a 8 - 10 hours a day work load will go long way to have highly satisfied employee and low attrition rate.

####Lets plot the 3 clusters with other important attributes
We will use other important attributes identified earlier as __not showing correlation or low correlation__ to employee satisfaction and employees leaving - 
promotion_last_5years, dept, work_accident, salary


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



###Exploratory analysis using Linear Regression
Let's build a model to determine how long an employee will stay using data from __employees who already left__

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

####Let's use caret package to anlyze significant fields for linear model

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

From model summary, we can see following attributes impacting time spend in company 

* satisfaction_level      
* number_project   
* average_montly_hours    
* promotion_last_5years (1 - promoted)
* dept - management           

Model _lm_time_spend_ has an R Squared of 0.7389789 and adjusted R-Squared of 0.7377862

####Let's predict using the model _lm_time_spend_

```r
predict_time_spend <- predict(lm_time_spend, newdata = hrTest)
```

####Let's measure the correlation

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

####Let's compute Root Mean Square Error for the model

```r
#let's see how good the model prediction was

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
####Linear model _lm_time_spend_ prediction with a correlation coefficient of 0.87 is providing a good prediction of time spent in company by employees with an RMSE of 0.48 years.

##Build a model to predict Employees leaving
* We will split available data set into training data (60%), validation data (10%) and Test data (10%)
* We will build the predictive model using training data. We will be trying both Logistic regression and random forest model.
* Validate both the models against validation data for repeatability of the model against unseen data and pick the best model of the two.

###Data

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

###Logistic Regression

Let's build a model to __predict if the employee will leave__ using Logistic Regression

####Let's identify significant attributes for logistics regression


```r
plot(varImp(train(left ~ ., data = hrTrain, method = "glm")))
```

![](HR_Analysis_files/figure-html/log_reg_model_signi_fields-1.png)<!-- -->
* Attributes - _number_project_, _time_spend_company_, _satisfaction_level_, _Work_accident_, _salary_, _last_evaluation, _average_monthly_hours_, _promotion_last_5years_ & _dept_ are significant for the logistics regression model to predict left (employees leaving left=1)

####Logistic Regression Model
By converting time_spend_company and number_project as factor in the model, we are getting better sensitivity for positive class 1 (employees left)

```r
#Logistics regression model
log_model <- train( left ~ as.factor(time_spend_company) + 
                      average_montly_hours + 
                      dept+
                      last_evaluation +
                      promotion_last_5years +
                      as.factor(number_project) +
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
## -3.3136  -0.3221  -0.1122   0.0000   3.9660  
## 
## Coefficients:
##                                     Estimate Std. Error z value Pr(>|z|)
## (Intercept)                       -4.740e+00  3.981e-01 -11.906  < 2e-16
## `as.factor(time_spend_company)3`   2.786e+00  2.251e-01  12.375  < 2e-16
## `as.factor(time_spend_company)4`   2.951e+00  2.439e-01  12.097  < 2e-16
## `as.factor(time_spend_company)5`   4.925e+00  2.402e-01  20.498  < 2e-16
## `as.factor(time_spend_company)6`   3.930e+00  2.575e-01  15.260  < 2e-16
## `as.factor(time_spend_company)7`  -1.433e+01  4.779e+02  -0.030 0.976082
## `as.factor(time_spend_company)8`  -1.404e+01  5.501e+02  -0.026 0.979634
## `as.factor(time_spend_company)10` -1.387e+01  5.027e+02  -0.028 0.977989
## average_montly_hours               8.272e-03  9.504e-04   8.703  < 2e-16
## depthr                             4.797e-02  2.200e-01   0.218 0.827363
## deptIT                            -4.184e-01  2.081e-01  -2.011 0.044339
## deptmanagement                    -2.815e-01  2.815e-01  -1.000 0.317248
## deptmarketing                      1.983e-01  2.193e-01   0.904 0.365937
## deptproduct_mng                   -3.266e-01  2.175e-01  -1.502 0.133149
## deptRandD                         -7.652e-01  2.349e-01  -3.257 0.001126
## deptsales                         -2.298e-01  1.708e-01  -1.345 0.178487
## deptsupport                       -2.027e-01  1.826e-01  -1.110 0.266905
## depttechnical                      3.994e-02  1.768e-01   0.226 0.821232
## last_evaluation                    2.261e+00  2.827e-01   7.995 1.29e-15
## promotion_last_5years1            -1.621e+00  4.917e-01  -3.298 0.000975
## `as.factor(number_project)3`      -5.485e+00  1.909e-01 -28.733  < 2e-16
## `as.factor(number_project)4`      -4.171e+00  1.400e-01 -29.787  < 2e-16
## `as.factor(number_project)5`      -3.383e+00  1.390e-01 -24.330  < 2e-16
## `as.factor(number_project)6`      -2.705e+00  1.661e-01 -16.285  < 2e-16
## `as.factor(number_project)7`       1.607e+01  4.325e+02   0.037 0.970357
## salarylow                          1.917e+00  1.967e-01   9.746  < 2e-16
## salarymedium                       1.434e+00  1.987e-01   7.217 5.33e-13
## Work_accident1                    -1.606e+00  1.404e-01 -11.436  < 2e-16
## satisfaction_level                -2.122e+00  1.822e-01 -11.647  < 2e-16
##                                      
## (Intercept)                       ***
## `as.factor(time_spend_company)3`  ***
## `as.factor(time_spend_company)4`  ***
## `as.factor(time_spend_company)5`  ***
## `as.factor(time_spend_company)6`  ***
## `as.factor(time_spend_company)7`     
## `as.factor(time_spend_company)8`     
## `as.factor(time_spend_company)10`    
## average_montly_hours              ***
## depthr                               
## deptIT                            *  
## deptmanagement                       
## deptmarketing                        
## deptproduct_mng                      
## deptRandD                         ** 
## deptsales                            
## deptsupport                          
## depttechnical                        
## last_evaluation                   ***
## promotion_last_5years1            ***
## `as.factor(number_project)3`      ***
## `as.factor(number_project)4`      ***
## `as.factor(number_project)5`      ***
## `as.factor(number_project)6`      ***
## `as.factor(number_project)7`         
## salarylow                         ***
## salarymedium                      ***
## Work_accident1                    ***
## satisfaction_level                ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 9880.1  on 8999  degrees of freedom
## Residual deviance: 4607.4  on 8971  degrees of freedom
## AIC: 4665.4
## 
## Number of Fisher Scoring iterations: 17
```

```r
#predict against the training data                           
glm_prediction_train <- predict(log_model,  hrTrain)

#Confusion matrix for prediction against training data
glm_confusion_matrix_train <- confusionMatrix(glm_prediction_train,
                                               hrTrain$left,
                                               dnn = c("Predicted", "actual"), positive = "1")

glm_confusion_matrix_train
```

```
## Confusion Matrix and Statistics
## 
##          actual
## Predicted    0    1
##         0 6371  417
##         1  486 1726
##                                           
##                Accuracy : 0.8997          
##                  95% CI : (0.8933, 0.9058)
##     No Information Rate : 0.7619          
##     P-Value [Acc > NIR] : < 2e-16         
##                                           
##                   Kappa : 0.7265          
##  Mcnemar's Test P-Value : 0.02364         
##                                           
##             Sensitivity : 0.8054          
##             Specificity : 0.9291          
##          Pos Pred Value : 0.7803          
##          Neg Pred Value : 0.9386          
##              Prevalence : 0.2381          
##          Detection Rate : 0.1918          
##    Detection Prevalence : 0.2458          
##       Balanced Accuracy : 0.8673          
##                                           
##        'Positive' Class : 1               
## 
```

####Logistic Regression model against validation data
Let us analyze the goodness of fit of the regression by applying the model's predictive performance to data that was not used in model estimation.

```r
#Lets test prediction using test data 
validationPrediction <- predict(log_model,hrValidation)
glm_confusion_matrix_validation <- confusionMatrix(validationPrediction,hrValidation$left, dnn = c("Predicted","actual"), positive = "1")

glm_confusion_matrix_validation
```

```
## Confusion Matrix and Statistics
## 
##          actual
## Predicted    0    1
##         0 2107  154
##         1  179  560
##                                        
##                Accuracy : 0.889        
##                  95% CI : (0.8772, 0.9)
##     No Information Rate : 0.762        
##     P-Value [Acc > NIR] : <2e-16       
##                                        
##                   Kappa : 0.6976       
##  Mcnemar's Test P-Value : 0.1884       
##                                        
##             Sensitivity : 0.7843       
##             Specificity : 0.9217       
##          Pos Pred Value : 0.7578       
##          Neg Pred Value : 0.9319       
##              Prevalence : 0.2380       
##          Detection Rate : 0.1867       
##    Detection Prevalence : 0.2463       
##       Balanced Accuracy : 0.8530       
##                                        
##        'Positive' Class : 1            
## 
```
####Comparing results from the model's prediction on validation data
  * overall Accuracy of 0.889 against validation data as compared to 0.8996667 given by model data,
  * predicting employees leaving company with 0.7843137 accuracy against validation data as compared to 0.805413 given by model data and
  * predicting employees staying with the company with 0.9216973 accuracy against validation data as compared to 0.9291235 given by model.
  * model is maintaining its accuracy levels and hence the model is repetitive  and is representative of the problem.


###Random Forest
Importing doMC package and setting it up to utilize parallel processing across 5 cores, to improve speed of Random Forest model generation on a training set,

```r
#To improve speed of Random Forest model genration on a training set, using doMC to utilize parallel processing across 5 cores
library(doMC)
registerDoMC(5)
```
####Random Forest Model

```r
#Random Forest Model
rf_model <- train(left~time_spend_company + 
                      average_montly_hours + 
                      dept+
                      last_evaluation +
                      promotion_last_5years +
                      number_project +
                      salary + 
                      Work_accident  + 
                      satisfaction_level ,
                  data = hrTrain,
                  method = "rf",
                trControl = trainControl(method = "cv",number = 5))

print(rf_model)
```

```
## Random Forest 
## 
## 9000 samples
##    9 predictor
##    2 classes: '0', '1' 
## 
## No pre-processing
## Resampling: Cross-Validated (5 fold) 
## Summary of sample sizes: 7201, 7200, 7199, 7200, 7200 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa    
##    2    0.9702220  0.9149287
##   10    0.9880000  0.9665000
##   18    0.9862223  0.9616521
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 10.
```

```r
#prediction on train data
rf_prediction_train = predict(rf_model, newdata = hrTrain)

rf_conf_matrix_train <- confusionMatrix(rf_prediction_train, hrTrain$left, dnn = c("Predicted","actual"), positive = "1")

rf_conf_matrix_train
```

```
## Confusion Matrix and Statistics
## 
##          actual
## Predicted    0    1
##         0 6857    0
##         1    0 2143
##                                      
##                Accuracy : 1          
##                  95% CI : (0.9996, 1)
##     No Information Rate : 0.7619     
##     P-Value [Acc > NIR] : < 2.2e-16  
##                                      
##                   Kappa : 1          
##  Mcnemar's Test P-Value : NA         
##                                      
##             Sensitivity : 1.0000     
##             Specificity : 1.0000     
##          Pos Pred Value : 1.0000     
##          Neg Pred Value : 1.0000     
##              Prevalence : 0.2381     
##          Detection Rate : 0.2381     
##    Detection Prevalence : 0.2381     
##       Balanced Accuracy : 1.0000     
##                                      
##        'Positive' Class : 1          
## 
```


####Random Forest Model against validation data
Let us analyze the goodness of fit of the model by applying the model’s predictive performance to data that was not used in model estimation.

```r
#Lets test prediction using test data 
rf_validationPrediction <- predict(rf_model,hrValidation)
rf_confusion_matrix_validation <- confusionMatrix(rf_validationPrediction,hrValidation$left, dnn = c("Predicted","actual"), positive = "1")

rf_confusion_matrix_validation
```

```
## Confusion Matrix and Statistics
## 
##          actual
## Predicted    0    1
##         0 2283   33
##         1    3  681
##                                           
##                Accuracy : 0.988           
##                  95% CI : (0.9834, 0.9916)
##     No Information Rate : 0.762           
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9664          
##  Mcnemar's Test P-Value : 1.343e-06       
##                                           
##             Sensitivity : 0.9538          
##             Specificity : 0.9987          
##          Pos Pred Value : 0.9956          
##          Neg Pred Value : 0.9858          
##              Prevalence : 0.2380          
##          Detection Rate : 0.2270          
##    Detection Prevalence : 0.2280          
##       Balanced Accuracy : 0.9762          
##                                           
##        'Positive' Class : 1               
## 
```

####Comparing results from the model's prediction on validation data
  * overall Accuracy of 0.988 against validation data as compared to 1 given by model data,
  * predicting employees leaving company with 0.9537815 accuracy against validation data as compared to 1 given by model data and
  * predicting employees staying with the company with 0.9986877 accuracy against validation data as compared to 1 given by model.
  * Though model is too perfect (accuracy of 1.0) it is maintaining its accuracy levels close enough with unseen data and hence the model is repetitive and is representative of the problem.

###Overall Random Forest Model is giving better prediction

##Prediction using random forest model


```r
#prediction on test data
rf_prediction = predict(rf_model, newdata = hrTest)

rf_conf_matrix <- confusionMatrix(rf_prediction, hrTest$left, dnn = c("Predicted","actual"), positive = "1")

rf_conf_matrix
```

```
## Confusion Matrix and Statistics
## 
##          actual
## Predicted    0    1
##         0 2280   21
##         1    5  693
##                                           
##                Accuracy : 0.9913          
##                  95% CI : (0.9873, 0.9943)
##     No Information Rate : 0.7619          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9759          
##  Mcnemar's Test P-Value : 0.003264        
##                                           
##             Sensitivity : 0.9706          
##             Specificity : 0.9978          
##          Pos Pred Value : 0.9928          
##          Neg Pred Value : 0.9909          
##              Prevalence : 0.2381          
##          Detection Rate : 0.2311          
##    Detection Prevalence : 0.2327          
##       Balanced Accuracy : 0.9842          
##                                           
##        'Positive' Class : 1               
## 
```

##Summarize Logistic Regression and Random Forest Models

```r
#prediction on test data
glm_testPrediction <- predict(log_model,hrTest)
glm_confusion_matrix_test <- confusionMatrix(glm_testPrediction,hrTest$left, dnn = c("Predicted","actual"), positive = "1")
```


```r
overall <- cbind(
  glm_confusion_matrix_train$overall,
  glm_confusion_matrix_validation$overall,
  glm_confusion_matrix_test$overall,
  rf_conf_matrix_train$overall,
  rf_confusion_matrix_validation$overall,
  rf_conf_matrix$overall
  )
  
byClass <- cbind(
  glm_confusion_matrix_train$byClass,
  glm_confusion_matrix_validation$byClass,
  glm_confusion_matrix_test$byClass,
  rf_conf_matrix_train$byClass,
  rf_confusion_matrix_validation$byClass,
  rf_conf_matrix$byClass
  )
  
all_conf_matrix <- rbind(overall, byClass)
all_conf_matrix <- data.frame(all_conf_matrix)
names(all_conf_matrix) <- c("glm_train","glm_valid", "glm_test", "rf_train","rf_valid", "rf_test")

all_conf_matrix$glm_train <- round(all_conf_matrix$glm_train, digits = 2)
all_conf_matrix$glm_valid <- round(all_conf_matrix$glm_valid, digits = 2)
all_conf_matrix$glm_test <- round(all_conf_matrix$glm_test, digits = 2)
all_conf_matrix$rf_train <- round(all_conf_matrix$rf_train, digits = 2)
all_conf_matrix$rf_valid <- round(all_conf_matrix$rf_valid, digits = 2)
all_conf_matrix$rf_test <- round(all_conf_matrix$rf_test, digits = 2)

rownames(all_conf_matrix)[3] <- "95% CI (Upper)"
rownames(all_conf_matrix)[4] <- "95% CI (Lower)"
rownames(all_conf_matrix)[5] <- "No Information Rate"
rownames(all_conf_matrix)[6] <- "P-Value [Acc > NIR]"
rownames(all_conf_matrix)[7] <- "Mcnemar's Test P-Value"

all_conf_matrix
```

```
##                        glm_train glm_valid glm_test rf_train rf_valid
## Accuracy                    0.90      0.89     0.89     1.00     0.99
## Kappa                       0.73      0.70     0.70     1.00     0.97
## 95% CI (Upper)              0.89      0.88     0.88     1.00     0.98
## 95% CI (Lower)              0.91      0.90     0.90     1.00     0.99
## No Information Rate         0.76      0.76     0.76     0.76     0.76
## P-Value [Acc > NIR]         0.00      0.00     0.00     0.00     0.00
## Mcnemar's Test P-Value      0.02      0.19     0.91      NaN     0.00
## Sensitivity                 0.81      0.78     0.78     1.00     0.95
## Specificity                 0.93      0.92     0.93     1.00     1.00
## Pos Pred Value              0.78      0.76     0.77     1.00     1.00
## Neg Pred Value              0.94      0.93     0.93     1.00     0.99
## Precision                   0.78      0.76     0.77     1.00     1.00
## Recall                      0.81      0.78     0.78     1.00     0.95
## F1                          0.79      0.77     0.77     1.00     0.97
## Prevalence                  0.24      0.24     0.24     0.24     0.24
## Detection Rate              0.19      0.19     0.18     0.24     0.23
## Detection Prevalence        0.25      0.25     0.24     0.24     0.23
## Balanced Accuracy           0.87      0.85     0.85     1.00     0.98
##                        rf_test
## Accuracy                  0.99
## Kappa                     0.98
## 95% CI (Upper)            0.99
## 95% CI (Lower)            0.99
## No Information Rate       0.76
## P-Value [Acc > NIR]       0.00
## Mcnemar's Test P-Value    0.00
## Sensitivity               0.97
## Specificity               1.00
## Pos Pred Value            0.99
## Neg Pred Value            0.99
## Precision                 0.99
## Recall                    0.97
## F1                        0.98
## Prevalence                0.24
## Detection Rate            0.23
## Detection Prevalence      0.23
## Balanced Accuracy         0.98
```

##Recommendations
* Random Forest Model is giving better prediction for employees leaving.
* Overall right sizing work load and ensuring an 8 - 10 hours a day work load will go long way to have highly satisfied employee and low attrition rate.
* High rating does not equate directly to level of satisfaction as shown by peaks in low satisfaction. Also, both at high and low rating level we see folks quitting. So maybe improved rating is not resulting into reward or the reward is not matching expectation?
* Employees who have spent more than 4 years may be finding work not exciting anymore or maybe feeling competitive enough to try opportunities. Maybe there is a dialogue needed here to understand what would make work more rewarding and satisfying.


