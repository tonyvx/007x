library(dplyr)
library(ggplot2)
library(tidyr)
library(caTools)
library(corrplot)

#Description of data set
description =  c(
    "Employee satisfaction level",
  "Last evaluation",
  "Number of projects",
  "Average monthly hours",
  "Time spent at the company",
  "Whether they have had a work accident",
  "Whether they have had a promotion in the last 5 years",
  "Sales",
  "Salary",
  "Whether the employee has left"
)
dataDescription <- function(hr_data) {
  return(str(hr_data))
  
}

#load Data Set
hrDF <- function() {
  hr_efficiency <- read.csv("./HR_comma_sep.csv", header = TRUE)
  return(hr_efficiency)
}



hr <- hrDF()
dataDescription(hr)



# Cannot see a correlation between promotion in last 5 years to emp satisfaction
plot_promotion_last_5years_observation <- function(){
  return("Cannot see a correlation between promotion in last 5 years to emp satisfaction")
}
plot_promotion_last_5years <- function(hr) {
  plotData <- hr %>% na.omit() %>%
    group_by(satisfaction_level, factor(promotion_last_5years)) %>%
    mutate(emp_count = n()) %>%
    ggplot(aes(
      x = satisfaction_level,
      y = emp_count,
      col = factor(promotion_last_5years)
    )) +
    geom_line() + 
    ggtitle(plot_promotion_last_5years_observation())
  return(plotData)
}

plot_number_project_observation <- function(){
  return("Though with less projects there are employees with high satisfaction,\n 3-5 projects seems to be common for high staisfaction")
}
plot_number_project <- function(hr){
  plotData <- hr %>% na.omit() %>%
    group_by(satisfaction_level, factor(number_project)) %>%
    mutate(emp_count = n()) %>%
    ggplot(aes(
      x = satisfaction_level,
      y = emp_count,
      col = factor(number_project)
    )) +
    geom_line() +
    ggtitle(plot_number_project_observation())
  return(plotData)
}

plot_last_evaluation_observation <- function(){
  return("Though there are employees with high satisfaction even with low evaluation, \n evaluation rating of 0.6 -1 seems to be high among employees with high satisfaction level")
}
plot_last_evaluation <- function(hr){
  plotData <- hr %>% na.omit() %>% 
    mutate(last_eval_cat = cut(last_evaluation, c(0, 0.2, 0.4, 0.6, 0.8, 1))) %>% 
    group_by(satisfaction_level, factor(last_eval_cat)) %>%
    mutate(emp_count = n()) %>%
    ggplot(aes(
    x = satisfaction_level,
    y = emp_count,
    col = factor(last_eval_cat)
  )) +
    geom_line() + 
    ggtitle(plot_last_evaluation_observation())
  return(plotData)
}



#Again there are employees who have high satisfaction level at low avg monthly hours
# 150-250 average monthly hours leads higher satisfaction level

plot_average_montly_hours <- function(hr){
  plotData <- hr %>% na.omit() %>% 
    mutate(monthly_avg_hrs_cat = cut(average_montly_hours, c(0, 50, 10, 150, 200, 250, 300, 350))) %>% 
    group_by(satisfaction_level, factor(monthly_avg_hrs_cat)) %>%
    mutate(emp_count = n()) %>%
    ggplot(aes(
    x = satisfaction_level,
    y = emp_count,
    col = factor(monthly_avg_hrs_cat)
  )) +
    geom_line() +
    ggtitle("150-250 average monthly hours leads higher satisfaction level")
  return(plotData)
}

#Technical , support, sales areas have higher satisfaction levels
plot_sales <- function(hr) {
  plotData <- hr %>% na.omit() %>%
    group_by(satisfaction_level, sales) %>% 
    mutate(emp_count = n()) %>% 
    ggplot(aes(
      x = satisfaction_level, 
      y = emp_count, 
      col = factor(sales))) +
    geom_line(alpha = 0.6) +
    ggtitle("Technical , support, sales areas have higher satisfaction levels")
  return(plotData)
}

#Cannot see a very clear correlation
plot_salary <- function(hr){
  plotData <- hr %>% na.omit() %>% 
    group_by(satisfaction_level, factor(salary)) %>% 
    mutate(emp_count = n()) %>% 
    ggplot(aes(
    x = satisfaction_level,
    y = emp_count,
    col = factor(salary)
  )) +
    geom_line() +
    ggtitle("Cannot see a very clear correlation")
  return(plotData)
}

#Cannot see a very clear correlation
plot_time_spend_company <- function(hr){
  plotData <- hr %>% na.omit() %>% 
    mutate(time_spend_company_cat = cut(time_spend_company, c(0,  4, 8, 10, 12, 14))) %>% 
    
    group_by(satisfaction_level, factor(time_spend_company)) %>% 
    mutate(emp_count = n()) %>% 
    ggplot(aes(
    x = satisfaction_level,
    y = emp_count,
    col = factor(time_spend_company_cat)
  )) +
    geom_line() +
    ggtitle("Most employees stay 0-4 years but \n employees staying on for 4-8 years creeps up between 0.75 to 1")
  return(plotData)
}

getSelectedPlot <- function(plot_type, hr) {
  if (plot_type == "plot_number_project") {
    plotData = plot_number_project(hr)
  } else if (plot_type == "plot_promotion_last_5years") {
    plotData = plot_promotion_last_5years(hr)
  } else if (plot_type == "plot_last_evaluation") {
    plotData = plot_last_evaluation(hr)
  } else if (plot_type == "plot_sales") {
    plotData = plot_sales(hr)
  } else if (plot_type == "plot_salary") {
    plotData = plot_salary(hr)
  } else if (plot_type == "plot_time_spend_company") {
    plotData = plot_time_spend_company(hr)
  }
  return(plotData)
}

getPlotTypes <- function() {
  plot_types = c(
    "number of projects vs satisfaction level" = "plot_number_project" ,
    "promotion in last 5 years vs satisfaction level" = "plot_promotion_last_5years",
    "last evaluation rating vs satisfaction level" = "plot_last_evaluation",
    "employee satisfaction level by departments " = "plot_sales",
    "employee satisfaction level by salary" = "plot_salary",
    "employee satisfaction level vs time spend in company "="plot_time_spend_company"
  )
  return(plot_types)
}
# set.seed(3000)
# 
# split = sample.split(hr$satisfaction_level, SplitRatio = 0.7)
# Train = subset(hr, split = TRUE)
# Test = subset(hr, split = FALSE)
# 
# lm_satisfaction = lm (
#   satisfaction_level ~ last_evaluation + number_project + average_montly_hours +
#     time_spend_company + promotion_last_5years  + salary,
#   data = Train
# )
# plot(lm_satisfaction)
# 
# abline(lm_satisfaction)
# 
# summary(lm_satisfaction)
# 
# 
# abline(predict(lm_satisfaction, newdata = Test) ,
#        coef(lm_satisfaction),
#        col = "red")
# 
# lm_satisfaction = lm (
#   satisfaction_level ~ last_evaluation + number_project + average_montly_hours +
#     time_spend_company + promotion_last_5years  + salary,
#   data = Train
# )