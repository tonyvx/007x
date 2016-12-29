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
  hr_efficiency$promotion_last_5years_f <-
    as.factor(hr_efficiency$promotion_last_5years)
  hr_efficiency$left_f <- as.factor(hr_efficiency$left)
  hr_efficiency$Work_accident_f <-
    as.factor(hr_efficiency$Work_accident)
  hr_efficiency$number_project_f <-
    as.factor(hr_efficiency$number_project)
  hr_efficiency$last_eval_cat = cut(hr_efficiency$last_evaluation, c(0, 0.2, 0.4, 0.6, 0.8, 1))
  hr_efficiency$monthly_avg_hrs_cat <-
    cut(hr_efficiency$average_montly_hours,
        c(0, 50, 10, 150, 200, 250, 300, 350))
  hr_efficiency$time_spend_company_cat <-
    cut(hr_efficiency$time_spend_company, c(0,  4, 8, 10, 12, 14))
  hr_efficiency$total_emp <- NROW(hr_efficiency)
  names(hr_efficiency)[9] <- "dept"
  return(hr_efficiency)
}



hr <- hrDF()
dataDescription(hr)



# Cannot see a correlation between promotion in last 5 years to emp satisfaction
plot_promotion_last_5years_observation <- function() {
  return("Cannot see a correlation between promotion in last 5 years to emp satisfaction")
}
plot_promotion_last_5years <- function(hr) {
  plotData <- hr %>% na.omit() %>%
    group_by(satisfaction_level, promotion_last_5years_f) %>%
    mutate(emp_count = n()) %>%
    ggplot(aes(x = satisfaction_level,
               y = emp_count,
               col = promotion_last_5years_f)) +
    geom_line() +
    ggtitle(plot_promotion_last_5years_observation())
  return(plotData)
}

plot_number_project_observation <- function() {
  return(
    "Though with less projects there are employees with high satisfaction,\n 3-5 projects seems to be common for high staisfaction"
  )
}
plot_number_project <- function(hr) {
  plotData <- hr %>% na.omit() %>%
    group_by(satisfaction_level, number_project_f) %>%
    mutate(emp_count = n()) %>%
    ggplot(aes(x = satisfaction_level,
               y = emp_count,
               col = number_project_f)) +
    geom_line() +
    ggtitle(plot_number_project_observation())
  return(plotData)
}

plot_last_evaluation_observation <- function() {
  return(
    "Though there are employees with high satisfaction even with low evaluation, \n evaluation rating of 0.6 -1 seems to be high among employees with high satisfaction level"
  )
}
plot_last_evaluation <- function(hr) {
  plotData <- hr %>% na.omit() %>%
    group_by(satisfaction_level, last_eval_cat) %>%
    mutate(emp_count = n()) %>%
    ggplot(aes(x = satisfaction_level,
               y = emp_count,
               col = last_eval_cat)) +
    geom_line() +
    ggtitle(plot_last_evaluation_observation())
  return(plotData)
}



#Again there are employees who have high satisfaction level at low avg monthly hours
# 150-250 average monthly hours leads higher satisfaction level

plot_average_montly_hours <- function(hr) {
  plotData <- hr %>% na.omit() %>%
    group_by(satisfaction_level, monthly_avg_hrs_cat) %>%
    mutate(emp_count = n()) %>%
    ggplot(aes(x = satisfaction_level,
               y = emp_count,
               col = monthly_avg_hrs_cat)) +
    geom_line() +
    ggtitle("150-250 average monthly hours leads higher satisfaction level")
  return(plotData)
}

#Technical , support, sales areas have higher satisfaction levels
plot_sales <- function(hr) {
  plotData <- hr %>% na.omit() %>%
    group_by(satisfaction_level, dept) %>%
    mutate(emp_count = n()) %>%
    ggplot(aes(x = satisfaction_level,
               y = emp_count,
               col = dept)) +
    geom_line(alpha = 0.6) +
    ggtitle("Technical , support, sales areas have higher satisfaction levels")
  return(plotData)
}

#Cannot see a very clear correlation
plot_salary <- function(hr) {
  plotData <- hr %>% na.omit() %>%
    group_by(satisfaction_level, salary) %>%
    mutate(emp_count = n()) %>%
    ggplot(aes(x = satisfaction_level,
               y = emp_count,
               col = salary)) +
    geom_line() +
    ggtitle("Cannot see a very clear correlation")
  return(plotData)
}

#Cannot see a very clear correlation
plot_time_spend_company <- function(hr) {
  plotData <- hr %>% na.omit() %>%
    group_by(satisfaction_level, time_spend_company_cat) %>%
    mutate(emp_count = n()) %>%
    ggplot(aes(x = satisfaction_level,
               y = emp_count,
               col = time_spend_company_cat)) +
    geom_line() +
    ggtitle(
      "Most employees stay 0-4 years but \n employees staying on for 4-8 years creeps up between 0.75 to 1"
    )
  return(plotData)
}

plot_correlation <- function(hr,prj_eval,time_in_co,num_prj) {
  hr_good_leaving_people2 <-
    hr %>% filter(last_evaluation >= prj_eval |
                    time_spend_company >= time_in_co | number_project > num_prj)
  hr_good_people_select <-
    hr_good_leaving_people2 %>% select(satisfaction_level, number_project:promotion_last_5years)
  M <- cor(hr_good_people_select)
  return(corrplot(M, method = "pie"))
}
plot_atrition_dept <- function(hr) {
  
   p <- hr %>% group_by(dept,left_f) %>% mutate(emp_count=n())  %>% ggplot(aes(x=dept, y=emp_count, col=left_f)) +geom_point()
  return(p)
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
    "employee satisfaction level vs time spend in company " = "plot_time_spend_company"
  )
  return(plot_types)
}


# set.seed(3000)
#
# split = sample.split(hr$satisfaction_level_cat, SplitRatio = 0.7)
# Train = subset(hr, split = TRUE)
# Test = subset(hr, split = FALSE)
#
# lm_satisfaction = lm (
#   satisfaction_level_cat ~ last_evaluation + number_project + average_montly_hours +
#     satisfaction_level + promotion_last_5years  + salary,
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
#   satisfaction_level_cat ~ last_evaluation + number_project + average_montly_hours +
#     satisfaction_level + promotion_last_5years  + salary,
#   data = Train
# )