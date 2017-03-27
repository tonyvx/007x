#Data Wrangling

#Rename the sales column to dept and make columns number_project, promotion_last_5years, left, Work_accident, sales,salary as factor

hr$number_project <- as.factor(hr$number_project)
hr$promotion_last_5years <- as.factor(hr$promotion_last_5years)
hr$left <- as.factor(hr$left)
hr$Work_accident <- as.factor(hr$Work_accident)
hr$sales <- as.factor(hr$sales)
hr$salary <- as.factor(hr$salary)

names(hr)[9] <- "dept"

number_project -> 
high satisfaction level employees involved in 3-6 projects, less than 3 and more than 6 satisfaction is low
last_evaluation is directly proportion to number_project
average_monthly_hours also is directly proportion to number_project
number of employees leaving (left=1) is high at 2 or less improves for 3 projects and then slowly creeps up until 6 projects and goes down

Looks like 3-6 projects would mean less employees quiting and good employee satisfaction.

average_monthly_hours
relation to satisfaction or last_evaluation is not very obvious
average_monthly_hours seems to go higher with number_project

employees leaving (left=1) 
	150 hours +/- 25 shows high number of employees leaving
	Again 225 hours and more employees leaving creaps to peak at around 250 hours and dips gradually to 300 hours

	175 hours - 225 hours for average_monthly_hours seems to have low rate of employees leaving

last_evaluation
	Between 0.4 to 0.6 evaluation rating, employees leaving (left=1) is high. 
	It goes down at mid level and then picks up at around 0.75 and peaks around 0.85
	So low and higher end of evaluation rating employees leaving is high.
	Though one's leaving at lower rating is good for the company but the employees leaving having high rating is a concern for the company. 	

Once you have clearly understood the axes you have to make the Y axis the dependant variable and X the independent one. Then the name of the graph becomes , Dependant variable vs Independent variable.Sep 2, 2015
	
	