library(tidyverse)
library(dplyr)
library(readr)
library(skimr)
library(janitor)
library(lubridate)
library(ggplot2)

#load dataset
hr <- read.csv('HR_Attrition.csv')
#filter out attrition data
attrition <- filter(hr, Attrition == "Yes")
hr <- clean_names(hr)
attrition <- clean_names(attrition)
#calculate attrition rate
attrition_rate <- nrow(attrition)/ nrow(hr) * 100

#Card metrics
total_attrition <- nrow(attrition)
current_employees <- nrow(hr) - nrow(attrition)

#attrition by department
n_unique(attrition$department)
#number of sales employees by attrition
sum(attrition$department == "Sales")
#total number of employees in sales department
sum(hr$department == "Sales")

#number of R&D employees by attrition
sum(attrition$department == "Research & Development")
#total number of employees in R&D department
sum(hr$department == "Research & Development")

#number of HR employees by attrition
sum(attrition$department == "Human Resources")
#total number of employees in HR department
sum(hr$department == "Human Resources")

#attrition by Job role
n_unique(attrition$job_role)
unique(attrition$job_role)

sum(attrition$job_role == "Sales Executive")
sum(hr$job_role == "Sales Executive")
sum(attrition$job_role == "Sales Representative")
sum(hr$job_role == "Sales Representative")
sum(attrition$job_role == "Research Director")
sum(hr$job_role == "Research Director")
sum(attrition$job_role == "Healthcare Representative")
sum(hr$job_role == "Healthcare Representative")
sum(attrition$job_role == "Manager")
sum(hr$job_role == "Manager")
sum(attrition$job_role == "Laboratory Technician")
sum(hr$job_role == "Laboratory Technician")
sum(attrition$job_role == "Research Scientist")
sum(hr$job_role == "Research Scientist")
sum(attrition$job_role == "Human Resources")
sum(hr$job_role == "Human Resources")
sum(attrition$job_role == "Manufacturing Director")
sum(hr$job_role == "Manufacturing Director")

#attrition by education
n_unique(attrition$education)
unique(attrition$education)

sum(attrition$education == "Associates Degree")
sum(hr$education == "Associates Degree")
sum(attrition$education == "Bachelor's Degree")
sum(hr$education == "Bachelor's Degree")
sum(attrition$education == "Master's Degree")
sum(hr$education == "Master's Degree")
sum(attrition$education == "High School")
sum(hr$education == "High School")
sum(attrition$education == "Doctoral Degree")
sum(hr$education == "Doctoral Degree")

#Group ages in attrition data
att_ages <- cut(attrition$age, breaks = c(18, 25, 35, 45, 55, max(attrition$age)), 
                labels = c("18-25", "26-35", "36-45", "46-55", "55>"))
table(att_ages)

#Group ages in hr data
hr_ages <- cut(hr$age, breaks = c(18, 25, 35, 45, 55, max(hr$age)), 
                labels = c("18-25", "26-35", "36-45", "46-55", "55>"))
table(hr_ages)

#Gender by attrition
sum(attrition$gender == "Male")
sum(hr$gender == "Male")
sum(attrition$gender == "Female")
sum(hr$gender == "Female")

# Convert "attrition_date" column to Date format
attrition$attrition_date <- as.Date(mdy_hms(attrition$attrition_date))

# Extract month and year from "attrition_date" column
attrition <- attrition %>% mutate(month = format(attrition_date, "%m"),
                    year = format(attrition_date, "%Y"))

# Group by year and month and count the number of attrition events in each group
attrition_grouped <- attrition %>% group_by(year, month) %>% summarise(count = n())

#average job satisfaction
mean(hr$job_satisfaction)

#average years at company
mean(hr$years_at_company)

