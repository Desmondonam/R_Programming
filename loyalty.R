getwd()

# Load the libraries
library(tidyverse)
library(modelr)
library(lessR)
library(psych)
library(caret)
library(leaps)
library(olsrr)


# descriptive analysis, building charts using demographic variables

data <- read.csv("Data Loyalty.csv")
head(data)
# measurement of scale
count(data, gender)
count(data, branch)
count(data, profess)
count(data, educatio)
count(data, loy1_1)
count(data, loy2_1)
count(data, loy3_1)
count(data, loy4_1 )
count(data, loy5_1 )
count(data, loy6_1 )
count(data, val1_1)
count(data, val2_1 )
count(data, val3_1 )
count(data, sat1_1 )
count(data, sat2_1 )
count(data, sat3_1 )
count(data, sav_1   )
count(data, loans_1 )
count(data, long_1    )
count(data, age_1 )
count(data, income_1       )
count(data, CLV)


BarChart(branch , data = data)
BarChart(profess , data = data)
BarChart(educatio , data = data)
BarChart(loy1_1 , data = data)
BarChart(loy2_1 , data = data)
BarChart(loy3_1   , data = data)
BarChart(loy4_1 , data = data)
BarChart(loy5_1 , data = data)
BarChart(loy6_1  , data = data)
BarChart(val1_1 , data = data)
BarChart(val2_1  , data = data)
BarChart(val3_1  , data = data)
BarChart(sat1_1  , data = data)
BarChart(sat2_1  , data = data)
BarChart(sat3_1  , data = data)
BarChart(sav_1    , data = data)
BarChart(loans_1  , data = data)
BarChart(income_1         , data = data)
BarChart(age_1    , data = data)


range(data$age_1)
summary(data$age_1)

range(data$sav_1)
summary(data$sav_1)

range(data$loans_1)
summary(data$loans_1)

range(data$CLV)
summary(data$CLV)

PieChart(gender, data = data)

describe(data)

#summary of the whole data
summary(data)

data[data$gender=="0"] <- "2" 
head(data)

# analysis

model <- CLV~loans_1  + income_1        + loy6_1 + age_1 +  long_1    
fit <- lm(model, data)
fit
summary(fit)



