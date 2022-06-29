getwd()
setwd("F:/Work/new")

library(readxl)
library(tidyverse)
library(openxlsx)
library(DataExplorer)
library(SmartEDA)
library(dlookr)

##########################################################################

path_to_excel_file <- "F:/Work/new/countycomplete.xlsx"

# Get sheet names of excel file
sheets <- excel_sheets(path_to_excel_file)

# Iterate through sheet names 4 through 7 reading the data
# Iteratively join using left_join() + reduce()
marketing_campaign_tbl <- sheets[4:7] %>%
  map(~ read_excel(path = path_to_excel_file, sheet = .)) %>%
  reduce(left_join, by = "ID")

# Resulting output in "glimpse" format. 
marketing_campaign_tbl %>% glimpse()


#############################################################################

data <- read.xlsx(file.choose())
data1 <- read.csv(file.choose())
head(data1)
summary(data1)

my_data <- read_excel("F:/Work/new/countycomplete.xlsx", header = TRUE)
head(my_data)
data <- read.xlsx("F:/Work/new/countycomplete.xlsx", 1, header = TRUE)

summary(data)


#state
state <- unique(data$Sum.of.fed_spending)
state
summary(state)

#travel time 
travel <- data1$mean_work_travel
travel
summary(travel)
mode(travel)
sd(travel)
var(travel)
z_travel = (travel - mean(travel))/sd(travel)
z_travel

# home ownership
home_own <- data1$home_ownership
home_own
summary(home_own)
mode(home_own)
sd(home_own)
var(home_own)
z_home_own = (pphouse - mean(home_own))/sd(home_own)
z_home_own


# person per household
pphouse <- data1$persons_per_household
pphouse
summary(pphouse)
mode(pphouse)
sd(pphouse)
var(pphouse)
z_pphouse = (pphouse - mean(pphouse))/sd(pphouse)
z_pphouse
data1$state
###############################################################
plot(data1$state, data1$mean_work_travel)




########################################
plot(data$X3)
plot(data$X5)
plot(data$X6)
plot(data$X7)
plot(data$X8)
plot(data$X9)



summary(data$X3)

create_report(data)
