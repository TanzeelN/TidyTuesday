library(tidyr)
library(pillar)
library(data.table)
Data <- read.csv("Long Beach Animal Shelter (03.03.2025)/Data.csv")


names(Data)

glimpse(Data)

dim(Data)
head(Data)

RescueData <- data.table(Data)


RescueData <- RescueData[,
                         .(
                             animal_id, 
                             animal_type, 
                             sex, 
                             dob, 
                             intake_date,
                             intake_condition,
                             intake_type,
                             outcome_date,
                             outcome_type
                             )]
# Data Cleaning that needs to be done:
# - Date columns need to be formatted (intake_date, outcome_date, dob)
# - remove na if in any field, change to unknown.

#Any NA Values are changed to "Unknown"
ColNames <- names(RescueData)

RescueData[, (ColNames) := lapply(.SD, function(x) fifelse(is.na(x),"Unknown", x)), .SDcols = ColNames]

sum(is.na(RescueData))

#Changing date fields to date type
DateCols <- c("intake_date","outcome_date")



RescueData[,(DateCols) := lapply(.SD,function(x) as.Date(x)), .SDcols = DateCols]

#Outcome Date has Na's so we are going to create a new column to exclude the entries if needed

RescueData[, unknown_outcome := lapply(outcome_date, function(x) fifelse(is.na(x),TRUE,FALSE))]

str(RescueData)
sum(is.na(RescueData))

RescueData[unknown_outcome == TRUE,.N]






