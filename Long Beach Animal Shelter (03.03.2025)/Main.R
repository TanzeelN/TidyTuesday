library(tidyr)
library(pillar)
library(data.table)
library(ggplot2)
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
DateCols <- c("intake_date","outcome_date", "dob")



RescueData[,(DateCols) := lapply(.SD,function(x) as.Date(x)), .SDcols = DateCols]

#Outcome Date has Na's so we are going to create a new column to exclude the entries if needed

RescueData[, unknown_outcome := lapply(outcome_date, function(x) fifelse(is.na(x),TRUE,FALSE))[[1]]]

RescueData[, unknown_dob := lapply(dob, function(x) fifelse(is.na(x),TRUE,FALSE))[[1]]]

sum(is.na(RescueData))

RescueData[unknown_outcome == TRUE,.N]

glimpse(RescueData)


#Data Analysis
#Initial Summarising & understanding the data includes:
#Unique ID Count1 inconclusive
#Variation of dog breeds
#Age at intake
#Diff beween time intake/outcome
# Outcomes 
# intakes


#Intake Count by Month & Year

Occurences <- RescueData[, .(intake_count = .N) , by = .(intake_date)][order(-intake_count)]


Occurences[,`:=`(Month = month(intake_date), Year = year(intake_date))]

OccurencesByMonth <- Occurences[,Total_Intakes := sum(intake_count), keyby = .(Year,Month)]

OccurencesByYear <- OccurencesByMonth[,Total_Intakes := sum(Total_Intakes), keyby = .(Year)]

PlotOccurencesByMonth <- ggplot(OccurencesByMonth, 
                           aes(
                               x = Month,
                               y = intake_count,
                               colour = as.factor(Year))) +
    scale_x_continuous(breaks = 1:12)+
    geom_smooth(span = 0.5, se = FALSE)
    

PlotOccurencesByYear <- ggplot(OccurencesByYear, 
                            aes(
                                x = as.factor(Year),
                                y = intake_count)
                            )+
                    geom_col()
                            
                            

#Plot Varitaion of animal with intake counts (stacked bar chart)

AnimalIntakes <- RescueData[,.(intake_count = .N), by = .(animal_type, year(intake_date))]

PlotAnimalIntakes <- ggplot(AnimalIntakes, aes(x = year, y = intake_count, fill = animal_type))+
    geom_bar(position = "stack", stat = "identity")

print(PlotAnimalIntakes)



#Age at intake plot

AgeAtIntakeData <- RescueData[,age_at_intake := intake_date - dob]

AgeAtIntakeData$age_at_intake

AgeAtIntakeData[,age_at_intake := age_at_intake/(12*30)]

AgeAtIntakeData <- AgeAtIntakeData[,age_at_intake:= as.numeric(age_at_intake)]

AgeAtIntakeData <- AgeAtIntakeData[!is.na(age_at_intake),]


AgeAtIntake <- AgeAtIntakeData[,round(age_at_intake,2), by =.(animal_type)]

#Do a plot varying for each animal box plot or a summary box plot.





print(OccurencesByMonth)
print(OccurencesByYear)
help(":=")



