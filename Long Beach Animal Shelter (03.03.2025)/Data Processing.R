library(tidyr)
library(pillar)
library(data.table)
library(ggplot2)
library(here)


RescueData <- data.table(Data)

#Extract columns of interest
RescueData <- RescueData[
    ,
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
    )
]

ColNames <- names(RescueData)

#Changing NA's to Unknown
RescueData[, (ColNames) := lapply(.SD, function(x) fifelse(is.na(x), "Unknown", x)), .SDcols = ColNames]


# Changing date fields to date type
DateCols <- c("intake_date", "outcome_date", "dob")



RescueData[, (DateCols) := lapply(.SD, function(x) as.Date(x)), .SDcols = DateCols]

# Outcome Date has Na's so we are going to create a new column to exclude the entries if needed

RescueData[, unknown_outcome := lapply(outcome_date, function(x) fifelse(is.na(x), TRUE, FALSE))[[1]]]

RescueData[, unknown_dob := lapply(dob, function(x) fifelse(is.na(x), TRUE, FALSE))[[1]]]


RescueData[unknown_outcome == TRUE, .N]


# Intake Count by Month & Year

Occurences <- RescueData[, .(intake_count = .N), by = .(intake_date)][order(-intake_count)]


Occurences[, `:=`(Month = month(intake_date), Year = year(intake_date))]

OccurencesByMonth <- Occurences[, .(Total_Intakes = sum(intake_count)), keyby = .(Year, Month)]
OccurencesByMonthAverage <- OccurencesByMonth[, .(Average_Intake = mean(Total_Intakes)), keyby = .(Month)]
OccurencesByYear <- OccurencesByMonth[, .(Total_Intakes = sum(Total_Intakes)), keyby = .(Year)]


# Calculating Variation of animal with intake counts (stacked bar chart)

AnimalIntakes <- RescueData[, .(intake_count = .N), by = .(animal_type, year(intake_date))]
## Table of animal intakes by year and animal
AnimalIntakesTable <- dcast(AnimalIntakes,
                            formula = animal_type ~ year,
                            value.var = "intake_count",
                            fill = 0
)

TopAnimalIntakes <- AnimalIntakes[order(year, -intake_count), head(.SD, 5), by = .(year)]
# Calculating the age of the animal when taken in by shelter

AgeAtIntakeData <- RescueData[, age_at_intake := intake_date - dob]

AgeAtIntakeData$age_at_intake

AgeAtIntakeData[, age_at_intake := age_at_intake / (12 * 30)]

AgeAtIntakeData <- AgeAtIntakeData[, age_at_intake := as.numeric(age_at_intake)]

AgeAtIntakeData <- AgeAtIntakeData[!is.na(age_at_intake), ]

AgeAtIntake <- AgeAtIntakeData[, .(age_at_intake = round(age_at_intake, 2)), by = .(animal_type)]
AgeAtIntake <- AgeAtIntake[age_at_intake > 0, ]
AgeAtIntake <- AgeAtIntake[animal_type %in% unique(TopAnimalIntakes$animal_type),]

# Analyzing the top 5 outcomes per animal


Outcome <- RescueData[, .(outcome_count = .N), by = .(animal_type, outcome_type)]
TopOutcomes <- Outcome[, .(outcome_count = sum(outcome_count)), by = .(outcome_type)][order(-outcome_count)][1:7]
TopOutcomesByAnimal <- Outcome[
    animal_type %in% unique(TopAnimalIntakes$animal_type)& outcome_type %in% unique(TopOutcomes$outcome_type),
    ]
