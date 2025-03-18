library(tidyr)
library(pillar)
library(data.table)
library(ggplot2)


custom_theme <- function() {
    theme_minimal() +
        theme(
            plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
            axis.line = element_line(linetype = "solid", arrow = arrow(length = unit(0.1, "inches"))),
            legend.position = "right"
        )
}




Data <- read.csv("Long Beach Animal Shelter (03.03.2025)/Data.csv")


names(Data)

glimpse(Data)

dim(Data)
head(Data)

RescueData <- data.table(Data)


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

RescueData[, (ColNames) := lapply(.SD, function(x) fifelse(is.na(x), "Unknown", x)), .SDcols = ColNames]

sum(is.na(RescueData))

# Changing date fields to date type
DateCols <- c("intake_date", "outcome_date", "dob")



RescueData[, (DateCols) := lapply(.SD, function(x) as.Date(x)), .SDcols = DateCols]

# Outcome Date has Na's so we are going to create a new column to exclude the entries if needed

RescueData[, unknown_outcome := lapply(outcome_date, function(x) fifelse(is.na(x), TRUE, FALSE))[[1]]]

RescueData[, unknown_dob := lapply(dob, function(x) fifelse(is.na(x), TRUE, FALSE))[[1]]]

sum(is.na(RescueData))

RescueData[unknown_outcome == TRUE, .N]

glimpse(RescueData)

# Intake Count by Month & Year

Occurences <- RescueData[, .(intake_count = .N), by = .(intake_date)][order(-intake_count)]


Occurences[, `:=`(Month = month(intake_date), Year = year(intake_date))]

OccurencesByMonth <- Occurences[, .(Total_Intakes = sum(intake_count)), keyby = .(Year, Month)]
OccurencesByMonthAverage <- OccurencesByMonth[, .(Average_Intake = mean(Total_Intakes)), keyby = .(Month)]
OccurencesByYear <- OccurencesByMonth[, .(Total_Intakes = sum(Total_Intakes)), keyby = .(Year)]


# Calculating Variation of animal with intake counts (stacked bar chart)

AnimalIntakes <- RescueData[, .(intake_count = .N), by = .(animal_type, year(intake_date))]
AnimalIntakesTable <- dcast(AnimalIntakes, 
                            formula = animal_type~year, 
                            value.var = "intake_count", 
                            fill = 0)



TopAnimalIntakes <- AnimalIntakes[order(year,-intake_count), head(.SD,5), by = .(year)]
# Calculating the age of the animal when taken in by centre

AgeAtIntakeData <- RescueData[, age_at_intake := intake_date - dob]

AgeAtIntakeData$age_at_intake

AgeAtIntakeData[, age_at_intake := age_at_intake / (12 * 30)]

AgeAtIntakeData <- AgeAtIntakeData[, age_at_intake := as.numeric(age_at_intake)]

AgeAtIntakeData <- AgeAtIntakeData[!is.na(age_at_intake), ]

AgeAtIntake <- AgeAtIntakeData[, .(age_at_intake = round(age_at_intake, 2)), by = .(animal_type)]
AgeAtIntake <- AgeAtIntake[age_at_intake > 0, ]

# Analyzing the top 5 outcomes per animal

names(RescueData)
head(RescueData$outcome_type)

Outcome <- RescueData[, .(outcome_count = .N), by = .(animal_type, outcome_type)]
TopOutcomes <- Outcome[, .(outcome_count = sum(outcome_count)), by = .(outcome_type)][order(-outcome_count)][1:7]
TopOutcomesByAnimal <- Outcome[, .SD[order(-outcome_count)][1:5], by = .(animal_type), .SDcols = c("outcome_type", "outcome_count")]

# Plots
# Number of Intakes Per Month
PlotOccurencesByMonth <- ggplot(OccurencesByMonth,
                            aes(
                                x = Month,
                                y = Total_Intakes,
                                colour = as.factor(Year))) +
                         scale_x_continuous(breaks = 1:12) +
                            geom_smooth(
                            span = 0.5,
                            se = FALSE) +
                         geom_smooth(
                            data = OccurencesByMonthAverage,
                            aes(x = Month, y = Average_Intake, linetype = "Average Intakes"),
                            span = 0.5,
                            se = FALSE,
                            inherit.aes = FALSE,
                            colour = "black",) +
                         scale_linetype_manual(values = c("Average Intakes" = "dashed"))+
                         scale_y_continuous(expand = c(0,0))+
                         labs(
                            title = "Animal Intakes By Month",
                            x = "Month",
                            y = "Number Of Intakes",
                            colour = "Year",
                            linetype = "") +
                         scale_fill_brewer(name = "Set3")+
                         theme_minimal() +
                         theme(
                            plot.title = element_text(hjust = 0.5,
                                                      face = "bold",
                                                      size = 14),
                            axis.line = element_line(linetype = 1,
                                                     arrow = arrow(
                                                                   length = unit(0.1, "inches")
                                                                   ) 
                                                     )
                            )
                         
                        
PlotOccurencesByMonth
# Number of Intakes Per Year
PlotOccurencesByYear <- ggplot(
                               OccurencesByYear,
                               aes(
                               x = as.factor(Year),
                               y = Total_Intakes,
                               fill = as.factor((Year))
                               )) +
                        geom_col() +
                        geom_text(aes(label = Total_Intakes), 
                                  vjust = -0.5, 
                                  fontface = "bold",
                                  size = 3)+
                        scale_fill_brewer(palette = "Set3")+
                        scale_y_continuous(expand = c(0,0), limits = c(0,5000))+
                        labs(
                            title = "Total Animal Intakes Per Year",
                            x = "Year",
                            y = "Total Intakes")+
                        theme_minimal()+
                        theme(
                              legend.position = "none",
                              plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
                              axis.line = element_line(linetype = 1,
                              arrow = arrow(length = unit(0.1, "inches"),ends = "last" )))

                        
                        
PlotOccurencesByYear

# Top 5 Animal Intakes Per Year
PlotAnimalIntakes <- ggplot(TopAnimalIntakes, aes(x = as.factor(year), y = intake_count, fill = animal_type)) +
                     geom_bar(position = "stack", stat = "identity") +
                     scale_fill_brewer(palette = "Set3") +
                     scale_y_continuous(expand = c(0,0))+
                     labs(
                         title = "Top 5 Animal Intakes per year",
                         x = "Year",
                         y = "Total Intakes",
                         fill = "Animal") +
                     theme_minimal() +
                     theme(plot.title = element_text(hjust = 0.5, face = "bold", size =14),
                           axis.line = element_line(linetype = "solid",
                                                    arrow = arrow(length = unit(0.1,"inches"))
                                                    )
                           )


    
PlotAnimalIntakes

# Age of animal at intake
PlotAgeAtIntake <- ggplot(
  AgeAtIntake,
  aes(x = animal_type, y = age_at_intake, fill = animal_type)) +
  geom_boxplot(outliers = FALSE, outlier.shape = NA) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  scale_y_continuous(breaks = 0:15, expand = c(0,0), limit = c(0,5)) +
  labs(
      title = "Age of animals at intake",
      x = "Animal",
      y = "Age At Intake",
      fill = "Animal") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.line = element_line(linetype = "solid", arrow = arrow(length = unit(0.1,"inches"))))
  
 

PlotAgeAtIntake
# Plot of outcomes
PlotOutcome <- ggplot(
  TopOutcomes,
  aes(x = factor(outcome_type, levels = TopOutcomes$outcome_type), y = outcome_count, fill = outcome_type)) +
  geom_col() +
  scale_y_continuous(expand = c(0,0))+
  scale_fill_brewer(palette = "Set3") +
  labs(
      title = "Outcomes",
      x = "Outcome Type",
      y = "Frequency Of Outcome",
      fill = "Outcome Type")+
  custom_theme()
PlotOutcome
# Plot of Outcomes by Animal
PlotOutcomeByAnimal <- ggplot(
  TopOutcomesByAnimal,
  aes(x = outcome_type, y = outcome_count, fill = animal_type)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_brewer(palette = "Set3") +
  geom_col() +
  labs(
      title = "Outcomes by Animal",
      x = "Outcome Type",
      y = "Frequency Of Outcome",
      fill = "Animal")+
  custom_theme()

PlotOutcomeByAnimal



# help(":=")
