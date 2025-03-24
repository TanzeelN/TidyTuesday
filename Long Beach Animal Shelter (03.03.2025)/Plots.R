library(tidyr)
library(pillar)
library(data.table)
library(ggplot2)


#Custom Theme simplify code
custom_theme <- function() {
    theme_minimal() +
        theme(
            plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
            axis.line = element_line(linetype = "solid", arrow = arrow(length = unit(0.1, "inches"))),
            legend.position = "right"
        )
}

# Number of Intakes Per Month
PlotOccurencesByMonth <- ggplot(
    OccurencesByMonth,
    aes(
        x = Month,
        y = Total_Intakes,
        colour = as.factor(Year)
    )
) +
    scale_x_continuous(breaks = 1:12) +
    geom_smooth(
        span = 0.5,
        se = FALSE
    ) +
    geom_smooth(
        data = OccurencesByMonthAverage,
        aes(x = Month, y = Average_Intake, linetype = "Average Intakes"),
        span = 0.5,
        se = FALSE,
        inherit.aes = FALSE,
        colour = "black",
    ) +
    scale_linetype_manual(values = c("Average Intakes" = "dashed")) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(
        title = "Animal Intakes By Month",
        x = "Month",
        y = "Number Of Intakes",
        colour = "Year",
        linetype = ""
    ) +
    scale_fill_brewer(name = "Set3") +
    theme_minimal() +
    custom_theme()


# Number of Intakes Per Year
PlotOccurencesByYear <- ggplot(
    OccurencesByYear,
    aes(
        x = as.factor(Year),
        y = Total_Intakes,
        fill = as.factor((Year))
    )
) +
    geom_col() +
    geom_text(aes(label = Total_Intakes),
              vjust = -0.5,
              fontface = "bold",
              size = 3
    ) +
    scale_fill_brewer(palette = "Set3") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 5000)) +
    labs(
        title = "Total Animal Intakes Per Year",
        x = "Year",
        y = "Total Intakes",
        fill = "Year"
    ) +
    theme_minimal() +
    custom_theme()

# Top 5 Animal Intakes Per Year
PlotAnimalIntakes <- ggplot(TopAnimalIntakes, aes(x = as.factor(year), y = intake_count, fill = animal_type)) +
    geom_bar(position = "stack", stat = "identity") +
    scale_fill_brewer(palette = "Set3") +
    scale_y_continuous(expand = c(0, 0)) +
    labs(
        title = "Top 5 Animal Intakes per year",
        x = "Year",
        y = "Total Intakes",
        fill = "Animal"
    ) +
    theme_minimal() +
    custom_theme()


# Age of animal at intake
PlotAgeAtIntake <- ggplot(
    AgeAtIntake,
    aes(x = animal_type, y = age_at_intake, fill = animal_type)
) +
    geom_boxplot(outliers = FALSE, outlier.shape = NA) +
    scale_fill_brewer(palette = "Set3") +
    theme_minimal() +
    scale_y_continuous(breaks = 0:15, expand = c(0, 0), limit = c(0, 5)) +
    labs(
        title = "Age of animals at intake",
        x = "Animal",
        y = "Age At Intake",
        fill = "Animal"
    ) +
    custom_theme()+
    theme(legend.position = "none")

# Plot of outcomes
PlotOutcome <- ggplot(
    TopOutcomes,
    aes(x = factor(outcome_type, levels = TopOutcomes$outcome_type), y = outcome_count, fill = outcome_type)
) +
    geom_col() +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_brewer(palette = "Set3") +
    labs(
        title = "Outcomes",
        x = "Outcome Type",
        y = "Frequency Of Outcome",
        fill = "Outcome Type"
    ) +
    custom_theme()

# Plot of Outcomes by Animal
PlotOutcomeByAnimal <- ggplot(
    TopOutcomesByAnimal,
    aes(x = outcome_type, y = outcome_count, fill = animal_type)
) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_brewer(palette = "Set3") +
    geom_col() +
    labs(
        title = "Outcomes by Animal",
        x = "Outcome Type",
        y = "Frequency Of Outcome",
        fill = "Animal"
    ) +
    custom_theme()

