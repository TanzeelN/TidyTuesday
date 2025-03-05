tuesdata <- tidytuesdayR::tt_load('2025-03-04')
Data <- tuesdata[[1]]

write.csv(Data,"Long Beach Animal Shelter (03.03.2025)/Data.csv")
