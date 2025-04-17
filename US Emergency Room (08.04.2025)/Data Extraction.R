tuesdata <- tidytuesdayR::tt_load('2025-04-08')


Data <- tuesdata[[1]]

fwrite(Data,"US Emergency Room (08.04.2025)/US_Care_Data.csv")

