library(tidyverse)
library(data.table)
library(here)
library(usmap)
library(ggplot2)
library(sf)
library(ggrepel)

ConvertHours <- function(DecHours){
    Hours <- floor(DecHours)
    Min <- round((DecHours - Hours) * 60)
    HhMm <- sprintf("%02d:%02d", as.integer(Hours), as.integer(Min))
    return(HhMm)
}



MapData <- usmap_transform(us_map())

Data <- fread(here("US Emergency Room (08.04.2025)","US_Care_Data.csv"))
StatePopulation <- fread(here("US Emergency Room (08.04.2025)", "StatePopulations.csv"))


glimpse(Data)

table(Data$measure_name)

#finding the ID for average time spent at hospital ()
unique(Data[,.(measure_id,measure_name)])


EmergencyTimes <- Data[measure_id == "OP_18b", .(state,measure_name,score)]

state.name
state.abb

States <- data.table(Name = state.name, Abb = state.abb) 

#Checking States names are the same case
#There are errors but we are going to go forward to do the plot.
StatePopulation[!NAME %in% state.name,]

EmergencyTimes[!state %in% state.abb,state]


StatePopulation <- merge(StatePopulation,States, all.x = TRUE,by.x = "NAME", by.y = "Name")
EmergencyTimes <- merge(EmergencyTimes, States, all.x = TRUE, by.x = "state", by.y = "Abb")

StateTimes <- merge(EmergencyTimes, StatePopulation[,.(Abb,POPESTIMATE2024)], all.x = TRUE,by.x = "state", by.y = "Abb")


StateTimes <- StateTimes[!is.na(POPESTIMATE2024),.(State = Name,Abb = state, Hours = round(score/60,2),Population = POPESTIMATE2024)]




SFStateTimes <- merge(MapData, StateTimes, by.x = "abbr", by.y = "Abb")

Central_Coords <- as.data.table(SFStateTimes)

Central_Coords[,Coords := st_centroid(geom)]

Central_Coords[,c("x","y") := as.data.table(st_coordinates(Coords))]
Central_Coords[,c("OrigX","OrigY") := .(x,y)]


CountriesSet1 <- c("New Hampshire", 
                    "Massachusetts", 
                    "Rhode Island", 
                    "Connecticut", 
                    "New Jersey", 
                    "Delaware", 
                    "Maryland",
                    "West Virginia",
                    "Virginia")

CountriesSet2 <- c("Florida", 
                   "Michigan")

CountriesSet3 <- c("Vermont")

Central_Coords[, Labelled := FALSE]
Central_Coords[full %in% CountriesSet1, Labelled := TRUE]


Central_Coords[full %in% CountriesSet1, x := x + 400000]
Central_Coords[full %in% CountriesSet2, x := x + 50000]
Central_Coords[full %in% CountriesSet3,`:=`(y = y + 200000, x = x - 50000)]
Central_Coords[full == "West Virginia", x := x + 250000]
Central_Coords[full == "Maryland"|full == "Connecticut", y := y - 20000]


##Calculating Normalized Population Data--------

#Calculating Mean


Central_Coords[,Scaled_Pop := 1 + scale(Population)]

Central_Coords[, interaction := 1 + scale(Scaled_Pop * Hours)]


Central_Coords[, Adjusted_Hours := Hours * interaction]

asd <- Central_Coords[,.(Hours,Adjusted_Hours,full,Population,Scaled_Pop,interaction)]

Central_Coords[,HoursPop := Hours * Population]

TotalPop <- sum(Central_Coords[,Population])
TotalHoursPop <- sum(Central_Coords[,HoursPop])
WeightedAverage <- TotalHoursPop/TotalPop

ScalingFactor <- mean(Central_Coords[,Hours])/WeightedAverage

Central_Coords[,Weighted_Hours := ScalingFactor * Hours]

Central_Coords[,c("HoursConverted", "WeightedHoursConverted") := lapply(.sd, ConvertHours), .SDcols = c("Hours", "Weighted_Hours")]

#Central_Coords[,c("Hours", "Weighted_Hours") := lapply(.SD,ConvertHours), .SDcols = c("Hours", "Weighted_Hours")]




StateTimesMap <- st_as_sf(Central_Coords)










ggplot(StateTimesMap) +
    geom_sf(
        aes(fill = Hours)) +
    scale_fill_gradient(
        low = "mistyrose",
        high = "darkred")+
    geom_text(data = filter(StateTimesMap,Labelled == FALSE),
        aes(label = full, x = x, y = y))+
    geom_text(data = filter(StateTimesMap,Labelled == TRUE),
              aes(label = full, x = x, y = y))+
    geom_segment(data = filter(StateTimesMap,Labelled == TRUE),
                aes(x = x-200000, y = y, xend = OrigX, yend = OrigY))+
    geom_segment(data = filter(StateTimesMap, full == "Vermont"),
                 aes(x = x , y =y-30000 , xend = OrigX, yend = OrigY))+
    theme_void()


ggplot(StateTimesMap) +
    geom_sf(
        aes(fill = Weighted_Hours)) +
    scale_fill_gradient(
        low = "mistyrose",
        high = "darkred")+
    geom_text(data = filter(StateTimesMap,Labelled == FALSE),
              aes(label = full, x = x, y = y))+
    geom_text(data = filter(StateTimesMap,Labelled == TRUE),
              aes(label = full, x = x, y = y))+
    geom_segment(data = filter(StateTimesMap,Labelled == TRUE),
                 aes(x = x-200000, y = y, xend = OrigX, yend = OrigY))+
    geom_segment(data = filter(StateTimesMap, full == "Vermont"),
                 aes(x = x , y =y-30000 , xend = OrigX, yend = OrigY))+
    theme_void()


ggplot(StateTimesMap) +
    geom_sf(
        aes(fill = Population)) +
    scale_fill_gradient(
        low = "lightgreen",
        high = "forestgreen")+
    geom_text(data = filter(StateTimesMap,Labelled == FALSE),
              aes(label = full, x = x, y = y))+
    geom_text(data = filter(StateTimesMap,Labelled == TRUE),
              aes(label = full, x = x, y = y))+
    geom_segment(data = filter(StateTimesMap,Labelled == TRUE),
                 aes(x = x-200000, y = y, xend = OrigX, yend = OrigY))+
    geom_segment(data = filter(StateTimesMap, full == "Vermont"),
                 aes(x = x , y =y-30000 , xend = OrigX, yend = OrigY))+
    theme_void()





