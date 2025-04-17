library(data.table)
library(tidyverse)
library(here)
library(ggplot2)
library(ggtext)
library(png)
library(ggthemes)
library(magick)
library(ggimage)
library(glue)
library(ggrepel)

PixarFilms <- fread(here("Pixar Films (11.03.2025)","PixarFilms.csv"))
PublicResponse <- fread(here("Pixar Films (11.03.2025)","PublicResponse.csv"))



Cars <- here("Pixar Films (11.03.2025)", "Cars.png")
FindingNemo <- here("Pixar Films (11.03.2025)", "FindingNemo.png")
MonstersInc <- here("Pixar Films (11.03.2025)", "MonstersInc.png")
ToyStory <- here("Pixar Films (11.03.2025)", "ToyStory.png")
Incredibles <- here("Pixar Films (11.03.2025)", "Incredibles.png")



glimpse(PixarFilms)
glimpse(PublicResponse)
sum(is.na(PublicResponse$critics_choice))


PixarData <- merge(PixarFilms,PublicResponse, by = "film" )

#Creating a mapping list for movies with more than a single movie

MappingSequels <- data.table(
                           series = c("Cars",
                             "Finding Nemo",
                             "Incredibles",
                             "Monsters, Inc.",
                             "Toy Story"),
                           number = list(
                             c(7, 12, 18),
                             c(17,5),
                             c(20,6),
                             c(14,4),
                             c(1,3,11,21)
                             )
                           )


MappingSequels <- MappingSequels[,  unlist(number), by = .(series)]
setnames(MappingSequels, "V1", "number")

#Merging the data with the original dataset & picking all the relevant columns
PixarSeriesData <- merge(PixarData, MappingSequels, by = "number", all.x = TRUE)


#Excluding Toy Story (missing Critics Choice)
SeriesDataAll <- PixarSeriesData[!is.na(series),
                              .(series,
                                film,
                                rotten_tomatoes,
                                metacritic,
                                critics_choice)]

SeriesData <- SeriesDataAll[series != "Toy Story" ,
                            round((rotten_tomatoes + metacritic + critics_choice)/3,1),
                            by = .(series,film)]
setnames(SeriesData, "V1", "average_rating")

#Calculating Toy Story Average

ToyStory1 <- SeriesDataAll[film == "Toy Story",
                           average_rating := (rotten_tomatoes + metacritic)/2,]

ToyStory1 <- ToyStory1[film == "Toy Story",.(series, film, average_rating)]

ToyStoryRemaining <- SeriesDataAll[series == "Toy Story" & film != "Toy Story",
                                   average_rating := (rotten_tomatoes + metacritic + critics_choice)/3]

ToyStoryRemaining <- ToyStoryRemaining[series == "Toy Story" & film != "Toy Story",.(series,film,average_rating)]


#Append it onto Series Data
SeriesDataAgg <- rbind(SeriesData, ToyStory1, ToyStoryRemaining)

SeriesDataAgg[,average_rating := round(average_rating,0)]




#labels = data.table(series = c("Cars", "Toy Story", "Monsters, Inc.","Incredibles","Finding Nemo"),
                               #image = c(Cars,ToyStory,MonstersInc,Incredibles,FindingNemo))



imagetribble <- tribble(
    ~series, ~ImageTag,
    "Cars", glue("<img src='{Cars}' width='52' height='35'/>"),
    "Finding Nemo", glue("<img src='{FindingNemo}' width='82' height='35'/>"),
    "Incredibles", glue("<img src='{Incredibles}' width='68' height='35'/>"),
    "Toy Story", glue("<img src='{ToyStory}' width='47' height='35'/>"),
    "Monsters, Inc.", glue("<img src='{MonstersInc}' width='45' height='35'/>")
)

imagetable <- as.data.table(imagetribble)

SeriesDataAgg <- merge(SeriesDataAgg,imagetable, by = "series", all.x = TRUE)

SeriesDataAgg[,vjust := 0.5]
SeriesDataAgg[,hjust := -0.15]
SeriesDataAgg[, film := trim(film)]
# SeriesDataAgg[film == "Toy Story", hjust := 0.2]
SeriesDataAgg[film == "Toy Story 3",`:=`(vjust = 1.3, hjust = 1.15)]
SeriesDataAgg[film == "Toy Story 2",`:=`(vjust = -0.2, hjust = 1.15)]
SeriesDataAgg[series == 'Cars', hjust := -0.3]

SeriesBoundary <- SeriesDataAgg[, .(ymin = min(average_rating), ymax = max(average_rating)), by = .(series)]
SeriesDataAgg <- merge(SeriesDataAgg,SeriesBoundary, by = "series", all.x = TRUE)
SeriesDataAgg[, series_numeric := as.numeric(factor(series, levels = unique(series)))]

#SeriesDataAgg[, Label := fifelse(average_rating == ymin | average_rating == ymax, film, "")]




library(ggforce)



#Plot
PlotSeries <- ggplot(SeriesDataAgg,
                     aes(x = series_numeric, y = average_rating, colour = film)) +
              geom_rect(aes(xmin = series_numeric - 0.02,
                  xmax = series_numeric + 0.02,
                  ymin = ymin - 0.23 ,
                  ymax = ymax + 0.23),
                  fill = "#FFB000", alpha = 0.3, color = NA) +
              geom_point(
                  size = 2,
                  colour = "#FFB000",
                  shape = 21,
                  stroke = 1.5, 
                  fill = "#1F5673") +
              
              geom_text(
                  aes(label = film, vjust = vjust, hjust = hjust),
                  colour = "#FFB000", size = 4) +
              scale_x_continuous(
                  breaks = unique(SeriesDataAgg$series_numeric),
                  labels = unique(SeriesDataAgg$ImageTag),
                  limits = c(0.9, 5.2)) +
              scale_y_continuous(
                  limits = c(50,100.01),
                  expand = c(0, 0),)+
    
              labs(
                  x = NULL,
                  y = "Average Critic Rating",
                  title = "Highest & lowest rated Pixar film in each series")+
              theme(
                  #Plot Related Formatting
                  plot.background = element_rect(fill = "#1F5673"),
                  #plot.margin = margin(30,30,30,30),
                  plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
                  #Panel Related Formatting
                  panel.background = element_rect(fill = "#1F5673"),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(colour = "#C7A9E0",linetype = 8, linewidth = 0.1),
                  panel.grid.minor = element_blank(),
                  #Axis Related Formatting
                  axis.text.x = element_markdown(),
                  axis.text.y = element_text(colour = "#FFB000",size = 10),
                  axis.ticks.y = element_blank(),
                  axis.line.y = element_line(colour = "#FFB000", linewidth = 1),
                  axis.ticks.x = element_line(colour = "#FFB000", linewidth = 1),
                  axis.ticks.length = unit(1, "mm"),
                  #Additional Formatting
                  text = element_text(colour = "#FFB000"))






PlotSeries


# Save the plot with a smaller size
ggsave(here("Pixar_Plot.png"), PlotSeries, width = 10, height = 6, dpi = 300)






# library(ggforce)
# PlotSeries <- 
#     ggplot(SeriesDataAgg, aes(x = series_numeric, y = average_rating, fill = series)) +
#     geom_ellipse(aes(x0 = series_numeric, y0 = (ymin + ymax) / 2, a = 0.4, b = (ymax - ymin) / 2, angle = 0), alpha = 0.3) + 
#     geom_point(size = 3, colour = "black", shape = 1, stroke = 1.5) +
#     geom_text(aes(label = film, vjust = vjust), colour = "black") +
#     coord_flip() +
#     scale_x_continuous(breaks = unique(SeriesDataAgg$series_numeric), labels = unique(SeriesDataAgg$ImageTag)) +  
#     theme(
#         legend.position = "none",
#         axis.text.y = element_markdown(),
#         plot.background = element_rect(fill = "#BAD7D4"),
#         panel.background = element_rect(fill = "#BAD7D4"),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank()
#     )
# 




# library(png)
# library(ggtext)
# library(tidyverse)
# lincoln <- readPNG("test.png") # replace with whatever
# labels <- c(virginica = "<img src='test.png' width='100' /><br>*virginica*") # replace with whatever
# 
# 
# df <- iris %>% 
#     filter(Species == "virginica")
# 
# ggplot(df, aes(Species, Sepal.Length)) +
#     geom_col() +
#     scale_x_discrete(labels = labels) +
#     theme(axis.text.x = ggtext::element_markdown())

