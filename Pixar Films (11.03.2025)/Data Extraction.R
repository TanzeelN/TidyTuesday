tuesdata <- tidytuesdayR::tt_load('2025-03-11')

PixarFilms <- tuesdata[[1]]
PublicResponse <- tuesdata[[2]]


write.csv(PixarFilms, "Pixar Films (11.03.2025)/PixarFilms.csv")
write.csv(PublicResponse, "Pixar Films (11.03.2025)/PublicResponse.csv")
