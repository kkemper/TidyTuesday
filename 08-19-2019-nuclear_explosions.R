library(tidyverse)
nuclear_explosions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")

head(nuclear_explosions)
summary(nuclear_explosions)

#By Magnitude Body
nuclear_explosions %>%
  ggplot(aes(longitude, latitude, color = country, size = magnitude_body)) +
  borders("world") + 
  geom_point() +
  coord_quickmap()

# By Magnitude Surface
nuclear_explosions %>%
  ggplot(aes(longitude, latitude, color = country, size = magnitude_surface)) +
  borders("world") + 
  geom_point() +
  coord_quickmap()
