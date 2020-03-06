library(tidyverse)

hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')

head(hotels)
summary(hotels)
glimpse(hotels)
str(hotels)
hotels %>%
  filter(reservation_status == "Check-In")
