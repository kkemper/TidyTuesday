library(tidyverse)

video_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv")

summary(video_games)
head(video_games)

video_games %>%
  arrange(number)

video_games %>%
  filter(is.na(metascore) != TRUE) %>%
  group_by(publisher) %>%

  summarize(mean(metascore)) %>%
  

video_games %>%
  separate(release_date, c("date", "year"), sep = ",") %>%
  group_by(year) %>%
  summarize(publisher)

View(video_games)
