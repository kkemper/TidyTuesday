library(tidyverse)

bird_impacts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-23/bird_impacts.csv")

head(bird_impacts)
summary(bird_impacts)

bird_impacts %>%
  group_by(species) %>%
  summarize(sum(cost_repairs_infl_adj)) %>%
  arrange(desc('sum(cost_repairs_infl_adj)'))
  
          