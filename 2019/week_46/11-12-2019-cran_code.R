library(tidyverse)

cran_code <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-12/loc_cran_packages.csv")

head(cran_code)
summary(cran_code)

cran_code %>%
  group_by(language) %>%
  summarize()
