# Tidy Tuesday Week 13 2020 - 2020-03-24 - Traumatic Brain Injury
library(tidyverse)
library(rvest)
library(scales)

# Read data
tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')
tbi_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_year.csv')
tbi_military <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_military.csv')

# Plot deaths by age
tbi_age %>%
  filter(type == "Deaths") %>%
  filter(age_group != "0-17" & age_group != "Total") %>%
  mutate(age_group = factor(age_group, levels = c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+"))) %>%
  group_by(age_group) %>%
  summarize(deaths = sum(number_est, na.rm = TRUE)) %>%
  ggplot(aes(age_group, deaths, fill = age_group)) +
  geom_histogram(stat = "identity") +
  labs(title = "Deaths due to traumatic brain injury in 2014 by age group",
       caption = "Data from https://www.cdc.gov/")

# Proportion of deaths by intentional self-harm
data <- tbi_age %>%
  filter(type == "Deaths") %>%
  group_by(injury_mechanism) %>%
  summarize(deaths = sum(number_est, na.rm = TRUE)) %>%
  mutate(prop = (deaths/sum(deaths) * 100))%>%
  arrange(desc(deaths)) %>%
  mutate(ypos = cumsum(prop) - 0.5 * prop)

barplot <- data %>%
  ggplot(aes(x = "", y = prop, fill = injury_mechanism)) + 
  geom_bar(width = 1, stat = "identity", color = "white")

pie <- barplot + coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(y = ypos, label = prop), size = 6, color = "white") +
  labs(title = "Causes of Traumatic Head Injury", subtitle = "2006 - 2014") +
  scale_fill_manual(values = mycols)
pie

