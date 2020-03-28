# Tidy Tuesday Week 13 2020 - 2020-03-24 - Traumatic Brain Injury
library(tidyverse)
library(rvest)
library(scales)
library(tidytext)
library(datasets)

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
  mutate(injury_mechanism = factor(injury_mechanism)) %>%
  group_by(injury_mechanism) %>%
  summarize(deaths = sum(number_est, na.rm = TRUE)) %>%
  mutate(prop = round(deaths/sum(deaths) * 100))

barplot <- data %>%
  ggplot(aes(x = "", y = prop, fill = injury_mechanism))+
  geom_bar(width = 1, stat = "identity", color = "white") +
  labs(x = "", y = "Proportion of Deaths", title = "Proportion of causes due to traumatic brain injury", subtitle = ("2006 - 2014")) + 
  guides(fill = guide_legend(title = "Injury Mechanism"))
barplot
