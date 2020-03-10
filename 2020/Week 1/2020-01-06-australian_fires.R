library(tidyverse)

rainfall <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/rainfall.csv")
temperature <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv")
head(rainfall)
head(temperature)

rainfall %>%
  group_by(city_name, year) %>%
  summarize(total_rain = sum(rainfall, na.rm = TRUE)) %>%
  filter(city_name != "Canberra", year > 1970) %>%
  ggplot(aes(year, total_rain, fill = city_name)) +
  geom_col() +
  labs(
    x = "Year",
    y = "Total Rain",
    title = "Australian rain by city since 1970"
  )

sydney_rainfall
# Convert rainfall$city_name to upper

# join on city name

# group by city, year

# summarize sum of rainfall
