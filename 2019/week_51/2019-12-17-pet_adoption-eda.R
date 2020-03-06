library(tidyverse)
library(ggthemes)
library(zipcode)
library(maps)
library(albersusa)
library(viridis)
library(rsconnect)

if(!exists("dog_decriptions")){
  dog_descriptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_descriptions.csv')
}

# Convert to factors
dog_descriptions$age <- factor(dog_descriptions$age, levels = c("Baby", "Young", "Adult", "Senior"))
dog_descriptions$sex <- factor(dog_descriptions$sex, levels = c("Female", "Male", "Unknown"))

# Filter for US and gender not Unknown
us_dogs <- dog_descriptions %>%
  filter(contact_country == "US", sex != "Unknown")

# Filter for Dogs for breed, age, sex, state
selected_dogs <- us_dogs %>%
  filter(sex == "Male", age == "Adult", breed_primary == "Pit Bull Terrier", contact_state == "TX" )

# Map zipcode to long and lat
selected_dogs$contact_zip <- clean.zipcodes(selected_dogs$contact_zip)
st <- selected_dogs %>% select(id, sex, age, breed_primary, contact_state, contact_zip) %>%
  mutate(state_name = tolower(state.name[match(contact_state, state.abb)]))
data(zipcode)
dogs_zip <- aggregate(data.frame(count = selected_dogs$id), list(zip = selected_dogs$contact_zip), length)
dogs_zip <- merge(dogs_zip, zipcode, by = 'zip')

# Plot to map

state <- map_data("state", region = st$state_name)

ggplot(dogs_zip, aes(longitude, latitude)) +
geom_polygon(data = state, aes(x = long, y = lat, group = group), color =  'gray', fill = NA, alpha = 0.35) +
  geom_point(aes(color = count), size = 4) +
  coord_fixed() +
  xlim(min(state$long), max(state$long)) + ylim(min(state$lat), max(state$lat)) +
  labs(title = "Available Dogs")
