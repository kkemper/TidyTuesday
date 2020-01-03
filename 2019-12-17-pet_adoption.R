## 2019-12-17 - Adoptable Dogs

library(tidyverse)
library(ggthemes)

dog_moves <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_moves.csv')
dog_travel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_travel.csv')
dog_descriptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_descriptions.csv')

head(dog_travel)
head(dog_moves)
head(dog_descriptions)

view(dog_moves)

# Convert to factors
dog_descriptions$age <- factor(dog_descriptions$age, levels = c("Baby", "Young", "Adult", "Senior"))
dog_descriptions$sex <- factor(dog_descriptions$sex, levels = c("Female", "Male", "Unknown"))

# U.S. Dog Adoptions by Age
us_dogs <- dog_descriptions %>%
  filter(contact_country == "US")

us_dog_count <- dog_descriptions %>%
  count(breed_primary) %>%
  filter(n > 500)

us_dogs <-  us_dogs %>%
  inner_join(us_dog_count, by = "breed_primary")

us_dogs %>%
  ggplot(aes(x = breed_primary, fill = age)) +
  geom_bar(position = "stack") +
  coord_flip() +
  theme_fivethirtyeight() +
  labs(title = "U.S. Dog Adoptions by Breed and Age", subtitle = "Currently Available", caption = "Data from Petfinder")

# Select for Dogs from Indiana
indiana_dogs <- dog_descriptions %>%
  filter(contact_state == "IN")
  
# Count breed_primary, remove ungendered
dog_count <- indiana_dogs %>%
  filter(sex != "Unknown") %>%
  count(breed_primary) %>%
  filter(n > 10)

indiana_dogs <-  indiana_dogs %>%
  inner_join(dog_count, by = "breed_primary")
  
  indiana_dogs %>%
    ggplot(aes(x = breed_primary, fill = sex)) +
    geom_bar(position = "stack") +
    coord_flip() +
    theme_fivethirtyeight() +
    labs(title = "Indiana Dog Adoptions", subtitle = "Currently Available", caption = "Data from Petfinder")

  indiana_dogs %>%
    ggplot(aes(x = breed_primary, fill = age)) +
    geom_bar(position = "stack") +
    coord_flip() +
    theme_fivethirtyeight() +
    labs(title = "Indiana Dog Adoptions", subtitle = "Currently Available", caption = "Data from Petfinder")

# Shiny App
  
  by_state <- us_dogs %>%
    group_by(contact_state)
  
  library(shiny)
  
  ui <- fluidPage(
    fluidRow(
      column(3,
             selectInput("location", "Location", choices = by_state$contact_state)),
      column(3,
             selectInput("sex", "Sex", choices = us_dogs$sex)),
      column(3,
      selectInput("age", "Age", choices = us_dogs$age)),
      column(3,
             selectInput("breed", "Breed", choices = us_dogs$breed_primary))
    ),
    fluidRow(
      column(12, plotOutput("by_zip"))
    )
  )
  
  server <- function(input, output, session) {
    selected_state <- reactive(us_dogs %>% filter(contact_state == input$location))
    selected_sex <- reactive(selected_state %>% filter(sex == input$sex))
    selected_age <- reactive(selected_sex %>% filter(age == age) %>%
      group_by(contact_city)
    )
  }
  
  output$by_city <- renderPlot
  selected_age() %>%
  ggplot(aes(x = contact_city)) +
    geom_bar() +
    coord_flip() +
    theme_fivethirtyeight() +
    labs(title = "Indiana Dog Adoptions", subtitle = "Currently Available", caption = "Data from Petfinder")
  
  shinyApp(ui, server)
  