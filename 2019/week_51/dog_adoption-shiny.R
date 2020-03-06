library(tidyverse)
library(shiny)
library(maps)
library(viridis)
library(zipcode)
library(albersusa)
library(shinyWidgets)

if(!exists("dog_decriptions")){
  dog_descriptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_descriptions.csv')
}

# Convert to factors
dog_descriptions$age <- factor(dog_descriptions$age, levels = c("Baby", "Young", "Adult", "Senior"))
dog_descriptions$sex <- factor(dog_descriptions$sex, levels = c("Female", "Male", "Unknown"))

# Filter for US and gender not Unknown
us_dogs <- dog_descriptions %>%
  filter(contact_country == "US", sex != "Unknown")

#Shiny App

ui <- fluidPage(
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        fluidRow(
        pickerInput("state", "State", choices = unique(us_dogs$contact_state), options = list(`actions-box` = TRUE),multiple = T),
        selectInput("breed", "Breed", choices = NULL),
        selectInput("age", "Age", choices = NULL),
        selectInput("sex", "Sex", choices = NULL)
        )
      ),
      mainPanel (
        plotOutput("map")
      )
    )
  ),
  fluidRow(
    tableOutput("data")
  )
)

server <- function(input, output, session) {
  state <- reactive({
    req(input$state)
    filter(us_dogs, contact_state == input$state)
  })
  observeEvent(state(), {
    breed_choices <- unique(state()$breed_primary)
    updateSelectInput(session, "breed", choices = breed_choices)
  })
  
  breed <- reactive({
    req(input$breed)
    filter(state(), breed_primary == input$breed)
  })
  observeEvent(breed(), {
    age_choices <- unique(breed()$age)
    updateSelectInput(session, "age", choices = age_choices)
  })
  
  age <- reactive({
    req(input$age)
    filter(breed(), age == input$age)
  })
  observeEvent(age(), {
    sex_choices <- unique(breed()$sex)
    updateSelectInput(session, "sex", choices = sex_choices)
  })
  
  output$data <- renderTable({
    req(input$sex)
    age() %>%
      filter(sex == input$sex) %>%
      select(id, breed_primary, age, sex, contact_state, contact_zip)
  })
  
  output$map <- renderPlot({
    # Map zipcode to long and lat
    selected_dogs <- age()
    selected_dogs <- selected_dogs %>%
      mutate(state_name = tolower(state.abb[match(contact_state, state.name)]))
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
    
  })
}

shinyApp(ui, server)