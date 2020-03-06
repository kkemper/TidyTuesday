#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
if(!exists("dog_descriptions")) {
    dog_descriptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_descriptions.csv')
}

# Convert to factors
dog_descriptions$age <- factor(dog_descriptions$age, levels = c("Baby", "Young", "Adult", "Senior"))
dog_descriptions$sex <- factor(dog_descriptions$sex, levels = c("Female", "Male", "Unknown"))

us_dogs <- dog_descriptions %>%
    filter(contact_country == "US", sex != "Unknown")

ui <- fluidPage(

    fluidRow(
        column(3,
               selectInput("location", "Location", choices = us_dogs$contact_state)),
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

# Define server logic required to draw a histogram
server <- function(input, output) {

    selected_dogs <- reactive(dog_descriptions %>%
        filter(sex == input$sex, age == input$age, breed_primary == input$breed, contact_state == input$location ))
    
    # Map zipcode to long and lat
    output$by_zip <- renderPlot({selected_dogs()$contact_zip <- clean.zipcodes(selected_dogs()$contact_zip)
    selected_dogs() %>% select(id, sex, age, breed_primary, contact_state,contact_zip)
    data(zipcode)
    dogs_zip <- aggregate(data.frame(count = selected_dogs$id), list(zip = selected_dogs()$contact_zip), length)
    dogs_zip <- merge(dogs_zip, zipcode, by = 'zip')
    
    # Plot to map
    
    state <- map_data("state", region = selected_dogs()$contact_state)
    
    ggplot(dogs_zip, aes(longitude, latitude)) +
        geom_polygon(data = state, aes(x = long, y = lat, group = group), color =  'gray', fill = NA, alpha = 0.35) +
        geom_point(aes(color = count), size = 4) +
        xlim(min(state$long), max(state$long)) + ylim(min(state$lat), max(state$lat)) +
        labs(title = "Available Dogs")
    
    }
)
}

# Run the application 
shinyApp(ui = ui, server = server)
