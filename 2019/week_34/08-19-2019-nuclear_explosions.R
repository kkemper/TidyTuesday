# libraries
library(tidyverse)
library(ggrepel)
library(maps)
library(skimr)
library(visdat)
library(tidylog)
library(lubridate)

# import data
nuclear_explosions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")

head(nuclear_explosions)

#By Magnitude Body
nuclear_explosions %>%
  ggplot(aes(longitude, latitude, color = country, size = magnitude_body)) +
  borders("world") + 
  geom_point() +
  coord_quickmap()

# By Magnitude Surface
nuclear_explosions %>%
  ggplot(aes(longitude, latitude, color = country, size = magnitude_surface)) +
  borders("world") + 
  geom_point() +
  coord_quickmap()

# based on others' contributions (@phillynerd)
# group into master-types
nuclear_explosions%>%
  count(type) %>%
  arrange(desc(n))

atm <- c("TOWER", "AIRDROP", "UW", "SURFACE", "CRATER", "SHIP", "ATMOSPH", "BALLOON", "ROCKET", "WATERSUR", "WATER SU", "BARGE")

ug <- c("SHAFT", "MINE", "TUNNEL", "GALLERY", "UG", "SHAFT/GR", "SHAFT/LG")

space <- "SPACE"

nuclear_explosions %>%
  count(country)

ne_clean <- nuclear_explosions %>%
  mutate(typecat = case_when(type %in% atm ~ "Atmospheric",
                             type %in% ug ~ "Underground",
                             type %in% space ~ "Space")) %>%
  filter(
    is.na(yield_lower) == F,
    is.na(yield_upper) == F) %>%
  mutate(AverageYield = (yield_lower + yield_upper)/2) %>%
  view()

skim(ne_clean)

ne_clean %>%
  ggplot(aes(x = AverageYield)) +
  geom_density()

basefig <- ne_clean %>%
  ggplot(aes(x = country, y = year, size = AverageYield, color = typecat)) +
  annotate(geom = "rect",
           xmin = -Inf,
           xmax = Inf,
           ymin = 1961,
           ymax = 1962,
           fill = "#03525E",
           alpha = 0.8) +
  annotate(geom = "rect",
           xmin = -Inf,
           xmax = Inf,
           ymin = 1958,
           ymax = 1961,
           fill = "#09c2de",
           alpha = 0.6) +
  geom_hline(yintercept = 1963, color = "#ffbb00", size = 1) + 
  geom_hline(yintercept = 1967, color = "#00ff44", size = 1) +
  geom_jitter(alpha = 0.4, width = 0.2) +
  scale_y_continuous(breaks = seq(min(ne_clean$year),
                                  max(ne_clean$year),
                                  5)) +
  scale_color_manual(values = c("#9e0c02", "#00ff44", "#ffdd00")) +
  scale_x_discrete(labels = c("PAKIST" = "Pakistan",
                              "INDIA" = "India",
                              "FRANCE" = "France",
                              "CHINA" = "China"))

annotatefig <- basefig +
  coord_flip() +
  annotate(geom = "text", x = 2, y = 1952,
           label = "USSR & US agree to\nbilateral testing moratorium",
           size = 3.5, color = "white") +
  geom_curve(aes(y = 1952, yend = 1959, x = 1.7, xend = 1.7),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.7, color = "white", curvature = 0.25) +
  annotate(geom = "text", x = 4, y = 1957,
           label = "Period represents peak\ntesting by megatonage",
           size = 3.5, color = "white") +
  geom_curve(aes(y = 1957, yend = 1961.5, x = 3.7, xend = 3.7),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.7,
             color = "white", curvature = 0.25) +
  annotate(geom = "text", x = 5.5, y = 1972,
           label = "Partial Nuclear Test Ban Treaty\nlimits all testing to underground",
           size = 3.5, color = "white") +
  geom_curve(aes(y = 1972, yend = 1963, x = 5.2, xend = 5.2),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.7,
             color = "white", curvature = -0.25) +
  annotate(geom = "text", x = 3, y = 1974,
           label = "Outer Space Treaty bans all testing\non the moon and other celestial bodies",
           size = 3.5, color = "white") + 
  geom_curve(aes(y = 1974, yend = 1967, x = 2.7, xend = 2.7),
              arrow = arrow(length = unit(0.07, "inch")), size = 0.7,
             color = "white", curvature = -0.25) +
  labs(title = "The Atomic Bomb and You",
       subtitle = sprintf("A Timeline of Nuclear Testing and Select Treaties: %s - %s",
       min(ne_clean$year),
       max(ne_clean$year)),
       caption = "Data: SIPRI | Treaty Information Wikipedia | Visualization: @phillynerd",
       size = "Average Yield\n(Kilotons of TNT)",
       color = "Testing Location")

finalfig <- annotatefig +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "gray20"),
        panel.border = element_rect(color = "white", fill = NA),
        plot.background = element_rect(fill = "gray20"),
        legend.background = element_rect(fill = "gray20"),
        legend.text = element_text(color = "white", size = 10),
        legend.title = element_text(color = "#ffbf00", size = 14),
        legend.position = "bottom",
        legend.key = element_rect(fill = NA, color = NA),
        axis.text = element_text(color = "white", size = 12),
        title = element_text(color = "white"),
        plot.title = element_text(size = 37, face = "italic", color = "#ffbf00"),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 13),
        axis.title = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 6, alpha = 1)),
         size = guide_legend(override.aes = list(color="white")))

# https://github.com/r0mymendez/R/blob/master/TidyTuesday/20190822-Nuclear%20Explosions/script.R

# libraries
library(rworldmap)
library(geosphere)
library(gpclib)
library(mapproj)
library(gganimate)
library(ggpubr)
library(animation)

# World Map
worldMap <- getMap()
world_points <- fortify(worldMap)
world_points$region <- world_points$id
world_df <- world_points[,c("long", "lat", "group", "region")]

max_year = max(nuclear_explosions$year)
min_year = min(nuclear_explosions$year)

invisible(
  saveGIF({
    
    for (i in min_year:max_year){
      
      worldmap <-
        ggplot() +
        geom_polygon(data = worldMap, aes(x = long, y = lat, group = group),
                     color = "#ffd300", fill = "#2a2a2a") +
        theme_void() +
        labs(title = 'Nuclear Explosions') +
        labs(caption = 'by @r0mymendez \n') +
        theme(
          legend.background = element_rect(fill="#2a2a2a"),
          legend.text = element_text(color = "white"),
          plot.background = element_rect(fill = "#ffd300", color = "#ffd300"),
          panel.background = element_rect(fill = "#ffd300"),
          legend.position = c(0.11, 0.32),
          legend.key = element_rect(fill = "#2a2a2a", color = NA),
          legend.title = element_text(color = "white", size = 20, hjust = 0),
          plot.title = element_text(hjust = 0.5,
                                    size = 50,
                                    color = "#2a2a2a",
                                    face = "bold"),
          plot.subtitle = element_text(hjust = 0.5,
                                       size = 15,
                                       face = "bold",
                                       color = "black")
        ) +
        geom_point(data = nuclear_explosions %>% filter(year == i),
                   aes(x = longitude,
                       y = latitude,
                       size = magnitude_surface + 0.5,
                       color = magnitude_body),
                   alpha = 0.3,
                   show.legend = FALSE) +
        scale_color_gradient(low = "#bf6068", high = "#8c041d") +
        scale_size_continuous(range = c(1,20))
      
      p3 <- ggplot(data = NULL, aes(x = min_year:max_year, y = 1)) + 
        geom_line() +
        geom_point(aes(fill = (x = min_year:max_year > i)), shape = 21, size = 5) +
        theme_void() +
        theme(legend.position = "none") +
        scale_fill_manual(values = c("#b2d1e0", "gold")) +
        geom_text(aes(x = i, y = 1, label = i), vjust = -1, size = 9, color = "white") +
        theme(panel.background = element_rect(fill = "#fcfcfc", color = "#cccccc")) + 
        theme(plot.background = element_rect(fill = "#2a2a2a", color = "black"))
      
      print(ggarrange(worldmap, p3, nrow = 2, ncol = 1, heights = c(1.4, 0.3)))
    }
  },
  movie.name = "nuclear_explosions.gif",
  interval = 1,
  ani.width = 1200,
  ani.height = 900))