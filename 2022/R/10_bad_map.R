# 30DayMapChallenge
# 2022
# Day 10 : A bad map
# Last updated : 2022-11-10

# Load packages ----

library(tidyverse)
library(showtext)

# Import fonts ----

font_add_google("Permanent Marker", "marker")
showtext_auto()

# Create map ----

world <- map_data("world")

france <- world |> 
  filter(region == "France")

(p <- ggplot() +
    geom_polygon(data = world,
                 aes(x = long, y = lat, group = group),
                 colour = NA, fill = "#ca004b") +
    geom_polygon(data = france,
                 aes(x = long, y = lat, group = group),
                 colour = NA, fill = "#ff00ff") +
    geom_rect(aes(xmin = -155, xmax = -145, ymin = -30, ymax = -20),
              fill = "#ca004b") +
    geom_rect(aes(xmin = -155, xmax = -145, ymin = -10, ymax = 0),
              fill = "#ff00ff") +
    annotate("text", x = -130, y = -5, label = "Yes", size = 20, colour = "#ff00ff") +
    annotate("text", x = -130, y = -25, label = "No", size = 20, colour = "#ca004b") +
    coord_fixed(1.3) +
    labs(title = "Are you in France ?",
         caption = "#30DayMapChallenge 2022 | 10 - a bad map | J.Kitt | Source : NA") +
    theme_void() +
    theme(panel.background = element_rect(fill = "#ffff00", colour = "#39ff14", size = 10,
                                          linetype = "dashed"),
          plot.background = element_rect(fill = "#fffcc9", colour = NA),
          plot.title = element_text(family = "marker", size = 100, hjust = 0.5,
                                    colour = "#fedd00",
                                    margin = margin(t = 20, b = 10)),
          plot.caption = element_text(family = "marker", size = 35, hjust = 0.5, colour = "#fedd00",
                                      margin = margin(t = 10, b = 10)))
  )

ggsave("2022/maps/10_bad_map.png", p, dpi = 320, height = 6, width = 12)
