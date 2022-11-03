# 30DayMapChallenge
# 2022
# Day 3 : Polygons
# Last updated : 2022-11-03

# Load packages ----

library(showtext)
library(tidyverse)
library(osmdata)

# Import fonts ----

font_add_google("Nova Square", "Nova Square")
showtext_auto()

# Get data ----

fresno_bb <- matrix(data = c(-119.93, -119.89, 36.67, 36.71),
                    nrow = 2, byrow = TRUE)
colnames(fresno_bb) <- c("min", "max")
rownames(fresno_bb) <- c("x", "y")

reservoir <- opq(bbox = fresno_bb) |> 
  add_osm_feature(key = "water", value = "reservoir") |> 
  osmdata_sf()

# Create map ----

p <- ggplot() +
  geom_sf(data = reservoir$osm_polygons,
          fill = "#08b3e5", colour = "#08b3e5") +
  labs(title = "Fresno-Clovis Water Treatment Plant",
       caption = "#30DayMapChallenge 2022 | 03 - polygons | J.Kitt | Source : OpenStreetMap") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#22e4ac", colour = "#22e4ac"),
        plot.background = element_rect(fill = "#22e4ac", colour = "#22e4ac"),
        plot.title = element_text(family = "Nova Square", colour = "#223f85",
                                  size = 100, hjust = 0.5, margin = margin(t = 25, b = 5)),
        plot.caption = element_text(colour = "#223f85", size = 25, hjust = 0.5,
                                    margin = margin(b = 10)))

# Export map ----

ggsave("2022/maps/03_polygons.png", p, dpi = 320, height = 6, width = 12)
