# 30DayMapChallenge
# 2022
# Day 8 : OpenStreetMap
# Last updated : 2022-11-08

# Load packages ----

library(tidyverse)
library(osmdata)
library(sf)
library(showtext)

# Import fonts ----

font_add_google("Bebas Neue", "bebas")
showtext_auto()

# Get data ----

cf_bb <- matrix(data = c(3.066137, 3.097065, 45.766805, 45.790012),
                    nrow = 2, byrow = TRUE)
colnames(cf_bb) <- c("min", "max")
rownames(cf_bb) <- c("x", "y")

cf_buildings <- opq(bbox = cf_bb) |> 
  add_osm_feature(key = "building") |> 
  osmdata_sf()

cf_building <- getbb("Clermont-Ferrand") |>  
  opq() |> 
  add_osm_feature(key = "building") |> 
  osmdata_sf()

# Create map ----

p <- ggplot() +
  geom_sf(data = cf_buildings$osm_polygons,
          inherit.aes = FALSE,
          size = 0.05,
          colour = "white",
          fill = NA) +
  labs(title = "Clermont-Ferrand",
       caption = "#30DayMapChallenge 2022 | 08 - OpenStreetMap | J.Kitt | Source : OSM") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#2e4b78", colour = NA),
        plot.background = element_rect(fill = "#2e4b78", colour = NA),
        plot.title = element_text(family = "bebas", size = 150,
                                  colour = "white", hjust = 0.5,
                                  margin = margin(t = 20, b = 20)),
        plot.caption = element_text(colour = "white", size = 25, hjust = 0.5,
                                    margin = margin(b = 10)))

# Export map ----

ggsave("2022/maps/08_osm.png", p, dpi = 320, height = 6, width = 12)
