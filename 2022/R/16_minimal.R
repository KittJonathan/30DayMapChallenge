# 30DayMapChallenge
# 2022
# Day 16 : Minimal
# Last updated : 2022-11-16

# Load packages ----

library(showtext)
library(tidyverse)
library(osmdata)

# Import fonts ----

font_add_google("MedievalSharp", "medieval")
showtext_auto()

# Get data ----

lakes <- getbb("Scotland") |> 
  opq() |> 
  add_osm_feature(key = "water",
                  value = "lake") |> 
  osmdata_sf()

custom_bb <- matrix(data = c(-4.769896, -4.284596, 57.113170, 57.410834),
                    nrow = 2, byrow = TRUE)
colnames(custom_bb) <- c("min", "max")
rownames(custom_bb) <- c("x", "y")

water <- opq(bbox = custom_bb) |> 
  add_osm_feature(key = "natural",
                  value = "water") |> 
  osmdata_sf()

highways <- opq(bbox = edin_bb) |> 
  add_osm_feature(key = "highway") |> 
  osmdata_sf()

monuments <- opq(bbox = edin_bb) |> 
  add_osm_feature(key = "historic") |> 
  osmdata_sf()


# Create map ----

ggplot() +
  geom_sf(data = water$osm_multipolygons)

+
  geom_sf(data = highways$osm_lines)

  geom_sf(data = buildings$osm_polygons,
          colour = "#a69cac", fill = "#161b33") +
  geom_sf(data = highways$osm_lines,
          colour = "#a69cac") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#161b33", colour = "#161b33"),
        plot.background = element_rect(fill = "#161b33", colour = "#161b33"))

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
