# 30DayMapChallenge
# 2022
# Day 15 : Food / drink
# Last updated : 2022-11-15

# Load packages ----

library(tidyverse)
library(osmdata)
library(sf)
library(showtext)

# Import fonts ----

font_add_google("Bebas Neue", "bebas")
showtext_auto()

# Get data ----

pubs_eng <- getbb("England") |> 
  opq() |> 
  add_osm_feature(key = "amenity",
                  value = "pub") |> 
  osmdata_sf()

pubs_eng_tbl <- pubs_eng$osm_points |> 
  as_tibble() |> 
  select(name, geometry)

pubs_ire <- getbb("Ireland") |> 
  opq() |> 
  add_osm_feature(key = "amenity",
                  value = "pub") |> 
  osmdata_sf()

pubs_ire_tbl <- pubs_ire$osm_points |> 
  as_tibble() |> 
  select(name, geometry)

pubs_sco <- getbb("Scotland") |> 
  opq() |> 
  add_osm_feature(key = "amenity",
                  value = "pub") |> 
  osmdata_sf()

pubs_sco_tbl <- pubs_sco$osm_points |> 
  as_tibble() |> 
  select(name, geometry)

# Remove duplicates ----

d1 <- rbind(pubs_eng_tbl, pubs_ire_tbl, pubs_sco_tbl) |> 
  distinct(geometry)

# Create map ----

p <- ggplot() +
  geom_sf(data = d1$geometry,
          size = 0.1,
          colour = "#f8b195") +
  geom_rect(aes(xmin = -3, xmax = 2.5,
                ymin = 49, ymax = 50.25),
            fill = "#355c7d", colour = "#355c7d") +
  geom_rect(aes(xmin = 1.5, xmax = 2.5,
                ymin = 50, ymax = 51),
            fill = "#355c7d", colour = "#355c7d") +
  labs(title = "Pubs in the British isles",
       caption = "#30DayMapChallenge 2022 | 15 - Food / drink | J.Kitt | Source : OpenStreetMap") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#355c7d", colour = "#355c7d"),
        plot.background = element_rect(fill = "#355c7d", colour = "#355c7d"),
        plot.title = element_text(family = "bebas", colour = "#f8b195",
                                  size = 100, hjust = 0.5,
                                  margin = margin(t = 10, b = 10)),
        plot.caption = element_text(family = "bebas", colour = "#f8b195",
                                    size = 40, hjust = 0.5,
                                    margin = margin(b = 10)))

# Export map ----

ggsave("2022/maps/15_food_drink.png", p, dpi = 320, height = 6, width = 12)