# 30DayMapChallenge
# 2022
# Day 6 : Network
# Last updated : 2022-10-21

# Load packages ----

library(tidyverse)
library(osmdata)
library(showtext)

# Import fonts ----

font_add_google("Bebas Neue", "bebas")
showtext_auto()


# Get data ----

lyon <- getbb("Lyon") |>  
  opq() |> 
  add_osm_feature(key = "route",
                  value = "subway") |> 
  osmdata_sf()

ligne_A <- lyon$osm_lines |> 
  filter(name == "Ligne A")

ligne_B <- lyon$osm_lines |> 
  filter(name == "Ligne B")

ligne_C <- lyon$osm_lines |> 
  filter(name == "Ligne C")

ligne_D <- lyon$osm_lines |> 
  filter(name == "Ligne D")

stations_list <- sort(unique((lyon$osm_points$name[!is.na(lyon$osm_points$name)])))

major_stations_list <- stations_list[stations_list %in% c("Bellecour", "Hôtel de Ville - Louis Pradel",
                                                          "Charpennes - Charles Hernu", "Saxe - Gambetta")]

major_stations <- list()

for (i in 1:length(major_stations_list)) {
  major_stations[[i]] <- lyon$osm_points |> 
    filter(name == major_stations_list[i]) |> 
    filter(row_number() == 1)
}

major_stations <- data.table::rbindlist(major_stations)

minor_stations_list <- stations_list[!stations_list %in% major_stations_list]

minor_stations <- list()

for (i in 1:length(minor_stations_list)) {
  minor_stations[[i]] <- lyon$osm_points |> 
    filter(name == minor_stations_list[i]) |> 
    filter(row_number() == 1)
}

minor_stations <- data.table::rbindlist(minor_stations)

# Create map ----

(p <- ggplot() +
   geom_sf(data = ligne_C$geometry,
           inherit.aes = FALSE,
           size = 4,
           colour = "#ed9801") +
  geom_sf(data = ligne_A$geometry,
          inherit.aes = FALSE,
          size = 4,
          colour = "#e50823") +
  geom_sf(data = ligne_B$geometry,
          inherit.aes = FALSE,
          size = 4,
          colour = "#009edf") +
  geom_sf(data = ligne_D$geometry,
          inherit.aes = FALSE,
          size = 4,
          colour = "#029e3b") +
  geom_sf(data = major_stations$geometry,
          inherit.aes = FALSE,
          size = 5,
          shape = 21,
          colour = "black",
          fill = "white",
          stroke = 1.5) +
   geom_sf(data = minor_stations$geometry,
           inherit.aes = FALSE,
           size = 2,
           colour = "white") +
   labs(title = "Lyon subway network",
        caption = "#30DayMapChallenge 2022 | 06 - network | J.Kitt | Source : OSM") +
   theme_void() +
   theme(panel.background = element_rect(fill = "#565656", colour = NA),
         plot.background = element_rect(fill = "#565656", colour = NA),
         plot.title = element_text(family = "bebas", size = 150,
                                   colour = "#cfcfcf", hjust = 0.5,
                                   margin = margin(t = 20, b = 20)),
         plot.caption = element_text(colour = "#cfcfcf", size = 25, hjust = 0.5,
                                     margin = margin(b = 10)))
)

ggsave("2022/maps/06_network.png", p, dpi = 320, height = 6, width = 12)
