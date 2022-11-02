# 30DayMapChallenge
# 2022
# Day 4 : Green
# Last updated : 2022-11-02

# Load packages ----

library(tidyverse)
library(osmdata)
library(showtext)

# Import fonts ----

font_add_google("Bebas Neue", "bebas")
showtext_auto()

# Get data ----

longleat_bb <- matrix(data = c(-2.28, -2.27, 51.18, 51.19),
                    nrow = 2, byrow = TRUE)
colnames(longleat_bb) <- c("min", "max")
rownames(longleat_bb) <- c("x", "y")

longleat <- opq(bbox = longleat_bb) |> 
  add_osm_feature(key = "barrier", value = "hedge") |> 
  osmdata_sf()

xmin 51.1882083673724, -2.278764623645247
xmax 51.18782170987019, -2.276830750945523
ymin 51.18744177368181, -2.277678328994222
ymax 51.188589978338584, -2.2777185621294467

longleat <- getbb("Warminster") |> 
  opq() |> 
  add_osm_feature(key = "barrier",
                  value = "hedge") |> 
  osmdata_sf()

villandry <- getbb("Villandry") |> 
  opq() |> 
  add_osm_feature(key = "barrier",
                  value = "hedge") |> 
  osmdata_sf()

edin_parks <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "leisure",
                  value = "park") |> 
  osmdata_sf()

edin_grass <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "landuse",
                  value = "grass") |> 
  osmdata_sf()

edin_no_na <- edinburgh$osm_polygons |> 
  filter(!is.na(name))
edin_wood <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "natural",
                  value = "wood") |> 
  osmdata_sf()

edin_trees <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "natural",
                  value = "tree") |> 
  osmdata_sf()

ligne_C <- lyon$osm_lines |> 
  filter(name == "Ligne C")

ggplot() +
  # geom_sf(data = edin_parks$osm_polygons, colour = NA, fill = "blue") +
  # geom_sf(data = edin_wood$osm_polygons, colour = NA, fill = "darkgreen") +
  geom_sf(data = longleat$osm_polygons, fill = "darkgreen")

evergreen <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "leaf_cycle",
                  value = "evergreen") |> 
  osmdata_sf()

semi_evergreen <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "leaf_cycle",
                  value = "semi_evergreen") |> 
  osmdata_sf()

deciduous <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "leaf_cycle",
                  value = "deciduous") |> 
  osmdata_sf()

semi_deciduous <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "leaf_cycle",
                  value = "semi_deciduous") |> 
  osmdata_sf()

ggplot() +
  geom_sf(data = evergreen$osm_points, col = "blue") +
  geom_sf(data = semi_evergreen$osm_points, col = "lightblue")

  theme_void() +
  theme(panel.background = element_rect(fill = "lightgreen", colour = NA))

edinburgh_parks <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "boundary",
                  value = "national_park") |> 
  osmdata_sf()

edinburgh <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "natural",
                  value = "tree") |> 
  osmdata_sf()

glasgow <- getbb("Glasgow") |> 
  opq() |> 
  add_osm_feature(key = "natural",
                  value = "tree") |> 
  osmdata_sf()

dundee <- getbb("Dundee") |> 
  opq() |> 
  add_osm_feature(key = "natural",
                  value = "tree") |> 
  osmdata_sf()

aberdeen <- getbb("Aberdeen") |> 
  opq() |> 
  add_osm_feature(key = "natural",
                  value = "tree") |> 
  osmdata_sf()

ggplot() +
  geom_sf(data = edinburgh_parks$osm_polygons) +
  theme_void() +
  theme(panel.background = element_rect(fill = "lightgreen", colour = NA))

ggplot() +
  geom_sf(data = edinburgh$osm_points) +
  theme_void() +
  theme(panel.background = element_rect(fill = "lightgreen", colour = NA))

ggplot() +
  geom_sf(data = dundee$osm_points) +
  theme_void() +
  theme(panel.background = element_rect(fill = "lightgreen", colour = NA))

ggplot() +
  geom_sf(data = cf$osm_points)


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
