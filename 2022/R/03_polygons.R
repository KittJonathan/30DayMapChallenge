# 30DayMapChallenge
# 2022
# Day 3 : Polygons
# Last updated : 2022-10-04

# Load packages ----

library(showtext)
library(tidyverse)
# library(geojsonio)
# library(jsonlite)
# library(sp)
# library(broom)
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

wastewater_plant <- opq(bbox = fresno_bb) |> 
  add_osm_feature(key = "man_made", value = "wastewater_plant") |> 
  osmdata_sf()

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

ggsave("2022/maps/03_polygons.png", p, dpi = 320, height = 6, width = 12)




wastewater_plant <- opq(bbox = fresno_bb) |> 
  add_osm_feature(key = "water", value = "wastewater") |> 
  osmdata_sf()

d1 <- opq(bbox = fresno_bb) |> 
  add_osm_feature(key = "water") |> 
  osmdata_sf()

d1 <- opq(bbox = fresno_bb) |> 
  add_osm_feature(key = "water") |> 
  osmdata_sf()

roads <- opq(bbox = fresno_bb) |> 
  add_osm_feature(key = "highway") |> 
  osmdata_sf()

d2 <- opq(bbox = fresno_bb) |> 
  add_osm_feature(key = "landfill") |> 
  osmdata_sf()

ggplot() +
  # geom_sf(data = roads$osm_lines) +
  geom_sf(data = wastewater_plant$osm_polygons,
          fill = "blue", colour = "lightblue")

d1
fresno_bb

d1 <- opq("Fresno") |> 
  add_osm_feature(key = "highway") |> 
  osmdata_sf()

d1

d1$bbox

# Extract data ----

# https://github.com/fraxen/tectonicplates
# https://r-graph-gallery.com/325-background-map-from-geojson-format-in-r.html

# d1 <- geojsonio::geojson_read("https://raw.githubusercontent.com/fraxen/tectonicplates/master/GeoJSON/PB2002_boundaries.json", what = "sp")
plates <- geojsonio::geojson_read("https://raw.githubusercontent.com/fraxen/tectonicplates/master/GeoJSON/PB2002_plates.json", what = "sp")

# tidy_plates <- tidy(plates)

# africa <- tidy(plates[plates@data$PlateName == "Africa", ])

# test <- d1[d1@data$Name == "CA-NA", ]

# subduction <- d1[d1@data$Type == "subduction", ]
# sub_tidy <- tidy(subduction)

# d2 <- tidy(d1)

# tect <- d1[d1@data$Type != "subduction", ]
# tect_tidy <- tidy(tect)

# bird <- d1[d1@data$Source == "Peter Bird, June 2002", ]
# bird_tidy <- tidy(bird)

# eu_af <- tidy(d1[d1@data$Name %in% c("EU-AF", "EU\\AF", "EU/AF", "NA-AF"), ])

world <- map_data("world")

plates_list <- sort(unique(plates@data$PlateName))

plates_tidy <- tidy(plates)

ggplot() +
  geom_polygon(data = world,
               aes(x = long, y = lat, group = group),
               colour = "#1b1d46", fill = "#1b1d46") +
  geom_polygon(data = tidy(plates[plates@data$PlateName == plates_list[8], ]),
               aes(x = long, y = lat, group = group),
               colour = "red", fill = NA, size = 2)

ggplot() +
  geom_polygon(data = world,
               aes(x = long, y = lat, group = group),
               colour = "#1b1d46", fill = "#1b1d46") +
  geom_polygon(data = tidy(plates[plates@data$PlateName ==  "Antarctica", ]),
               aes(x = long, y = lat, group = group),
               colour = "red", fill = NA, size = 1, alpha = 0.5) +
  coord_map("ortho", orientation = c(-90, 0, 0))


  geom_polygon(data = tidy(plates[plates@data$PlateName %in% c("Africa", "Antarctica", "Nazca",
                                                               "South America"), ]),
               aes(x = long, y = lat, group = group),
               colour = "red", fill = "red", size = 1, alpha = 0.5)

ggplot() +
  geom_polygon(data = world,
               aes(x = long, y = lat, group = group),
               colour = "#1b1d46", fill = "#1b1d46") +
  geom_polygon(data = tidy_plates, aes( x = long, y = lat, group = group), color = "red", size = 1, fill = NA) 


ggplot() +
  geom_line(data = tect_tidy, aes( x = long, y = lat, group = group), color = "white", fill = NA) +
  # theme_void() +
  coord_map()


