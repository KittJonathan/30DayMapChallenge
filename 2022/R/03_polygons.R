# 30DayMapChallenge
# 2022
# Day 3 : Polygons
# Last updated : 2022-10-04

# Load packages ----

library(tidyverse)
# library(geojsonio)
# library(jsonlite)
# library(sp)
# library(broom)
library(osmdata)

# Get data ----

fresno_bb <- matrix(data = c(-119.93, -119.89, 36.67, 36.71),
                    nrow = 2, byrow = TRUE)
colnames(fresno_bb) <- c("min", "max")
rownames(fresno_bb) <- c("x", "y")

d1 <- opq(bbox = fresno_bb) |> 
  add_osm_feature(key = "water") |> 
  osmdata_sf()

ggplot() +
  geom_sf(data = d1$osm_polygons)

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


