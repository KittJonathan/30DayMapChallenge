# 30DayMapChallenge
# 2022
# Day 4 : Green
# Last updated : 2022-10-11

# Load packages ----

library(tidyverse)
# library(geojsonio)
# library(jsonlite)
# library(sp)
# library(broom)
library(osmdata)

# Get data ----

trossachs_bb <- matrix(data = c(-5.13, -4.05, 55.95, 56.49),
                    nrow = 2, byrow = TRUE)
colnames(trossachs_bb) <- c("min", "max")
rownames(trossachs_bb) <- c("x", "y")

water <- opq(bbox = trossachs_bb) |> 
  add_osm_feature(key = "natural", value = "water") |> 
  osmdata_sf()

ggplot() +
  geom_sf(data = water$osm_polygons)

edinburgh_parks <- opq("Manhattan") |> 
  add_osm_feature(key = "leisure",
                  value = "park") |> 
  osmdata_sf()

ggplot() +
  geom_sf(data = edinburgh_parks$osm_polygons)

copenhagen <- opq("Copenhagen") |> 
  add_osm_feature(key = "cycleway") |> 
  osmdata_sf()

edinburgh2 <- opq("Edinburgh") |> 
  add_osm_feature(key = "highway", value = "cycleway") |> 
  osmdata_sf()

ggplot() +
  # geom_sf(data = edinburgh$osm_lines) +
  geom_sf(data = copenhagen$osm_lines)

edinburgh <- opq("Edinburgh") |> 
  add_osm_feature(key = "natural",
                  value = c("fell", "grassland", "heath",
                            "moor", "scrub", "shrubbery", "tree",
                            "tree_row", "tundra", "wood")) |>
  osmdata_sf()

leaf_type <- opq("Edinburgh") |> 
  add_osm_feature(key = "leaf_type") |> 
  osmdata_sf()

ggplot() +
  geom_sf(data = leaf_type$osm_points,
          colour = leaf_type$osm_points$name)

ggplot() +
  geom_sf(data = edinburgh$osm_polygons,
          fill = "green") +
  geom_sf(data = edinburgh$osm_points,
          colour = "green", size = 0.25)

oslo_cycleway <- opq("Oslo") |> 
  add_osm_feature(key = "highway",
                  value = "cycleway") |>
  osmdata_sf()

ggplot() +
  geom_sf(data = oslo_cycleway$osm_lines)

oslo <- opq("Oslo") |> 
  add_osm_feature(key = "natural") |> 
  osmdata_sf()

ggplot() +
  geom_sf(data = oslo$osm_polygons)



parks <- opq("Edinburgh") |> 
  add_osm_feature(key = "leisure", value = "park") |> 
  osmdata_sf()

gardens <- opq("Edinburgh") |> 
  add_osm_feature(key = "leisure", value = "garden") |> 
  osmdata_sf()

d1 <- opq("Edinburgh") |> 
  add_osm_feature(key = "leisure", value %in% c("park", "garden")) |> 
  osmdata_sf()

ggplot() +
  geom_sf(data = parks$osm_polygons) +
  geom_sf(data = gardens$osm_polygons)

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


