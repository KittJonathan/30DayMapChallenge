# 30DayMapChallenge
# 2022
# Day 26 : Island(s)
# Last updated : 2022-10-21

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

scotland <- map_data("world") |> 
  filter(region == "UK",
         subregion == "Scotland")

ggplot() +
  geom_polygon(data = scotland,
               aes(x = long, y = lat, group = group),
               colour = NA, fill = "#ca004b") +
  # xlim(-5.56, -5.44) +
  # ylim(57.67, 57.71) +
  coord_fixed(1.3)

57.694700171884485, -5.521885903850846
57.69182887939162, -5.46422622844872

loch_maree_bb <- matrix(data = c(-5.5218, -5.4642, 57.6824, 57.7031),
                    nrow = 2, byrow = TRUE)
colnames(loch_maree_bb) <- c("min", "max")
rownames(loch_maree_bb) <- c("x", "y")

eilean_subhainn <- opq(bbox = loch_maree_bb) |> 
  add_osm_feature(key = "water") |> 
  osmdata_sf()

p <- ggplot() +
  geom_sf(data = eilean_subhainn$osm_polygons,
          fill = "green", colour = "green") +
  xlim(-5.5218, -5.4642) +
  ylim(57.6824, 57.7031) +
  theme(panel.background = element_rect(fill = "blue"),
        plot.background = element_rect(fill = "blue"))

ggsave("2022/maps/26_islands.png", p, dpi = 320, height = 6, width = 12)

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


