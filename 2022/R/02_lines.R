# 30DayMapChallenge
# 2022
# Day 2 : Lines
# Last updated : 2022-10-11

# Load packages ----

# library(osmdata)
library(reshape2)
library(showtext)
library(tidyverse)
# library(geojsonio)
# library(jsonlite)
# library(sp)
# library(broom)

# Import fonts ----

font_add_google("Zen Tokyo Zoo", "Zen Tokyo Zoo")
# font_add_google("Glory", "Glory")
showtext_auto()

# Volcano dataset ----

d1 <- reshape2::melt(volcano)

(p <- ggplot() +
  geom_contour(data = d1, aes(Var1, Var2, z = value,
                              colour = stat(level)),
               size = 0.5, show.legend = FALSE) +
    scale_color_gradient(low = "#e1f5fe", high = "#039be5") +
    labs(title = "Maunga Whau",
         subtitle = "contour lines",
         caption = "#30DayMapChallenge 2022 | 02 - lines | J.Kitt | Source : volcano dataset in R") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#152238", colour = "#152238"),
        plot.background = element_rect(fill = "#152238", colour = "#152238"),
        plot.title = element_text(family = "Zen Tokyo Zoo", colour = "#e1f5fe",
                                  size = 100, hjust = 0.5, margin = margin(t = 25, b = 5)),
        plot.subtitle = element_text(family = "Zen Tokyo Zoo", colour = "#e1f5fe",
                                     size = 75, hjust = 0.5, margin = margin(b = 25)),
        plot.caption = element_text(colour = "#e1f5fe", size = 25, hjust = 0.5,
                                    margin = margin(b = 10)))
)

ggsave("2022/maps/02_lines.png", p, dpi = 320, height = 6, width = 12)

# Get data ----

getbb("France")

d1 <- getbb("Lyon") |> 
  opq() |> 
  add_osm_feature(key = "route",
                  value = "train") |> 
  osmdata_sf()

ggplot() +
  geom_sf(data = d1$osm_lines)

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


