# 30DayMapChallenge
# 2022
# Day 4 : Green
# Last updated : 2022-11-02

# Load packages ----

library(tidyverse)
library(osmdata)
library(patchwork)
# library(showtext)

# Import fonts ----

# font_add_google("Bebas Neue", "bebas")
# showtext_auto()

# Set coordinates ----

coord <- tibble(
  name = "longleat",
  long = -2.277649,
  lat = 51.188063)

# Get data ----

longleat_bb <- matrix(data = c(-2.27853, -2.27690, 51.18744, 51.18858),
                      nrow = 2, byrow = TRUE)
colnames(longleat_bb) <- c("min", "max")
rownames(longleat_bb) <- c("x", "y")

longleat_maze <- opq(bbox = longleat_bb) |> 
  add_osm_feature(key = "barrier", value = "hedge") |> 
  osmdata_sf()

# Create maps ----

world <- map_data("world") |> 
  filter(region == "UK")

loc <- ggplot() +
  geom_polygon(data = world,
                 aes(x = long, y = lat, group = group),
                 colour = NA,
                 fill = "#999999",
                 alpha = 0.5) +
    geom_point(data = coord,
               aes(x = long, y = lat),
               colour = "#ee4d5a") +
  coord_fixed(1.3) +
  theme_void()

maze <- ggplot() +
  geom_sf(data = longleat_maze$osm_lines,
          colour = "#95bb72",
          size = 0.75) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#4b6043", colour = NA),
        plot.background = element_rect(fill = "#4b6043", colour = NA))


m <- loc + maze
m

ggsave("2022/maps/04_green.png", m, dpi = 320, height = 6, width = 12)
