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

# Get data ----

andrassy_castle_bb <- matrix(data = c(21.17392, 21.17495, 48.01742, 48.01850),
                             nrow = 2, byrow = TRUE)
colnames(andrassy_castle_bb) <- c("min", "max")
rownames(andrassy_castle_bb) <- c("x", "y")

andrassy_castle_maze <- opq(bbox = andrassy_castle_bb) |> 
  add_osm_feature(key = "barrier", value = "hedge") |> 
  osmdata_sf()


pisani_bb <- matrix(data = c(12.01272, 12.01353, 45.40928, 45.40982),
                     nrow = 2, byrow = TRUE)
colnames(pisani_bb) <- c("min", "max")
rownames(pisani_bb) <- c("x", "y")

pisani_maze <- opq(bbox = pisani_bb) |> 
  add_osm_feature(key = "barrier", value = "hedge") |> 
  osmdata_sf()

longleat_bb <- matrix(data = c(-2.27853, -2.27690, 51.18744, 51.18858),
                      nrow = 2, byrow = TRUE)
colnames(longleat_bb) <- c("min", "max")
rownames(longleat_bb) <- c("x", "y")

longleat_maze <- opq(bbox = longleat_bb) |> 
  add_osm_feature(key = "barrier", value = "hedge") |> 
  osmdata_sf()

dole_bb <- matrix(data = c(-158.03805, -158.03658, 21.52404, 21.52588),
                  nrow = 2, byrow = TRUE)
colnames(dole_bb) <- c("min", "max")
rownames(dole_bb) <- c("x", "y")

dole_maze <- opq(bbox = dole_bb) |> 
  add_osm_feature(key = "barrier", value = "hedge") |> 
  osmdata_sf()

peace_bb <- matrix(data = c(-5.95423, -5.95219, 54.25781, 54.25910),
                   nrow = 2, byrow = TRUE)
colnames(peace_bb) <- c("min", "max")
rownames(peace_bb) <- c("x", "y")

peace_maze <- opq(bbox = peace_bb) |> 
  add_osm_feature(key = "barrier", value = "hedge") |> 
  osmdata_sf()

marlborough_bb <- matrix(data = c(-1.35018, -1.34918, 51.83705, 51.83784),
                         nrow = 2, byrow = TRUE)
colnames(marlborough_bb) <- c("min", "max")
rownames(marlborough_bb) <- c("x", "y")

marlborough_maze <- opq(bbox = marlborough_bb) |> 
  add_osm_feature(key = "barrier", value = "hedge") |> 
  osmdata_sf()

# Create maps ----

m1 <- ggplot() +
  geom_sf(data = andrassy_castle_maze$osm_polygons,
          colour = "#95bb72",
          fill = "#4b6043",
          size = 0.75) +
  geom_sf(data = andrassy_castle_maze$osm_lines,
          colour = "#95bb72",
          size = 0.75) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#4b6043", colour = NA),
        plot.background = element_rect(fill = "#4b6043", colour = NA))

m2 <- ggplot() +
  geom_sf(data = pisani_maze$osm_lines,
          colour = "#95bb72",
          size = 0.75) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#4b6043", colour = NA),
        plot.background = element_rect(fill = "#4b6043", colour = NA))

m3 <- ggplot() +
  geom_sf(data = longleat_maze$osm_lines,
          colour = "#95bb72",
          size = 0.75) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#4b6043", colour = NA),
        plot.background = element_rect(fill = "#4b6043", colour = NA))

m4 <- ggplot() +
  geom_sf(data = dole_maze$osm_lines,
          colour = "#95bb72",
          size = 0.75) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#4b6043", colour = NA),
        plot.background = element_rect(fill = "#4b6043", colour = NA))

m5 <- ggplot() +
  geom_sf(data = peace_maze$osm_lines,
          colour = "#95bb72",
          size = 0.75) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#4b6043", colour = NA),
        plot.background = element_rect(fill = "#4b6043", colour = NA))

m6 <- ggplot() +
  geom_sf(data = marlborough_maze$osm_lines,
          colour = "#95bb72",
          size = 0.75) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#4b6043", colour = NA),
        plot.background = element_rect(fill = "#4b6043", colour = NA))

m <- m1 + m2 + m3 + m4 + m5 + m6
m

ggsave("2022/maps/04_green.png", m, dpi = 320, height = 6, width = 12)
