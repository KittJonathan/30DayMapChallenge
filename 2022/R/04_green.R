# 30DayMapChallenge
# 2022
# Day 4 : Green
# Last updated : 2022-11-04

# Load packages ----

library(tidyverse)
library(osmdata)
library(patchwork)
library(showtext)

# Import fonts ----

font_add_google("Stick", "Stick")
showtext_auto()

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
  filter(region == "UK" & subregion != "Northern Ireland")

loc <- ggplot() +
  geom_polygon(data = world,
               aes(x = long, y = lat, group = group),
               colour = NA, fill = "#75975e", alpha = 0.5) +
  geom_point(data = coord,
             aes(x = long, y = lat),
             colour = "#ddead1", size = 1.5) +
  geom_text(data = coord,
            aes(x = long + 0.25, y = lat + 0.25),
            hjust = 0, label = "Longleat", family = "Stick", size = 15, 
            colour = "#ddead1") +
  coord_fixed(1.3) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#4b6043", colour = NA),
        plot.background = element_rect(fill = "#4b6043", colour = NA))

maze <- ggplot() +
  geom_sf(data = longleat_maze$osm_lines,
          colour = "#95bb72",
          size = 0.75) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#4b6043", colour = NA),
        plot.background = element_rect(fill = "#4b6043", colour = NA))

# Assemble maps with {patchwork} ----

m <- loc + maze +
  plot_annotation(
    title = "Longleat hedge maze",
    caption = "#30DayMapChallenge 2022 | 04 - green | J.Kitt | Source : OpenStreetMap",
    theme = theme(
      panel.background = element_rect(fill = "#4b6043", colour = NA),
      plot.background = element_rect(fill = "#4b6043", colour = NA),
      plot.title = element_text(family = "Stick",
                                size = 100, colour = "#95bb72", hjust = 0.5,
                                margin = margin(t = 15, b = 15)),
      plot.caption = element_text(colour = "#95bb72", hjust = 0.5, size = 25))) +
  plot_layout(widths = c(1, 3))

# Save map ----

ggsave("2022/maps/04_green.png", m, dpi = 320, height = 6, width = 12)