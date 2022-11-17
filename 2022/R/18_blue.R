# 30DayMapChallenge
# 2022
# Day 18 : Blue
# Last updated : 2022-11-17

# URL used ----

# https://stackoverflow.com/questions/58549852/small-multiple-maps-with-geom-sf-at-the-same-spatial-scale

# Load packages ----

library(patchwork)
library(showtext)
library(tidyverse)
library(osmdata)

# Import fonts ----

font_add_google("Comfortaa", "comfortaa")
showtext_auto()

# Extract list of deepest lochs ----

url <- "https://en.wikipedia.org/wiki/List_of_lochs_of_Scotland"

webpage <- rvest::read_html(url)

tables <- rvest::html_nodes(webpage, "table.wikitable") %>%
  rvest::html_table(header = TRUE, na.strings = c(NA, ""), convert = TRUE)

lochs_tbl <- tables[[1]]

rm(url, webpage, tables)

# Clean list of deepest lochs ----

lochs_tbl <- lochs_tbl |> 
  janitor::clean_names() |> 
  select(loch, max_depth_m) |> 
  arrange(desc(max_depth_m)) |> 
  head(3)

# Get OSM data ----

water <- getbb("Scotland") |> 
  opq() |> 
  add_osm_feature(key = "natural",
                  value = "water") |> 
  osmdata_sf()

# Subset OSM data ----

for (i in 1:nrow(lochs_tbl)) {
  
  assign(lochs_tbl$loch[i], select(filter(water$osm_multipolygons,
                                   name == lochs_tbl$loch[i]), "name", "geometry"))
  
}

# 3 lochs in one table ----

three_lochs <- do.call(rbind, mget(lochs_tbl$loch))

# Calculate centroids for the 10 lochs and add loch nb ----

three_lochs <- three_lochs |> 
  mutate(centroid = sf::st_centroid(geometry)) |>
  left_join(lochs_tbl, by = c("name" = "loch")) |> 
  mutate(loch_nb = 1:3)

# Create maps ----

p0 <- ggplot() +
  geom_sf(data = coast$osm_lines, col = "#2f387b", size = 0.25) +
  # geom_sf(data = three_lochs$centroid, col = "#7991bd", size = 2) +
  geom_sf_text(data = three_lochs$centroid,
               aes(label = three_lochs$loch_nb),
               family = "comfortaa", colour = "#7991bd", size = 15) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#0c023e", colour = "#0c023e"),
    plot.background = element_rect(fill = "#0c023e", colour = "#0c023e"))
  

padding <- 0.2

graph <- function(x){
  ggplot2::ggplot(ten_lochs[x,]) +
    geom_sf(fill = "#7991bd", colour = "#7991bd") +
    labs(title = paste0(ten_lochs[x, ]$loch_nb, " - ", ten_lochs[x, ]$name),
         subtitle = paste0(ten_lochs[x, ]$max_depth_m, "m")) +
    coord_sf(xlim = c(ten_lochs$centroid[[x]][1]-padding , 
                      ten_lochs$centroid[[x]][1]+padding), 
             ylim = c(ten_lochs$centroid[[x]][2]-padding , 
                      ten_lochs$centroid[[x]][2]+padding), 
             expand = FALSE) +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "#0c023e", colour = "#0c023e"),
      plot.background = element_rect(fill = "#0c023e", colour = "#0c023e"),
      plot.title = element_text(family = "comfortaa", colour = "#7991bd",
                                size = 35, hjust = 0.5,
                                margin = margin(t = 20, b = 5)),
      plot.subtitle = element_text(family = "comfortaa", colour = "#7991bd",
                                size = 35, hjust = 0.5,
                                margin = margin(b = 5)))
}

plot_list <- lapply(X = 1:nrow(ten_lochs), FUN = graph)

p <- (p0 + plot_list[[1]] + plot_list[[2]] + plot_list[[3]]) +
  plot_layout(nrow = 1) +
  plot_annotation(title = "Deepest lochs in Scotland",
                  caption = "#30DayMapChallenge 2022 | 18 - blue | J.Kitt | Source : OpenStreetMap",
                  theme = 
                    theme(
                      panel.background = element_rect(fill = "#0c023e", colour = "#0c023e"),
                      plot.background = element_rect(fill = "#0c023e", colour = "#0c023e"),
                      plot.title = element_text(family = "comfortaa", colour = "#7991bd",
                                                size = 75, hjust = 0.5,
                                                margin = margin(t = 20, b = 5)),
                      plot.caption = element_text(family = "comfortaa", colour = "#7991bd",
                                                  size = 25, hjust = 0.5)))

# Export map ----

ggsave("2022/maps/18_blue.png", p, dpi = 320, height = 6, width = 12)
