# 30DayMapChallenge
# 2022
# Day 1 : Points
# Last updated : 2022-11-01

# Load packages ----

library(parzer)
library(showtext)
library(tidyverse)

# Import fonts ----

font_add_google(name = "Finger Paint", family = "paint")
showtext_auto()

# Extract data ----

url <- "https://fr.wikipedia.org/wiki/Point_chaud_(géologie)"

webpage <- rvest::read_html(url)

tables <- rvest::html_nodes(webpage, "table.wikitable") %>%
  rvest::html_table(header = TRUE, na.strings = c(NA, ""), convert = TRUE)

hotspots <- tables[[1]]

rm(tables, url, webpage)

# Clean data ----

hotspots_clean <- hotspots |> 
  janitor::clean_names() |> 
  dplyr::select(nom, plaque, position) |> 
  dplyr::filter(!is.na(position)) |> 
  tidyr::separate(position, into = c("lat", "long"), sep = ",") |> 
  dplyr::mutate(long = str_replace_all(long, "\\[.+?\\]", "")) |> 
  dplyr::mutate(long = str_replace_all(long, "O", "W")) |> 
  dplyr::mutate(lat_2 = parzer::parse_lat(lat),
                long_2 = parzer::parse_lon(long))

# Create map ----

world <- map_data("world")

(p <- ggplot() +
  geom_polygon(data = world,
               aes(x = long, y = lat, group = group),
               colour = NA,
               fill = "#999999",
               alpha = 0.5) +
  geom_point(data = hotspots_clean,
             aes(x = long_2, y = lat_2),
             colour = "#ee4d5a") +
  coord_fixed(1.3) +
  labs(title = "Volcanic hot spots",
       caption = "#30DayMapChallenge 2022 | 01 - points | J.Kitt | Source : Wikipedia") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black", colour = "black"),
    plot.background = element_rect(fill = "black", colour = "black"),
    plot.title = element_text(family = "paint", colour = "#ee4d5a",
                              size = 75, margin = margin(t = 20)),
    plot.caption = element_text(colour = "#ee4d5a", size = 25, hjust = 0.5,
                                margin = margin(b = 10)))
)

# Export map ----

ggsave("2022/maps/01_points.png", p, dpi = 320, height = 6, width = 12)