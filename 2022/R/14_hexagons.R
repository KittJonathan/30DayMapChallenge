# 30DayMapChallenge
# 2022
# Day 14 : Hexagons
# Last updated : 2022-10-21

# Load packages ----

library(broom)
library(countrycode)
library(geojsonio)
library(rgdal)
library(rgeos)
# library(patchwork)
library(showtext)
library(tidyverse)

# Import fonts ----

font_add_google("Bebas Neue", "bebas")
showtext_auto()


# Get data ----

# https://r-graph-gallery.com/328-hexbin-map-of-the-usa.html
# https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map

states <- tibble(
  name = c(state.name, "District of Columbia"),
  abb = c(state.abb, "DC")) |> 
  arrange(abb)

spdf <- geojsonio::geojson_read("2022/data/us_states_hexgrid.geojson",
                                what = "sp")
spdf@data <- spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

spdf@data <- spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

spdf_fortified <- tidy(spdf, region = "google_name") %>% 
  left_join(states, by = c("id" = "name"))

centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

# Create map ----

(p <- ggplot() +
   geom_polygon(data = spdf_fortified,
                aes(x = long, y = lat, group = group),
                colour = "black", fill = "white") +
   geom_text(data = centers,
             aes(x = x, y = y, label = id),
             colour = "black",
             size = 15) +
   coord_map())
ggsave("2022/maps/14_hexagons.png", p, dpi = 320, width = 12, height = 6)
