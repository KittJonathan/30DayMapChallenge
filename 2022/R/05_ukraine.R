# 30DayMapChallenge
# 2022
# Day 5 : Ukraine
# Last updated : 2022-11-05

# Load packages ----

library(tidyverse)

# Get data ----

ukraine <- map_data("world") |> 
  filter(region == "Ukraine")

# Create map ----

p <- ggplot() +
  geom_polygon(data = ukraine,
               aes(x = long, y = lat, group = group),
               colour = NA, fill = "#ffd500") +
  labs(caption = "#30DayMapChallenge 2022 | 05 - Ukraine | J.Kitt") +
  coord_fixed(1.3) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#005bbb", colour = NA),
        plot.background = element_rect(fill = "#005bbb", colour = NA),
        plot.caption = element_text(colour = "#ffd500", size = 10, hjust = 0.5,
                                    margin = margin(b = 10)))

# Export map ----

ggsave("2022/maps/05_ukraine.png", p, dpi = 320, height = 6, width = 12)
