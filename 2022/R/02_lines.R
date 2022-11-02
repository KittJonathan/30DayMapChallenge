# 30DayMapChallenge
# 2022
# Day 2 : Lines
# Last updated : 2022-11-02

# Load packages ----

library(reshape2)
library(showtext)
library(tidyverse)

# Import fonts ----

font_add_google("Zen Tokyo Zoo", "Zen Tokyo Zoo")
showtext_auto()

# Clean data ----

d1 <- reshape2::melt(volcano)

# Create map ----

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

# Export map ----

ggsave("2022/maps/02_lines.png", p, dpi = 320, height = 6, width = 12)
