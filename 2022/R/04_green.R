# 30DayMapChallenge
# 2022
# Day 4 : Green
# Last updated : 2022-11-02

# Load packages ----

library(tidyverse)
library(osmdata)
library(ggfx)
# library(showtext)

# Import fonts ----

# font_add_google("Bebas Neue", "bebas")
# showtext_auto()

# Get data ----

marlborough_bb <- matrix(data = c(-1.35018, -1.34918, 51.83705, 51.83784),
                       nrow = 2, byrow = TRUE)
colnames(marlborough_bb) <- c("min", "max")
rownames(marlborough_bb) <- c("x", "y")

marlborough_hedges <- opq(bbox = marlborough_bb) |> 
  add_osm_feature(key = "barrier", value = "hedge") |> 
  osmdata_sf()

hampton_bb <- matrix(data = c(-0.33817, -0.33709, 51.40604, 51.40643),
                         nrow = 2, byrow = TRUE)
colnames(hampton_bb) <- c("min", "max")
rownames(hampton_bb) <- c("x", "y")

hamtpon_hedges <- opq(bbox = hampton_bb) |> 
  add_osm_feature(key = "barrier", value = "hedge") |> 
  osmdata_sf()

longleat_bb <- matrix(data = c(-2.27853, -2.27690, 51.18744, 51.18858),
                     nrow = 2, byrow = TRUE)
colnames(longleat_bb) <- c("min", "max")
rownames(longleat_bb) <- c("x", "y")

longleat_hedges <- opq(bbox = longleat_bb) |> 
  add_osm_feature(key = "barrier", value = "hedge") |> 
  osmdata_sf()

dole_bb <- matrix(data = c(-158.03805, -158.03658, 21.52404, 21.52588),
                      nrow = 2, byrow = TRUE)
colnames(dole_bb) <- c("min", "max")
rownames(dole_bb) <- c("x", "y")

dole_hedges <- opq(bbox = dole_bb) |> 
  add_osm_feature(key = "barrier", value = "hedge") |> 
  osmdata_sf()

peace_bb <- matrix(data = c(-5.95423, -5.95219, 54.25781, 54.25910),
                  nrow = 2, byrow = TRUE)
colnames(peace_bb) <- c("min", "max")
rownames(peace_bb) <- c("x", "y")

peace_hedges <- opq(bbox = peace_bb) |> 
  add_osm_feature(key = "barrier", value = "hedge") |> 
  osmdata_sf()

ggplot() + geom_sf(data = marlborough_hedges$osm_lines)

ggplot(data = ) +
  geom_sf(data = longleat_hedges$osm_lines)


ggplot() + geom_sf(data = hamtpon_hedges$osm_lines)
ggplot() + geom_sf(data = longleat_hedges$osm_lines)
ggplot() + geom_sf(data = dole_hedges$osm_lines)
ggplot() + geom_sf(data = peace_hedges$osm_lines, col = "darkgreen")

villandry_bb <- matrix(data = c(0.512, 0.517, 47.3375, 47.3408),
                       nrow = 2, byrow = TRUE)
colnames(villandry_bb) <- c("min", "max")
rownames(villandry_bb) <- c("x", "y")

hedges <- opq(bbox = villandry_bb) |> 
  add_osm_feature(key = "barrier", value = "hedge") |> 
  osmdata_sf()

ggplot() +
  geom_sf(data = hedges$osm_polygons) +
  geom_sf(data = hedges$osm_multipolygons) +
  geom_sf(data = hedges$osm_points, size = 0.05) +
  geom_sf(data = hedges$osm_lines) +
  geom_sf(data = grass$osm_polygons) +
  geom_sf(data = scrub$osm_points, col = "red") +
  xlim(c(0.512, 0.51635))

water <- opq(bbox = villandry_bb) |> 
  add_osm_feature(key = "water") |> 
  osmdata_sf()

# buildings <- opq(bbox = villandry_bb) |> 
#   add_osm_feature(key = "building") |> 
#   osmdata_sf()

# buildings <- buildings$osm_polygons |> 
#   filter(!is.na(name))

grass <- opq(bbox = villandry_bb) |> 
  add_osm_feature(key = "landuse", value = "grass") |> 
  osmdata_sf()

hedges <- opq(bbox = villandry_bb) |> 
  add_osm_feature(key = "barrier", value = "hedge") |> 
  osmdata_sf()

scrub <- opq(bbox = villandry_bb) |> 
  add_osm_feature(key = "natural", value = "scrub") |> 
  osmdata_sf()

tree_row <- opq(bbox = villandry_bb) |> 
  add_osm_feature(key = "natural", value = "tree_row") |> 
  osmdata_sf()

fountain <- opq(bbox = villandry_bb) |> 
  add_osm_feature(key = "amenity", value = "fountain") |> 
  osmdata_sf()

surface <- opq(bbox = villandry_bb) |> 
  add_osm_feature(key = "surface") |> 
  osmdata_sf()

surface


ggplot() +
  geom_sf(data = grass$osm_polygons, fill = "darkgreen", col = NA) +
  geom_sf(data = water$osm_polygons, fill = "lightblue", col = "NA") +
  geom_sf(data = hedges$osm_lines, col = "lightgreen", fill = "NA") +
  geom_sf(data = scrub$osm_polygons, fill = "green", col = "NA") +
  geom_sf(data = fountain$osm_polygons, fill = "lightblue", col = NA) +
  geom_sf(data = tree_row$osm_lines, col = "green4") +
  xlim(c(0.512, 0.51635))

longleat_bb <- matrix(data = c(-2.28, -2.25, 51.18, 51.19),
                       nrow = 2, byrow = TRUE)
colnames(longleat_bb) <- c("min", "max")
rownames(longleat_bb) <- c("x", "y")

hedges <- opq(bbox = longleat_bb) |> 
  add_osm_feature(key = "barrier", value = "hedge") |> 
  osmdata_sf()

ggplot() +
  geom_sf(data = hedges$osm_lines, col = "darkgreen")



villandry_grass <- opq(bbox = villandry_bb) |> 
  add_osm_feature(key = "landuse", value = "grass") |> 
  osmdata_sf()



ggplot() +
  geom_sf(data = grass$osm_polygons, fill = "darkgreen", col = NA) +
  geom_sf(data = water$osm_polygons, fill = "blue", col = NA) +
  geom_vline(xintercept = 0.51655)



####


landuse_forest <- getbb("Scotland") |> 
  opq() |> 
  add_osm_feature(key = "landuse",
                  value = "forest") |> 
  osmdata_sf()

france_motorways <- getbb("Scotland") |> 
  opq() |> 
  add_osm_feature(key = "n",
                  value = "motorway") |> 
  osmdata_sf()

ggplot() +
  geom_sf(data = landuse_grass$osm_polygons)

longleat_bb <- matrix(data = c(-2.27854, -2.27696, 51.18745, 51.18858),
                      nrow = 2, byrow = TRUE)
colnames(longleat_bb) <- c("min", "max")
rownames(longleat_bb) <- c("x", "y")

longleat_maze <- opq(bbox = longleat_bb) |> 
  add_osm_feature(key = "barrier", value = "hedge") |> 
  osmdata_sf()



villandry_flowerbed <- opq(bbox = villandry_bb) |> 
  add_osm_feature(key = "landuse", value = "flowerbed") |> 
  osmdata_sf()

ggplot() +
  geom_sf(data = longleat_maze$osm_polygons)



villandry_tree <- opq(bbox = villandry_bb) |> 
  add_osm_feature(key = "natural", value = "tree") |> 
  osmdata_sf()

villandry <- getbb("Villandry") |> 
  opq() |> 
  add_osm_feature(key = "leisure",
                  value = "garden") |> 
  osmdata_sf()

ggplot() +
  geom_sf(data = villandry$osm_polygons) +
  geom_sf(data = villandry_tree$osm_points)

leisure_garaden <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "leisure",
                  value = "garden") |> 
  osmdata_sf()

botanic_garden <- leisure_garaden$osm_polygons |> 
  filter(name == "Royal Botanic Garden Edinburgh")

ggplot() +
  geom_sf(data = botanic_garden$geometry)

natural_grassland <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "natural",
                  value = "grassland") |> 
  osmdata_sf()

natural_heath <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "natural",
                  value = "heath") |> 
  osmdata_sf()

natural_tree <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "natural",
                  value = "tree") |> 
  osmdata_sf()

ggplot() +
  geom_sf(data = leisure_garaden$osm_polygons)

+
  geom_sf(data = natural_heath$osm_polygons)
  geom_sf(data = natural_tree$osm_points, size = 0.25)

boundary_protected_area <- getbb("Scotland") |> 
  opq() |> 
  add_osm_feature(key = "boundary",
                  value = "protected_area") |> 
  osmdata_sf()

boundary_national_park <- getbb("Scotland") |> 
  opq() |> 
  add_osm_feature(key = "boundary",
                  value = "national_park") |> 
  osmdata_sf()

boundary_forest <- getbb("Scotland") |> 
  opq() |> 
  add_osm_feature(key = "boundary",
                  value = "forest") |> 
  osmdata_sf()

highway_cycleway <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "highway",
                  value = "cycleway") |> 
  osmdata_sf()

cycleway_lane <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "cycleway",
                  value = "lane") |> 
  osmdata_sf()

cycleway_opposite <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "cycleway",
                  value = "opposite") |> 
  osmdata_sf()

cycleway_opposite_lane <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "cycleway",
                  value = "opposite_lane") |> 
  osmdata_sf()

cycleway_track <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "cycleway",
                  value = "track") |> 
  osmdata_sf()

cycleway_opposite_track <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "cycleway",
                  value = "opposite_track") |> 
  osmdata_sf()

cycleway_share_busway <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "cycleway",
                  value = "share_busway") |> 
  osmdata_sf()

cycleway_opposite_share_busway <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "cycleway",
                  value = "opposite_share_busway") |> 
  osmdata_sf()

cycleway_shared_lane <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "cycleway",
                  value = "shared_lane") |> 
  osmdata_sf()

ggplot() +
  geom_sf(data = highway_cycleway$osm_lines) +
  geom_sf(data = cycleway_lane$osm_lines) +
  geom_sf(data = cycleway_opposite$osm_lines) +
  geom_sf(data = cycleway_opposite_lane$osm_lines) +
  geom_sf(data = cycleway_track$osm_lines) +
  geom_sf(data = cycleway_opposite_track$osm_lines) +
  geom_sf(data = cycleway_share_busway$osm_lines) +
  geom_sf(data = cycleway_opposite_share_busway$osm_lines) +
  geom_sf(data = cycleway_shared_lane$osm_lines)
  
landuse_allotments <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "landuse",
                  value = "allotments") |> 
  osmdata_sf()

landuse_grass <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "landuse",
                  value = "grass") |> 
  osmdata_sf()

leisure_park <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "leisure",
                  value = "park") |> 
  osmdata_sf()

leisure_pitch <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "leisure",
                  value = "pitch") |> 
  osmdata_sf()

ggplot() +
  geom_sf(data = leisure_park$osm_polygons)

longleat_bb <- matrix(data = c(-2.279, -2.276, 51.187, 51.189),
                    nrow = 2, byrow = TRUE)
colnames(longleat_bb) <- c("min", "max")
rownames(longleat_bb) <- c("x", "y")

longleat <- opq(bbox = longleat_bb) |> 
  add_osm_feature(key = "barrier", value = "hedge") |> 
  osmdata_sf()



xmin 51.18839186512715, -2.2785361134135966
xmax 51.18767976345923, -2.276923091226947
ymin 51.187433138527375, -2.277723087691085
ymax 51.188566622344446, -2.2776344887368705

longleat <- getbb("Warminster") |> 
  opq() |> 
  add_osm_feature(key = "barrier",
                  value = "hedge") |> 
  osmdata_sf()

villandry <- getbb("Villandry") |> 
  opq() |> 
  add_osm_feature(key = "barrier",
                  value = "hedge") |> 
  osmdata_sf()

edin_parks <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "leisure",
                  value = "park") |> 
  osmdata_sf()

edin_grass <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "landuse",
                  value = "grass") |> 
  osmdata_sf()

edin_no_na <- edinburgh$osm_polygons |> 
  filter(!is.na(name))
edin_wood <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "natural",
                  value = "wood") |> 
  osmdata_sf()

edin_trees <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "natural",
                  value = "tree") |> 
  osmdata_sf()

ligne_C <- lyon$osm_lines |> 
  filter(name == "Ligne C")

ggplot() +
  # geom_sf(data = edin_parks$osm_polygons, colour = NA, fill = "blue") +
  # geom_sf(data = edin_wood$osm_polygons, colour = NA, fill = "darkgreen") +
  geom_sf(data = longleat$osm_polygons, fill = "darkgreen")

evergreen <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "leaf_cycle",
                  value = "evergreen") |> 
  osmdata_sf()

semi_evergreen <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "leaf_cycle",
                  value = "semi_evergreen") |> 
  osmdata_sf()

deciduous <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "leaf_cycle",
                  value = "deciduous") |> 
  osmdata_sf()

semi_deciduous <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "leaf_cycle",
                  value = "semi_deciduous") |> 
  osmdata_sf()

ggplot() +
  geom_sf(data = evergreen$osm_points, col = "blue") +
  geom_sf(data = semi_evergreen$osm_points, col = "lightblue")

  theme_void() +
  theme(panel.background = element_rect(fill = "lightgreen", colour = NA))

edinburgh_parks <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "boundary",
                  value = "national_park") |> 
  osmdata_sf()

edinburgh <- getbb("Edinburgh") |> 
  opq() |> 
  add_osm_feature(key = "natural",
                  value = "tree") |> 
  osmdata_sf()

glasgow <- getbb("Glasgow") |> 
  opq() |> 
  add_osm_feature(key = "natural",
                  value = "tree") |> 
  osmdata_sf()

dundee <- getbb("Dundee") |> 
  opq() |> 
  add_osm_feature(key = "natural",
                  value = "tree") |> 
  osmdata_sf()

aberdeen <- getbb("Aberdeen") |> 
  opq() |> 
  add_osm_feature(key = "natural",
                  value = "tree") |> 
  osmdata_sf()

ggplot() +
  geom_sf(data = edinburgh_parks$osm_polygons) +
  theme_void() +
  theme(panel.background = element_rect(fill = "lightgreen", colour = NA))

ggplot() +
  geom_sf(data = edinburgh$osm_points) +
  theme_void() +
  theme(panel.background = element_rect(fill = "lightgreen", colour = NA))

ggplot() +
  geom_sf(data = dundee$osm_points) +
  theme_void() +
  theme(panel.background = element_rect(fill = "lightgreen", colour = NA))

ggplot() +
  geom_sf(data = cf$osm_points)


lyon <- getbb("Lyon") |>  
  opq() |> 
  add_osm_feature(key = "route",
                  value = "subway") |> 
  osmdata_sf()

ligne_A <- lyon$osm_lines |> 
  filter(name == "Ligne A")

ligne_B <- lyon$osm_lines |> 
  filter(name == "Ligne B")

ligne_C <- lyon$osm_lines |> 
  filter(name == "Ligne C")

ligne_D <- lyon$osm_lines |> 
  filter(name == "Ligne D")

stations_list <- sort(unique((lyon$osm_points$name[!is.na(lyon$osm_points$name)])))

major_stations_list <- stations_list[stations_list %in% c("Bellecour", "Hôtel de Ville - Louis Pradel",
                                                          "Charpennes - Charles Hernu", "Saxe - Gambetta")]

major_stations <- list()

for (i in 1:length(major_stations_list)) {
  major_stations[[i]] <- lyon$osm_points |> 
    filter(name == major_stations_list[i]) |> 
    filter(row_number() == 1)
}

major_stations <- data.table::rbindlist(major_stations)

minor_stations_list <- stations_list[!stations_list %in% major_stations_list]

minor_stations <- list()

for (i in 1:length(minor_stations_list)) {
  minor_stations[[i]] <- lyon$osm_points |> 
    filter(name == minor_stations_list[i]) |> 
    filter(row_number() == 1)
}

minor_stations <- data.table::rbindlist(minor_stations)

# Create map ----

(p <- ggplot() +
   geom_sf(data = ligne_C$geometry,
           inherit.aes = FALSE,
           size = 4,
           colour = "#ed9801") +
  geom_sf(data = ligne_A$geometry,
          inherit.aes = FALSE,
          size = 4,
          colour = "#e50823") +
  geom_sf(data = ligne_B$geometry,
          inherit.aes = FALSE,
          size = 4,
          colour = "#009edf") +
  geom_sf(data = ligne_D$geometry,
          inherit.aes = FALSE,
          size = 4,
          colour = "#029e3b") +
  geom_sf(data = major_stations$geometry,
          inherit.aes = FALSE,
          size = 5,
          shape = 21,
          colour = "black",
          fill = "white",
          stroke = 1.5) +
   geom_sf(data = minor_stations$geometry,
           inherit.aes = FALSE,
           size = 2,
           colour = "white") +
   labs(title = "Lyon subway network",
        caption = "#30DayMapChallenge 2022 | 06 - network | J.Kitt | Source : OSM") +
   theme_void() +
   theme(panel.background = element_rect(fill = "#565656", colour = NA),
         plot.background = element_rect(fill = "#565656", colour = NA),
         plot.title = element_text(family = "bebas", size = 150,
                                   colour = "#cfcfcf", hjust = 0.5,
                                   margin = margin(t = 20, b = 20)),
         plot.caption = element_text(colour = "#cfcfcf", size = 25, hjust = 0.5,
                                     margin = margin(b = 10)))
)

ggsave("2022/maps/06_network.png", p, dpi = 320, height = 6, width = 12)
