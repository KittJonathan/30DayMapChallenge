# 30DayMapChallenge
# 2022
# Day 1 : Points
# Last updated : 2022-10-03

# Load packages ----

library(tidyverse)

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
  dplyr::select(nom, plaque, position)

hotspots_clean
