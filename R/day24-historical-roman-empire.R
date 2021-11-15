#' Digital Atlas of the Roman Empire
#' https://imperium.ahlfeldt.se/
#'
#' Terms of Use
#' All content on the Digital Atlas of the Roman Empire (DARE) is published
#' under the Creative Commons Attribution-ShareAlike 3.0 (CC BY-SA 3.0) license.
#' Data may be shared, adapted or used under the condition that a reference with
#' link to http://imperium.ahlfeldt.se, or to the relevant page on DARE as the
#' source, is provided. If you alter, transform, or build upon this work,
#' you may distribute the resulting work only under the same or similar license
#' to this one.
#'
#' API call
#' The API returns information about ancient places in GeoJSON format.
#' The base URL of the API is http://imperium.ahlfeldt.se/api/geojson.php
#' The bbox and point methods return information about places including geometry,
#' name, ancient name(s), country and feature type.
#' The place-by-id method returns extended information about the place.
#' The GeoJSON API interface has Cross-origin Resource Sharing (CORS) enabled.
#' Alternatively the API will return JSONP when a 'callback' parameter has been supplied.


pacman::p_load("tidyverse", "here", "glue", "ggtext", "colorspace", "geojson",
               "geojsonsf", "sf", "paletteer", "jsonlite", "rvest")

# api_base_url <- "http://imperium.ahlfeldt.se/api/geojson.php"
# zoom_level <- 10
# api_request_url <- glue("{api_base_url}?zoom={zoom_level}")
#
# json <- read_json(api_request_url)
#
# empire_sf <- sf_geojson(json)


#' http://awmc.unc.edu/awmc/map_data/shapefiles/political_shading/
#'
#' For Shapefiles, The suggested citation is the following:
#' Ancient World Mapping Center. “Shapefile name”. <http://awmc.unc.edu/wordpress/map-files/>
#' [Accessed: April 23, 2013 11:00am]
#'
#' roman_empire_60_bc_provinces.shp
#' roman_empire_bc_60_extent.shp

## Scrape & download shapefiles ================================================

# Scrape relevant links
scrape_download_links <- function(url, download_link_regex) {
  page <- read_html(url)
  links <- html_nodes(page, css = "a") %>% html_attr("href")
  grep(download_link_regex, links, value = TRUE)
}

url <- "http://awmc.unc.edu/awmc/map_data/shapefiles/political_shading/"
# page <- read_html(url)
# links <- html_nodes(page, css = "a") %>% html_attr("href")
# (links_roman_bc60 <- grep("roman_empire_(60_bc|bc_60|ad_14)_", links, value = TRUE))

links_roman_bc60 <- scrape_download_links(url, "roman_empire_(60_bc|bc_60|ad_14)_")

# Download shapefiles for extent and provinces
data_dir <- here("data", "roman_empire")

download_repo_files <- function(base_url, rel_links, data_dir, force = FALSE) {
  if (!dir.exists(data_dir) | force) {
    dir.create(data_dir)
    walk(rel_links, ~download.file(paste0(base_url, .x),
                                          destfile = here(data_dir, .x)))
  } else {
    message(glue("Folder {data_dir} already exists. Skipping download."))
  }
}

download_repo_files(url, links_roman_bc60, data_dir)

# if (!dir.exists(data_dir)) {
#   dir.create(data_dir)
#   walk(links_roman_bc60, ~download.file(paste0(url, .x),
#                                         destfile = here(data_dir, .x)))
# }

# ... shapefile for water
water_repo_url <- "http://awmc.unc.edu/awmc/map_data/shapefiles/physical_data/openwater/roman_open_water/"
links_water <- scrape_download_links(water_repo_url, "roman_open_water")
download_repo_files(water_repo_url, links_water, here(data_dir, "water_shp"))

empire_extent <- st_read(here(data_dir, "roman_empire_ad_14_extent.shp"))
empire_provinces <- st_read(here(data_dir, "roman_empire_ad_14_provinces.shp"))
water <- st_read(here(data_dir, "water_shp", "roman_open_water.shp"))

ggplot(empire_extent) +
  geom_sf(data = water, fill = "blue") +
  geom_sf() +
  geom_point(data = NULL,
             aes(x = 6.83, y = 50.93), col = "red", size = 5)


#' https://www.interkart.de/alte-karten-globen/alte-regionen-und-landkarten/alte-historische-karten-italien/1850-das-romische-reich-unter-augustus-und-bis-zum-tode-trajan-an16b003.html#sirv-viewer-102042373600


#' Roman Open Water:
#' http://awmc.unc.edu/awmc/map_data/shapefiles/physical_data/openwater/roman_open_water/





provinces <- tibble(province = c("Italia", "Hispania"),
                   coordinates = c(
                     st_geometry(st_point(c(-3.9, 40.0))),
                     st_geometry(st_point(c( 8.6, 42.1)))
                     ))



provinces <- tibble(province = c("Italia", "Hispania"),
                   coordinates = c(
                     st_geometry(st_point(c( 12.0, 42.1))),
                     st_geometry(st_point(c(-3.9, 40.0)))
                   ),
                   angle = c(320, 5))


ggplot(empire_extent) +
  geom_sf(fill = lighten("#D0BDA6", 0.9), linetype = "dashed", size = 0.05) +
  geom_sf(aes(col = factor(OBJECTID %% 4)),
          fill = NA, size = 1, show.legend = FALSE) +
  geom_point(data = NULL,
             aes(x = 6.83, y = 50.93), col = "red", size = 5) +
  geom_sf_text(data = provinces,
                aes(geometry = coordinates,
                    label = str_to_upper(province),
                    angle = angle),
                hjust = 0.5, vjust = -0.5, family = "Forum", size = 7
               ) +
  scale_color_discrete_qualitative(palette = "Harmonic", alpha = 0.2) +
  coord_sf() +
  cowplot::theme_map() +
  theme(plot.background = element_rect(fill = "grey97"))
ggsave("foo.png", dpi = 600, width = 12, height = 10)

