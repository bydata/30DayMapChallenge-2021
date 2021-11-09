pacman::p_load("tidyverse", "here", "glue", "ggtext", "colorspace",
               "sf", "osmdata", "geojsonsf", "jsonlite")

## GEOMETRIES ==================================================================
## Area of Cologne
coords_cgn <- getbb("Cologne, Germany", format_out = "sf_polygon")
coords_cathedral <- getbb("Kölner Dom, Cologne, Germany",
                          featuretype = "church")
coords_rhine <- opq(bbox = 'Cologne, Germany') %>%
  add_osm_feature(key = 'name:de', value = 'Rhein', value_exact = TRUE) %>%
  osmdata_sf()
coords_rhine_cgn <- sf::st_intersection(coords_cgn, coords_rhine$osm_lines)


## GET DATA ====================================================================
#' Source: https://www.offenedaten-koeln.de/dataset/adresse
#' Source: https://www.offenedaten-koeln.de/dataset/geb%C3%A4udemodell-stadt-k%C3%B6ln-2010
#'    Projection: 31466 - DHDN / Gauss-Kruger zone 2. VG

# url_addresses <- "https://www.offenedaten-koeln.de/sites/default/files/Adresse_0.zip"
# filepath_addresses_zip <- here("data", "Adresse_0.zip")
# if (!file.exists(filepath_addresses_zip)) {
#   download.file(url_addresses, destfile = filepath_addresses_zip, mode = "wb")
#   unzip(filepath_addresses_zip, exdir = here("data", "addresses"))
# }
# filepath_addresses_shp <- here("data", "addresses", "Adresse.shp")
# shp <- st_read(filepath_addresses_shp)

urls_buildings <- paste0("https://www.offenedaten-koeln.de/sites/default/files/dachansicht_lod2_part",
                         1:3, ".zip")
filepaths_buildings_zip <- here("data", "cologne_buildings",
                               paste0("dachansicht_lod2_part", 1:3, ".zip"))
folder_buildings <- here("data", "cologne_buildings")
filepath_buildings_dataframe <- here(folder_buildings, "cologne_buildings.rds")

if (!file.exists(filepath_buildings_dataframe)) {
  if (!file.exists(filepaths_buildings_zip[1])) {
    dir.create(folder_buildings)
    walk2(urls_buildings, filepaths_buildings_zip,
          ~download.file(url = .x, destfile = .y, mode = "wb"))
    walk(filepaths_buildings_zip, unzip, exdir =  folder_buildings)
  }

  filepaths_shp <- here(folder_buildings,
                        list.files(folder_buildings, pattern = ".*\\.shp$"))
  buildings <- map_dfr(filepaths_shp, st_read)

  # Save buildings dataframe with geometry
  write_rds(buildings, filepath_buildings_dataframe,
            compress = "gz")
} else {
  buildings <- read_rds(filepath_buildings_dataframe)
}

#' Set the coordinate reference system
#' According to comments/documentation: 31466 - DHDN / Gauss-Kruger zone 2. VG
st_crs(buildings$geometry) <- "EPSG:31466"
st_crs(buildings$geometry)

buildings2 <- st_zm(buildings, drop = TRUE, what = "ZM")


## PLOT ========================================================================

# Annotations
plot_titles <- list(
  title = "BUILDINGS OF COLOGNE",
  subtitle = glue("{sp::dd2dms(round(coords_cathedral['y', 'min'], 2), NS = TRUE)},
    {sp::dd2dms(round(coords_cathedral['x', 'min'], 2))}"),
  caption = "Data: **Open Data Cologne** (last update: 2021-10-29),
  **OpenStreetMap contributors** | Visualization: **Ansgar Wolsing**"
)

p <- ggplot(coords_cgn) +
  geom_sf(fill = "#d1a1e3", color = "grey91", size = 0.25) +
  # plot Rhine
  geom_sf(data = coords_rhine_cgn,
          aes(geometry = geometry),
          size = 2, col = "#854e99") +
  geom_sf(data = buildings2,
          aes(geometry = geometry),
          fill = "#482d52",
          color = NA
          ) +
  # scale_fill_manual(values = point_colors) +
  coord_sf() +
  labs(
    title = plot_titles$title,
    subtitle = plot_titles$subtitle,
    caption = plot_titles$caption
  ) +
  cowplot::theme_map(font_family = "Roboto") +
  theme(
    plot.background = element_rect(color = NA, fill = "#854e99"),
    legend.position = "top",
    legend.justification = "left",
    text = element_text(color = "grey92", lineheight = 1.3),
    plot.title = element_text(color = "white",
                              family = "Oswald",
                              face = "plain",
                              size = 48,
                              margin = margin(t = 6, b = 12)),
    plot.subtitle = element_textbox_simple(size = 16,
                                           hjust = 0.5,
                                           margin = margin(t = 4, b = 0)),
    plot.caption = element_textbox_simple(size = 10,
                                          margin = margin(t = 8, b = 8)))
ggsave(here("plots", "day09_monochrome_buildings.png"),
       plot = p, dpi = 600, width = 10, height = 10)

