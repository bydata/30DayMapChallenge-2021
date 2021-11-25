pacman::p_load("tidyverse", "here", "glue", "ggtext", "colorspace", "sf",
               "rvest", "raster", "rnaturalearth")

#' http://awmc.unc.edu/awmc/map_data/shapefiles/political_shading/
#'
#' For Shapefiles, The suggested citation is the following:
#' Ancient World Mapping Center. “Shapefile name”. <http://awmc.unc.edu/wordpress/map-files/>
#' [Accessed: April 23, 2013 11:00am]


## Scrape & download shapefiles ================================================

# Scrape relevant links
scrape_download_links <- function(url, download_link_regex) {
  page <- read_html(url)
  links <- html_nodes(page, css = "a") %>% html_attr("href")
  grep(download_link_regex, links, value = TRUE)
}

download_repo_files <- function(base_url, rel_links, data_dir, force = FALSE) {
  if (!dir.exists(data_dir) | force) {
    dir.create(data_dir)
    walk(rel_links, ~download.file(paste0(base_url, .x),
                                   destfile = here(data_dir, .x)))
  } else {
    message(glue("Folder {data_dir} already exists. Skipping download."))
  }
}


url <- "http://awmc.unc.edu/awmc/map_data/shapefiles/political_shading/"
links_roman_empire <- scrape_download_links(url, "roman_empire_(60_bc|bc_60|ad_14|ad_117|ad_75|ad_69)")
# Download shapefiles for extent and provinces
data_dir <- here("data", "roman_empire")
download_repo_files(url, links_roman_empire, data_dir, force = TRUE)

# # ... shapefile for water
# water_repo_url <- "http://awmc.unc.edu/awmc/map_data/shapefiles/physical_data/openwater/roman_open_water/"
# links_water <- scrape_download_links(water_repo_url, "roman_open_water")
# download_repo_files(water_repo_url, links_water, here(data_dir, "water_shp"))

empire_extent <- st_read(here(data_dir, "roman_empire_ad_69_extent.shp")) %>%
  st_make_valid()

ggplot(empire_extent) +
  geom_sf() +
  geom_point(data = NULL,
             aes(x = 6.83, y = 50.93), col = "red", size = 5)


## Hillshade -------------------------------------------------------------------
dir_raster <- here("data", "raster_data")
ne_type <- "SR_50M"
if (!file.exists(here(dir_raster, ne_type))) {
  ne_download(scale = 50, type = ne_type, category = "raster",
              destdir = here(dir_raster, ne_type), load = FALSE)
}
hillshade_raster <- ne_load(scale = 50, category = "raster", type = "",
                  destdir = dir_raster, file_name = ne_type, returnclass = "sf")

hillshade_raster_agg <- raster::aggregate(hillshade_raster, fact = 3)

hillshade <- hillshade_raster_agg %>%
  crop(extent(-11, 40, 22, 55)) %>%
  as("SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(value = ne_type) %>%
  mutate(geometry = map2(x, y, ~st_point(c(.x, .y)))) %>%
  st_as_sf()
st_crs(hillshade) <- "EPSG:4326"
hillshade_sf <- st_as_sf(hillshade)

hillshade_empire_sf <- hillshade_sf %>%
  st_make_valid() %>%
  st_join(empire_extent, left = FALSE)


## Shapefiles for continents in the background ---------------------------------

europe <- ne_countries(continent = "Europe", returnclass = "sf")
asia_africa <- ne_countries(continent = c("Africa", "Asia"), returnclass = "sf")

# Labels for provinces with coordinates and font size
provinces <- tibble(province = c("Italia", "Hispania", "Gallia", "Asia",
                                 "Macedonia", "Dalmatia", "Aegyptus"),
                    coordinates = c(
                      st_geometry(st_point(c(12.2, 42.1))),
                      st_geometry(st_point(c(-3.9, 40.0))),
                      st_geometry(st_point(c(2.3, 46.3))),
                      st_geometry(st_point(c(29.1, 38))),
                      st_geometry(st_point(c(21.8, 42.0))),
                      st_geometry(st_point(c(17.3, 44.4))),
                      st_geometry(st_point(c(29.4, 29.7)))
                    ),
                    angle = c(320, 5, -5, 2, 67, -29, 0),
                    font_size = c(4, 4, 6, 4, 3, 3, 3))

other_countries_color <- lighten(desaturate("#EFE3D5", 0.2), 0.2)

plot_titles <- list(
  title = "IMPERIUM ROMANUM",
  subtitle = "The first urban settlement on the grounds of modern-day Cologne was
  *Oppidum Ubiorum*, founded in 38 BC by the Ubii, a Cisrhenian Germanic tribe.
  In AD 50, the Romans founded *Colonia Claudia Ara Agrippinensium* (Cologne) on
  the river Rhine and the city became the provincial capital of Germania Inferior in AD 85.
  The map shows the extent of the Roman Empire in AD 69.",
  caption = "Source: *Ancient World Mapping Center.* (roman_empire_ad_69_extent.shp),
  Subtitle: Wikipedia | Visualization: *Ansgar Wolsing*"
)

p <- ggplot(empire_extent) +
  geom_sf(data = europe,
          col = NA, fill = other_countries_color) +
  geom_sf(data = asia_africa,
          col = NA, fill = other_countries_color) +
  geom_sf(data = hillshade_empire_sf,
          aes(col = value, fill = value), size = 0.5, alpha = 0.5,
          show.legend = FALSE) +
  geom_sf(fill = "#faf3dc",
          # linetype = "dashed",
          size = 0.05,
          alpha = 0.8) +
  # geom_sf(col = alpha("darkblue", 0.1),
  #         fill = NA, size = 1.2,
  #         show.legend = FALSE) +
  geom_sf(col = "grey98", alpha = 0.5,
          fill = NA, size = 0.6,
          show.legend = FALSE) +
  geom_point(data = NULL,
             aes(x = 6.83, y = 50.93), col = "black", size = 4) +
  geom_text(data = NULL,
             aes(x = 7.7, y = 51.2),
            label = "CCAA", fontface = "bold",
            col = "black", hjust = 0, size = 5,
            family = "Cardo") +
  geom_sf_text(data = provinces,
                aes(geometry = coordinates,
                    label = str_to_upper(province),
                    angle = angle, size = font_size),
                hjust = 0.5, vjust = -0.5, family = "Libre Baskerville",
               col = "grey37"
               ) +
  scale_size_identity() + # size the province labels according to the font_size column
  scale_color_continuous_sequential(palette = "Grays") +
  coord_sf(xlim = c(-10, 38), ylim = c(26, 58)) +
  labs(title = plot_titles$title,
       subtitle = plot_titles$subtitle,
       caption = plot_titles$caption) +
  cowplot::theme_map(font_family = "Cardo") +
  theme(plot.background = element_rect(color = NA, fill = "grey97"),
        panel.background = element_rect(color = "grey60",
                                        # fill = lighten("#faf3dc", 0.9)
                                        fill = alpha("steelblue", 0.8)
                                        ),
        plot.title = element_text(family = "Forum", face = "plain", size = 28,
                                  hjust = 0.5, margin = margin(t = 4, b = 12)),
        plot.subtitle = element_textbox_simple(family = "Cardo", face = "plain",
                                               size = 12, hjust = 0.5,
                                               margin = margin(b = 16)),
        plot.caption = element_textbox_simple(size = 9,
                                              margin = margin(t = 8)))
ggsave(here("plots", "day24-historical.png"), dpi = 600, width = 8, height = 8)

