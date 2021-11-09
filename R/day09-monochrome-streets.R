pacman::p_load("tidyverse", "here", "glue", "ggtext", "sf", "osmdata")

## GEOMETRIES ==================================================================

crs <- "EPSG:4326"

## Shape Cologne
shape_cgn <- getbb("Cologne, Germany", format_out = "sf_polygon")
st_crs(shape_cgn) <- crs

# Get streets
highway_features <- opq(bbox = shape_cgn) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

highway_features_cgn <- highway_features$osm_lines %>%
  filter(., st_intersects(., shape_cgn, sparse = FALSE)[, 1]) %>%
  st_intersection(shape_cgn)

street_types <- list(
  large = c("motorway", "primary", "motorway_link", "primary_link"),
  medium = c("secondary", "tertiary", "secondary_link", "tertiary_link"),
  small = c("residential", "living_street", "unclassified", "service", "footway")
)


## PLOT ========================================================================

plot_titles <- list(
  title = "STREETS OF COLOGNE",
  subtitle = glue("{sp::dd2dms(round(coords_cathedral['y', 'min'], 2), NS = TRUE)},
    {sp::dd2dms(round(coords_cathedral['x', 'min'], 2))}"),
  caption = "Data: **OpenStreetMap contributors** | Visualization: **Ansgar Wolsing**"
)

p <- ggplot() +
  geom_sf(data = shape_cgn,
          fill = "#1c1c1c") +
  geom_sf(data = filter(highway_features_cgn, !highway %in% unlist(street_types)),
          size = 0.1, alpha = 0.4, col = "#e9e9e9") +
  geom_sf(data = filter(highway_features_cgn, highway %in% street_types$small),
          size = 0.1, alpha = 0.8, col = "#e9e9e9") +
  geom_sf(data = filter(highway_features_cgn, highway %in% street_types$medium),
          size = 0.15, col = "#e9e9e9") +
  geom_sf(data = filter(highway_features_cgn, highway %in% street_types$large),
          size = 0.4, col = "#f5f5f5") +
  labs(title = plot_titles$title,
       subtitle = plot_titles$subtitle,
       caption = plot_titles$caption) +
  cowplot::theme_map(font_family = "Roboto") +
  theme(plot.background = element_rect(color = NA, fill = "grey1"),
        text = element_text(color = "grey92"),
        plot.title = element_text(color = "white",
                                  family = "Oswald",
                                  face = "plain",
                                      size = 42,
                                      margin = margin(t = 6, b = 12)),
        plot.subtitle = element_textbox_simple(size = 16,
                                               hjust = 0.5,
                                               margin = margin(t = 4, b = 0)),
        plot.caption = element_textbox_simple(size = 10,
                                              margin = margin(t = 8, b = 8)))
ggsave(here("plots", "day09_monochrome-streets.png"),
       plot = p, dpi = 600, width = 10, height = 10)

