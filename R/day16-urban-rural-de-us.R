pacman::p_load("tidyverse", "here", "glue", "ggtext", "sf", "osmdata", "patchwork")

## COLOGNE, GERMANY vs. COLOGNE, MINNESOTA, USA ##


## GEOMETRIES ==================================================================

crs <- "EPSG:4326"

get_geo_features <- function(place, crs = "EPSG:4326") {

  ## Shape
  message(glue("Get shape of {place}."))
  shape <- getbb(place, format_out = "sf_polygon")
  bbox <- getbb(place, format_out = "matrix")

  # If multipolygon is available use it, otherwise use polygon
  if (!is.null(shape[["multipolygon"]])) {
    shape <- shape[["multipolygon"]]
  } else if (!is.null(shape[["polygon"]])) {
    shape <- shape[["polygon"]]
  } else {
    # ...
  }
  st_crs(shape) <- crs

  # Get streets
  message(glue("Get street features for {place}."))
  highway_features <- opq(place) %>%
    add_osm_feature(key = "highway") %>%
    osmdata_sf()

  st_crs(highway_features$osm_lines) <- crs

  message(glue("Intersect street features and shape for {place}"))
  highway_features_filtered <- highway_features$osm_lines %>%
    filter(., st_intersects(., shape, sparse = FALSE)[, 1]) %>%
    st_intersection(shape)

  # return results in a list
  list("shape" = shape,
       "bbox" = bbox,
       "highway_features" = highway_features,
       "highway_features_filtered" = highway_features_filtered)

}

features_us <- get_geo_features("Cologne, Minnesota, USA")
features_de <- get_geo_features("Cologne, Germany")
write_rds(features_de, here("data", "day16_features_de.rds"))

ggplot() +
  geom_sf(data = features_us$shape) +
  geom_sf(data = features_us$highway_features_filtered,
          col = "red")

street_types <- list(
  large = c("motorway", "primary", "motorway_link", "primary_link"),
  medium = c("secondary", "tertiary", "secondary_link", "tertiary_link"),
  small = c("residential", "living_street", "unclassified", "service", "footway")
)


street_plot <- function(feature_set, place_name, info_str, fill_color) {
  feature_set$shape$place_name <- place_name
  ggplot() +
    geom_sf(data = feature_set$shape, fill = fill_color, col = "grey89", size = 0.3) +
    # geom_sf(data = filter(feature_set$highway_features_filtered, !highway %in% unlist(street_types)),
    #         size = 0.1, alpha = 0.4, col = "#e9e9e9") +
    # geom_sf(data = filter(feature_set$highway_features_filtered, highway %in% street_types$small),
    #         size = 0.1, alpha = 0.8, col = "#e9e9e9") +
    geom_sf(data = filter(feature_set$highway_features_filtered, highway %in% street_types$medium),
            size = 0.15, col = "#e9e9e9") +
    geom_sf(data = filter(feature_set$highway_features_filtered, highway %in% street_types$large),
            size = 0.4, col = "#f5f5f5") +
    facet_wrap(vars(place_name)) +
    cowplot::theme_map(font_family = "Roboto") +
    theme(text = element_text(color = "grey92"),
          plot.title = element_text(color = "white",
                                    family = "Oswald",
                                    face = "plain",
                                    size = 42,
                                    hjust = 0.5,
                                    margin = margin(t = 6, b = 12)),
          plot.subtitle = element_textbox_simple(size = 16,
                                                 hjust = 0.5,
                                                 margin = margin(t = 4, b = 0)),
          plot.caption = element_textbox_simple(size = 10,
                                                hjust = 0.5,
                                                margin = margin(t = 8, b = 8)),
          strip.background = element_rect(color = NA, fill = fill_color),
          strip.text = element_markdown(color = "white", face = "bold",
                                        margin = margin(t = 8, b = 8)),
          panel.background = element_rect(color = NA,
                                          fill = alpha(fill_color, 0.8)))
}


info_str_us <-
  "**Population:** 1,981 (2020)<br>
  **Area:** 4.95 km<sup>2</sup><br>
  **Population density: 387.59 inhabitants/km<sup>2</sup>**
  "

p_us <- street_plot(features_us, "Cologne, Minnesota", info_str = info_str_us, fill_color = "darkgreen")
ggsave("foo_us.png", plot = p_us, device = ragg::agg_png, width = 10, height = 10)

info_str_de <-
  "**Population:** x (y)<br>
  **Area:** a.bc km<sup>2</sup><br>
  **Population density: xyz inhabitants/km<sup>2</sup>**
  "

p_de <- street_plot(features_de, "Cologne, Germany", info_str = info_str_de, fill_color = "grey12")
ggsave("foo_de.png", plot = p_de, device = ragg::agg_png, width = 10, height = 10)

p_us_de <- p_us + p_de +
  plot_annotation(title = "Two Cities of Cologne") +
  plot_layout(widths = c(1, 1.5), heights = c(1, 1))
ggsave("foo_us_de.png", plot = p_us_de, device = ragg::agg_png, width = 10, height = 8)

