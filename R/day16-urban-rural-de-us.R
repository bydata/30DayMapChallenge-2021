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

#' In order to get the same extent for both facets despite different shapes,
#' we have to define a buffer box. Since Cologne, DE has the larger area, we take
#' its bounding box as a reference
box = list(
  "width" = features_de$bbox["x", "max"] - features_de$bbox["x", "min"],
  "height" = features_de$bbox["y", "max"] - features_de$bbox["y", "min"]
)

# Create the plot for given location with given features
street_plot <- function(feature_set, place_name, info_str, fill_color,
                        box = NULL, use_box_aspect_ratio = TRUE) {
  feature_set$shape$place_name <- place_name
  feature_set$shape$facet_label <- glue("<b style='font-weight:bold; font-size: 20pt'>
  {place_name}</b><br><br>{info_str}")

  # Add a buffer to the city's bounding box
  width <-  feature_set$bbox["x", "max"] - feature_set$bbox["x", "min"]
  height <- feature_set$bbox["y", "max"] - feature_set$bbox["y", "min"]
  if (!missing(box)) {
    aspect_ratio <- ifelse(use_box_aspect_ratio,
                           (box$width / box$height) / (width/height),
                           1)
    print(aspect_ratio)
    coord_limits <- list(
      x = c("min" = feature_set$bbox["x", "min"] - aspect_ratio * (box$width - width) / 2,
            "max" = feature_set$bbox["x", "max"] + aspect_ratio * (box$width - width) / 2),
      y = c("min" = feature_set$bbox["y", "min"] - (box$height - height) / 2,
            "max" = feature_set$bbox["y", "max"] + (box$height - height) / 2)
    )
  } else {
    coord_limits <- list(
      x = c("min" = feature_set$bbox["x", "min"], "max" = feature_set$bbox["x", "max"]),
      y = c("min" = feature_set$bbox["y", "min"], "max" = feature_set$bbox["y", "max"])
    )
  }

  print(coord_limits)
  print(feature_set$bbox)

  ggplot() +
    geom_sf(data = feature_set$shape, fill = fill_color, col = "grey89", size = 0.3) +
    geom_sf(data = filter(feature_set$highway_features_filtered, highway %in% street_types$small),
            size = 0.1, alpha = 0.8, col = "#e9e9e9") +
    geom_sf(data = filter(feature_set$highway_features_filtered, highway %in% street_types$medium),
            size = 0.15, col = "#e9e9e9") +
    geom_sf(data = filter(feature_set$highway_features_filtered, highway %in% street_types$large),
            size = 0.4, col = "#f5f5f5") +
    facet_wrap(vars(facet_label)) +
    coord_sf(xlim = coord_limits$x, ylim = coord_limits$y) +
    cowplot::theme_map(font_family = "Roboto") +
    theme(text = element_text(color = "grey92"),
          strip.background = element_rect(color = NA, fill = fill_color),
          strip.text = element_markdown(color = "white", hjust = 0,
                                        lineheight = 1.4,
                                        margin = margin(t = 12, l = 12, b = 12)),
          panel.background = element_rect(color = NA,
                                          fill = alpha(fill_color, 0.4)),
          plot.background = element_rect(color = NA, fill = "white"))
}

# Plot for Cologne, Minnesota
pop_density <- (1981 / 4.95) %>%
  scales::number(accuracy = 1, big.mark = ",")
info_str_us <- glue(
  "**Population:** 1,981<br>
  **Area:** 4.95 km<sup>2</sup><br>
  **Population density:** {pop_density} inhabitants/km<sup>2</sup>
  ")
p_us <- street_plot(features_us, "Cologne, Minnesota", info_str = info_str_us,
                    fill_color = "#0a5e0a", box = box)


# Plot for Cologne, Germany
pop_density <- (1083498 / 405.15) %>%
  scales::number(accuracy = 1, big.mark = ",")
info_str_de <- glue(
  "**Population:** 1,083,498<br>
  **Area:** 405.15 km<sup>2</sup><br>
  **Population density:** {pop_density} inhabitants/km<sup>2</sup>
  ")
p_de <- street_plot(features_de, "Cologne, Germany", info_str = info_str_de,
                    fill_color = "grey12", box = box)

# Combine the plots in one chart
p_us_de <- p_us + p_de +
  plot_annotation(title = "Two Cities of Cologne",
                  subtitle = NULL,
                  caption = "Data: **OpenStreetMap contributors**,
                  Population as of 2020 (Source: **Wikipedia**) |
                  Visualization: **Ansgar Wolsing**",
                  theme = theme(
                    text = element_text(color = "grey35", family = "Roboto"),
                    plot.title = element_markdown(color = "grey4",
                                                  family = "Oswald",
                                                  face = "plain",
                                                  size = 42,
                                                  hjust = 0.5,
                                                  margin = margin(t = 6, b = 12)),
                    plot.subtitle = element_textbox_simple(size = 14,
                                                     hjust = 0.5,
                                                     halign = 0.5,
                                                     margin = margin(t = 4, b = 12)),
                    plot.caption = element_markdown(size = 10,
                                                    hjust = 0.5,
                                                    margin = margin(t = 8, b = 8))
                    )
  ) +
  plot_layout()
ggsave(here("plots", "day16-two-cities-of-cologne.png"), plot = p_us_de, device = ragg::agg_png, width = 10, height = 8)

