pacman::p_load("tidyverse", "here", "glue", "ggtext", "sf", "paletteer",
               "osmdata", "lubridate", "gganimate", "colorspace")

## GET DATA ====================================================================

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

features_cgn <- get_geo_features("Cologne, Germany")
write_rds(features_cgn, here("data", "day19_features_cgn_de.rds"))


features_cgn <- read_rds(here("data", "day19_features_cgn_de.rds"))
bike_counters <- read_rds(here("data", "counters_677_daily.rds"))

# Rhine

# Get water
water_features <- opq(bbox = features_cgn$shape) %>%
  add_osm_feature(key = "water") %>%
  osmdata_sf()
rhine <- opq(bbox = features_cgn$shape) %>%
  add_osm_feature(key = "name:de", value = "Rhein") %>%
  osmdata_sf()
rhine_cgn <- rhine$osm_multilines %>%
  st_intersection(features_cgn$shape)
water_features_polygons_cgn <- st_intersection(water_features$osm_polygons, features_cgn$shape) %>%
  st_filter(rhine_cgn,
            .predicate = function(x, y) st_is_within_distance(x, y, dist = 0.1))


# Correct typo in street name
bike_counters$info <- bike_counters$info %>%
  mutate(name = case_when(
    name == "09 Alphons-Sibermann-Weg" ~ "09 Alphons-Silbermann-Weg",
    name == "Zülpicher Neu kpl" ~ "Zülpicher Straße",
    name == "07 Alfred Schütte kpl" ~ "Alfred-Schütte-Allee",
    name == "Universitätsstr. kpl" ~ "Universitätsstraße",
    TRUE ~ name))

# Bike counter location (full streets)
bike_counter_locations <-
  str_remove_all(bike_counters$info$name,
                 "(\\d{2}|kpl\\.?|Rad|Neu\\b)") %>%
  str_trim()

(matched_locations <- features_cgn$highway_features_filtered %>%
  st_drop_geometry() %>%
  filter(name %in% bike_counter_locations) %>%
  pull(name) %>%
  unique())
length(matched_locations)
# Which locations have not been matched in the highway features set?
setdiff(bike_counter_locations, matched_locations)

# join bike counter info and data
bike_counters_prep <- inner_join(bike_counters$info, bike_counters$data,
                                   by = c("idPdc" = "id")) %>%
  arrange(idPdc, date) %>%
  mutate(date = as_date(date)) %>%
  group_by(idPdc) %>%
  # rolling average in 7-day window
  mutate(count_roll7 = zoo::rollmean(comptage, k = 7, align = "right", fill = NA),
         count_roll28 = zoo::rollmean(comptage, k = 28, align = "right", fill = NA)) %>%
  ungroup() %>%
  filter(date >= as_date("2020-01-01"), date <= as_date("2020-12-31"))

## calculate bounding box from bike counter locations
bike_counters_centroid <- bike_counters_prep %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_centroid() %>%
  select(geometry) %>%
  slice(1)

bike_counters_circle <- st_buffer(bike_counters_centroid, dist = 0.1)
st_crs(bike_counters_circle) <- "EPSG:4326"

features_cgn$highway_features_filtered_circle <-
  st_intersection(features_cgn$highway_features_filtered, bike_counters_circle)

street_types <- list(
  large = c("motorway", "primary", "motorway_link", "primary_link"),
  medium = c("secondary", "tertiary", "secondary_link", "tertiary_link"),
  small = c("residential", "living_street", "unclassified", "service", "footway")
)

p <- ggplot() +
  geom_sf(data = features_cgn$shape, fill = "grey84", col = NA) +
  geom_sf(data = water_features_polygons_cgn,
          fill = "steelblue", alpha = 0.8, col = NA) +
  # geom_sf(data = filter(features_cgn$highway_features_filtered, highway %in% street_types$small),
  #         size = 0.15, col = "#e9e9e9") +
  # geom_sf(data = filter(features_cgn$highway_features_filtered, highway %in% street_types$medium),
  #         size = 0.25, col = "#e9e9e9") +
  geom_sf(data = filter(features_cgn$highway_features_filtered, highway %in% street_types$large),
          size = 0.5, col = "#f5f5f5") +
  geom_point(data = bike_counters_prep,
             aes(x = lon, y = lat, size = count_roll28, fill = count_roll7),
             shape = 21, col = "white", stroke = 0.1) +
  # scale_fill_viridis_c(option = "magma") +
  scico::scale_fill_scico(palette = "berlin") +
  scale_size_continuous(range = c(0.5, 10)) +
  # coord_sf(xlim = c(6.83, 7.01), ylim = c(50.88, 50.97)) +
  coord_sf(xlim = c(6.85, 7.00), ylim = c(50.90, 50.97)) +
  guides(size = guide_legend(override.aes = list(color = "grey50"))) +
  labs(title = "One year of bike traffic in Cologne",
       subtitle = "Daily bike traffic as measured by bicycle counters at 15 locations
       in Cologne, Germany, in 2020<br><br>
       <b style='color:grey70; font-size: 20pt'>{frame_time}</b>",
       caption = "Data: **OpenStreetMap** contributors, **Stadt Köln/eco-visio.net** |
       Visualization: **Ansgar Wolsing**",
       fill = "7-day rolling daily avg.", size = "28-day rolling daily avg.") +
  # cowplot::theme_map(font_family = "Roboto", font_size = 16) +
  theme_minimal(base_family = "Roboto", base_size = 14) +
  theme(plot.background = element_rect(color = NA, fill = "grey97"),
        panel.background = element_rect(color = NA, fill = "grey93"),
        legend.position = "bottom",
        legend.title = element_text(color = "grey45", size = 8, face = "bold"),
        legend.text = element_text(color = "grey45", size = 7),
        legend.title.align = 0.5,
        legend.box.just = 0.5,
        legend.key.height = unit(2, "mm"),
        text = element_text(color = "grey35"),
        plot.title = element_markdown(color = "black", face = "bold"),
        plot.subtitle = element_textbox_simple(margin = margin(t = 6, b = 12)),
        plot.caption = element_textbox_simple(margin = margin(t = 12, b = 12)),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())
ggsave("foo2.png", plot = p, dpi = 150, width = 1200, height = 1200, units = "px")

p_anim <- p +
  transition_time(date) +
  enter_fade() +
  exit_fade()

anim <- animate(p_anim, fps = 24, nframes = 365 / 2,
                width = 1200, height = 1200, res = 150, device = "ragg_png")

anim_save(here("plots", "day20-movement-bike_counters.gif"), anim)





## With streets highlighted directly ---------

bike_counter_features <- bike_counters_prep %>%
  mutate(name = str_remove_all(name, "(\\d{2}|kpl\\.?|Rad|Neu\\b)") %>%
           str_trim()) %>%
  inner_join(features_cgn$highway_features_filtered, by = "name")



p <- ggplot() +
  geom_sf(data = features_cgn$shape, fill = "grey70", col = NA) +
  geom_sf(data = filter(features_cgn$highway_features_filtered, highway %in% street_types$medium),
          size = 0.15, col = "#e9e9e9") +
  geom_sf(data = filter(features_cgn$highway_features_filtered, highway %in% street_types$large),
          size = 0.4, col = "#f5f5f5") +
  # Bike counters
  geom_sf(data = bike_counter_features,
          aes(geometry = geometry,
              col = comptage, size = comptage),
          # col = "deeppink", size = 1
          ) +
  scale_color_viridis_d(option = "magma") +
  scale_size_continuous(range = c(0.5, 2)) +
  coord_sf(xlim = c(6.83, 7.01), ylim = c(50.88, 51.01)) +
  labs(title = "Bike traffic in Cologne",
       subtitle = "Daily bike traffic as measured by bike counters at 15 locations
       in Cologne, Germany<br><br>
       <b style='color:grey70; font-size: 20pt'>{frame_time}</b>") +
  cowplot::theme_map(font_family = "Roboto", font_size = 16) +
  theme(plot.background = element_rect(color = NA, fill = "white"),
        panel.background = element_rect(color = NA, fill = "white"),
        legend.position = "bottom",
        legend.text = element_text(color = "grey85"),
        text = element_text(color = "grey90"),
        plot.title = element_markdown(color = "white"),
        plot.subtitle = element_markdown())
p

p_anim <- p +
  transition_time(date) +
  enter_fade() +
  exit_fade()

anim <- animate(p_anim, fps = 24,
                width = 900, height = 900, res = 100)

anim_save(here("plots", "day20-movement-bike_counters-lines.gif"), anim)



