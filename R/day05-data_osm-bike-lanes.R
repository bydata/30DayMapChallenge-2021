pacman::p_load("tidyverse", "here", "glue", "ggtext", "colorspace", "scico",
               "sf", "osmdata", "gganimate")

## GEOMETRIES ==================================================================

crs <- "EPSG:4326"

## Shape Cologne
shape_cgn <- getbb("Cologne, Germany", format_out = "sf_polygon")
st_crs(shape_cgn) <- crs

#' Which keys to retrieve?
#' https://help.openstreetmap.org/questions/64879/get-all-bicycle-infrastructure-for-a-city
#' all roads that have additional tags indicating cycle infrastructure
#'      (cycleway=lane and cycleway=track)
#' all highway=cycleway
#' all highway=footway and highway=path that allow bicycle use or are
#'      intended for such (bicycle=yes, bicycle=designated, bicycle=official)

bike_lanes <- opq(bbox = shape_cgn) %>%
  add_osm_feature(key = "cycleway", value = "lane") %>%
  add_osm_feature(key = "cycleway", value = "track") %>%
  add_osm_feature(key = "highway", value = "cycleway") %>%
  add_osm_feature(key = "highway", value = "footway") %>%
  add_osm_feature(key = "highway", value = "path") %>%
  osmdata_sf()

highway_features <- opq(bbox = shape_cgn) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

data_dir <- here("data", "cologne_bikelanes")
# if (!dir.exists(data_dir)) {
#   dir.create(data_dir)
#   message(glue("Created folder {data_dir}"))
#   write_rds(bike_lanes,
#             here(data_dir, "bike_lanes_osm_raw.rds"),
#             compress = "gz")
# } else {
#   bike_lanes <- read_rds(here(data_dir, "bike_lanes_osm_raw.rds"))
# }

bike_lanes_lines <- bike_lanes$osm_lines %>%
  select(osm_id, name, bicycle, bridge, bridge.name,
         cycleway, starts_with("cycleway\\."),
         highway,
         surface, zone.maxspeed, geometry)

#' Filtering for highways
#' (only keep key = "highway", value = "footway|path" if
#' bicycle=yes|designated|official)

bike_lanes_lines %>% st_drop_geometry() %>% count(cycleway, sort = TRUE)
bike_lanes_lines %>% st_drop_geometry() %>% count(highway, sort = TRUE)

bike_lanes_lines_filtered <- bike_lanes_lines %>%
  filter(
    !is.na(cycleway) |
      highway == "cycleway" |
      highway == "path" & bicycle %in% c("yes", "designated", "official") |
      highway == "footway" & bicycle %in% c("yes", "designated", "official")

  )
st_crs(bike_lanes_lines_filtered) <- crs

# library(tictoc)
# tic("st_intersection only")
# bike_lanes_cgn <- st_intersection(shape_cgn, bike_lanes_lines_filtered)
# toc()

tic("st_intersects + st_intersection")
bike_lanes_cgn <- bike_lanes_lines_filtered %>%
  filter(., st_intersects(., shape_cgn, sparse = FALSE)[, 1]) %>%
  st_intersection(shape_cgn)
toc()
write_rds(bike_lanes_cgn, here(data_dir, "bike_lanes_cgn.rds"))

tic("highways: st_intersects + st_intersection")
highway_features_cgn <- highway_features$osm_lines %>%
  filter(., st_intersects(., shape_cgn, sparse = FALSE)[, 1]) %>%
  st_intersection(shape_cgn)
toc()
write_rds(highway_features_cgn, here(data_dir, "highway_features_cgn.rds"))


#' Check which cycleway OSM values are present
#' Exhaustive list of values: https://wiki.openstreetmap.org/wiki/Key:cycleway

bike_lanes_cgn %>%
  st_drop_geometry() %>%
  count(surface, sort = TRUE)


plot_titles <- list(
  title = "Bike Lanes in Cologne",
  subtitle = "Bike Lanes are colored by their surface type",
  caption = "Data: **OpenStreetMap** | Visualization: **Ansgar Wolsing**"
)

p <- bike_lanes_cgn %>%
  mutate(
    surface = ifelse(is.na(surface), "Other/Unknown", surface),
    surface_grp = fct_lump(surface, prop = 0.05,
                           other_level = "Other/Unknown")) %>%
ggplot() +
  geom_sf(data = shape_cgn,
          fill = "grey21") +
  geom_sf(data = filter(highway_features_cgn, highway != "motorway"),
          size = 0.2, col = "grey62") +
  geom_sf(data = filter(highway_features_cgn, highway == "motorway"),
          size = 0.4, col = "grey70") +
  geom_sf(aes(col = surface_grp, fill = surface_grp),
          size = 0.2
  ) +
  paletteer::scale_color_paletteer_d("jcolors::pal3",
                                     labels = as_labeller(function(x)
                                       str_to_title(str_replace_all(x, "_", " ")))) +
  paletteer::scale_fill_paletteer_d("jcolors::pal3") +
  guides(color = guide_legend(override.aes = list("size" = 1.5)),
         fill = "none") +
  labs(title = plot_titles$title,
       subtitle = plot_titles$subtitle,
       caption = plot_titles$caption,
       color = "Surface type") +
  cowplot::theme_map() +
  theme(plot.background = element_rect(color = NA, fill = "grey50"),
        text = element_text(color = "grey92", family = "Roboto"),
        plot.title = element_markdown(color = "white", family = "Chivo",
                                      size = 24,
                                      margin = margin(t = 6, b = 12)),
        plot.subtitle = element_textbox_simple(),
        plot.caption = element_textbox_simple(size = 8),
        legend.position = c(0.7, 0.9),
        legend.title = element_text(size = 12, family = "Chivo"),
        legend.text = element_text(size = 10, family = "Roboto Light"),
        legend.key.width = unit(2, "mm"))
ggsave(here("plots", "day05_osmdata_bike-lanes.png"),
       plot = p, dpi = 600, width = 10, height = 8)
