pacman::p_load("tidyverse", "here", "glue", "ggtext", "colorspace", "scico",
               "sf", "osmdata", "geojsonsf", "jsonlite", "lubridate")


## LOAD GEOMETRY ===============================================================

#' https://www.offenedaten-koeln.de/dataset/stadtteile
#' https://www.offenedaten-koeln.de/sites/default/files/Stadtteil.zip
shp_districts <- st_read(here("data", "cologne_stadtteile", "Stadtteil", "Stadtteil.shp"))

#' https://offenedaten-koeln.de/dataset/stadtbezirke-koeln
url_boroughs <- "https://geoportal.stadt-koeln.de/arcgis/rest/services/Basiskarten/kgg/MapServer/4/query?where=objectid+is+not+null&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=4326&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=pjson"
boroughs_sf <- st_read(here("data", "cologne_stadtbezirke", "stadtbezirke_koeln.json"))


## CREATE A HONEYCOMB GRID =====================================================
#' Intro to creating hexmaps with sf
#' https://rpubs.com/dieghernan/beautifulmaps_I

shape <- st_transform(shp_districts, 3857) %>% select(STT_NAME)
initial <- shape
initial$index_target <- 1:nrow(initial)
target <- st_geometry(initial)

# Create the grid of hexagons
grid <- st_make_grid(target,
                     cellsize = 2.5 * 1000,
                     crs = st_crs(initial),
                     what = "polygons",
                     square = FALSE # for hex, TRUE for squares
)
# Add index, transform list to dataframe
grid <- st_sf(index = 1:length(lengths(grid)), grid)

# We identify the grids that belongs to a entity by assessing the centroid
cent_grid <- st_centroid(grid)
cent_merge <- st_join(cent_grid, initial["index_target"], left = FALSE)
grid_new <- inner_join(grid, st_drop_geometry(cent_merge))

# # Honeycomb
# Honeygeom <- aggregate(
#   grid_new,
#   by = list(grid_new$index_target),
#   FUN = min,
#   do_union = FALSE
# )

# # Lets add the df
# Honeycomb <- left_join(
#   Honeygeom %>%
#     select(index_target),
#   st_drop_geometry(initial)
# ) %>%
#   select(-index_target)


# ggplot(Honeycomb) +
#   geom_sf(aes(fill = STT_NAME), show.legend = FALSE) +
#   geom_sf_text(aes(label = STT_NAME), size = 2) +
#   scale_fill_scico_d(palette = 'lapaz') +
#   cowplot::theme_map()


## LOAD DATA ===================================================================
#' Child care centers in Cologne
#' Source: Offene Daten Köln,
#' https://www.offenedaten-koeln.de/dataset/kindertagesstaetten-koeln

url_kitas_private <- "https://geoportal.stadt-koeln.de/arcgis/rest/services/familie_partnerschaft_kinder/kitas/MapServer/0/query?where=objectid+is+not+null&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=4326&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=pjson"
url_kitas_public <- "https://geoportal.stadt-koeln.de/arcgis/rest/services/familie_partnerschaft_kinder/kitas/MapServer/1/query?where=objectid+is+not+null&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=4326&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=pjson"
kitas_private_sf <- st_read(url_kitas_private)
kitas_public_sf <- st_read(url_kitas_public)

# Calculates the total daily hours from opening hours
calculate_opening_duration <- function(from1, to1, from2, to2) {
  to1 - from1 + ifelse(!is.na(from2) & !is.na(to2), to2 - from2, seconds(0))
}

kitas_sf <- bind_rows(kitas_private_sf, kitas_public_sf, .id = "type") %>%
  mutate(type = ifelse(type == "1", "private", "public")) %>%
  mutate(across(starts_with("einrichtungoeff_"), hms),
         # set to missing if opening time is "00:00:00"
         across(starts_with("einrichtungoeff_"), ~na_if(.x, seconds(0))),
         opening_duration =
           calculate_opening_duration(einrichtungoeff_mo1_von,
                                     einrichtungoeff_mo1_bis,
                                     einrichtungoeff_mo2_von,
                                     einrichtungoeff_mo2_bis) +
           calculate_opening_duration(einrichtungoeff_di1_von,
                                      einrichtungoeff_di1_bis,
                                      einrichtungoeff_di2_von,
                                      einrichtungoeff_di2_bis) +
           calculate_opening_duration(einrichtungoeff_mi1_von,
                                      einrichtungoeff_mi1_bis,
                                      einrichtungoeff_mi2_von,
                                      einrichtungoeff_mi2_bis) +
           calculate_opening_duration(einrichtungoeff_do1_von,
                                      einrichtungoeff_do1_bis,
                                      einrichtungoeff_do2_von,
                                      einrichtungoeff_do2_bis) +
           calculate_opening_duration(einrichtungoeff_fr1_von,
                                      einrichtungoeff_fr1_bis,
                                      einrichtungoeff_fr2_von,
                                      einrichtungoeff_fr2_bis),
         opening_duration_hours = as.numeric(opening_duration) / 3600
         )

rm(list = c("kitas_private_sf", "kitas_public_sf"))


# Join the spacial objects to locate kindergardens in hexagons
st_crs(grid_new)
st_crs(kitas_sf)
kitas_sf <- st_transform(kitas_sf, st_crs(grid_new))
grid_new_kitas <- st_join(grid_new, kitas_sf, join = st_intersects)

# Lets add the df
grid_kitas <- left_join(
  grid_new_kitas,
  st_drop_geometry(initial),
  by = "index_target") %>%
  select(-index_target)


grid_kitas_agg <- aggregate(
  select(grid_kitas, STT_NAME, opening_duration_hours, geometry = grid),
  by = list(grid_kitas$STT_NAME),
  FUN = mean,
  na.rm = TRUE,
  do_union = FALSE
)


## PLOT ========================================================================

# Annotations
plot_titles <- list(
  title = "Opening times of kindergardens in Cologne",
  subtitle = "The color of the honeycombs indicates the average weekly
  opening durations of kindergardens and day care centers within that area.
  Grey honeycombs means no data.",
  caption = "Data: **Open Data Cologne** (last update: 2021-10-29) |
  Visualization: **Ansgar Wolsing**"
)

ggplot() +
  geom_sf(data = grid_kitas_agg,
          aes(fill = opening_duration_hours),
          col = "white", size = 0.25) +
  geom_sf(data = boroughs_sf,
          fill = "grey90", lty = "solid",
          col = "grey32", size = 0.5, alpha = 0.2) +
  ggrepel::geom_label_repel(data = boroughs_sf,
                            aes(geometry = geometry,
                                label = name),
                            stat = "sf_coordinates",
                size = 3, fill = "grey8", family = "Chivo",
                label.size = 0, alpha = 0.8, col = "white"
                ) +
  scale_fill_scico(palette = "bamako", alpha = 0.7) +
  scale_alpha_continuous(range = c(0.1, 1)) +
  guides(fill = guide_legend(title.position = "top")) +
  labs(title = plot_titles$title,
       subtitle = plot_titles$subtitle,
       caption = plot_titles$caption,
       fill = "Average weekly openings hours") +
  cowplot::theme_map() +
  theme(# legend.position = "top",
        plot.background = element_rect(color = NA, fill = "grey20"),
        text = element_text(color = "grey97", family = "Roboto"),
        plot.title = element_text(color = "white",
                                  margin = margin(t = 6, b = 12)),
        plot.subtitle = element_textbox_simple(size = 10),
        plot.caption = element_textbox_simple(size = 8),
        legend.position = c(0.6, 0.87),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.key.width = unit(3, "mm"),
        legend.direction = "horizontal"
        )
ggsave(here("plots", "day04_hexagons.png"), dpi = 600, width = 8, height = 8)


