pacman::p_load("tidyverse", "here", "glue", "ggtext", "colorspace",
               "sf", "osmdata", "geojsonsf", "jsonlite")


## GEOMETRIES ==================================================================
## Area of Cologne
coords_cgn <- getbb("Cologne, Germany", format_out = "sf_polygon")
coords_cgn
coords_cathedral <- getbb("Kölner Dom, Cologne, Germany",
                          featuretype = "church")


## GET DATA =================================
#' Child care centers in Cologne
#' Source: Offene Daten Köln,
#' https://www.offenedaten-koeln.de/dataset/kindertagesstaetten-koeln

url_kitas_private <- "https://geoportal.stadt-koeln.de/arcgis/rest/services/familie_partnerschaft_kinder/kitas/MapServer/0/query?where=objectid+is+not+null&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=4326&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=pjson"
url_kitas_public <- "https://geoportal.stadt-koeln.de/arcgis/rest/services/familie_partnerschaft_kinder/kitas/MapServer/1/query?where=objectid+is+not+null&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=4326&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=pjson"
kitas_private_sf <- st_read(url_kitas_private)
kitas_public_sf <- st_read(url_kitas_public)

kitas_sf <- bind_rows(kitas_private_sf, kitas_public_sf, .id = "type") %>%
  mutate(type = ifelse(type == "1", "private", "public"))


## PLOT ========================================================================

# Download from https://toppng.com/show_download/154246/kolner-dom/large (creator: John3)
cathedral_filepath <- here("toppng.com-kölner-dom-2000x3401.png")
cathedral_img <- png::readPNG(cathedral_filepath)

point_colors <- c("private" = "#482d52", "public" = "grey68")

# Annotations
plot_titles <- list(
  title = str_to_upper("Kindergardens in Cologne, Germany"),
  subtitle = glue(
    "Locations of <b style='color:{point_colors[\"private\"]}'>private</b> and
       <b style='color:{point_colors[\"public\"]}'>public</b> kindergardens and
       day care centers"),
  caption = "Data: **Open Data Cologne** (last update: 2021-10-29),
  **OpenStreetMap** |
  Visualization: **Ansgar Wolsing** |
  Image credit Cologne Cathedral: **John3** (toppng.com)"
)

ggplot(coords_cgn) +
  geom_sf(fill = "#d1a1e3", color = "grey91", size = 0.25) +
  # mer losse d'r Dom in Kölle
  ggimage::geom_image(data = as.data.frame(t(coords_cathedral[, "min"])),
                      aes(x, y),
                      image = cathedral_filepath,
                      alpha = 0.3, size = 0.5, col = "#854e99") +
  geom_sf(data = kitas_sf,
             aes(geometry = geometry,
                 fill = type),
          shape = 21, col = "white", size = 2, alpha = 0.6,
          show.legend = FALSE) +
  scale_fill_manual(values = point_colors) +
  coord_sf() +
  labs(
    title = plot_titles$title,
    subtitle = plot_titles$subtitle,
      caption = plot_titles$caption
  ) +
  cowplot::theme_map(font_family = "Montserrat") +
  theme(
        plot.background = element_rect(color = NA, fill = "#854e99"),
        legend.position = "top",
        legend.justification = "left",
        text = element_text(color = "grey93", lineheight = 1.3),
        plot.title = element_textbox_simple(color = "white", size = 24,
                                            family = "Source Sans Pro",
                                            # face = "bold",
                                            margin = margin(t = 4, b = 12)),
        plot.subtitle = element_textbox_simple(size = 16,
                                               margin = margin(t = 4, b = 0)),
        plot.caption = element_textbox_simple(size = 8,
                                              margin = margin(t = 8, b = 8)))
ggsave(here("plots", "day01_points_01.png"), dpi = 600, width = 8, height = 8)



