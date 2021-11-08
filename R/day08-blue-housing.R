pacman::p_load("tidyverse", "here", "glue", "ggtext", "sf", "paletteer",
               "pdftools", "lubridate", "patchwork", "ggbeeswarm")

## GEOMETRY ====================================================================
#' https://www.offenedaten-koeln.de/dataset/stadtteile
#' https://www.offenedaten-koeln.de/sites/default/files/Stadtteil.zip
shp_districts <- st_read(here("data", "cologne_stadtteile", "Stadtteil", "Stadtteil.shp"))
shp_districts <- st_transform(shp_districts, crs = 4326)

#' https://offenedaten-koeln.de/dataset/stadtbezirke-koeln
url_boroughs <- "https://geoportal.stadt-koeln.de/arcgis/rest/services/Basiskarten/kgg/MapServer/4/query?where=objectid+is+not+null&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=4326&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=pjson"
boroughs_sf <- st_read(here("data", "cologne_stadtbezirke", "stadtbezirke_koeln.json"))
boroughs_sf <- st_transform(boroughs_sf, crs = 4326)

## DATA  =======================================================================
# Get file
url_district_info <- "https://www.stadt-koeln.de/mediaasset/content/pdf15/statistik-standardinformationen/k%C3%B6lner_stadtteilinformationen_2020_fertig.pdf"
filepath_district_info <- here("data", "stadtteilinfo.pdf")
download.file(url_district_info, destfile = filepath_district_info)

# TODO: Extract table via https://www.pdftron.com/pdf-tools/pdf-table-extraction/
wohnflaeche <- read_delim(here("data", "cologne_wohnfläche.tsv"),
                          delim = "\t", locale = locale(decimal_mark = ","),
                          skip = 1) %>%
  mutate(nr = as.character(nr))
years <- c(2010, 2015, 2019, 2020)
colnames(wohnflaeche) <- c("id", "district",
                           paste("area_per_accomodation", years, sep = "_"),
                           paste("area_per_inhabitant", years, sep = "_"))


## PLOT ========================================================================
text_color <- "#0e304a"

# geom_curve with customized defaults
geom_curve2 <- function(..., curvature = 0.125) {
  geom_curve(
    color = text_color,
    arrow = arrow(angle = 17, length = unit(2.5, "mm")),
    curvature = curvature,
    size = 0.35,
    ...
  )
}

# get the centroid of a given district
district_centroid <- function(district, shp) {
  st_coordinates(
    st_centroid(
      shp[shp$STT_NAME == district,]
      )
    )
}


# average housing space for Cologne overall
avg_housing_space_cgn <- wohnflaeche$area_per_inhabitant_2020[wohnflaeche$district == "Köln insgesamt"]

plot_titles <- list(
  title = "Housing space in Cologne",
  subtitle = glue(
    "The floor area per person is a key indicators of dwelling comfort.
  The average housing space in Cologne is <b>{avg_housing_space_cgn}
  m<sup>2</sup></b> per person,
  but varies a lot between districts.
  <br><br>
  Boroughs are labelled for better orientation."),
    caption = "Data: **Stadt Köln** (Shapes; Stadtteilinformation 2020, pp. 52-53) |
  Visualization: **Ansgar Wolsing**"
)

p <- wohnflaeche %>%
  # exclude borough information (1-digit ids)
  filter(id > 100) %>%
  select(id, district, area_per_inhabitant_2020) %>%
  inner_join(shp_districts, by = c("id" = "STT_NR")) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry,
              fill = area_per_inhabitant_2020),
          col = "white", size = 0.1) +
  geom_sf(data = boroughs_sf,
          fill = NA, col = "grey97", size = 0.5) +
  geom_label(data = boroughs_sf,
                            aes(geometry = geometry,
                                label = name),
                            stat = "sf_coordinates",
                            size = 3, fill = "#1d3242", family = "Chivo",
                            label.size = 0, alpha = 0.8, col = "white"
  ) +

  ## Annotations --------------------------

  # Custom subtitle via geom_textbox
  geom_textbox(aes(x = 6.56, y = 51.038,
                   label = plot_titles$subtitle),
               hjust = 0, vjust = 1, box.colour = NA, fill = NA,
               col = "#0e304a", family = "Roboto", size = 5,
               box.padding = unit(c(5.5, 5.5, 5.5, 2), "pt"),
               width = unit(3, "inch")) +

  # Annotation for Gremberghoven
  geom_textbox(aes(x = 7.005, y = 51.045,
                   label = "With 25.4 square meters per person, **Gremberghoven** is the district
                   with the least average housing space."),
               hjust = 0, box.colour = NA, fill = NA, size = 3.5,
               col = text_color, family = "Roboto Light") +
  geom_curve2(aes(
    x = 7.005 + 0.01,
    y = 51.045 - 0.018,
    xend = district_centroid("Gremberghoven", shp_districts)[, "X"],
    yend = district_centroid("Gremberghoven", shp_districts)[, "Y"]))  +

  # Annotation for Hahnwald
  geom_textbox(aes(x = 6.78, y = 50.86,
                   label = "With 88.8 square meters, **Hahnwald** is the district
                   with the highest average housing space per person."),
               hjust = 0, box.colour = NA, fill = NA, size = 3.5,
               col = text_color, family = "Roboto Light") +
  geom_curve2(aes(
    x = 6.798 + 0.11,
    y = 50.86 + 0.005,
    xend = district_centroid("Hahnwald", shp_districts)[, "X"],
    yend = district_centroid("Hahnwald", shp_districts)[, "Y"]))  +

  paletteer::scale_fill_paletteer_c(
    palette = "pals::kovesi.linear_blue_95_50_c20") +
  guides(fill = guide_colorbar(title.position = "top",
                               title = "Average floor area per person (m<sup>2</sup>)")) +
  labs(title = plot_titles$title,
       caption = plot_titles$caption) +
  coord_sf(expand = FALSE) +
  cowplot::theme_map() +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    text = element_text(family = "Roboto", color = text_color),
    plot.title = element_text(size = 28, color = "#297bba",
                              margin = margin(t = 0, b = 12)),
    plot.subtitle = element_blank(),  # replaced by custom annotation
    plot.caption = element_textbox_simple(size = 8,
                                          margin = margin(t = 12, b = 4)),
    panel.background = element_rect(color = NA),
    legend.title = element_markdown(size = 10),
    legend.text = element_text(size = 8, color = text_color),
    legend.position = c(0.01, 0.92), # c(0.05, 0.2),
    legend.direction = "horizontal",
    legend.key.width = unit(10, "mm"),
    legend.key.height = unit(3, "mm")
  )
ggsave(here("plots", "day08-blue-area_living.png"),
       plot = p, dpi = 600, width = 9, height = 8)


# Beeswarm plot of average housing -------

p_bee <- wohnflaeche %>%
  # exclude borough information (1-digit ids)
  filter(id > 100) %>%
  ggplot(aes(x = factor(1), y = area_per_inhabitant_2020)) +
  geom_beeswarm(aes(fill = area_per_inhabitant_2020),
                shape = 21, col = "grey97", size = 4,
                cex = 4,
                show.legend = FALSE) +
  paletteer::scale_fill_paletteer_c(
    palette = "pals::kovesi.linear_blue_95_50_c20") +
  coord_flip(ylim = c(20, 100)) +
  labs(title = "Distribution",
       x = NULL, y = NULL) +
  theme_minimal(base_family = "Roboto") +
  theme(
    plot.title = element_text(color = text_color, family = "Roboto",
                              face = "bold", size = 10),
    plot.title.position = "plot",
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(size = 0.1, color = "grey90"),
    axis.text.y = element_blank()
  )

# Inset ---------------------------
p_inset <- p +
  # Arrow pointing to the beeswarm plot
  geom_curve2(aes(
    x = 6.78,
    y = 50.86 + 0.004,
    xend = 6.717,
    yend = 50.872),
    curvature = -0.1)  +
  inset_element(p_bee, 0.0, 0.05, 0.3, 0.3)
ggsave(here("plots", "day08-blue-area_living_inset.png"),
       plot = p_inset, dpi = 600, width = 9, height = 8)

