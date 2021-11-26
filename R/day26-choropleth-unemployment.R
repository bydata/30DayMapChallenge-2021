pacman::p_load("tidyverse", "here", "glue", "ggtext", "sf")


## GEOMETRY & DATA =============================================================
#' Source: https://www.offenedaten-koeln.de/dataset/arbeitsmarkt-statistik-koeln
url <- "https://geoportal.stadt-koeln.de/arcgis/rest/services/Statistische_Daten/QMFS_Arbeitsmarkt/MapServer/1/query?where=objectid+is+not+null&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=4326&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=false&resultOffset=&resultRecordCount=&f=geojson"
shp_alo <- st_read(url)
shp_alo <- st_transform(shp_alo, "EPSG:25832")

glimpse(shp_alo)


## PLOT ========================================================================
p <-
  shp_alo %>%
    st_drop_geometry() %>%
    select(name, am_alo_insg_ap, am_alo_unter25_ap) %>%
    pivot_longer(cols = -name, names_to = "group", values_to = "alo_perc") %>%
    mutate(group = case_when(
      group == "am_alo_insg_ap" ~ "Overall",
      group == "am_alo_unter25_ap" ~ "Under 25 years"
    )) %>%
    inner_join(select(shp_alo, name, geometry), by = "name") %>%
    ggplot() +
  geom_sf(aes(geometry = geometry, fill = alo_perc),
          col = "white", size = 0.2) +
  paletteer::scale_fill_paletteer_c("ggthemes::Orange-Blue-White Diverging",
                                    direction = -1) +
  # paletteer::scale_fill_paletteer_c("grDevices::PuOr") +
  coord_sf() +
  guides(fill = guide_colorsteps(title = "Unemployment Rate (%)",
                                 title.position = "top")) +
  facet_wrap(vars(group)) +
  labs(title = "Unemployment in Cologne",
       subtitle = "Unemployment rates in the districts as of December 2020
       (grey = no data)",
       caption = "Source: **Stadt Köln** | Visualization: **Ansgar Wolsing**") +
  cowplot::theme_map(font_family = "Roboto") +
  theme(
    plot.background = element_rect(color = NA, fill = "grey81"),
    panel.background = element_rect(color = NA, fill = "grey88"),
    legend.position = "bottom",
    legend.justification = "center",
    text = element_text(color = "grey10"),
    plot.title = element_markdown(family = "Roboto", face = "bold",
                                  size = 24,
                                  margin = margin(t = 4, b = 8)),
    plot.subtitle = element_textbox_simple(margin = margin(t = 0, b = 18)),
    plot.caption = element_textbox_simple(color = "grey30",
                                          margin = margin(t = 16, b = 2)),
    strip.background = element_rect(color = NA, fill = "grey40"),
    strip.text = element_text(color = "grey94", face = "bold"),
    legend.key.width = unit(12, "mm"),
    legend.key.height = unit(4, "mm"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )
ggsave(here("plots", "day26-choropleth_alo_facets.png"), plot = p, dpi = 300,
       width = 8, height = 7)
