pacman::p_load("tidyverse", "here", "glue", "ggtext", "sf", "paletteer")

## GET DATA ====================================================================
#' Land use plan ("Flächennutzungsplan") of Cologne

# lup_url <- "https://geoportal.stadt-koeln.de/arcgis/rest/services/planen_und_bauen/flaechennutzungsplan/MapServer/7/query?where=objectid+is+not+null&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=4326&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=pjson"
# lup <- st_read(lup_url)

lup_urls <- c(
  "https://geoportal.stadt-koeln.de/arcgis/rest/services/planen_und_bauen/flaechennutzungsplan/MapServer/3/query?f=json&where=%20objectid%20IS%20NOT%20NULL%20&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100",
  "https://geoportal.stadt-koeln.de/arcgis/rest/services/planen_und_bauen/flaechennutzungsplan/MapServer/7/query?f=json&where=%20objectid%20IS%20NOT%20NULL%20AND%20subtype%20%3D%201%20&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100",
  "https://geoportal.stadt-koeln.de/arcgis/rest/services/planen_und_bauen/flaechennutzungsplan/MapServer/7/query?f=json&where=%20objectid%20IS%20NOT%20NULL%20AND%20subtype%20%3D%2012%20&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100",
  "https://geoportal.stadt-koeln.de/arcgis/rest/services/planen_und_bauen/flaechennutzungsplan/MapServer/7/query?f=json&where=%20objectid%20IS%20NOT%20NULL%20AND%20subtype%20%3D%207%20&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100",
  "https://geoportal.stadt-koeln.de/arcgis/rest/services/planen_und_bauen/flaechennutzungsplan/MapServer/7/query?f=json&where=%20objectid%20IS%20NOT%20NULL%20AND%20subtype%20%3D%2013%20&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100",
  "https://geoportal.stadt-koeln.de/arcgis/rest/services/planen_und_bauen/flaechennutzungsplan/MapServer/7/query?f=json&where=%20objectid%20IS%20NOT%20NULL%20AND%20subtype%20%3D%206%20&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100",
  "https://geoportal.stadt-koeln.de/arcgis/rest/services/planen_und_bauen/flaechennutzungsplan/MapServer/7/query?f=json&where=%20objectid%20IS%20NOT%20NULL%20AND%20subtype%20%3D%2014%20&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100",
  "https://geoportal.stadt-koeln.de/arcgis/rest/services/planen_und_bauen/flaechennutzungsplan/MapServer/7/query?f=json&where=%20objectid%20IS%20NOT%20NULL%20AND%20subtype%20%3D%2033%20&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100",
  "https://geoportal.stadt-koeln.de/arcgis/rest/services/planen_und_bauen/flaechennutzungsplan/MapServer/7/query?f=json&where=%20objectid%20IS%20NOT%20NULL%20AND%20subtype%20%3D%209%20&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100",
  "https://geoportal.stadt-koeln.de/arcgis/rest/services/planen_und_bauen/flaechennutzungsplan/MapServer/7/query?f=json&where=%20objectid%20IS%20NOT%20NULL%20AND%20subtype%20%3D%2020%20&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100",
  "https://geoportal.stadt-koeln.de/arcgis/rest/services/planen_und_bauen/flaechennutzungsplan/MapServer/7/query?f=json&where=%20objectid%20IS%20NOT%20NULL%20AND%20subtype%20%3D%2011%20&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100",
  "https://geoportal.stadt-koeln.de/arcgis/rest/services/planen_und_bauen/flaechennutzungsplan/MapServer/7/query?f=json&where=%20objectid%20IS%20NOT%20NULL%20AND%20subtype%20%3D%202%20&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100",
  "https://geoportal.stadt-koeln.de/arcgis/rest/services/planen_und_bauen/flaechennutzungsplan/MapServer/7/query?f=json&where=%20objectid%20IS%20NOT%20NULL%20AND%20subtype%20%3D%205%20&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100",
  "https://geoportal.stadt-koeln.de/arcgis/rest/services/planen_und_bauen/flaechennutzungsplan/MapServer/7/query?f=json&where=%20objectid%20IS%20NOT%20NULL%20AND%20subtype%20%3D%2022%20&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100",
  "https://geoportal.stadt-koeln.de/arcgis/rest/services/planen_und_bauen/flaechennutzungsplan/MapServer/7/query?f=json&where=%20objectid%20IS%20NOT%20NULL%20AND%20subtype%20%3D%203%20&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100",
  "https://geoportal.stadt-koeln.de/arcgis/rest/services/planen_und_bauen/flaechennutzungsplan/MapServer/7/query?f=json&where=%20objectid%20IS%20NOT%20NULL%20AND%20subtype%20%3D%204%20&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100",
  "https://geoportal.stadt-koeln.de/arcgis/rest/services/planen_und_bauen/flaechennutzungsplan/MapServer/7/query?f=json&where=%20objectid%20IS%20NOT%20NULL%20AND%20subtype%20%3D%2015%20&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100",
  "https://geoportal.stadt-koeln.de/arcgis/rest/services/planen_und_bauen/flaechennutzungsplan/MapServer/7/query?f=json&where=%20objectid%20IS%20NOT%20NULL%20AND%20subtype%20%3D%2016%20&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100",
  "https://geoportal.stadt-koeln.de/arcgis/rest/services/planen_und_bauen/flaechennutzungsplan/MapServer/6/query?f=json&where=%20objectid%20IS%20NOT%20NULL%20&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100",
  "https://geoportal.stadt-koeln.de/arcgis/rest/services/planen_und_bauen/flaechennutzungsplan/MapServer/5/query?f=json&where=%20objectid%20IS%20NOT%20NULL%20&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100",
  "https://geoportal.stadt-koeln.de/arcgis/rest/services/planen_und_bauen/flaechennutzungsplan/MapServer/7/query?f=json&where=%20objectid%20IS%20NOT%20NULL%20AND%20subtype%20%3D%2021%20&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100",
  "https://geoportal.stadt-koeln.de/arcgis/rest/services/planen_und_bauen/flaechennutzungsplan/MapServer/4/query?f=json&where=%20objectid%20IS%20NOT%20NULL%20&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100",
  "https://geoportal.stadt-koeln.de/arcgis/rest/services/planen_und_bauen/flaechennutzungsplan/MapServer/7/query?f=json&where=%20objectid%20IS%20NOT%20NULL%20AND%20subtype%20%3D%2017%20&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100",
  "https://geoportal.stadt-koeln.de/arcgis/rest/services/planen_und_bauen/flaechennutzungsplan/MapServer/7/query?f=json&where=%20objectid%20IS%20NOT%20NULL%20AND%20subtype%20%3D%2010%20&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100",
  "https://geoportal.stadt-koeln.de/arcgis/rest/services/planen_und_bauen/flaechennutzungsplan/MapServer/1/query?f=json&where=%20objectid%20IS%20NOT%20NULL%20&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100",
  "https://geoportal.stadt-koeln.de/arcgis/rest/services/planen_und_bauen/flaechennutzungsplan/MapServer/7/query?f=json&where=%20objectid%20IS%20NOT%20NULL%20AND%20subtype%20%3D%2023%20&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100",
  "https://geoportal.stadt-koeln.de/arcgis/rest/services/planen_und_bauen/flaechennutzungsplan/MapServer/7/query?f=json&where=%20objectid%20IS%20NOT%20NULL%20AND%20subtype%20%3D%208%20&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100",
  "https://geoportal.stadt-koeln.de/arcgis/rest/services/planen_und_bauen/flaechennutzungsplan/MapServer/7/query?f=json&where=%20objectid%20IS%20NOT%20NULL%20AND%20subtype%20%3D%200%20&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100",
  "https://geoportal.stadt-koeln.de/arcgis/rest/services/planen_und_bauen/flaechennutzungsplan/MapServer/7/query?f=json&where=%20objectid%20IS%20NOT%20NULL%20AND%20subtype%20%3D%2019%20&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100",
  "https://geoportal.stadt-koeln.de/arcgis/rest/services/planen_und_bauen/flaechennutzungsplan/MapServer/2/query?f=json&where=%20objectid%20IS%20NOT%20NULL%20&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=102100"
)

length(unique(lup_urls))
lup_urls <- unique(lup_urls)
lup <- map_dfr(lup_urls, st_read)


#' https://offenedaten-koeln.de/dataset/stadtbezirke-koeln
url_boroughs <- "https://geoportal.stadt-koeln.de/arcgis/rest/services/Basiskarten/kgg/MapServer/4/query?where=objectid+is+not+null&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=4326&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=pjson"
boroughs_sf <- st_read(here("data", "cologne_stadtbezirke", "stadtbezirke_koeln.json"))

lup %>%
  st_drop_geometry() %>%
  count(nutzung, sort = TRUE)
lup %>%
  st_drop_geometry() %>%
  count(nutzung)
lup %>%
  filter(is.na(nutzung)) %>%
  select(erlaeuterung)


lup_prep <- lup %>%
  mutate(nutzung2 = replace_na(nutzung, "Unbekannt"),
         nutzung2 = str_remove(nutzung2, "Fläche für(\\s(den|die|das))?"),
         nutzung2 = case_when(
           str_detect(nutzung2, "Grünfläche") ~ "Grünfläche",
           str_detect(nutzung2, "Sonderbaufläche") ~ "Sonderbaufläche",
           TRUE ~ nutzung2),
         nutzung2 = str_trim(nutzung2)
         )

lup_prep %>%
  st_drop_geometry() %>%
  count(nutzung2)


#' Urban land use categories
#' https://www.semanticscholar.org/paper/Urban-land-use-classes-with-fuzzy-membership-and-on-Zhan-Molenaar/37f786405ce6e88fcd5e4cd6f2e72efce11fdc96/figure/2
#' https://www.bbc.co.uk/bitesize/guides/z3n9gdm/revision/1
#' https://www.gesetze-im-internet.de/planzv_90/BJNR000580991.html
#' https://www.land.com/buying/guide-to-land-use-definitions

# Translation of use categories
de_en_translation <- c(
  "Bahnanlagen" = "Railroad facilities",
  "Besonderes Wohngebiet" = "Special residential area",
  "Forstwirtschaft, Erholungswald" = "Forestry, recreational forest",
  "Gemeinbedarf" = "Institutional",
  "Gemischte Baufläche" = "Mixed construction area",
  "Gewerbegebiet" = "Business park",
  "Grünfläche" = "Green urban area",
  "Hauptverkehrszüge" = "Railroad",
  "Industriegebiet" = "Industrial use",
  "Kerngebiet" = "Business area",
  "Landwirtschaft" = "Agriculture",
  "Luftverkehr" = "Airport",
  "Mischgebiet" = "Mixed use area",
  "Sanierungsgebiet" = "Redevelopment area",
  "Sonderbaufläche" = "Special-purpose area",
  "Unbekannt" = "Unknown",
  "Ver- und Entsorgung" = "Power plants & dump sites",
  "Waldfläche mit besonderer Nutzung" = "Forestry with special use",
  "Wasserflächen" = "Waterbody",
  "Windenergieanlagen" = "Wind energy plants",
  "Wohnbaufläche" = "Residential area"
  )



lup_prep <- lup_prep %>%
  select(nutzung2) %>%
  mutate(
    landuse = de_en_translation[nutzung2],
    landuse_grp = fct_collapse(
      landuse,
      "Residential" = c("Residential area", "Special residential area"),
      "Transport" = c("Railroad facilities", "Airport", "Railroad"),
      "Forestry" = c("Forestry, recreational forest", "Forestry with special use"),
      "Industry" = c("Industrial use", "Business park"),
      "Commercial" = c("Business area")
    ))

paletteer_d("trekcolors::lcars_cardassian")
landuse_colors <- c(
  "Residential area" = "#BFCAFEFF" , "Special residential area" = "#8B799CFF",
  "Railroad facilities"  = "#8BEAFFFF", "Airport" = "#80A0E0FF",
  "Railroad" = "#B0C8F8FF",
  "Green urban area" = "#3C999CFF",
  "Agriculture" = "#A1B3E2FF",
  "Forestry, recreational forest" = "#9B5928FF",
  "Forestry with special use" = "#CA480DFF",
  "Industrial use" = "#FFE705FF", "Business park" = "#2F7270FF"
)


plot_titles <- list(
  title = "Zoning Plan Cologne",
  subtitle = "A zoning plan is issued by the municipal authorities
       and provides guidelines on how a particular area can be used.
       It determines where housing, businesses, agricultural and commercial uses
       may be established.
       This plot shows selected land use categories.",
  caption = "Source: **Municipality of Cologne**, released 2016, last update 2021 |
       Visualization: **Ansgar Wolsing**"
)

p <- lup_prep %>%
  filter(landuse_grp %in% c("Residential", "Green urban area", "Forestry",
                            "Industry", "Transport", "Agriculture")) %>%
  mutate(landuse2 = factor(landuse, levels = c("Agriculture",
                           "Railroad facilities", "Airport", "Railroad",
                           "Industrial use", "Business park",
                           "Forestry, recreational forest", "Forestry with special use",
                           "Green urban area",
                           "Residential area", "Special residential area"
                           ))) %>%
  ggplot() +
  geom_sf(data = boroughs_sf, fill = "grey20",
          col = NA, size = 0.25) +
  geom_sf(aes(fill = landuse2),
          col = NA, size = 0.15, show.legend = TRUE) +
  scale_fill_manual(values = landuse_colors) +
  guides(fill = guide_legend(ncol = 4, title = NULL,
                             override.aes = list(color = "grey4", size = 0.5))) +
  facet_wrap(vars(landuse_grp),
             labeller = as_labeller(str_to_upper)) +
  labs(title = plot_titles$title,
       subtitle = plot_titles$subtitle,
       caption = plot_titles$caption) +
  cowplot::theme_map(font_family = "Helvetica Neue") +
  theme(plot.background = element_rect(color = NA, fill = "grey4"),
        legend.position = "bottom",
        legend.justification = "center",
        legend.key.height = unit(4, "mm"),
        legend.spacing.x = unit(0.25, "cm"),
        legend.text = element_text(size = 10),
        text = element_text(color = "grey91"),
        plot.title = element_text(size = 36, family = "Oswald", face = "plain",
                                  hjust = 0.5, margin = margin(t = 6, b = 16)),
        plot.subtitle = element_textbox_simple(margin = margin(b = 24)),
        plot.caption = element_markdown(hjust = 0, margin = margin(t = 36)),
        strip.text = element_text(family = "Helvetica Neue", face = "bold",
                                  margin = margin(t = 8, b = 8)),
        strip.background = element_rect(color = "white", linetype = "dotted",
                                        fill = "grey8"),
        panel.spacing.y = unit(1.25, "cm"),
        panel.spacing.x = unit(1, "cm"))
ggsave(here("plots", "day17-landuse-en_facets.png"), plot = p,
       dpi = 600, width = 9.5, height = 9)


## ALL IN ONE


plot_titles$subtitle <- "A zoning plan is issued by the municipal authorities
and provides guidelines on how a particular area can be used.
It determines where housing, businesses, agricultural and commercial uses
may be established."
p2 <- lup_prep %>%
  mutate(landuse = na_if(landuse, "Unknown")) %>%
ggplot() +
  geom_sf(data = boroughs_sf,
          col = "grey90", size = 1.5) +
  geom_sf(aes(fill = landuse),
          col = "grey97", size = 0.15, show.legend = TRUE) +
  paletteer::scale_fill_paletteer_d("khroma::discrete_rainbow",
                                    na.value = "grey60",
                                    na.translate = FALSE) +
  guides(fill = guide_legend(nrow = 7, title = NULL,
                             override.aes = list(color = "grey4", size = 0.5))) +
  labs(title = plot_titles$title,
       subtitle = plot_titles$subtitle,
       caption = plot_titles$caption) +
  cowplot::theme_map(font_family = "Helvetica Neue") +
  theme(plot.background = element_rect(color = NA, fill = "grey4"),
        legend.position = "bottom",
        legend.justification = "center",
        legend.key.height = unit(4, "mm"),
        legend.spacing.x = unit(0.25, "cm"),
        legend.text = element_text(size = 10),
        text = element_text(color = "grey91"),
        plot.title = element_text(size = 36, family = "Oswald", face = "plain",
                                  hjust = 0, margin = margin(t = 6, b = 16)),
        plot.subtitle = element_textbox_simple(margin = margin(b = 24)),
        plot.caption = element_markdown(hjust = 0, margin = margin(t = 36)),
        strip.text = element_text(family = "Helvetica Neue", face = "bold",
                                  margin = margin(t = 8, b = 8)),
        strip.background = element_rect(color = "white", linetype = "dotted",
                                        fill = "grey8"),
        panel.spacing.y = unit(1.25, "cm"),
        panel.spacing.x = unit(1, "cm"))
ggsave(here("plots", "day17-landuse.png"), plot = p2, dpi = 600,
       width = 8, height = 10)


