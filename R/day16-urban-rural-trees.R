pacman::p_load("tidyverse", "here", "glue", "ggtext", "sf")

## GET DATA ====================================================================
#' Tree population in Cologne, 2020
#' Source: Offene Daten Köln,
#' https://www.offenedaten-koeln.de/dataset/baumkataster-koeln

url_trees_2020 <- "https://www.offenedaten-koeln.de/sites/default/files/20200610_Baumbestan_Koeln.zip"
data_dir <- here("data", "cologne_trees")
if (!dir.exists(data_dir)) {
  dir.create(data_dir)
  download.file(url_trees_2020, destfile = here(data_dir, "Shapes_Bestand_Einzelbaeume_Koeln_2020.zip"))
  unzip(here(data_dir, "Shapes_Bestand_Einzelbaeume_Koeln_2020.zip"),
        exdir = here(data_dir, "Shapes_Bestand_Einzelbaeume_Koeln_2020"))
}

crs <- "EPSG:4326"
trees_2020_sf <- st_read(here(data_dir,
                              "Shapes_Bestand_Einzelbaeume_Koeln_2020",
                              "Baumbestand.shp"))
trees_2020_sf <- st_transform(trees_2020_sf, crs)
st_crs(trees_2020_sf)


## Basic EDA ===================================================================

trees_2020_sf %>%
  st_drop_geometry() %>%
  ggplot(aes(ANZAHL)) +
  geom_boxplot()

trees_2020_sf %>%
  st_drop_geometry() %>%
  filter(ANZAHL <= 100) %>%
  ggplot(aes(ANZAHL)) +
  geom_histogram(binwidth = 1)

trees_2020_sf %>%
  st_drop_geometry() %>%
  filter(ANZAHL == 0) %>%
  count(beschreibu)

# Replace NA with 1 for ANZAHL (number of trees)
trees_2020_sf_prep <- trees_2020_sf %>%
  mutate(ANZAHL2 = replace_na(ANZAHL, 1),
         ANZAHL2 = ifelse(ANZAHL2 == 0, 1, ANZAHL2))


## Reduce to a smaller circle around the city center
cgn_centroid <- # st_centroid(shape_cgn) %>%
  st_point(c(6.93, 50.94558)) %>%
  st_sfc() %>%
  st_sf(crs = "EPSG:4326") %>%
  st_transform(crs = "EPSG:3857")

cgn_circle <- st_buffer(cgn_centroid, dist = 7500)


trees_2020_sf_prep_intersect <- st_intersection(
  cgn_circle,
  st_transform(trees_2020_sf_prep, "EPSG:3857"))


# Number of trees (sum, not number of rows)
n_trees_cgn <- sum(trees_2020_sf_prep$ANZAHL2)
n_trees_cgn_circle <- sum(trees_2020_sf_prep_intersect$ANZAHL2)
n_trees_cgn_fmt <- scales::number(n_trees_cgn, big.mark = ",")
n_trees_cgn_circle_fmt <- scales::number(n_trees_cgn_circle, big.mark = ",")

base_font <- "Lato"
span_tag <- glue("<span style='font-family:{base_font} Bold'>")
plot_titles <- list(
  title = "<span style='color:deeppink'>TREES</span> OF COLOGNE",
  subtitle = glue("{span_tag}{n_trees_cgn_fmt} trees</span> are registered in the
  (yet to be completed) tree cadastre of the Municipality of Cologne.
  {span_tag}{n_trees_cgn_circle_fmt} trees</span> are displayed in this visualization."),
  caption = glue("Data: {span_tag}Municipality of Cologne</span>, Tree Cadastre |
  Visualization: {span_tag}Ansgar Wolsing</span>"))

p <- ggplot() +
  geom_sf(data = cgn_circle,
          fill = "grey12", col = "grey70", size = 1.25) +
  geom_sf(data = trees_2020_sf_prep_intersect,
          aes(size = ANZAHL2),
          col = "deeppink", alpha = 0.6,
          show.legend = FALSE) +
  geom_sf(data = cgn_circle,
          fill = NA, col = "grey97", size = 1) +
  scale_size_continuous(range = c(0.0005, 0.25)) +
  coord_sf() +
  labs(title = plot_titles$title,
       subtitle = plot_titles$subtitle,
       caption = plot_titles$caption) +
  cowplot::theme_map() +
  theme(
    plot.background = element_rect(color = NA, fill = "black"),
    text = element_text(family = paste(base_font, "Light"), color = "grey90",
                        lineheight = 1.33),
    plot.title = element_markdown(color = "white", family = "Bangers",
                              face = "plain", hjust = 0.5, size = 40,
                              margin = margin(t = 6, b = 18)),
    plot.subtitle = element_textbox_simple(hjust = 0.5, halign = 0.5,
                                           margin = margin(t = 0, b = 12)),
    plot.caption = element_markdown(hjust = 0.5, size = 9,
                                    margin = margin(t = 12, b = 4))

  )
ggsave(here("plots", "day16_urban-rural_trees.png"), plot = p,
       dpi = 600, width = 8, height = 8)


