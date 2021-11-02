pacman::p_load("tidyverse", "here", "glue", "ggtext", "colorspace",
               "sf", "osmdata", "paletteer", "rvest", "rnaturalearth",
               "stars")

#' https://github.com/ropensci/rnaturalearth
#' https://www.naturalearthdata.com/features/
# Download shaded relief raster (SR) in high/low resolution (HR/LR)
dir_raster <- here("data", "raster_data", "SR_LR")
if (!file.exists(dir_raster)) {
  ne_download(scale = 50, type = "SR_LR", category = "raster",
                   destdir = dir_raster, load = FALSE)
}

# relief <- raster::raster(here(dir_raster, "SR_LR.tif"))
# relief_spdf <- as(relief, "SpatialPixelsDataFrame")
# relief2 <- as.data.frame(relief_spdf) %>%
#   rename(value = SR_LR)

relief <- raster::raster(here(dir_raster, "SR_LR.tif")) %>%
  as("SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(value = SR_LR)

relief_filtered <- relief %>%
  filter(x >= 4, x <= 10, y >= 46, y <= 52.5) %>%
  mutate(geometry = map2(x, y, ~st_point(c(.x, .y)))) %>%
  st_as_sf()
st_crs(relief_filtered) <- "EPSG:4326"

rm(list = c("relief"))

coords_rhine <- opq(bbox = 'Cologne, Germany') %>%
  add_osm_feature(key = 'name:de', value = 'Rhein', value_exact = TRUE) %>%
  osmdata_sf()

coords_cathedral <- getbb("Kölner Dom, Cologne, Germany",
                          featuretype = "church")

ggplot() +
  geom_raster(data = relief_filtered,
              aes(x, y, alpha = value), fill = "white",
              show.legend = FALSE) +
  geom_sf(data = coords_rhine$osm_lines,
          col = "darkblue", size = 1.1) +
  geom_point(aes(coords_cathedral["x", "min"],
                 coords_cathedral["y", "min"]),
             shape = 25, fill = "grey8", col = "white", size = 7) +
  annotate("richtext",
           label = "The Rhine",
           x = 4.1, y = 49.5,
           family = "Noto Serif Display",
           size = 18, col = "grey2",
           label.color = NA, fill = NA,
           hjust = 0,
  ) +
  annotate("richtext",
           label = "<i>Source:</i> Rein Anteriur, CH<br>
           <i>Mouth:</i> North Sea, NL<br>
           <i>Length:</i> 1,233 km",
           x = 4.2, y = 49.15,
           family = "Noto Serif Display",
           size = 5, col = "grey11",
           label.color = NA, fill = NA,
           hjust = 0, vjust = 1, lineheight = 1.3
  ) +
  annotate("label", label = "Cologne",
           x = coords_cathedral["x", "min"] + 0.2,
           y = coords_cathedral["y", "min"],
           size = 4, label.size = 0, label.r = unit(0.05, "lines"),
           fill = "grey12", col = "white", alpha = 0.6,
           hjust = 0,
           family = "Roboto", fontface = "bold") +
  scale_alpha(c(0.7, 0)) +
  labs(caption = "Data: **Natural Earth**, **OpenStreetMap** |
       Visualization: **Ansgar Wolsing**",
       x = NULL, y = NULL) +
  coord_sf(xlim = c(4.01, 10), expand = FALSE) +
  theme_minimal(base_family = "Roboto") +
  theme(plot.background = element_rect(color = NA, fill = "grey98"),
        panel.background = element_rect(color = NA, fill = "#b8a149"),
        panel.grid = element_blank(),
        plot.margin = margin(t = 0, l = 16, r = 16, b = 2),
        plot.caption = element_markdown(family = "Roboto", size = 11,
                                              hjust = 0.5, color = "grey35",
                                        margin = margin(t = 20, b = 6)))
ggsave(here("plots", "day02_lines.png"), dpi = 600,
       width = 6, height = 10)

