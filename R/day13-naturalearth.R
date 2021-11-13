# Download development version of devtools which fixes ne_download issue
# devtools::install_github("ropensci/rnaturalearth")
pacman::p_load("tidyverse", "sf", "glue", "here", "scico", "rnaturalearth",
               "ggtext", "raster", "ggfx")


#' https://github.com/ropensci/rnaturalearth
## RIVERS
dir_raster <- here("data", "raster_data")
ne_type <- "rivers_lake_centerlines"
if (!file.exists(here(dir_raster, ne_type))) {
  ne_download(scale = 10, type = ne_type, category = "physical",
              destdir = here(dir_raster, ne_type), load = FALSE)
}
rivers <- ne_load(scale = 10, category = "physical", type = ne_type,
                  destdir = here(dir_raster, ne_type), returnclass = "sf")
rhine <- rivers[rivers$name_en == "Rhine", ]

ggplot(rhine) +
  geom_sf()


#' https://www.naturalearthdata.com/features/
# Download shaded relief raster (SR) in high/low resolution (HR/LR)
ne_type <- "SR_LR"
if (!file.exists(dir_raster)) {
  ne_download(scale = 50, type = ne_type, category = "raster",
              destdir = dir_raster, load = FALSE)
}

raster <- ne_load(scale = 10, category = "raster", type = "",
                  destdir = dir_raster, file_name = ne_type, returnclass = "sf")

extent <- extent(3.5, 10.5, 46.5, 52)
relief <- raster %>%
  crop(extent) %>%
  as("SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  rename(value = SR_LR) %>%
  mutate(geometry = map2(x, y, ~st_point(c(.x, .y)))) %>%
  st_as_sf()
st_crs(relief) <- "EPSG:4326"


ggplot(relief) +
  # Relief
  geom_raster(aes(x, y), fill = "black") +
  geom_raster(aes(x, y, alpha = value), fill = "grey90",
              show.legend = FALSE) +
  # The Rhine river course
  geom_sf(data = rhine,
          col = "white", size = 0.9) +
  # TITLE + key facts
  annotate("richtext",
           label = "The Rhine",
           x = 4.0, y = 49.5,
           family = "Noto Serif Display",
           size = 14, col = "grey96",
           label.color = NA, fill = NA,
           hjust = 0,
  ) +
  annotate("richtext",
           label = "<i>Source:</i> Rein Anteriur, CH<br>
           <i>Mouth:</i> North Sea, NL<br>
           <i>Length:</i> 1,233 km",
           x = 4.1, y = 49.15,
           family = "Noto Serif Display",
           size = 4, col = "grey90",
           label.color = NA, fill = NA,
           hjust = 0, vjust = 1, lineheight = 1.3
  ) +
  scale_alpha(range = c(0.005, 0.15)) +
  labs(caption = "Data: **Natural Earth** | Visualization: **Ansgar Wolsing**",
       x = NULL, y = NULL) +
  coord_sf(xlim = c(4, 10.5)) +
  theme_minimal(base_family = "Roboto") +
  theme(plot.background = element_rect(color = NA, fill = "grey10"),
        panel.background = element_rect(color = NA, fill = NA),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        panel.spacing = unit(10, "mm"),
        plot.margin = margin(t = 0, l = 0, r = 0, b = 0),
        plot.caption = element_markdown(family = "Roboto", size = 8,
                                        hjust = 0, color = "grey75",
                                        margin = margin(t = 2, l = 20, b = 2)))
ggsave(here("plots", "day13_naturalearth.png"), dpi = 1000,
       width = 5, height = 6.8)

