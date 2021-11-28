pacman::p_load("tidyverse", "here", "glue", "ggtext", "sf", "rnaturalearth")
shape_cgn <- getbb("Cologne, Germany", format_out = "sf_polygon")
st_crs(shape_cgn) <- "EPSG:4326"
st_centroid(shape_cgn)

world <- ne_countries(returnclass = "sf")


p <- ggplot(world) +
  geom_sf(fill = "deeppink", col = NA, alpha = 0.75, size = 0.05) +
  coord_sf(crs = "+proj=laea +y_0=0 +lon_0=6.974 +lat_0=50.95 +ellps=WGS84 +no_defs") +
  labs(title = "THE <span style='color:deeppink'>EARTH</span> IS NOT FLAT",
       caption = "Data: **OpenStreetMap** contributors | Visualization: **Ansgar Wolsing**") +
  cowplot::theme_map() +
  theme(plot.background = element_rect(color = NA, fill = "grey9"),
        text = element_text(family = "Montserrat", color = "grey80"),
        plot.title = element_markdown(family = "Bangers", face = "plain",
                                      color = "white", size = 36, hjust = 0.5),
        plot.caption = element_markdown(size = 7, hjust = 0.5)
  )
ggsave(here("plots", "day28_earth-not-flat.png"), plot = p, dpi = 300,
       width = 6, height = 6)



p + geom_point(data = shape_cgn,
               aes(x = st_coordinates(st_centroid(geometry))[, "X"],
                   y = st_coordinates(st_centroid(geometry))[, "Y"]),
               size = 1, col = "white") +
  ggforce::geom_mark_circle(
    data = shape_cgn,
    aes(label = "Cologne",
        fill = "Cologne",
        x = st_coordinates(st_centroid(geometry))[, "X"],
        y = st_coordinates(st_centroid(geometry))[, "Y"]),
    fill = "white", col = "white", size = 0.2,
    expand = unit(3, "mm"),
    con.colour = "white", con.cap = unit(1, "mm"),
    label.family = "Montserrat", label.fontsize = 8,
    label.colour = "grey16",
    label.margin = margin(1.5, 1.5, 1.5, 1.5, "mm"),
    label.fontface = "bold", label.buffer = unit(1, "mm"))
ggsave(here("plots", "day28_earth-not-flat_with_cologne.png"), dpi = 300,
       width = 6, height = 6)

