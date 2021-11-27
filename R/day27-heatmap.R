pacman::p_load("tidyverse", "here", "glue", "ggtext", "colorspace", "scico",
               "sf", "osmdata")

#' https://unfallatlas.statistikportal.de/
#' https://unfallatlas.statistikportal.de/_opendata2021.html
#' Licence: https://www.govdata.de/dl-de/by-2-0
#' Download shapefiles manually

data_dir <- here("data", "unfallatlas")
years <- 2016:2020
filepaths <- here(data_dir, years, glue("Unfallorte{years}_LinRef.shp"))
# 2016 files are named slightly different
filepaths <- gsub("Unfallorte2016_", "Unfaelle_2016_", filepaths)

accidents <- map(filepaths, st_read)
accidents <- set_names(accidents, years)
accidents <- map(accidents, st_zm)
# transform numeric variables from character to numeric
accidents <- map(accidents, ~mutate(.x, across(starts_with("Ist"), as.numeric),
                                    IstPKWxRad = IstPKW * IstRad
                                    ))


## Shape Cologne
shape_cgn <- getbb("Cologne, Germany", format_out = "sf_polygon")
st_crs(shape_cgn) <-  "EPSG:4326"
shape_cgn <- st_transform(shape_cgn, st_crs(accidents[["2020"]]))

regional_key_cgn <- list("ULAND" = "05", "UREGBEZ" = "3", "UKREIS" = "15")


# accidents_2020 <- accidents[["2020"]]
# merge all years to one dataframe
accidents_all_years <- bind_rows(accidents, .id = "year")

accidents_2020_cgn <- st_intersection(accidents_all_years, shape_cgn)

accidents_2020_cgn %>%
  filter(ULAND == regional_key_cgn$ULAND,
         UREGBEZ == regional_key_cgn$UREGBEZ,
         UKREIS == regional_key_cgn$UKREIS) %>%
  ggplot() +
  geom_sf(data = shape_cgn) +
  geom_sf(size = 0.1, alpha = 0.3)



## Create tile grid ------------------------------------------------------------

shape <- st_transform(shape_cgn, 3857)
initial <- shape
# initial$index_target <- 1:nrow(initial)
target <- st_geometry(initial)

# Create a tile grid
grid <- st_make_grid(target,
                     cellsize = 1 * 1000,
                     crs = st_crs(initial),
                     what = "polygons",
                     square = FALSE
)
# Add index, transform list to dataframe
grid <- st_sf(index = 1:length(lengths(grid)), grid)
cent_merge <- st_join(st_centroid(grid), initial, left = FALSE)
grid_new <- inner_join(grid, st_drop_geometry(cent_merge))

# ensure both geometries share the same CRS
st_crs(grid_new)
st_crs(accidents_2020_cgn)
grid_new <- st_transform(grid_new, st_crs(accidents_2020_cgn))
# add a constant (will be summed up in the aggregate step)
accidents_2020_cgn$n_accidents = 1
grid_new_accidents <- st_join(grid_new, accidents_2020_cgn, join = st_intersects)

grid_new_accidents_agg <- aggregate(
  select(grid_new_accidents, index, n_accidents, starts_with("Ist")),
  by = list(grid_new_accidents$index),
  FUN = sum,
  na.rm = TRUE,
  do_union = FALSE
)


p <- grid_new_accidents_agg %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = n_accidents),
          col = "grey10", size = 0.1) +
  paletteer::scale_fill_paletteer_c("ggthemes::Red-Black-White Diverging",
                                    direction = -1,
                                    aesthetics = list("fill", "color")) +
  guides(fill = guide_colorsteps(title = "# accidents", title.position = "top")) +
  cowplot::theme_map() +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    legend.position = c(0.1, 0.1),
    legend.direction = "horizontal",
    legend.key.width = unit(15, "mm")

  )
ggsave(here("plots", "day27-heatmap.png"), plot = p, dpi = 300, width = 8, height = 8)

plot_titles <- list(
  title = "Road accidents in Cologne",
  subtitle = "Cumulative number of accidents with passenger cars, bicycles,
  or both invoved per cell from 2016 to 2020. No indication of liability",
  caption = "Data: **Unfallatlas, Statistisches Bundesamt** (dl-de/by-2-0),
  shape: **OpenStreetMap** contributors |
  Visualization: **Ansgar Wolsing**"
)

p <- grid_new_accidents_agg %>%
  pivot_longer(cols = starts_with("Ist"), names_to = "involved_type", values_to = "count") %>%
  mutate(involved_type = str_remove(involved_type, "Ist")) %>%
  filter(involved_type %in% c("Rad", "PKW", "PKWxRad")) %>%
  mutate(involved_type_en = case_when(
    involved_type == "Rad" ~ "Bicycle",
    involved_type == "PKW" ~ "Passenger car",
    involved_type == "PKWxRad" ~ "Passenger car & Bicycle"
  ),
  involved_type_en = factor(
    involved_type_en,
    levels = c("Passenger car", "Passenger car & Bicycle", "Bicycle"))) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = count),
          col = NA, size = 0) +
  paletteer::scale_fill_paletteer_c(
    "ggthemes::Red-Black-White Diverging",
    direction =- 1,
    aesthetics = list("fill", "color")) +
  facet_wrap(vars(involved_type_en)) +
  guides(fill = guide_colorsteps(title = "No. of accidents", title.position = "top")) +
  labs(title = plot_titles$title,
       subtitle = plot_titles$subtitle,
       caption = plot_titles$caption) +
  cowplot::theme_map(font_family = "Roboto", font_size = 10) +
  theme(
    plot.background = element_rect(color = NA, fill = "grey90"),
    legend.position = "bottom",
    legend.justification = "center",
    legend.direction = "horizontal",
    legend.key.width = unit(15, "mm"),
    legend.key.height = unit(3, "mm"),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6),
    text = element_text(color = "grey25"),
    strip.text = element_text(face = "bold", hjust = 0.5),
    strip.background = element_rect(color = NA, fill = "grey70"),
    panel.background = element_rect(color = NA, fill = "grey94"),
    plot.title = element_markdown(color = "grey2",
                                  margin = margin(t = 4, b = 8)),
    plot.subtitle = element_textbox_simple(margin = margin(t = 0, b = 12)),
    plot.caption = element_textbox_simple(color = "grey35", size = 6,
                                          margin = margin(t = 12, b = 2))
  )
ggsave(here("plots", "day27-heatmap.png"), plot = p,
       dpi = 300, width = 8, height = 4.5)





