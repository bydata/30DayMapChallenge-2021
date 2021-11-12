pacman::p_load("tidyverse", "sf", "glue", "here", "scico", "rnaturalearth",
               "countrycode", "cartogram", "ggtext", "gganimate")

world <- ne_countries(returnclass = "sf") %>%
  st_transform(crs = "+proj=moll")

europe <- ne_countries(continent = "Europe", returnclass = "sf") %>%
  st_transform(crs = "+proj=moll")


ggplot(world) +
  geom_sf(aes(geometry = geometry),
          size = 0.1) +
  coord_sf() +
  theme_minimal()

ggplot(europe) +
  geom_sf(aes(geometry = geometry),
          size = 0.1) +
  coord_sf(xlim = c(-10, 30), ylim = c(20, 60)) +
  theme_minimal()


## GET DATA ====================================================================
url <- "https://www.offenedaten-koeln.de/sites/default/files/Geburts_Zuzugsorte_Koeln_V1.csv"
immi <- read_csv2(url,
                  col_names = c("bundesland_staat_nr", "bundesland_staat",
                                "ags_staat_schluessel", "gemeinde_staat",
                                "n_geburtsort", "n_zuzug"), skip = 1)
immi_countries <- immi %>%
  # exclude German federal states (id 001-016)
  filter(bundesland_staat_nr > "016") %>%
  rename(country = bundesland_staat) %>%
  mutate(country = ifelse(country == "Weißrussland", "Belarus", country)) %>%
  arrange(-n_geburtsort) %>%
  mutate(country_en = countrycode(country,
                                  origin = "country.name.de",
                                  destination = "country.name"),
         country_code = countrycode(country,
                                    origin = "country.name.de",
                                    destination = "iso3c"))

immi_countries %>%
  anti_join(st_drop_geometry(world), by = c("country_code" = "gu_a3"))

immi_countries_world <- immi_countries %>%
  right_join(world, by = c("country_code" = "gu_a3")) %>%
  st_as_sf()
class(immi_countries_world)

immi_countries_world %>%
  ggplot() +
  # geom_sf(data = world, size = 0.1, col = "grey89", fill = "grey80") +
  geom_sf(aes(geometry = geometry, fill = n_geburtsort),
          size = 0.1, col = "grey89") +
  # scale_fill_binned(trans = "pseudo_log") +
  scale_fill_viridis_c(trans = "pseudo_log") +
  # scico::scale_fill_scico(palette = "bamako") +
  coord_sf() +
  theme_minimal() +
  theme(axis.text = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(10, "mm"),
        legend.key.height = unit(3, "mm"),
        plot.background = element_rect(color = NA, fill = "grey12"),
        panel.grid = element_line(size = 0.1, color = "grey89"))
ggsave(here("plots", "day12-population.png"),
       dpi = 600, width = 10, height = 9)

#' https://r-charts.com/spatial/cartogram-ggplot2/
carto_data_immi <- immi_countries_world %>%
  # mutate(n_geburtsort = replace_na(n_geburtsort, 0)) %>%
  mutate(representation = "contiguous cartogram") %>%
  cartogram_cont(weight = "n_geburtsort")

ggplot(carto_data_immi) +
  geom_sf(aes(fill = n_geburtsort),
          col = NA) +
  geom_sf_label(data = filter(carto_data_immi, n_geburtsort > 2500),
               aes(label = country_code),
               stat = "sf_coordinates",
               size = 3, fill = "#1d3242", family = "Chivo",
               label.size = 0, alpha = 0.8, col = "white") +
  scale_fill_scico(palette = "bamako", na.value = "grey90") +
  coord_sf() +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(10, "mm"),
        legend.key.height = unit(3, "mm"),
        plot.background = element_rect(color = NA, fill = "white"),
        # panel.grid = element_line(size = 0.1, color = "grey89"),
        panel.grid= element_blank()
        )
ggsave(here("plots", "day12-population-cartogram.png"),
       dpi = 600, width = 10, height = 9)


#' https://r-charts.com/spatial/cartogram-ggplot2/
carto_data_immi_dorling <- immi_countries_world %>%
  mutate(representation = "dorling cartogram") %>%
  filter(!is.na(n_geburtsort)) %>%
  # mutate(n_geburtsort = replace_na(n_geburtsort, 0)) %>%
  cartogram_dorling(weight = "n_geburtsort")

ggplot(carto_data_immi_dorling) +
  geom_sf(aes(fill = n_geburtsort),
          col = NA)


representations_df <- immi_countries_world %>%
  mutate(representation = "map (mollweide projection)") %>%
  bind_rows(carto_data_immi, carto_data_immi_dorling) %>%
  mutate(representation = factor(representation,
                                 levels = c("map (mollweide projection)", "contiguous cartogram", "dorling cartogram"))) %>%
  filter(!(representation %in% c("contiguous cartogram", "dorling cartogram") & is.na(n_geburtsort)))


p <- representations_df %>%
  # repair a few invalid geoms
  # https://r-spatial.org/r/2017/03/19/invalid.html#corrup-or-invalid-geometries
  st_make_valid() %>%
  # TODO: experimentation with missing countries
  filter(!is.na(n_geburtsort)) %>%
  # remove Antarktika which only appears in the map representation
  filter(geounit != "Antarctica") %>%
  ggplot() +
  geom_sf(aes(fill = n_geburtsort),
          col = NA) +
  geom_sf_label(data = . %>% filter(n_geburtsort > 4000),
                aes(label = country_code),
                stat = "sf_coordinates",
                size = 2, fill = "#1d3242", family = "Chivo",
                label.size = 0, alpha = 0.3, col = "white") +
  viridis::scale_fill_viridis(option = "E", trans = "log2",
                              labels = scales::label_number(),
                              breaks = c(300, 1000, 5000, 30000),
                              name = "# of residents") +
  # facet_wrap(vars(representation)) +
  coord_sf() +
  labs(title = "Country of birth of Cologne residents",
       subtitle = "Cologne residents by country of birth outside Germany as of 2014.
       Listed are places of birth with more than 100 persons.
       In the cartograms, the countries of origin are enlarged or reduced according
       to the number of inhabitants of Cologne.
       <br><br><br>
       <span style='font-family:Helvetica Neue Medium;font-size:10pt;color:grey50'>
       {str_to_title(closest_state)}
       </span>",
       caption = "Data: <b style='font-family:Helvetica Neue'>Open Data Cologne</b>,
       <b style='font-family:Helvetica Neue'>NaturalEarthData</b> |
       Visualization: <b style='font-family:Helvetica Neue'>Ansgar Wolsing</b>") +
  theme_minimal(base_family = "Helvetica Neue Light", base_size = 9) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        # legend.position = "bottom",
        legend.position = c(0.075, 0.25),
        legend.key.width = unit(3, "mm"),
        legend.key.height = unit(6, "mm"),
        legend.text = element_text(size = 6),
        plot.background = element_rect(color = NA, fill = "white"),
        # panel.grid = element_line(size = 0.1, color = "grey89"),
        panel.grid = element_blank(),
        text = element_text(color = "grey35", lineheight = 1.25),
        plot.title = element_text(family = "Oswald",
                                  color = "black", face = "plain",
                                  margin = margin(t = 6, b = 8)),
        plot.title.position = "plot",
        plot.subtitle = element_textbox_simple(size = 8, margin = margin(b = 0)),
        plot.caption = element_markdown(size = 6, color = "grey45", hjust = 0)
  )
p

p_anim <- p + transition_states(representation, transition_length = 3, state_length = 2)
animate(p_anim, width = 1200, height = 900, res = 200, units = "px")
anim_save(here("plots", "day12-population-animated.gif"),
          nframes = 180, fps = 24, rewind = TRUE, device = "ragg_png", units = "in")

