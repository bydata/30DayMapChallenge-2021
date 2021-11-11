pacman::p_load("tidyverse", "here", "glue", "ggtext", "colorspace", "ggforce",
               "rayshader", "sf")

## Election results in Cologne's districts ===================

url_btw_results_districts <- "https://wahlen.stadt-koeln.de/prod/BTW2021/05315000/praesentation/Open-Data-Bundestagswahl4711.csv?ts=1632676666514"
btw_results_districts <- read_csv2(url_btw_results_districts)

# D1 / F1 : Christlich Demokratische Union Deutschlands
# D2 / F2 : Sozialdemokratische Partei Deutschlands
# D3 / F3 : Freie Demokratische Partei
# D4 / F4 : Alternative für Deutschland
# D5 / F5 : BÜNDNIS 90/DIE GRÜNEN
# D6 / F6 : DIE LINKE
# D7 / F7 : Partei für Arbeit, Rechtsstaat, Tierschutz, Elitenförderung und basisdemokratische Initiative

colnames(btw_results_districts) <- janitor::make_clean_names(colnames(btw_results_districts))
# Rename variables for biggest parties and remove the others
btw_results_districts_long <- btw_results_districts %>%
  rename(wahlberechtigte = a,
         waehler = b,
         erst_ungueltig = c,
         erst_gueltig = d,
         zweit_ungueltig = e,
         zweit_gueltig = f,
         erst_cdu = d1,
         zweit_cdu = f1,
         erst_spd = d2,
         zweit_spd = f2,
         erst_fdp = d3,
         zweit_fdp = f3,
         erst_afd = d4,
         zweit_afd = f4,
         erst_gruene = d5,
         zweit_gruene = f5,
         erst_linke = d6,
         zweit_linke = f6,
         erst_diepartei = d7,
         zweit_diepartei = f7) %>%
  select(datum:zweit_diepartei, -c(a1:a3, b1)) %>%
  # calculate vote shares
  mutate(across(erst_cdu:erst_diepartei, .fns = ~.x/erst_gueltig, .names = "{.col}_perc"),
         across(zweit_cdu:zweit_diepartei, .fns = ~.x/zweit_gueltig, .names = "{.col}_perc")) %>%
  select(-c(erst_cdu:zweit_diepartei)) %>%
  pivot_longer(cols = c(erst_cdu_perc:zweit_diepartei_perc),
               names_to = "variable",
               values_to = "stimmenanteil") %>%
  separate(variable, into = c("stimme", "partei"), sep = "_")


wbt_districts <- btw_results_districts_long %>%
  distinct(gebiet_name, wahlberechtigte, waehler) %>%
  mutate(wbt = waehler / wahlberechtigte)

# Overall turnout in Cologne (calculated on sums)
wbt_cgn <- wbt_districts %>%
  summarize(wbt = sum(waehler) / sum(wahlberechtigte)) %>%
  pull(wbt)



## GEOMETRY ====================================================================
#' https://www.offenedaten-koeln.de/dataset/stadtteile
#' https://www.offenedaten-koeln.de/sites/default/files/Stadtteil.zip

shp_districts <- st_read(here("data", "cologne_stadtteile", "Stadtteil", "Stadtteil.shp"))
shp_districts <- st_transform(shp_districts, crs = 4326)


## PLOT ========================================================================


p <- wbt_districts %>%
  inner_join(shp_districts, by = c("gebiet_name" = "STT_NAME")) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = wbt),
          size = 0, color = NA) +
  scale_fill_continuous_diverging(palette = "Vik",
                              mid = wbt_cgn,
                              aesthetics = c("fill"),
                              labels = scales::label_percent(accuracy = 1)
                              ) +
  guides(
    fill = guide_colorbar(title = "Turnout"),
    # fill = "none",
           color = "none") +
  cowplot::theme_map(font_family = "Roboto") +
  theme(plot.background = element_rect(color = NA, fill = "white"),
        legend.text = element_text(color = "grey50"),
        legend.position = "bottom",
        legend.key.width = unit(10, "mm"),
        legend.key.height = unit(2.5, "mm")
        )

plot_gg(p, width = 7, height = 8,
        height_aes = "fill",
        multicore = TRUE,
        preview = FALSE,
        raytrace = TRUE,
        phi = 35, theta = 0,
        scale = 300, zoom = 0.45,
        # params passed to plot_3d()
        solid = TRUE,
        solidcolor = "white",
        soliddepth = 0,
        background = "white",
        baseshape =  "circle",
        obj_material = rayrender::diffuse(),
        ground_material = rayrender::diffuse(),
        # reduce_size = 0,
        linewidth = 0, # default: 2
        windowsize = c(1600, 1600) # see https://github.com/tylermorganwall/rayshader/issues/70
        )

# render_label with plot_gg: https://github.com/tylermorganwall/rayshader/issues/82

title <- "Turnout in Cologne's districts (Federal Election 2021)"
title_font <- "Oswald"
title_size <- 48

render_snapshot(here("plots", "day11-3d-turnout_snapshot.png"),
                ground_size = 100000,
                title_text = title,
                title_font = title_font,
                title_size = title_size,
                smooth_line = TRUE,
                # software_render = TRUE,
                # webshot = TRUE,
                cache_filename = here("plots", "day11-3d-snapshot-cache.png"),
                windowsize = c(1600, 1600)
)

render_highquality(here("plots", "day11-3d-turnout-hi.png"),
                   title_text = title,
                   title_font = title_font,
                   title_size = title_size,
                   # smooth_line = TRUE,
                   # software_render = TRUE,
                   cache_filename = here("plots", "day11-3d-snapshot-hi-cache.png")
)


#' from: https://arthurwelle.github.io/RayshaderWalkthrough/index.html
#parameters for 360 positions
phivechalf <- 30 + 60 * 1/(1 + exp(seq(-7, 20, length.out = 180)/2))
phivecfull <- c(rep("88.2", 30), phivechalf, rev(phivechalf))
thetavec <- c(rep("0", 30), 0 + 60 * sin(seq(0,359,length.out = 360) * pi/180))
zoomvec <- 0.25 + 0.4 * 1/(1 + exp(seq(-5, 20, length.out = 180)))
zoomvecfull <- c(rep("0.65", 30),zoomvec, rev(zoomvec))

render_movie(here("plots", "day11-3d-turnout.gif"),
             type = "custom",
             width = 8, height = 6,
             frames = 390, fps = 24,
             hi = phivecfull,
             zoom = zoomvecfull,
             theta = thetavec,
             title_text = title,
             title_font = title_font,
             title_size = title_size)

render_movie(here("plots", "day11-3d-turnout.mp4"),
             type = "custom",
             width = 8, height = 6,
             frames = 390, fps = 24,
             hi = phivecfull,
             zoom = zoomvecfull,
             theta = thetavec,
             title_text = title,
             title_font = title_font,
             title_size = title_size)


rgl::rgl.clear()
rgl::rgl.close()

#' If needed, compress and optimize GIF at:
#' 1. https://www.iloveimg.com/compress-image/compress-gif
#' 2. https://ezgif.com/optimize
