#' INSPIRATION: https://github.com/VictimOfMaths/30DayMapChallenge/blob/main/Day3_Polygons.R
#' Colin Angus

pacman::p_load("tidyverse", "here", "glue", "ggtext", "colorspace",
               "sf", "osmdata", "paletteer", "rvest", "jsonlite")

## GEOMETRIES ==================================================================
## Area of Cologne
cgn_polygon <- getbb("Cologne, Germany", format_out = "sf_polygon")
st_crs(cgn_polygon) <- "EPSG:4326"

#' Football clubs in Cologne
#' https://de.wikipedia.org/wiki/Kategorie:Fu%C3%9Fballverein_aus_K%C3%B6ln
page <- read_html("https://de.wikipedia.org/wiki/Kategorie:Fu%C3%9Fballverein_aus_K%C3%B6ln")
clubs <- html_nodes(page, "div.mw-category ul a") %>%
  html_text()
clubs

# Limit to clubs down to Oberliga (5th division)
clubs_1to5div <- c("1. FC Köln",
                "SV Deutz 05",
                "FC Junkersdorf",
                "FC Pesch",
                "SC Fortuna Köln",
                "FC Viktoria Köln")

#' Football grounds
#' Search on Google Maps
grounds <- tibble(
  club = clubs_1to5div,
  ground = c("RheinEnergie Stadion",
             "BB Bank Sportpark",
             "Ostkampfbahn",
             "Helmut-Kusserow-Sportanlage",
             "Südstadion",
             "Sportpark Höhenberg"),
  coordinates = st_sfc(
    st_point(c(6.875, 50.933611)),
    st_point(c(6.9791233, 50.9259744)),
    st_point(c(6.8755043, 50.933527)),
    st_point(c(6.8688709, 51.001883)),
    st_point(c(6.94361, 50.9175)),
    st_point(c(7.03039, 50.9452))
    ),
  club_icon = c(
    here("data", "Emblem_1.FC_Köln.svg"),
    here("data", "Logo_Deutz_05.svg"),
    NA,
    here("data", "FC_Pesch_Logo.svg"),
    here("data", "SC_Fortuna_Koeln_Logo_since_2019.svg"),
    here("data", "FC_Viktoria_Köln_1904_Logo.svg")
  )
) %>%
  # exclude FC Junkersdorf which uses a ground at Sportpark Müngersdorf (1. FC Köln)
  filter(club != "FC Junkersdorf") %>%
  st_as_sf(crs = 4326)


#' Club icons
#' Attributions:
#'    Viktoria Köln: Von FC Viktoria Köln 1904 e.V. - Extracted from PDF [1] and converted to SVG, Gemeinfrei, https://commons.wikimedia.org/w/index.php?curid=85249908
#'    1. FC Köln: Von Autor unbekannt - https://fc.de/typo3conf/ext/bra_projectfiles/Resources/Public/vorschaltseite/img/svg/01-logo.svg, Gemeinfrei, https://commons.wikimedia.org/w/index.php?curid=99279386
#'    SV Deutz 05: Von Sportvereinigung Deutz 05 e. V. - vectorized from Sportvereinigung Deutz 05 e. V.-Website [1], Gemeinfrei, https://commons.wikimedia.org/w/index.php?curid=85273833
#'    Fortuna Köln: Von S.C. Fortuna Köln e.V. - S.C. Fortuna Köln e.V. - SVG extracted from [1], Gemeinfrei, https://commons.wikimedia.org/w/index.php?curid=86903779
#'    FC Pesch: Von FC Pesch 1956 e.V. - FC Pesch 1956 e.V., Gemeinfrei, https://commons.wikimedia.org/w/index.php?curid=86282215
#'
#'

## VORONOI TESSELATION =========================================================

# Create Voronoi cells based on club grounds
voronoi <- grounds %>%
  st_union() %>%
  st_voronoi() %>%
  st_collection_extract()

# intersect with Cologne shape
voronoi <- voronoi[unlist(st_intersects(grounds, voronoi))] %>%
  st_intersection(cgn_polygon)


## PLOT ========================================================================

seed <- 4711
set.seed(seed)
voronoi %>%
  ggplot() +
  geom_sf(aes(fill = sample(grounds$club, size = nrow(grounds), replace = FALSE)),
          col = "white", size = 0.5, show.legend = FALSE) +
  geom_sf(data = grounds,
          aes(geometry = coordinates), size = 3,
          shape = 21, col = "white", fill = "grey12") +
  ggimage::geom_image(aes(x = st_coordinates(grounds$coordinates)[, "X"]),
                      y = st_coordinates(grounds$coordinates)[, "Y"],
                      image = grounds$club_icon) +
  geom_sf_label(data = grounds,
                aes(geometry = coordinates, label = club),
                size = 3, label.size = 0, label.r = unit(0.05, "lines"),
                fill = "grey12", col = "white", alpha = 0.6,
                hjust = 0, nudge_x = 0.012, nudge_y = -0.005,
                family = "Chivo") +
  scale_fill_paletteer_d("cartography::green.pal", dynamic = TRUE) +
  labs(
    # title = "Support your local club",
    title = "Mer stonn zo dir, FC Kölle<sup>*</sup>",
    subtitle = "Nearest (semi)-professional football teams' grounds to every point in Cologne",
    caption = "<sup>\\*</sup><i>Translation:</i> We stand by you, FC Kölle.<br><br>
    Data: **OpenStreetMap** | Visualization: **Ansgar Wolsing** |
    Images credits: Wikipedia, the respective clubs") +
  cowplot::theme_map() +
  theme(plot.background = element_rect(color = NA, fill = "#1D1D59"),
        text = element_text(family = "Roboto Light", color = "white"),
        plot.title = element_markdown(family = "Roboto", size = 28,
                                      margin = margin(b = 18)),
        plot.subtitle = element_textbox_simple(size = 14),
        plot.caption = element_textbox_simple(size = 9)
        )
set.seed(seed)
ggsave(here("plots/day03_polygons_football_grounds.png"), dpi = 600,
       width = 8, height = 8)

