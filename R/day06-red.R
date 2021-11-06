pacman::p_load("tidyverse", "here", "glue", "ggtext", "colorspace", "ggforce")

## Grid of Cologne's districts ===============================
grid <- read_csv(here::here("data", "grid_koeln_stadtteile.csv"))
grid

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


btw_winner_district <-
  btw_results_districts_long %>%
  select(gebiet_name, stimme, partei, stimmenanteil)   %>%
  group_by(gebiet_name, stimme) %>%
  arrange(-stimmenanteil, .by_group = TRUE) %>%
  mutate(rang = rank(-stimmenanteil, ties.method = "first"),
         difference = stimmenanteil - lead(stimmenanteil)) %>%
  slice_max(order_by = stimmenanteil, n = 1, with_ties = FALSE) %>%
  ungroup()


# Party colors
party_colors <- c("CDU/CSU" = "grey9",
                  "SPD" = "#ca0002", ## "#E3000F",
                  "Grüne" = rgb(100, 161, 45, maxColorValue = 255),
                  "FDP" = darken("#ffed00", 0.1),
                  "Linke" = "purple",
                  "AfD" = rgb(0, 158, 224, maxColorValue = 255))


## PLOT ========================================================================

## Prepared data frame for plotting
df_plot <- btw_results_districts_long %>%
  select(gebiet_name, stimme, partei, stimmenanteil)  %>%
  mutate(partei = str_to_upper(partei)) %>%
  filter(partei == "SPD") %>%
  inner_join(grid, by = c("gebiet_name" = "gebiet-name"))


# Draw the grid map with customized titles
draw_grid_map <- function(df, plot_titles) {
  df %>%
    mutate(stimme = ifelse(stimme == "erst", plot_titles$first_vote,
                           plot_titles$second_vote)) %>%
    ggplot(aes(col, row)) +
    geom_point(
      aes(fill = partei, alpha = stimmenanteil),
      size = 11.5, shape = 22, stroke = 1.5, color = "transparent"
    ) +
    geom_point(
      aes(color = partei),
      size = 11.5, shape = 22, stroke = 1.5, fill = "transparent"
    ) +
    # geom_text(aes(label = paste(col, row, sep = "x"))) +
    coord_fixed() +
    scale_x_continuous(limits = c(-.5, max(grid$col) + 2)) +
    scale_y_reverse(expand = c(.05, .05)) +
    scale_alpha_continuous(range = c(0.1, 1),
                           breaks = seq(0.1, 0.4, 0.1),
                           # show labels in percent format
                           labels = paste(100 * seq(0.1, 0.4, 0.1), "%")
                           ) +
    scale_color_manual(values = party_colors, name = NULL) +
    scale_fill_manual(values = party_colors, name = NULL) +
    facet_wrap(vars(stimme), ncol = 2) +
    guides(fill = "none", col = "none",
           alpha = guide_legend(override.aes = list(
             fill = party_colors["SPD"], color = party_colors["SPD"],
             size = 6),
             # title.position = "top"
             )) +
    labs(title = plot_titles$title,
         subtitle = plot_titles$subtitle,
         caption = plot_titles$caption,
         alpha = plot_titles$alpha) +
    theme_minimal(base_family = "Roboto", base_size = 16) +
    theme(plot.background = element_rect(color = NA, fill = "white"),
          legend.position = "bottom",
          legend.key.width = unit(4, "mm"),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          panel.grid = element_blank(),
          text = element_text(color = "grey40", lineheight = 1.2),
          plot.title = element_markdown(family = "Source Sans Pro SemiBold",
                                        color = "black", size = 24,
                                        margin = margin(t = 4, b = 12)),
          plot.subtitle = element_textbox_simple(size = 14,
                                                 margin = margin(t = 2, b = 16)),
          plot.caption = element_textbox_simple(margin = margin(t = 20, b = 4)),
          strip.text = element_text(face = "bold", color = "grey45", size = 16,
                                    margin = margin(t = 12, b = 20)),
          panel.spacing.x = unit(16, "mm"),
          panel.background = element_rect(color = NA),
          axis.title = element_blank(),
          axis.text = element_blank())
}


## English version -------------------------------------------------------------

plot_titles_en <- list(
  title = glue("Vote shares of <b style='color:{party_colors[\"SPD\"]}'>
  Social Democrats (SPD)</b><br>
  in the German Federal Election 2021 in Cologne"),
  subtitle = "Each tile represents a district of Cologne.
  The more *intense* the color of a tile, the *higher* the vote share of the SPD
  within that district.",
  caption = "Data: **Stadt Köln** |
       Grid: **Ansgar Wolsing & Cedric Scherer** |
       Visualization: **Ansgar Wolsing**",
  alpha = "Vote share SPD",
  first_vote = "First vote (candidate)",
  second_vote = "Second vote (party)")

draw_grid_map(df_plot, plot_titles_en)

ggsave(here::here("plots", "day06_red_vote-share-spd_en.png"),
       device = ragg::agg_png,
       dpi = 600, width = 12, height = 8.5)


## German version -------------------------------------------------------------

plot_titles_de <- list(
  title = glue("Stimmenanteile der <b style='color:{party_colors[\"SPD\"]}'>
  SPD</b> in den Kölner Stadtteilen bei der Bundestagswahl 2021"),
  subtitle = "Jede Kachel repräsentiert einen Kölner Stadtteil.
  Je stärker die Färbung einer Kachel, desto höher der Stimmenanteil der SPD in
  diesem Stadtteil.",
  caption = "Quelle: **Stadt Köln** |
       Grid: **Ansgar Wolsing & Cedric Scherer** |
       Visualisierung: **Ansgar Wolsing**",
  alpha = "Stimmenanteil SPD",
  first_vote = "Erststimmen",
  second_vote = "Zweitstimmen")

# Districts of Mülheim borough
districts_lauterbach <- c("Buchforst", "Buchheim", "Dellbrück", "Dünnwald",
                          "Flittard", "Höhenhaus", "Holweide", "Mülheim",
                          "Stammheim")

df_plot %>%
  # filter(stimme == "erst") %>%
  # Tweaking dataframe for Lauterbach annotation
  mutate(wk_lauterbach = gebiet_name %in% districts_lauterbach) %>%
draw_grid_map(plot_titles_de) +
  ## Lauterbach annotation
  geom_mark_rect(aes(fill = wk_lauterbach,
                     filter = wk_lauterbach & stimme == "Erststimmen",
                     label = str_wrap("Hier gewann Karl Lauterbach
                                        das Direktmandat", 16)
                     ),
                 label.buffer = unit(1, "mm"),
                 # do not fill the rectangle, this would imply a higher vote share
                 fill = NA,
                 label.colour = "grey38",
                 size = 0.9, color = "grey20", label.fontsize = 9.5)
ggsave(here::here("plots", "day06_red_vote-share-spd_de.png"),
       device = ragg::agg_png,
       dpi = 600, width = 12, height = 8.5)

