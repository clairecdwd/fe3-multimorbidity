# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# Standardised intersection plots --------
# Requires: temp_site, temp_country, set_order, n_intersections, sex_colors
# Creates:  plot_upset_std, plot_upset_site_std, fn_upset_std,
#           path_upset_std, path_upset_site_std
# Feeds:    Figure 1 (country panels), Supplementary figure 3 (site panels)
#
# Bars are the age- and sex-standardised prevalence of each exclusive
# combination, that is the proportion of the national population aged 40 and
# over expected to have that exact combination and no other from those shown.

# Aggregate weighted intersections --------
# Membership is encoded as an integer bitmask so that the exclusive intersection
# for each participant is a single key, which keeps this cheap on wide data.
aggregate_intersections <- function(data, weight_col, sets, n = n_intersections,
                                    by_sex = FALSE) {

  mat <- as.matrix(data[sets])
  mat[is.na(mat)] <- FALSE
  storage.mode(mat) <- "logical"

  has_any <- rowSums(mat) > 0
  mat     <- mat[has_any, , drop = FALSE]
  w_pct   <- data[[weight_col]][has_any] * 100
  sex     <- if (by_sex) as.character(data$con_sex[has_any]) else NULL

  bit = 2L ^ (seq_along(sets) - 1L)
  key = as.integer(mat %*% bit)

  totals <- tibble(key = key, w = w_pct) |>
    summarise(w_total = sum(w, na.rm = TRUE), .by = key) |>
    slice_max(w_total, n = n) |>
    arrange(desc(w_total)) |>
    mutate(intersection_id = row_number())

  bars <- if (by_sex) {
    tibble(key = key, w = w_pct, con_sex = sex) |>
      filter(key %in% totals$key) |>
      summarise(prev_pct = sum(w, na.rm = TRUE), .by = c(key, con_sex)) |>
      left_join(totals |> select(key, intersection_id), by = "key") |>
      select(intersection_id, con_sex, prev_pct)
  } else {
    totals |> transmute(intersection_id, prev_pct = w_total)
  }

  matrix_long <- map2_dfr(totals$intersection_id, totals$key, function(id, k) {
    tibble(intersection_id = id, set = sets, present = as.logical(bitwAnd(k, bit)))
  })

  list(bars = bars, matrix = matrix_long)
}

# One panel: bars above, membership matrix below --------
weighted_upset_panel <- function(data, weight_col, title,
                                 y_max = NULL, hide_y = FALSE,
                                 hide_set_labels = FALSE, fill_by_sex = FALSE) {

  agg <- aggregate_intersections(data, weight_col, set_order,
                                 n = n_intersections, by_sex = fill_by_sex)

  # The weights must sum to 100 within the unit being standardised; if they do
  # not, a stratum failed to match the population reference
  total_w = sum(data[[weight_col]], na.rm = TRUE) * 100
  if (abs(total_w - 100) > 1) {
    warning("Panel '", title, "': weights sum to ", round(total_w, 2),
            ", expected 100.", call. = FALSE)
  }

  agg$bars$intersection_id   <- factor(agg$bars$intersection_id,
                                       levels = seq_len(n_intersections))
  agg$matrix$intersection_id <- factor(agg$matrix$intersection_id,
                                       levels = seq_len(n_intersections))
  agg$matrix$set <- factor(agg$matrix$set, levels = rev(set_order))

  bar_labels <- if (fill_by_sex) {
    agg$bars |> summarise(prev_pct = sum(prev_pct), .by = intersection_id)
  } else {
    agg$bars
  }

  p_bars <- ggplot(agg$bars, aes(x = intersection_id, y = prev_pct)) +
    {if (fill_by_sex) geom_col(aes(fill = con_sex), width = 0.9)
     else geom_col(fill = "grey30", width = 0.9)} +
    geom_text(data = bar_labels, aes(label = sprintf("%.1f", prev_pct)),
              vjust = -0.3, size = 2.6) +
    {if (fill_by_sex) scale_fill_manual(values = sex_colors)} +
    scale_x_discrete(drop = FALSE) +
    {if (!is.null(y_max)) coord_cartesian(ylim = c(0, y_max))} +
    labs(y = "Prevalence of exact combination (%)", x = NULL, title = title) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major.x = element_line(colour = "grey92"),
          panel.grid.major.y = element_line(colour = "grey92"),
          panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold", size = 12),
          axis.title.y = if (hide_y) element_blank() else element_text(size = 10),
          axis.text.y  = if (hide_y) element_blank() else element_text(size = 9))

  connectors <- agg$matrix |>
    filter(present) |>
    summarise(set_min = set[which.min(as.integer(set))],
              set_max = set[which.max(as.integer(set))],
              .by = intersection_id) |>
    filter(set_min != set_max)

  p_matrix <- ggplot(agg$matrix, aes(x = intersection_id, y = set)) +
    geom_point(aes(colour = present), size = 2.5) +
    {if (nrow(connectors) > 0)
      geom_segment(data = connectors,
                   aes(x = intersection_id, xend = intersection_id,
                       y = set_min, yend = set_max),
                   colour = "black", linewidth = 0.5, inherit.aes = FALSE)} +
    scale_colour_manual(values = c(`TRUE` = "black", `FALSE` = "grey80")) +
    scale_x_discrete(drop = FALSE) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "grey97", linewidth = 5),
          panel.grid.minor = element_blank(),
          axis.text.y = if (hide_set_labels) element_blank() else element_text(size = 9))

  p_bars / p_matrix + plot_layout(heights = c(2, 1))
}

# Common y limit, taken from the data rather than hardcoded --------
upset_std_ymax = max(
  aggregate_intersections(temp_site, "w_site", set_order)$bars$prev_pct,
  aggregate_intersections(temp_country, "w_country", set_order)$bars$prev_pct
) * 1.1

## Site panels: Supplementary figure 3 --------
plot_upset_site_std <-
  (weighted_upset_panel(temp_site |> filter(site == "Gambia rural"),
                        "w_site", "Gambia rural", y_max = upset_std_ymax) |
   weighted_upset_panel(temp_site |> filter(site == "Gambia urban"),
                        "w_site", "Gambia urban", y_max = upset_std_ymax,
                        hide_y = TRUE, hide_set_labels = TRUE) |
   weighted_upset_panel(temp_site |> filter(site == "Zimbabwe urban"),
                        "w_site", "Zimbabwe urban", y_max = upset_std_ymax,
                        hide_y = TRUE, hide_set_labels = TRUE) |
   weighted_upset_panel(temp_site |> filter(site == "South Africa rural"),
                        "w_site", "South Africa rural", y_max = upset_std_ymax,
                        hide_y = TRUE, hide_set_labels = TRUE) |
   weighted_upset_panel(temp_site |> filter(site == "South Africa urban"),
                        "w_site", "South Africa urban", y_max = upset_std_ymax,
                        hide_y = TRUE, hide_set_labels = TRUE))

## Country panels: Figure 1 --------
plot_upset_std <-
  (weighted_upset_panel(temp_country |> filter(country == "Gambia"),
                        "w_country", "Gambia", y_max = upset_std_ymax,
                        fill_by_sex = TRUE) |
   weighted_upset_panel(temp_country |> filter(country == "Zimbabwe"),
                        "w_country", "Zimbabwe", y_max = upset_std_ymax,
                        hide_y = TRUE, hide_set_labels = TRUE, fill_by_sex = TRUE) |
   weighted_upset_panel(temp_country |> filter(country == "South Africa"),
                        "w_country", "South Africa", y_max = upset_std_ymax,
                        hide_y = TRUE, hide_set_labels = TRUE, fill_by_sex = TRUE))

path_upset_std      = file.path(plot_dir, "plot_upset_standardised.png")
path_upset_site_std = file.path(plot_dir, "plot_upset_sites_standardised.png")

ggsave(path_upset_std, plot = plot_upset_std, width = 12, height = 8, dpi = 300)
ggsave(path_upset_site_std, plot = plot_upset_site_std, width = 15, height = 9, dpi = 300)

fn_upset_std = paste0(
  "Intersections shown are exclusive, that is the proportion of people with that ",
  "unique combination of conditions is shown. Largest ", n_intersections,
  " exclusive intersections shown per panel, ranked by age- and sex-standardised ",
  "prevalence of the unique disease combination. Intersection prevalence ",
  "(annotation, %) is the proportion of the national population aged 40 years and ",
  "over expected to have that exact combination of conditions, with no others from ",
  "the ", length(set_order), " listed. The 'no conditions' intersection is excluded from ",
  "display. Standardised by direct standardisation to national population structure ",
  "(sex by ", paste(age_strata, collapse = ", "), " years). Country panels additionally ",
  "weight the rural and urban sub-sites by their national population proportions; ",
  "Zimbabwe contributed an urban site only, which is assumed representative.")
