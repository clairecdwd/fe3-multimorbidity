# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# Risk factor figures --------
# Requires: rf_univ_output, rf_agesex_output, rf_agesexsep_output,
#           add_rf_labels(), site_colors, lin1, fn_rf_base
# Creates:  plot_rf_univagesex, plot_rf_agesexsep, fn_rf_fig
# Feeds:    Supplementary figures 7 and 8

# Reference rows arrive from the denominator join with no estimate; they are
# dropped here rather than left for ggplot to remove with a warning.
rf_plot_data <- function(x) {
  x |>
    add_rf_labels() |>
    filter(!is.na(term_label), !is.na(estimate))
}

plot_rf <- function(data, facet = FALSE) {
  p <- data |>
    mutate(`Odds ratio` = estimate) |>
    ggplot(aes(y = term_label,
               xmin = conf.low, xmax = conf.high, x = `Odds ratio`,
               col = site,
               linetype = outcome_label, shape = outcome_label,
               alpha = outcome_label)) +
    geom_point(position = position_dodge2(width = 0.85, reverse = TRUE)) +
    geom_linerange(position = position_dodge2(width = 0.85, reverse = TRUE),
                   linewidth = lin1 * 1.2) +
    geom_vline(xintercept = 1, linewidth = 0.4, linetype = "dashed") +
    coord_cartesian(xlim = c(0, 4)) +
    scale_y_discrete(labels = function(x) str_replace_all(x, "\\(", "\n(")) +
    scale_color_manual(values = site_colors, drop = FALSE) +
    scale_alpha_manual(values = c(1, 0.4)) +
    labs(linetype = "Multimorbidity", shape = "Multimorbidity",
         alpha = "Multimorbidity", col = "Site") +
    theme_minimal() +
    theme(axis.title.y = element_blank())

  if (facet) p <- p + facet_grid(cols = vars(model))
  p
}

## Supplementary figure 7 --------
plot_rf_univagesex <- bind_rows(rf_univ_output, rf_agesex_output) |>
  rf_plot_data() |>
  mutate(model = factor(model,
                        levels = c("Univariable", "Adjusted for age and sex"))) |>
  plot_rf(facet = TRUE)

## Supplementary figure 8 --------
plot_rf_agesexsep <- rf_agesexsep_output |>
  rf_plot_data() |>
  plot_rf()

fn_rf_fig = paste0(
  fn_rf_base, " The dashed vertical line marks an odds ratio of 1. Estimates ",
  "outside the plotted range are not shown.")
