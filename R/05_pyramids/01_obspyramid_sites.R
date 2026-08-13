# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# Observed condition counts by age, sex and site --------
# Requires: df, site_levels, female_colors, male_colors
# Creates:  obspyramid_sites, obspyramid_sites_exchiv,
#           fill_levels, condition_palette, lut, legendlabels_poppyramid
# Feeds:    supplementary figure 4
#
# Proportion of participants with 0 to 5+ conditions, within each age band, sex
# and site. These are observed sample proportions: no population standardisation
# is applied, so the two halves of each bar are each rescaled to 100%.
#
# The main and the excluding-HIV versions differ only in the count variable, so
# one function is defined here and called twice.

## Fill scale --------
# One sequential palette per sex. Levels run 5 down to 0 so that the darker
# fills, meaning more conditions, stack outwards from the centre line.
fill_levels = c(paste("Female", 5:0, sep = "_"),
                paste("Male",   5:0, sep = "_"))

condition_palette = c(setNames(female_colors, paste("Female", 0:5, sep = "_")),
                      setNames(male_colors,   paste("Male",   0:5, sep = "_")))

count_labels = c("5+ conditions", "4 conditions", "3 conditions",
                 "2 conditions", "1 condition", "0 conditions")

# Grouped legend: one block per sex, each showing the same six count labels.
# Built from fill_levels rather than from the data, so the legend order is fixed
# whatever order the rows arrive in.
lut <- key_group_lut(group   = str_remove(fill_levels, "_[0-9]+$"),
                     members = fill_levels)

legendlabels_poppyramid <- scale_fill_manual(
  name   = "N conditions",
  breaks = rev(c(paste("Male", 5:0, sep = "_"), paste("Female", 5:0, sep = "_"))),
  labels = rev(rep(count_labels, times = 2)),
  values = condition_palette)

## Pyramid --------
# count_var is the number of conditions: multim_n, or multim_n_exchiv for the
# sensitivity analysis that does not count HIV as a condition.
build_obspyramid_sites <- function(count_var) {

  # A missing count would be silently dropped from a bar rather than shown, so
  # it fails here instead
  stopifnot("condition count must not be missing" = !anyNA(df[[count_var]]))

  df_obs <- df |>
    summarise(
      `0 conditions`  = sum(.data[[count_var]] == 0,  na.rm = TRUE),
      `1 conditions`  = sum(.data[[count_var]] == 1,  na.rm = TRUE),
      `2 conditions`  = sum(.data[[count_var]] == 2,  na.rm = TRUE),
      `3 conditions`  = sum(.data[[count_var]] == 3,  na.rm = TRUE),
      `4 conditions`  = sum(.data[[count_var]] == 4,  na.rm = TRUE),
      `5+ conditions` = sum(.data[[count_var]] >= 5,  na.rm = TRUE),
      .by = c(con_age_band, con_sex, site)) |>
    pivot_longer(ends_with("conditions")) |>
    mutate(
      name = factor(name, levels = c("5+ conditions", "4 conditions",
                                     "3 conditions", "2 conditions",
                                     "1 conditions", "0 conditions")),
      fill_group = paste(con_sex, name, sep = "_"),
      fill_group = str_remove(fill_group, " conditions"),
      fill_group = str_remove(fill_group, "\\+"),
      fill_group = factor(fill_group, levels = fill_levels),
      site = factor(site, levels = site_levels),
      # Hyphen rather than en dash in the axis labels, which are rendered in a
      # narrow facet strip
      con_age_band = str_replace(con_age_band, "–", "-"))

  df_obs |>
    ggplot(aes(x = con_age_band, y = value, fill = fill_group)) +
    geom_bar(stat = "identity", position = "fill",
             data = df_obs |> filter(con_sex == "Male")) +
    geom_bar(stat = "identity", position = "fill", aes(y = -value),
             data = df_obs |> filter(con_sex == "Female")) +
    coord_flip() +
    labs(x = "Age category", y = "N") +
    geom_hline(yintercept = 0, col = "grey40") +
    facet_wrap(vars(site)) +
    theme_minimal() +
    guides(fill = guide_legend_group(key = lut, nrow = 3)) +
    # Standalone appearance only. The compile script adds plot_standardformat,
    # which is a complete theme and so replaces everything set here.
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(size = 8),
      legendry.legend.subtitle.position = "top",
      legendry.legend.subtitle = element_text(size = 9),
      strip.text.x = element_text(size = 9, face = "bold"),
      axis.title.x = element_text(size = 10, margin = margin(t = 8)),
      axis.title.y = element_text(size = 10, margin = margin(r = 8))) +
    legendlabels_poppyramid +
    scale_y_continuous(
      labels = function(x) scales::label_comma(accuracy = 2)(abs(x * 100)),
      name = "%")
}

obspyramid_sites        <- build_obspyramid_sites("multim_n")
obspyramid_sites_exchiv <- build_obspyramid_sites("multim_n_exchiv")
