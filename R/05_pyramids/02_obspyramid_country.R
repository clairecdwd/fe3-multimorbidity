# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# Observed condition counts by age, sex and country --------
# Requires: df, country_levels, female_colors, male_colors
# Creates:  obspyramid, fill_levels, condition_palette, lut,
#           legendlabels_poppyramid
# Feeds:    figure 2 panel B
#
# Proportion of participants with 0 to 5+ conditions, within each age band and
# sex, pooled across the sites in each country. These are pooled sample
# proportions, not population-standardised values: participants contribute
# according to how many were recruited at each site, so this panel is read
# alongside panel A, which applies the national rural/urban weighting.
#
# `lut` and `legendlabels_poppyramid` are defined here rather than relied on
# from another pyramid script, so this file stands alone whatever the source
# order.

## Fill scale --------
# One sequential palette per sex. Levels run 5 down to 0 so that the darker
# fills, meaning more conditions, stack outwards from the centre line.
fill_levels = c(paste("Female", 5:0, sep = "_"),
                paste("Male",   5:0, sep = "_"))

condition_palette = c(setNames(female_colors, paste("Female", 0:5, sep = "_")),
                      setNames(male_colors,   paste("Male",   0:5, sep = "_")))

count_labels = c("5+ conditions", "4 conditions", "3 conditions",
                 "2 conditions", "1 condition", "0 conditions")

lut <- key_group_lut(group   = str_remove(fill_levels, "_[0-9]+$"),
                     members = fill_levels)

legendlabels_poppyramid <- scale_fill_manual(
  name   = "N conditions",
  breaks = rev(c(paste("Male", 5:0, sep = "_"), paste("Female", 5:0, sep = "_"))),
  labels = rev(rep(count_labels, times = 2)),
  values = condition_palette)

## Condition counts --------
stopifnot("condition count must not be missing" = !anyNA(df$multim_n))

df_obs_country <- df |>
  summarise(
    `0 conditions`  = sum(multim_n == 0, na.rm = TRUE),
    `1 conditions`  = sum(multim_n == 1, na.rm = TRUE),
    `2 conditions`  = sum(multim_n == 2, na.rm = TRUE),
    `3 conditions`  = sum(multim_n == 3, na.rm = TRUE),
    `4 conditions`  = sum(multim_n == 4, na.rm = TRUE),
    `5+ conditions` = sum(multim_n >= 5, na.rm = TRUE),
    .by = c(con_age_band, con_sex, site2)) |>
  pivot_longer(ends_with("conditions")) |>
  mutate(
    name = factor(name, levels = c("5+ conditions", "4 conditions",
                                   "3 conditions", "2 conditions",
                                   "1 conditions", "0 conditions")),
    fill_group = paste(con_sex, name, sep = "_"),
    fill_group = str_remove(fill_group, " conditions"),
    fill_group = str_remove(fill_group, "\\+"),
    fill_group = factor(fill_group, levels = fill_levels),
    site2 = factor(as.character(site2), levels = country_levels),
    # Hyphen rather than en dash in the axis labels, which are rendered in a
    # narrow facet strip
    con_age_band = str_replace(con_age_band, "–", "-"))

## Pyramid --------
obspyramid <- df_obs_country |>
  ggplot(aes(x = con_age_band, y = value, fill = fill_group)) +
  geom_bar(stat = "identity", position = "fill",
           data = df_obs_country |> filter(con_sex == "Male")) +
  geom_bar(stat = "identity", position = "fill", aes(y = -value),
           data = df_obs_country |> filter(con_sex == "Female")) +
  coord_flip() +
  labs(x = "Age category", y = "N", fill = "N conditions") +
  geom_hline(yintercept = 0, col = "grey40") +
  facet_wrap(vars(site2), scales = "free") +
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
