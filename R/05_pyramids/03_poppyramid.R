# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# Population pyramid of condition counts, by country --------
# Requires: df, pop, ruralperc_gam, ruralperc_sa, country_levels,
#           female_colors, male_colors
# Creates:  poppyramid, fill_levels, condition_palette, lut,
#           legendlabels_poppyramid
# Feeds:    figure 2 panel A
#
# The observed distribution of condition counts within each age band and sex at
# each site is applied to the national population of that age band and sex, and
# the rural and urban sites are combined using the national rural/urban split.
# The bars are therefore numbers of people in the national population, not
# numbers of participants.
#
# The population workbook is read once in 02_prepare/03_population_reference.R
# and used from there.

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

## Observed distribution of condition counts --------
df_counts <- df |>
  summarise(
    `0 conditions`  = sum(multim_n == 0, na.rm = TRUE),
    `1 conditions`  = sum(multim_n == 1, na.rm = TRUE),
    `2 conditions`  = sum(multim_n == 2, na.rm = TRUE),
    `3 conditions`  = sum(multim_n == 3, na.rm = TRUE),
    `4 conditions`  = sum(multim_n == 4, na.rm = TRUE),
    `5+ conditions` = sum(multim_n >= 5, na.rm = TRUE),
    .by = c(con_age_band, con_sex, site, site2)) |>
  pivot_longer(ends_with("conditions")) |>
  mutate(n_age_band = sum(value),
         perc = value / n_age_band,
         .by = c(con_age_band, con_sex, site, site2))

## Applied to the national population --------
# Rural and urban sites are weighted by the share of the national population
# living in each. Zimbabwe contributed an urban site only, which is applied to
# the whole national population.
df_weighted <- df_counts |>
  left_join(pop, by = c("con_age_band" = "age_band", "con_sex" = "sex",
                        "site2" = "country")) |>
  mutate(n = case_when(
    site == "South Africa rural" ~ total_n * perc * ruralperc_sa,
    site == "South Africa urban" ~ total_n * perc * (1 - ruralperc_sa),
    site == "Gambia rural"       ~ total_n * perc * ruralperc_gam,
    site == "Gambia urban"       ~ total_n * perc * (1 - ruralperc_gam),
    site == "Zimbabwe urban"     ~ total_n * perc,
    .default = NA_real_))

# A site renamed upstream, or an age band that fails to match the population
# reference, would otherwise drop a bar from the figure without saying so
stopifnot("population weighting produced missing counts: check the site names above and the age band join" =
            !anyNA(df_weighted$n))

df_poppyramid <- df_weighted |>
  summarise(n = sum(n), .by = c(site2, con_age_band, con_sex, name)) |>
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
poppyramid <- df_poppyramid |>
  ggplot(aes(x = con_age_band, y = n, fill = fill_group)) +
  geom_bar(stat = "identity", position = "stack",
           data = df_poppyramid |> filter(con_sex == "Male")) +
  geom_bar(stat = "identity", position = "stack", aes(y = -n),
           data = df_poppyramid |> filter(con_sex == "Female")) +
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
    labels = function(x) scales::label_comma(accuracy = 1)(abs(x / 1000)),
    name = "N (thousands)")
