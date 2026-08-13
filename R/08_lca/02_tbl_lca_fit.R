# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# Latent class model fit and class composition --------
# Requires: 08_lca/01_read_lca_output.R (lca_fit_raw, df_lca, condition_lookup),
#           00_settings/02_parameters.R (defn_mm), 00_settings/03_colours.R
#           (two_cols), 00_settings/04_plots.R (plot_standardformat, save_fig)
# Creates:  lca_oe, lca_oe_included, table_lca_fit, table_lca_postprob,
#           plot_lca_oe_nogam
# Creates:  supplementary table 11 - table_lca_fit
#           supplementary table 12 - table_lca_postprob
#           figure 3               - plot_lca_oe_nogam
# Feeds:    R/09_compile/01_compile_main.R and 02_compile_supplement.R

# Model fit statistics (supplementary table 11) --------
table_lca_fit <- lca_fit_raw |>
  select(Site = site_label, N, Model, df, AIC, BIC, SSBIC) |>
  flextable() |>
  merge_v(j = c(1, 2)) |>
  tidy_table()

# Observed and expected condition frequencies --------
# For each site, class and condition: the proportion of the class with the
# condition, over the proportion of the whole site with it. A ratio above one
# means the condition is over-represented in that class.
#
# na.rm so that a missing condition status does not void the cell. Missing
# status counts as not having the condition, which is how the condition
# variables are treated everywhere else in the analysis.
obsv <- df_lca |>
  select(site, class, all_of(defn_mm)) |>
  group_by(site, class) |>
  mutate(N = n()) |>
  pivot_longer(all_of(defn_mm), names_to = "condition") |>
  group_by(site, N, class, condition) |>
  summarise(n = sum(value == "Yes", na.rm = TRUE), .groups = "drop") |>
  mutate(n_N = n / N)

expt <- df_lca |>
  select(site, all_of(defn_mm)) |>
  group_by(site) |>
  mutate(N_total = n()) |>
  pivot_longer(all_of(defn_mm), names_to = "condition") |>
  group_by(site, N_total, condition) |>
  summarise(n_total = sum(value == "Yes", na.rm = TRUE), .groups = "drop") |>
  mutate(n_N_total = n_total / N_total)

lca_oe <- obsv |>
  left_join(expt, by = c("site", "condition")) |>
  mutate(o_e = round(n_N / n_N_total, 2))

## Inclusion rule --------
# A condition is shown only where more than ten people at that site have it. The
# rule is applied once, here, before the class filter, so it counts everyone at
# the site including class 0. Both the table and the figure below read this
# object, so the two cannot disagree about which conditions qualify.
lca_oe_included <- lca_oe |>
  group_by(site, condition) |>
  mutate(N_site = sum(N)) |>
  filter(sum(n, na.rm = TRUE) > 10) |>
  ungroup()

# Class composition (supplementary table 12) --------
postprob_wide <- lca_oe_included |>
  mutate(n_perc = paste0(n, " (", round(n_N * 100, 1), "%)")) |>
  filter(class != 0) |>
  mutate(class_site = paste0(site, "_Class", class,
                             " (N = ", N, "; ", round(N / N_site * 100, 1), "%)")) |>
  select(class_site, condition, nperc = n_perc, oe = o_e) |>
  pivot_wider(names_from = class_site,
              values_from = c("nperc", "oe"),
              names_glue = "{class_site}_{.value}") |>
  left_join(condition_lookup, by = "condition") |>
  select(-condition)

# Sorting the column names groups each site's n (%) and observed:expected
# columns by class rather than by statistic
table_lca_postprob <- postprob_wide |>
  select(Condition2,
         all_of(sort(setdiff(names(postprob_wide), "Condition2")))) |>
  select(Condition2,
         starts_with("Gambia rural"),
         starts_with("Gambia urban"),
         starts_with("Zimbabwe urban"),
         starts_with("South Africa rural"),
         starts_with("South Africa urban")) |>
  arrange(Condition2) |>
  flextable() |>
  separate_header(split = "[_]") |>
  tidy_table()

# Figure 3 --------
# Observed to expected ratios for the two South African sites and urban
# Zimbabwe. The Gambia sites are not shown: too few conditions met the
# inclusion rule for the two-class solution to be interpretable there.
plot_lca_oe_nogam <- lca_oe_included |>
  filter(class != 0) |>
  filter(n > 0) |>
  filter(!str_detect(as.character(site), "Gam")) |>
  left_join(condition_lookup, by = "condition") |>
  mutate(Class = factor(class, levels = c(1, 2),
                        labels = c("Class 1", "Class 2"))) |>
  ggplot(aes(y = Condition2, x = o_e, col = Class, group = Class, size = n)) +
  geom_vline(xintercept = 1, col = "grey40") +
  geom_point() +
  geom_line(orientation = "y", linewidth = lin1, alpha = 0.3) +
  facet_wrap(vars(site)) +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values = two_cols) +
  labs(x = "Observed:expected ratio", y = NULL) +
  plot_standardformat

save_fig(plot_lca_oe_nogam, file.path(plot_dir, "figure_3_lca_oe.png"),
         height = 5)
