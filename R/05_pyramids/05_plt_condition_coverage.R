# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# Prevalence across the age range --------
# Requires: df, cond_colors, alp1, lin1, siz1
# Creates:  plot_conditionsoverage_country, plot_conditionsoverage_sites
# Feeds:    figure 2 panel C, supplementary figure 6
#
# Prevalence of the three individually measured conditions and of
# multimorbidity, by age band: pooled by country for the main figure, and split
# by sex and site for the supplement.

## Parameters --------
# Age bands with 10 or fewer participants are not plotted: a loess fitted
# through a handful of people is not interpretable and the points swing the
# smooth. The rule is the same in both panels, applied to whichever stratum the
# panel is built on - age band by country for the main figure, age band by sex
# and site for the supplement.
min_stratum_n = 10

## Prevalence --------
# Complete-case prevalence: the denominator is the number of participants with a
# non-missing answer, not the number in the stratum, so a single missing value
# does not void the cell and drop the point from the panel.
prevalence_pct <- function(x) {
  100 * sum(x == "Yes", na.rm = TRUE) / sum(!is.na(x))
}

# One summary function for both panels, so the two cannot drift apart
summarise_prevalence <- function(group_vars) {
  df |>
    summarise(
      n              = n(),
      Diabetes       = prevalence_pct(ce_diab_dx),
      HIV            = prevalence_pct(hiv_pos_comb),
      Hypertension   = prevalence_pct(ce_hbp_dx),
      Multimorbidity = prevalence_pct(multm),
      .by = all_of(group_vars)) |>
    filter(n > min_stratum_n) |>
    pivot_longer(c(Diabetes, HIV, Hypertension, Multimorbidity))
}

## By country --------
# Multimorbidity is the summary measure and is drawn dotted to separate it from
# the three individual conditions. The linetype is set literally and read with
# scale_linetype_identity(): on the default discrete scale ggplot assigns line
# types by level order, so "dotted" sorts first and lands on the wrong series.
df_coverage_country <- summarise_prevalence(c("con_age_band", "site2")) |>
  mutate(linetype = if_else(name == "Multimorbidity", "dotted", "solid"))

plot_conditionsoverage_country <- df_coverage_country |>
  ggplot(aes(x = con_age_band, y = value, col = name, fill = name, group = name,
             linetype = linetype)) +
  geom_smooth(method = "loess", span = 2, alpha = alp1, linewidth = lin1) +
  geom_point(size = siz1) +
  facet_wrap(vars(site2)) +
  scale_colour_manual(values = cond_colors) +
  scale_fill_manual(values = cond_colors) +
  scale_linetype_identity() +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Age group", y = "Prevalence of condition", color = NULL) +
  guides(fill = "none", linetype = "none")

## By sex and site --------
# Sex is shown by line type and point shape rather than by colour, which is
# already carrying condition
df_coverage_sites <- summarise_prevalence(c("con_age_band", "con_sex", "site"))

plot_conditionsoverage_sites <- df_coverage_sites |>
  ggplot(aes(x = con_age_band, y = value, col = name, fill = name, group = name)) +
  geom_smooth(data = df_coverage_sites |> filter(con_sex == "Male"),
              linetype = "solid",
              method = "loess", span = 2, alpha = alp1, linewidth = lin1) +
  geom_point(data = df_coverage_sites |> filter(con_sex == "Male"),
             size = siz1, shape = 16) +
  geom_smooth(data = df_coverage_sites |> filter(con_sex == "Female"),
              linetype = "dashed",
              method = "loess", span = 2, alpha = alp1, linewidth = lin1) +
  geom_point(data = df_coverage_sites |> filter(con_sex == "Female"),
             size = siz1, shape = 0) +
  facet_wrap(vars(site)) +
  scale_colour_manual(values = cond_colors) +
  scale_fill_manual(values = cond_colors) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Age group", y = "Prevalence of condition", color = NULL) +
  guides(fill = "none")
