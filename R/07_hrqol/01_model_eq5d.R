# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# EQ-5D and multimorbidity --------
# Requires: 02_prepare/01_load_data.R (df, sites), 00_settings/02_parameters.R
#           (eq5d_upper, eq5d_floor, adjust_agesexsep, site_levels),
#           01_functions/fit_by_site.R, 01_functions/format_estimates.R,
#           01_functions/tidy_table.R
# Creates:  df_cceq5dindex, footnote_eq5d, tbl_eq5d_univar, tbl_eq5d_asew,
#           temp_tbl_eq5d_univar, table_eq5dindex, table_eq5d_all_all,
#           table_eq5dvas
# Creates:  supplementary table 8  - table_eq5dindex
#           supplementary table 9  - table_eq5d_all_all
#           supplementary table 10 - table_eq5dvas
# Feeds:    R/08_lca/06_tbl_eq5d_combo.R, which builds table 2 and
#           supplementary table 17 from tbl_eq5d_asew and temp_tbl_eq5d_univar
#
# Each model family is fitted through the site loop in 01_functions/fit_by_site.R,
# called with a different fitting function.

# Derived EQ-5D variables --------
# This block mutates the shared `df`. eq5dindex is floored at eq5d_floor here,
# so every script sourced after this one sees the floored variable, not the raw
# one. Nothing downstream needs the unfloored index, but the dependency is real
# and is the reason this script is sourced before 08_lca.
#
# `lessfull` marks anyone below the censoring point. The comparison is made a
# hair below eq5d_upper because an index of exactly 0.9 can be stored as
# 0.8999999999, which a bare `< eq5d_upper` would misclassify.
eq5d_tol = 1e-6

df <- df |>
  mutate(lessfull = eq5dindex < (eq5d_upper - eq5d_tol),
         full = if_else(lessfull, 0, 1)) |>
  mutate(eq5dindex = if_else(eq5dindex < eq5d_floor, eq5d_floor, eq5dindex))

# Complete cases on the value index
df_cceq5dindex <- df |>
  filter(complete.cases(full))

footnote_eq5d = paste(df |> filter(is.na(eq5dindex)) |> nrow(),
                      "people excluded from these analyses due to missing EQ5D data")

# Median comparisons --------
# Wilcoxon rank-sum (Mann-Whitney) test of the difference in medians between
# people with and without multimorbidity, within each site.

wilcox_by_site <- function(data, outcome) {
  split(data, data$site) |>
    imap(function(df_site, site_name) {
      tibble(site = site_name,
             p.value = wilcox.test(reformulate("multm", response = outcome),
                                   data = df_site)$p.value)
    }) |>
    bind_rows() |>
    # site comes back as a character from the split; restore the factor so that
    # the join below is not a factor to character join
    mutate(site = factor(site, levels = levels(data$site)))
}

fn_wilcox = "* p value for Mann Whitney test for difference in medians"

## EQ-5D value index (supplementary table 8) --------
eq5dindex_summary <- df_cceq5dindex |>
  group_by(site, multm) |>
  summarise(n = n(),
            median = median(eq5dindex, na.rm = TRUE),
            iqr.lower = quantile(eq5dindex, 0.25, na.rm = TRUE),
            iqr.upper = quantile(eq5dindex, 0.75, na.rm = TRUE),
            perclessfull = sum(lessfull, na.rm = TRUE) / n() * 100,
            .groups = "drop") |>
  mutate(median_iqr = format_est_ci(median, iqr.lower, iqr.upper),
         perclessfull = paste0(format(round(perclessfull, 1), nsmall = 1), "%")) |>
  select(site, multm, n, `Median (IQR)` = median_iqr, `% <0.9` = perclessfull) |>
  pivot_wider(names_from = multm,
              values_from = c(n, `Median (IQR)`, `% <0.9`))

table_eq5dindex <- eq5dindex_summary |>
  left_join(wilcox_by_site(df, "eq5dindex"), by = "site") |>
  mutate(`p value*` = format_p(p.value)) |>
  select(-p.value) |>
  mutate(`n/N` = paste0(n_Yes, "/", (n_Yes + n_No)), .after = "site") |>
  select(-n_Yes, -n_No) |>
  select(everything(), starts_with("% <0.9")) |>
  flextable() |>
  separate_header(split = "_") |>
  add_footer_lines(fn_wilcox) |>
  tidy_table()

## EQ-5D visual analogue scale (supplementary table 10) --------
eq5dvas_summary <- df_cceq5dindex |>
  group_by(site, multm) |>
  summarise(n = n(),
            median = median(eq5d5l_vas2_uk_eng, na.rm = TRUE),
            iqr.lower = quantile(eq5d5l_vas2_uk_eng, 0.25, na.rm = TRUE),
            iqr.upper = quantile(eq5d5l_vas2_uk_eng, 0.75, na.rm = TRUE),
            .groups = "drop") |>
  mutate(median_iqr = format_est_ci(median, iqr.lower, iqr.upper)) |>
  select(site, multm, n, `Median (IQR)` = median_iqr) |>
  pivot_wider(names_from = multm, values_from = c(n, `Median (IQR)`))

table_eq5dvas <- eq5dvas_summary |>
  left_join(wilcox_by_site(df, "eq5d5l_vas2_uk_eng"), by = "site") |>
  mutate(`p value*` = format_p(p.value)) |>
  select(-p.value) |>
  mutate(`n/N` = paste0(n_Yes, "/", (n_Yes + n_No)), .after = "site") |>
  select(-n_Yes, -n_No) |>
  flextable() |>
  separate_header(split = "_") |>
  add_footer_lines(fn_wilcox) |>
  tidy_table()

# Models --------
# multm and multm_exchiv are the exposures; the EQ-5D index (or, for the
# logistic family, being at full health) is the outcome. Three model families
# are fitted at each site, in two adjustment sets.

mm_exposures = c("multm", "multm_exchiv")

## Model families --------
# Tobit, censored at the top of the EQ-5D value scale
fit_tobit <- function(formula, data) {
  vglm(formula = formula, data = data, tobit(Upper = eq5d_upper))
}

# Coefficients are taken from the fitted object rather than from the model
# frame. vglm stores the name of its data argument rather than the data, so
# anything that reaches back for the model frame cannot find it once the fitting
# call has returned. Confidence intervals are Wald, as confintvglm defaults to.
tidy_tobit <- function(model) {
  cf <- coef(summary(model))
  ci <- confintvglm(model)

  tibble(term = rownames(cf),
         estimate = cf[, 1],
         std.error = cf[, 2],
         statistic = cf[, 3],
         p.value = cf[, 4],
         conf.low = ci[, 1],
         conf.high = ci[, 2]) |>
    # Tobit fits two linear predictors, so there are two intercepts
    filter(!str_starts(term, fixed("(Intercept)")))
}

# Generalised additive model, all terms parametric, as a sensitivity analysis
fit_gam <- function(formula, data) {
  gam(formula = formula, data = data)
}
tidy_gam <- function(model) {
  tidy(model, conf.int = TRUE, parametric = TRUE)
}

# Logistic, with the EQ-5D value index dichotomised at the top of the scale. The
# outcome is `lessfull`, an index below eq5d_upper, so the odds ratio is for
# reporting any difficulty rather than none
fit_logistic <- function(formula, data) {
  glm(formula = formula, data = data, family = binomial(link = "logit"))
}

tidy_logistic <- function(model) {
  tidy(model, conf.int = TRUE, exponentiate = TRUE)
}

## Term handling --------
# Keep only the exposure terms, matched by name, so the intercept and every
# adjustment term drop out together whatever the adjustment set contains.
tidy_eq5d_terms <- function(results) {
  results |>
    left_join(split_term(unique(results$term), mm_exposures), by = "term") |>
    filter(variable %in% mm_exposures) |>
    mutate(est_ci = format_est_ci(estimate, conf.low, conf.high),
           p_val = format_p(p.value)) |>
    select(site, model, variable, level, estimate, conf.low, conf.high,
           p.value, est_ci, p_val)
}

## Fit all three families for one adjustment set --------
run_eq5d_models <- function(adjust) {

  md_tobit <- fit_by_site(data = df_cceq5dindex, sites = sites,
                          outcome = "eq5dindex", exposures = mm_exposures,
                          adjust = adjust,
                          fit_fun = fit_tobit, tidy_fun = tidy_tobit)
  cat("\nModels discarded, Tobit:\n")
  print(attr(md_tobit, "dropped"))

  md_gam <- fit_by_site(data = df_cceq5dindex, sites = sites,
                        outcome = "eq5dindex", exposures = mm_exposures,
                        adjust = adjust,
                        fit_fun = fit_gam, tidy_fun = tidy_gam)
  cat("\nModels discarded, GAM:\n")
  print(attr(md_gam, "dropped"))

  md_logistic <- fit_by_site(data = df_cceq5dindex, sites = sites,
                             outcome = "lessfull", exposures = mm_exposures,
                             adjust = adjust,
                             fit_fun = fit_logistic, tidy_fun = tidy_logistic)
  cat("\nModels discarded, logistic:\n")
  print(attr(md_logistic, "dropped"))

  bind_rows(
    md_tobit    |> mutate(model = "Tobit regression"),
    md_gam      |> mutate(model = "GAM"),
    md_logistic |> mutate(model = "Logistic regression")) |>
    tidy_eq5d_terms()
}

## Univariable --------
tbl_eq5d_univar <- run_eq5d_models(adjust = "") |>
  attach_denominators(data = df_cceq5dindex, exposures = mm_exposures)

## Adjusted for age, sex and socioeconomic position --------
tbl_eq5d_asew <- run_eq5d_models(adjust = adjust_agesexsep) |>
  attach_denominators(data = df_cceq5dindex, exposures = mm_exposures)

temp_tbl_eq5d_univar <- tbl_eq5d_univar |>
  mutate(adj = "Univariable")

# Supplementary table 9 --------
# All three model families, both adjustment sets, side by side. The multimorbidity
# excluding HIV rows are not shown for the Gambia sites, where no participants
# had HIV and the two definitions are therefore identical.
df_eq5d_all_all <- tbl_eq5d_asew |>
  mutate(adj = "Adjusted for age, sex and SEP") |>
  bind_rows(temp_tbl_eq5d_univar) |>
  mutate(Site = factor(site, levels = site_levels),
         Outcome = case_when(
           variable == "multm" ~ "Multimorbidity",
           variable == "multm_exchiv" ~ "Multimorbidity excluding HIV")) |>
  filter(level == "Yes") |>
  select(Site, Outcome, `n/N` = an_N, Model = model,
         `Coefficient (95% CI)` = est_ci, `p value` = p_val, adj) |>
  pivot_wider(names_from = adj, values_from = c(`Coefficient (95% CI)`, `p value`),
              names_glue = "{adj}_{.value}") |>
  select(Site, Outcome, `n/N`, Model, starts_with("Univariable"), everything()) |>
  filter(!(str_detect(as.character(Site), "Gambia") &
             Outcome == "Multimorbidity excluding HIV")) |>
  arrange(Site, Outcome)

table_eq5d_all_all <- df_eq5d_all_all |>
  flextable() |>
  separate_header() |>
  merge_v(j = c(1, 2, 3), combine = TRUE) |>
  merge_v(j = 1) |>
  add_footer_lines(footnote_eq5d) |>
  tidy_table(label_cols = 1:4) |>
  bold(j = 1:2, bold = TRUE)
