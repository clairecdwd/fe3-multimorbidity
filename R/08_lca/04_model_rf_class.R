# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# Risk factors for latent class assignment --------
# Requires: 08_lca/01_read_lca_output.R (df_lca), 02_prepare/01_load_data.R
#           (sites), 00_settings/02_parameters.R (exposures, adjust_agesexsep,
#           bmi_ref), 01_functions/fit_by_site.R,
#           01_functions/format_estimates.R, 01_functions/model_flextables.R
# Creates:  multin_univ_output, multin_agesexsep_output,
#           table_rf_class_zim, table_rf_class_sar, table_rf_class_sau,
#           fn_rfclass
# Creates:  supplementary table 14 - table_rf_class_zim (urban Zimbabwe)
#           supplementary table 15 - table_rf_class_sar (rural South Africa)
#           supplementary table 16 - table_rf_class_sau (urban South Africa)
# Feeds:    R/09_compile/02_compile_supplement.R
#
# Multinomial logistic regression of latent class on each risk factor in turn,
# fitted separately at each site. Class 0, no conditions, is the reference.

## Model specification --------
tidy_multinom <- function(model) {
  tidy(model, conf.int = TRUE, exponentiate = TRUE)
}

# BMI is the one exposure whose reference category is not its first factor
# level, so it is set explicitly within each site.
set_bmi_reference <- function(data) {
  data |>
    mutate(ce_bmi_cat = relevel(ce_bmi_cat, ref = bmi_ref))
}

exposure_labels_class = c(
  con_sex              = "Sex",
  con_age_great40_10yr = "Age: per 10 years >40 years",
  que_edm              = "Education",
  que_wealthindex_3cat = "Wealth index tertile",
  que_alc_exc          = "Excess alcohol",
  que_esm              = "Ever smoker",
  ce_bmi_cat           = "BMI category",
  que_food_insec_sc    = "Food security",
  phy_low              = "Low physical activity")

# Exposures that also appear in the age, sex and socioeconomic position
# adjustment set. Their adjusted estimates would be conditioned on themselves,
# so they are not reported from the adjusted models.
exposures_in_adjustment = c("con_sex", "con_age_great40_10yr", "que_edm",
                            "que_wealthindex_3cat", "que_food_insec_sc")

## Denominators --------
# n is the number of people with a given exposure level who were assigned to a
# given class; N is the number with that exposure level at that site. Reference
# levels have no model term, so they arrive with NA estimates and are pushed to
# the bottom of each block by multin_flextable().
attach_class_denominators <- function(results, data, exposures) {
  denom <- data |>
    droplevels() |>
    select(site, class, all_of(exposures)) |>
    select(where(is.factor)) |>
    mutate(across(everything(), as.character)) |>
    pivot_longer(any_of(exposures), names_to = "variable", values_to = "level") |>
    filter(!is.na(level)) |>
    count(site, variable, level, y.level = class, name = "n") |>
    group_by(site, variable, level) |>
    mutate(N = sum(n)) |>
    ungroup()

  full_join(results, denom, by = c("site", "variable", "level", "y.level"))
}

## Fit --------
run_class_models <- function(adjust, keep_variables) {

  results <- fit_by_site(data = df_lca, sites = sites, outcome = "class",
                         exposures = exposures, adjust = adjust,
                         fit_fun = nnet::multinom, tidy_fun = tidy_multinom,
                         prepare_site = set_bmi_reference)

  cat("\nModels discarded:\n")
  print(attr(results, "dropped"))

  results |>
    left_join(split_term(unique(results$term), exposures), by = "term") |>
    mutate(level = na_if(level, ""),
           est_ci = format_est_ci(estimate, conf.low, conf.high),
           p_value_fmt = format_p(p.value)) |>
    select(site, outcome, y.level, term, variable, level,
           estimate, conf.low, conf.high, p.value, est_ci, p_value_fmt) |>
    attach_class_denominators(data = df_lca, exposures = exposures) |>
    filter(variable %in% keep_variables) |>
    mutate(variable_label = unname(exposure_labels_class[variable]))
}

multin_univ_output <- run_class_models(adjust = "",
                                       keep_variables = exposures) |>
  mutate(model = "Univariable")

multin_agesexsep_output <- run_class_models(
  adjust = adjust_agesexsep,
  keep_variables = setdiff(exposures, exposures_in_adjustment)) |>
  mutate(model = "Adjusted for age, sex and socioeconomic position")

# Supplementary tables 14 to 16 --------
# One table per site, from the adjusted models. The Gambia sites are not
# tabulated: the two-class solution was not interpretable there.
#
# multin_flextable() renders a single model, and it is the adjusted models that
# are shown. The univariable panel is one line away if it is wanted:
#   multin_univ_output |> filter(site == "Zimbabwe urban") |> multin_flextable()
table_rf_class_zim <- multin_agesexsep_output |>
  filter(site == "Zimbabwe urban") |>
  multin_flextable()

table_rf_class_sar <- multin_agesexsep_output |>
  filter(site == "South Africa rural") |>
  multin_flextable()

table_rf_class_sau <- multin_agesexsep_output |>
  filter(site == "South Africa urban") |>
  multin_flextable()

# Footnote --------
# The models are multinomial logistic and the estimates are odds ratios. One
# footnote serves all three tables.
fn_rfclass = paste(
  "Footnotes: (N) is the number of people with the specified level of the",
  "exposure at that site; n is the corresponding number of people assigned to",
  "the specified latent class. Odds ratios (OR) and 95% confidence intervals",
  "(95%CI) are from multinomial logistic regression models with latent class",
  "as the outcome and class 0, no conditions, as the reference class, adjusted",
  "for age, sex, education, wealth index tertile and food security. Wald p",
  "values are shown. Abbreviations: BMI = body mass index;",
  "ref = reference category of variable.")
