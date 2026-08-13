# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# Risk factors for multimorbidity --------
# Requires: df, sites, exposures, outcomes, adjust_agesex, adjust_agesexsep,
#           bmi_ref, fit_by_site(), attach_denominators(), split_term()
# Creates:  df_model, rf_univ_output, rf_agesex_output, rf_agesexsep_output,
#           rf_term_labels, n_for_rfmodels
# Feeds:    Supplementary figures 7 and 8, Supplementary tables 20 and 21
#
# Site-stratified logistic regression, one exposure per model. Estimates are
# odds ratios: glm(family = binomial) uses the default logit link.
#
# The analysis sample is complete cases across every exposure except physical
# activity, so the univariable estimates are conditioned on covariate
# completeness rather than being fitted on the full sample. The N is recorded
# below and reported in the figure and table footnotes.

# Analysis sample --------
df_model <- df |>
  filter(complete.cases(que_edm),
         complete.cases(que_alc_exc),
         complete.cases(que_esm),
         complete.cases(ce_bmi_cat),
         complete.cases(que_food_insec_sc))

n_for_rfmodels     = nrow(df_model)
n_for_rfmodelsphys = df_model |> filter(complete.cases(phy_low)) |> nrow()

cat("Risk factor models: N =", n_for_rfmodels,
    "of", nrow(df), "participants;",
    n_for_rfmodelsphys, "with physical activity recorded\n")

# Model fitting --------
fit_logistic <- function(formula, data) {
  glm(formula = formula, data = data, family = binomial)
}

tidy_logistic <- function(model) {
  tidy(model, conf.int = TRUE, exponentiate = TRUE) |>
    filter(term != "(Intercept)")
}

# BMI is releveled inside each site so that the reference is the healthy to
# overweight category rather than the underweight category the factor is
# delivered with
prepare_site_rf <- function(data) {
  data |> mutate(ce_bmi_cat = relevel(ce_bmi_cat, ref = bmi_ref))
}

# An exposure that is also in the adjustment set cannot be its own exposure:
# con_age_great40_10yr is con_age_great40 / 10, so including both makes the
# model singular. They are excluded from the exposure list up front rather than
# filtered out of the results afterwards.
exposures_for <- function(adjust) {
  in_adjust = exposures[map_lgl(exposures, ~ str_detect(adjust, fixed(.x)))]
  age_terms = if (str_detect(adjust, fixed("con_age_great40"))) "con_age_great40_10yr" else character()
  setdiff(exposures, union(in_adjust, age_terms))
}

run_rf <- function(adjust, model_label) {
  exp_set = exposures_for(adjust)

  out <- fit_by_site(data = df_model, sites = sites,
                     outcome = outcomes, exposures = exp_set,
                     adjust = adjust,
                     fit_fun = fit_logistic, tidy_fun = tidy_logistic,
                     prepare_site = prepare_site_rf)

  cat("\nModels discarded,", model_label, ":\n")
  print(attr(out, "dropped"))

  out |>
    left_join(split_term(unique(out$term), exposures), by = "term") |>
    filter(variable %in% exp_set) |>
    mutate(est_ci = format_est_ci(estimate, conf.low, conf.high),
           p_value_fmt = format_p(p.value),
           model = model_label) |>
    attach_denominators(data = df_model, exposures = exp_set)
}

rf_univ_output      <- run_rf("",                  "Univariable")
rf_agesex_output    <- run_rf(adjust_agesex,       "Adjusted for age and sex")
rf_agesexsep_output <- run_rf(adjust_agesexsep,
                              "Adjusted for age, sex and socioeconomic position")

# Term labels --------
# One lookup, used by both the tables and the figures, so a label cannot be
# edited in one and not the other. Each label states the contrast in the
# direction the treatment contrast actually estimates: que_food_insec_sc has
# levels c("Food secure", "Food insecure"), so the estimate is for insecure
# versus secure.
rf_term_labels = tribble(
  ~term,                                  ~term_label,
  "que_esmYes",                           "Smoking: Ever smoker (vs non-smoker)",
  "que_alc_excYes",                       "Alcohol: Excess intake (vs not)",
  "que_food_insec_scFood insecure",       "Food security: Insecure (vs secure)",
  "phy_lowLow",                           "Physical activity: Low (vs not low)",
  "ce_bmi_catObese",                      "BMI: Obese (vs healthy-overweight)",
  "ce_bmi_catUnderweight",                "BMI: Underweight (vs healthy-overweight)",
  "que_wealthindex_3catHigh",             "Wealth index: High (vs low)",
  "que_wealthindex_3catMiddle",           "Wealth index: Middle (vs low)",
  "que_edmNo/primary",                    "Education: None or primary (vs at least secondary)",
  "con_sexFemale",                        "Sex: Female (vs male)",
  "con_age_great40_10yr",                 "Age: Per 10 years over 40 years")

# Display order, bottom to top on the figures
rf_term_order = rev(rf_term_labels$term_label)

add_rf_labels <- function(x) {
  x |>
    left_join(rf_term_labels, by = "term") |>
    mutate(term_label = factor(term_label, levels = rf_term_order),
           variable_label = str_remove(term_label, ":.*$"),
           site = factor(site, levels = site_levels),
           outcome_label = case_when(
             outcome == "multm"        ~ "Including HIV",
             outcome == "multm_exchiv" ~ "Excluding HIV",
             .default = NA_character_),
           outcome_label = factor(outcome_label,
                                  levels = c("Including HIV", "Excluding HIV")))
}

fn_rf_base = paste0(
  "Odds ratios and 95% confidence intervals from site-stratified logistic ",
  "regression models, each including one exposure and multimorbidity status ",
  "(two or more conditions) as the outcome. Age was included as a continuous term. ",
  "Analysis restricted to participants with complete data on all exposures ",
  "(N=", n_for_rfmodels, "); physical activity was additionally missing for ",
  n_for_rfmodels - n_for_rfmodelsphys, " participants.")
