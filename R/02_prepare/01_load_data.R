# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# Load and validate the analysis dataset --------
# Requires: data_path (set in run_analysis.R), 00_settings/02_parameters.R
# Creates:  df, sites
# Feeds:    everything
#
# The public repository starts from a cleaned, de-identified analysis dataset.
# Data cleaning is not part of this repository: the derivation of the chronic
# condition variables involves participant free-text responses and is not
# publishable. assets/data_dictionary.md documents exactly what this dataset
# must contain and how each variable was derived, so that the analysis can be
# reproduced against an equivalently prepared dataset.

df <- read_dta(data_path)

# Restore factors from the Stata value labels
df <- df |>
  dplyr::mutate_if(haven::is.labelled, haven::as_factor)

# Validation --------
# Fail loudly and early rather than part way through a model loop. Every
# expectation below is documented in assets/data_dictionary.md.

required_vars = c(
  "f03_hid", "con_hid", "site", "site2", "con_sex", "con_age",
  "con_age_band", "con_age_great40", "con_age_great40_10yr",
  defn_mm,
  "multm", "multm_exchiv", "multm_comp", "multim_n", "multim_n_exchiv",
  "hiv_pos_known", "ce_hbp_known", "ce_diab_known",
  "que_edm", "que_wealthindex_3cat", "que_food_insec_sc",
  "que_alc_der", "que_alc_exc", "que_esm", "que_cur", "que_cig",
  "ce_bmi", "ce_bmi_cat", "phy_low", "que_alc_wk",
  "eq5dindex", "eq5d5l_vas2_uk_eng")

missing_vars = setdiff(required_vars, names(df))

if (length(missing_vars) > 0) {
  stop("Analysis dataset is missing required variables:\n  ",
       paste(missing_vars, collapse = ", "),
       "\nSee assets/data_dictionary.md.", call. = FALSE)
}

# Factor level order determines the reference category under R's default
# treatment contrasts, so it is checked rather than assumed.
expected_levels = list(
  site              = site_levels,
  site2             = country_levels,
  multm             = c("No", "Yes"),
  multm_exchiv      = c("No", "Yes"),
  multm_comp        = c("No", "Yes"),
  que_food_insec_sc = c("Food secure", "Food insecure"),
  ce_bmi_cat        = c("Underweight", "Healthy-overweight", "Obese"),
  phy_low           = c("Not low", "Low"),
  que_edm           = c("At least secondary", "No/primary"))

for (v in names(expected_levels)) {
  got = levels(df[[v]])
  want = expected_levels[[v]]
  if (!identical(got, want)) {
    stop("Unexpected factor levels for ", v, ".\n",
         "  expected: ", paste(want, collapse = " | "), "\n",
         "  found:    ", paste(got, collapse = " | "),
         "\nThe first level is the model reference category. ",
         "See assets/data_dictionary.md.", call. = FALSE)
  }
}

# All 24 condition variables must be No/Yes factors in that order
cond_bad = defn_mm[!map_lgl(defn_mm, ~ identical(levels(df[[.x]]), c("No", "Yes")))]

if (length(cond_bad) > 0) {
  stop("Condition variables must be factors with levels No, Yes in that order. ",
       "Problem variables: ", paste(cond_bad, collapse = ", "), call. = FALSE)
}

# Vector of sites to loop over. Named `sites`, not `site`, so that it cannot
# shadow the column of the same name.
sites = levels(df$site)

cat("Analysis dataset loaded:", nrow(df), "participants,",
    length(sites), "sites\n")

df |> tabyl(site)
