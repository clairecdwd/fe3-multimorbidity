# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# Age- and sex-standardised prevalence, national reference --------
# Requires: df, pop_weights_country, site_country, site_setting_weights,
#           age_stratum(), site_order, tidy_table()
# Creates:  tbl_std_morbidities
# Feeds:    supplementary table 5
#
# Direct standardisation to the national population aged 40 and over, in three
# sampling strata by sex.
#   site rows - each sub-site standardised to its own country's age-sex structure
#   Overall   - unweighted mean of the three country-level estimates, where each
#               country combines its rural and urban sub-sites by the national
#               rural/urban population split (Zimbabwe: urban site only, assumed
#               representative)
#
# The population reference, the site to country mapping, the rural/urban weights
# and age_stratum() all come from 02_prepare/03_population_reference.R.

## Parameters --------
# Standardised prevalence and its confidence interval are reported to one
# decimal place.
dp_prev = 1

## Sub-site standardised prevalence --------
# Each sub-site standardised to its country's population aged 40 and over. This
# is the building block for both the site rows and the Overall row.
std_prev_subsite_one <- function(data, var) {
  data |>
    filter(con_age >= 40, !is.na(.data[[var]])) |>
    mutate(stratum = age_stratum(con_age)) |>
    select(-any_of("country")) |>
    left_join(site_country |> select(site, country), by = "site") |>
    summarise(n_stratum = n(),
              n_yes     = sum(.data[[var]] == "Yes"),
              .by = c(site, country, con_sex, stratum)) |>
    mutate(p = n_yes / n_stratum,
           var_p = p * (1 - p) / n_stratum) |>
    left_join(pop_weights_country,
              by = c("country", "con_sex" = "sex", "stratum")) |>
    summarise(p_subsite   = sum(weight * p,       na.rm = TRUE),
              var_subsite = sum(weight^2 * var_p, na.rm = TRUE),
              .by = c(site, country)) |>
    mutate(variable = var)
}

## Site rows and Overall row --------
# Overall is built in two steps: combine sub-sites within each country by the
# rural/urban population split, then take the unweighted mean of the three
# country estimates, so that a country is not represented twice as heavily
# simply because two sites were sampled there.
build_results <- function(data, var) {
  sub <- std_prev_subsite_one(data, var)

  site_rows <- sub |>
    transmute(site,
              p_std  = p_subsite,
              se_std = sqrt(var_subsite),
              variable)

  country_step <- sub |>
    left_join(site_setting_weights, by = c("site", "country")) |>
    summarise(p_country   = sum(setting_weight * p_subsite,     na.rm = TRUE),
              var_country = sum(setting_weight^2 * var_subsite, na.rm = TRUE),
              .by = country)

  overall_row <- country_step |>
    summarise(n_countries = n(),
              p_std       = mean(p_country),
              se_std      = sqrt(sum(var_country) / n_countries^2)) |>
    transmute(site = "Overall", p_std, se_std, variable = var)

  bind_rows(site_rows, overall_row) |>
    mutate(lci = pmax(0, p_std - 1.96 * se_std),
           uci = pmin(1, p_std + 1.96 * se_std))
}

## Conditions shown --------
disease_vars <- tribble(
  ~var,           ~label,
  "multm",        "Multimorbidity",
  "multm_comp",   "Complex multimorbidity",
  "multm_exchiv", "Multimorbidity excluding HIV",
  "hiv_pos_comb", "HIV",
  "que_tb",       "Ever had tuberculosis",
  "ce_hbp_dx",    "Hypertension",
  "que_heart",    "Heart disease",
  "que_cho",      "Hypercholesterolaemia",
  "ce_diab_dx",   "Diabetes",
  "que_grd",      "GORD",
  "que_bow",      "Bowel disease",
  "que_anm",      "Anaemia",
  "que_stk",      "Stroke",
  "que_epi",      "Epilepsy",
  "que_dem",      "Dementia",
  "que_park",     "Parkinsons disease",
  "que_nrp",      "Neuropathy",
  "que_dep",      "Mood disorders",
  "que_szp",      "Severe mental illness",
  "que_rhe",      "Arthritis",
  "que_ost",      "Fragility fracture",
  "que_gout",     "Gout",
  "que_asth",     "Chronic lung disease",
  "que_kid",      "Kidney disease",
  "que_can",      "Cancer",
  "que_thy",      "Thyroid disease",
  "que_all",      "Allergy")

## Results --------
# Wald intervals are used throughout. They are known to be unreliable for the
# lowest-prevalence conditions; a gamma interval (PHEindicatormethods::phe_dsr)
# would be preferable there and is noted as a limitation in the paper.
cell_fmt = paste0("%.", dp_prev, "f (%.", dp_prev, "f–%.", dp_prev, "f)")

df_results_std <- map(disease_vars$var, ~ build_results(df, .x)) |>
  bind_rows() |>
  left_join(disease_vars, by = c("variable" = "var")) |>
  mutate(cell = sprintf(cell_fmt, 100 * p_std, 100 * lci, 100 * uci))

## Table --------
# Header N is the number of participants aged 40 and over contributing to each
# column, and the Overall column carries their total. site is coerced to
# character before the bind so that the Overall row cannot be dropped by a
# factor level mismatch.
n_by_site <- df |>
  filter(con_age >= 40) |>
  count(site) |>
  mutate(site = as.character(site))

header_n <- bind_rows(tibble(site = "Overall", n = sum(n_by_site$n)),
                      n_by_site) |>
  deframe()

df_wide_std <- df_results_std |>
  select(label, site, cell) |>
  mutate(site  = factor(site, levels = site_order),
         label = factor(label, levels = disease_vars$label)) |>
  pivot_wider(names_from = site, values_from = cell) |>
  arrange(label) |>
  select(label, all_of(site_order))

header_labels = setNames(
  c("Variable", paste0(site_order, "\n(N=", header_n[site_order], ")")),
  c("label", site_order))

tbl_std_morbidities <- df_wide_std |>
  flextable() |>
  set_header_labels(values = as.list(header_labels)) |>
  tidy_table(label_cols = 1)
