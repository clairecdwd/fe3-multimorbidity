# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# Age- and sex-standardised prevalence, WHO World Standard --------
# Requires: df, site_country, age_stratum(), site_order, tidy_table()
# Creates:  tbl_std_morbidities_who
# Feeds:    supplementary table 6
#
# Direct standardisation to the WHO World Standard Population, applied
# separately to males and females and then combined 50:50.
#   site rows - each sub-site standardised to the WHO standard
#   Overall   - unweighted mean of the three country estimates, each of which is
#               the unweighted mean of its sub-sites. Sub-sites are independent
#               estimates against the same external reference, so no national
#               rural/urban weighting is applied here; that is the difference
#               from supplementary table 5.
#
# The site to country mapping and age_stratum() come from
# 02_prepare/03_population_reference.R. The WHO standard itself is kept in this
# file because nothing else uses it.

## Parameters --------
dp_prev = 1

## WHO World Standard Population --------
# Five-year age weights per 100,000. Source: Ahmad OB et al. (2001). Age
# Standardization of Rates: A New WHO Standard. GPE Discussion Paper No. 31.
# WHO, Geneva.
who_world_standard <- tribble(
  ~age_band,  ~who_weight,
  "0–4",       8860,
  "5–9",       8690,
  "10–14",     8600,
  "15–19",     8470,
  "20–24",     8220,
  "25–29",     7930,
  "30–34",     7610,
  "35–39",     7150,
  "40–44",     6590,
  "45–49",     6040,
  "50–54",     5370,
  "55–59",     4550,
  "60–64",     3720,
  "65–69",     2960,
  "70–74",     2210,
  "75–79",     1520,
  "80–84",      910,
  "85–89",      440,
  "90–94",      150,
  "95–99",       40,
  "100+",        10) |>
  mutate(who_weight = who_weight / 100000)

# Collapsed to the three sampling strata and rescaled to sum to one within the
# population aged 40 and over, then split evenly between the sexes.
who_weights <- who_world_standard |>
  mutate(age_low = as.integer(str_extract(age_band, "^\\d+")),
         stratum = case_when(
           age_low >= 40 & age_low <= 50 ~ "40–54",
           age_low >= 55 & age_low <= 65 ~ "55–69",
           age_low >= 70                 ~ "≥70",
           .default = NA_character_)) |>
  filter(!is.na(stratum)) |>
  summarise(age_weight = sum(who_weight), .by = stratum) |>
  mutate(age_weight = age_weight / sum(age_weight)) |>
  crossing(sex = c("Male", "Female")) |>
  mutate(weight = age_weight * 0.5) |>
  select(sex, stratum, weight)

stopifnot("WHO standardisation weights must sum to 1" =
            abs(sum(who_weights$weight) - 1) < 1e-6)

## Sub-site standardised prevalence --------
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
    left_join(who_weights, by = c("con_sex" = "sex", "stratum")) |>
    summarise(p_subsite   = sum(weight * p,       na.rm = TRUE),
              var_subsite = sum(weight^2 * var_p, na.rm = TRUE),
              .by = c(site, country)) |>
    mutate(variable = var)
}

## Site rows and Overall row --------
build_results <- function(data, var) {
  sub <- std_prev_subsite_one(data, var)

  site_rows <- sub |>
    transmute(site,
              p_std  = p_subsite,
              se_std = sqrt(var_subsite),
              variable)

  country_step <- sub |>
    summarise(n_sites     = n(),
              p_country   = mean(p_subsite),
              var_country = sum(var_subsite) / n_sites^2,
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
# Multimorbidity excluding HIV is not in this table; it is in the
# nationally standardised table only.
disease_vars <- tribble(
  ~var,           ~label,
  "multm",        "Multimorbidity",
  "multm_comp",   "Complex multimorbidity",
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
cell_fmt = paste0("%.", dp_prev, "f (%.", dp_prev, "f–%.", dp_prev, "f)")

df_results_who <- map(disease_vars$var, ~ build_results(df, .x)) |>
  bind_rows() |>
  left_join(disease_vars, by = c("variable" = "var")) |>
  mutate(cell = sprintf(cell_fmt, 100 * p_std, 100 * lci, 100 * uci))

## Table --------
n_by_site <- df |>
  filter(con_age >= 40) |>
  count(site) |>
  mutate(site = as.character(site))

header_n <- bind_rows(tibble(site = "Overall", n = sum(n_by_site$n)),
                      n_by_site) |>
  deframe()

df_wide_who <- df_results_who |>
  select(label, site, cell) |>
  mutate(site  = factor(site, levels = site_order),
         label = factor(label, levels = disease_vars$label)) |>
  pivot_wider(names_from = site, values_from = cell) |>
  arrange(label) |>
  select(label, all_of(site_order))

header_labels = setNames(
  c("Variable", paste0(site_order, "\n(N=", header_n[site_order], ")")),
  c("label", site_order))

tbl_std_morbidities_who <- df_wide_who |>
  flextable() |>
  set_header_labels(values = as.list(header_labels)) |>
  tidy_table(label_cols = 1)
