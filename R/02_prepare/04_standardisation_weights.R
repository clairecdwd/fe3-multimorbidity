# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# Direct standardisation weights --------
# Requires: df, pop_weights_country, site_country, site_setting_weights,
#           age_stratum(), set_order, defn_mm_labels
# Creates:  df_std, temp_site, temp_country
# Feeds:    figure 1, supplementary figure 3, supplementary tables 18 and 19
#
# Each participant carries a weight equal to their stratum's share of the
# national population aged 40 and over, divided by the number of participants
# recruited into that stratum. Weights sum to one within the unit they
# standardise: site for w_site, country for w_country.
#
#   w_site    - each sub-site standardised to its own country's age-sex structure
#   w_country - additionally scales sub-sites by the national rural/urban split,
#               so sub-sites contribute by population share rather than by how
#               many people were recruited at each

df_std <- df |>
  filter(con_age >= 40, !is.na(con_sex)) |>
  mutate(stratum = age_stratum(con_age)) |>
  select(-any_of("country")) |>
  left_join(site_country |> select(site, country), by = "site") |>
  left_join(site_setting_weights |> select(site, setting_weight), by = "site")

## Site-level weights --------
site_weights <- df_std |>
  mutate(n_cell_site = n(), .by = c(site, con_sex, stratum)) |>
  left_join(pop_weights_country |> select(country, sex, stratum, weight),
            by = c("country", "con_sex" = "sex", "stratum")) |>
  mutate(w_site = weight / n_cell_site) |>
  select(-weight, -n_cell_site)

## Country-level weights --------
country_weights <- df_std |>
  mutate(n_cell_subsite = n(), .by = c(site, con_sex, stratum)) |>
  left_join(pop_weights_country |> select(country, sex, stratum, weight),
            by = c("country", "con_sex" = "sex", "stratum")) |>
  mutate(w_country = (weight * setting_weight) / n_cell_subsite) |>
  select(-weight, -n_cell_subsite)

## Membership matrices --------
# Conditions as logical columns under their display labels, which is the form
# both the intersection plots and the combination tables consume.
build_membership <- function(weighted_df, weight_col) {
  weighted_df |>
    select(f03_hid, all_of(defn_mm), con_sex, con_age, site, country,
           all_of(weight_col)) |>
    mutate(across(all_of(defn_mm), ~ .x %in% "Yes")) |>
    rename(all_of(defn_mm_labels)) |>
    select(f03_hid, all_of(set_order), con_sex, con_age, site, country,
           all_of(weight_col))
}

temp_site    <- build_membership(site_weights,    "w_site")
temp_country <- build_membership(country_weights, "w_country")

## Checks --------
# Weights must sum to 1 within the unit they standardise. A shortfall means the
# population join failed for some stratum, which would silently bias every
# standardised estimate downwards.
check_w_site <- temp_site |>
  summarise(total_w = sum(w_site, na.rm = TRUE), n_na = sum(is.na(w_site)), .by = site)

check_w_country <- temp_country |>
  summarise(total_w = sum(w_country, na.rm = TRUE), n_na = sum(is.na(w_country)),
            .by = country)

print(check_w_site)
print(check_w_country)

if (any(abs(check_w_site$total_w - 1) > 0.01) || any(check_w_site$n_na > 0)) {
  warning("Site standardisation weights do not sum to 1. ",
          "Check that every age-sex stratum matched the population reference.",
          call. = FALSE)
}

if (any(abs(check_w_country$total_w - 1) > 0.01) || any(check_w_country$n_na > 0)) {
  warning("Country standardisation weights do not sum to 1. ",
          "Check that every age-sex stratum matched the population reference.",
          call. = FALSE)
}
