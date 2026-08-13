 # Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# National population reference --------
# Requires: pop_path (set in run_analysis.R), 00_settings/02_parameters.R
# Creates:  pop, pop_ref, pop_weights_country, site_country,
#           site_setting_weights, age_stratum()
# Feeds:    figure 2, supplementary tables 5, 6, 18 and 19,
#           supplementary figures 3, 4 and 5
#
# Read once here and reused by every script that needs it

read_pop_sheet <- function(sheet, country) {
  read_excel(pop_path, sheet = sheet) |>
    select(-Total) |>
    rename(age_band = `Age group`) |>
    pivot_longer(c(Male, Female), names_to = "sex", values_to = "total_n") |>
    mutate(country = country)
}

pop <- bind_rows(
  read_pop_sheet("Zimbabwe", "Zimbabwe"),
  read_pop_sheet("Gambia", "Gambia"),
  read_pop_sheet("South Africa", "South Africa")) |>
  # The Gambia sheet reports 85–89 and 90+ separately as well as an 85+ total.
  # Dropping the sub-bands avoids double counting against the study's top band.
  filter(!(country == "Gambia" & age_band %in% c("85–89", "90+")))

## Sampling stratum --------
age_stratum <- function(age) {
  case_when(
    age >= 40 & age <= 54 ~ "40–54",
    age >= 55 & age <= 69 ~ "55–69",
    age >= 70             ~ "≥70",
    .default = NA_character_)
}

## Population collapsed to the three sampling strata, aged 40 and over --------
pop_ref <- pop |>
  mutate(age_low = as.integer(str_extract(age_band, "^\\d+")),
         stratum = case_when(
           age_low >= 40 & age_low <= 50 ~ "40–54",
           age_low >= 55 & age_low <= 65 ~ "55–69",
           age_low >= 70                 ~ "≥70",
           .default = NA_character_)) |>
  filter(!is.na(stratum)) |>
  summarise(pop_n = sum(total_n), .by = c(country, sex, stratum))

pop_weights_country <- pop_ref |>
  mutate(weight = pop_n / sum(pop_n), .by = country)

## Site to country and rural/urban mapping --------
site_country <- tribble(
  ~site,                  ~country,        ~setting,
  "Gambia rural",         "Gambia",        "rural",
  "Gambia urban",         "Gambia",        "urban",
  "Zimbabwe urban",       "Zimbabwe",      "urban",
  "South Africa rural",   "South Africa",  "rural",
  "South Africa urban",   "South Africa",  "urban")

ruralperc <- tribble(
  ~country,        ~rural_prop,
  "Gambia",        ruralperc_gam,
  "South Africa",  ruralperc_sa,
  "Zimbabwe",      ruralperc_zim)

# Weight of each sub-site within its country. Zimbabwe has an urban site only,
# with ruralperc_zim = 0, so its urban weight is 1.
site_setting_weights <- site_country |>
  left_join(ruralperc, by = "country") |>
  mutate(setting_weight = if_else(setting == "rural", rural_prop, 1 - rural_prop)) |>
  select(site, country, setting_weight)

## Checks --------
stopifnot(
  "site_setting_weights must sum to 1 within each country" =
    all(abs(summarise(site_setting_weights, s = sum(setting_weight), .by = country)$s - 1) < 1e-8),
  "pop_weights_country must sum to 1 within each country" =
    all(abs(summarise(pop_weights_country, s = sum(weight), .by = country)$s - 1) < 1e-8)
)

cat("Population reference loaded for", n_distinct(pop_ref$country), "countries\n")
