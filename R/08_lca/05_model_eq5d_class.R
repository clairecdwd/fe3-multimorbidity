# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# EQ-5D value index by latent class --------
# Requires: 08_lca/01_read_lca_output.R (df_lca), 07_hrqol/01_model_eq5d.R
#           (the derived variable `full`, and fit_tobit() / tidy_tobit()),
#           02_prepare/01_load_data.R (sites), 00_settings/02_parameters.R
#           (adjust_agesexsep), 01_functions/fit_by_site.R
# Creates:  df_cceq5dindex_lca, md_eqclass_tobit_univar, md_eqclass_tobit_asew,
#           class0, temp_eqclass_tobit_univar
# Feeds:    R/08_lca/06_tbl_eq5d_combo.R, which builds table 2 and
#           supplementary table 17
#
# Tobit regression of the EQ-5D value index on latent class, censored at the top
# of the scale, univariable and adjusted for age, sex and socioeconomic
# position. Class 0, no conditions, is the reference.

# Complete cases --------
# Named apart from df_cceq5dindex in 07_hrqol/01_model_eq5d.R: that frame is
# built from df, this one from df_lca.
df_cceq5dindex_lca <- df_lca |>
  filter(complete.cases(full))

# Models --------
md_eqclass_tobit_univar <- fit_by_site(data = df_cceq5dindex_lca, sites = sites,
                                       outcome = "eq5dindex",
                                       exposures = "class", adjust = "",
                                       fit_fun = fit_tobit,
                                       tidy_fun = tidy_tobit)

cat("\nModels discarded, Tobit by class, univariable:\n")
print(attr(md_eqclass_tobit_univar, "dropped"))

md_eqclass_tobit_univar <- md_eqclass_tobit_univar |>
  filter(str_starts(term, "class")) |>
  mutate(model = "Tobit regression", model2 = "Univariable")

md_eqclass_tobit_asew <- fit_by_site(data = df_cceq5dindex_lca, sites = sites,
                                     outcome = "eq5dindex",
                                     exposures = "class",
                                     adjust = adjust_agesexsep,
                                     fit_fun = fit_tobit,
                                     tidy_fun = tidy_tobit)

cat("\nModels discarded, Tobit by class, adjusted:\n")
print(attr(md_eqclass_tobit_asew, "dropped"))

# Keeping only the terms that begin with the exposure name drops the intercept
# and every adjustment term at once, whatever the adjustment set contains.
md_eqclass_tobit_asew <- md_eqclass_tobit_asew |>
  filter(str_starts(term, "class")) |>
  mutate(model = "Tobit regression", model2 = "Adjusted for age, sex and SEP")

# Reference class --------
# Class 0 is the reference and so has no estimate. It is carried as an explicit
# row so that it appears in the table with a blank cell rather than being absent.
class0 <- tibble(
  site = sites,
  term = "No conditions",
  `Coefficient (95% CI)` = NA_character_,
  `p value` = NA_character_,
  model2 = "Univariable")

# Denominators --------
# n / N for each class, by site. The Gambia sites are dropped: the two-class
# solution was not interpretable there.
temp_eqclass_tobit_univar <- df_cceq5dindex_lca |>
  count(site, class, name = "n") |>
  filter(!is.na(class)) |>
  group_by(site) |>
  mutate(N = sum(n),
         an_N = paste0(n, "/", N)) |>
  ungroup() |>
  mutate(site = as.character(site),
         term = case_when(
           class == "0" ~ "No conditions",
           class == "1" ~ "Cardiometabolic multimorbidity",
           class == "2" ~ "HIV-related multimorbidity")) |>
  select(site, term, n, N, an_N) |>
  filter(!str_detect(site, "Gambia"))
