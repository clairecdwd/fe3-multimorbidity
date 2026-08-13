# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# EQ-5D value index: multimorbidity and latent class together --------
# Requires: 07_hrqol/01_model_eq5d.R (tbl_eq5d_asew, temp_tbl_eq5d_univar),
#           08_lca/05_model_eq5d_class.R (md_eqclass_tobit_univar,
#           md_eqclass_tobit_asew, class0, temp_eqclass_tobit_univar),
#           00_settings/02_parameters.R (site_levels),
#           01_functions/format_estimates.R, 01_functions/tidy_table.R
# Creates:  df_eq5d_tobit_all, tidy_md_eqclass_all, table_eq5d_class_tobit,
#           table_eq5d_tobit_combo
# Creates:  table 2                - table_eq5d_tobit_combo
#           supplementary table 17 - table_eq5d_class_tobit
# Feeds:    R/09_compile/01_compile_main.R and 02_compile_supplement.R
#
# Table 2 stacks two sets of Tobit models: the multimorbidity models from
# 07_hrqol, and the latent class models from 05_model_eq5d_class.R.

# Multimorbidity, Tobit only --------
# A data frame, so named df_eq5d_tobit_all rather than table_eq5d_tobit_all: the
# table_ prefix is reserved for the flextables the compile scripts insert.
df_eq5d_tobit_all <- tbl_eq5d_asew |>
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
  filter(Model == "Tobit regression") |>
  select(-Model) |>
  arrange(Site, Outcome)

# Latent class, Tobit --------
eqclass_wide <- bind_rows(md_eqclass_tobit_univar, md_eqclass_tobit_asew) |>
  mutate(est_ci = format_est_ci(estimate, conf.low, conf.high),
         p_val = format_p(p.value)) |>
  select(site, term, model2,
         `Coefficient (95% CI)` = est_ci, `p value` = p_val) |>
  mutate(term = if_else(term == "class1",
                        "Cardiometabolic multimorbidity",
                        "HIV-related multimorbidity")) |>
  bind_rows(class0) |>
  arrange(term) |>
  filter(!str_detect(site, "Gam")) |>
  pivot_wider(names_from = model2,
              values_from = c(`Coefficient (95% CI)`, `p value`),
              names_glue = "{model2}_{.value}")

## Join guard --------
# A duplicated site and term key would duplicate rows of table 2 without saying
# so, which `relationship = "one-to-one"` catches.
#
# The obvious companion, `unmatched = "error"`, is not available: a full join
# cannot drop rows, so full_join() does not take that argument. The equivalent
# check is written out here instead, so an estimate without a denominator, or a
# denominator without an estimate, stops the run rather than appearing as a
# half-empty row.
eqclass_unmatched <- bind_rows(
  anti_join(distinct(eqclass_wide, site, term),
            distinct(temp_eqclass_tobit_univar, site, term),
            by = c("site", "term")),
  anti_join(distinct(temp_eqclass_tobit_univar, site, term),
            distinct(eqclass_wide, site, term),
            by = c("site", "term")))

if (nrow(eqclass_unmatched) > 0) {
  stop("Latent class estimates and denominators do not cover the same site and ",
       "class combinations. Unmatched:\n  ",
       paste(eqclass_unmatched$site, eqclass_unmatched$term,
             sep = " / ", collapse = "\n  "),
       "\nThe usual cause is a Tobit model that fit_by_site() discarded; check ",
       "the dropped table printed by 05_model_eq5d_class.R. Table 2 would ",
       "otherwise carry a row with a denominator and no estimate.",
       call. = FALSE)
}

tidy_md_eqclass_all <- eqclass_wide |>
  full_join(temp_eqclass_tobit_univar, by = c("site", "term"),
            relationship = "one-to-one") |>
  select(Site = site, Outcome = term, `n/N` = an_N,
         starts_with("Uni"), starts_with("Adj")) |>
  mutate(Site = factor(Site, levels = site_levels),
         Outcome = factor(Outcome, levels = c("No conditions",
                                              "Cardiometabolic multimorbidity",
                                              "HIV-related multimorbidity"))) |>
  arrange(Site, Outcome)

# Supplementary table 17 --------
# Association between latent class assignment and EQ-5D value index, using
# Tobit regression.
table_eq5d_class_tobit <- tidy_md_eqclass_all |>
  flextable() |>
  merge_v(j = 1) |>
  separate_header() |>
  tidy_table()

# Table 2 --------
table_eq5d_tobit_combo <- bind_rows(df_eq5d_tobit_all, tidy_md_eqclass_all) |>
  flextable() |>
  merge_v(j = 1) |>
  separate_header() |>
  tidy_table()
