# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# Export the latent class model input for Stata --------
# Requires: df, defn_mm, site_levels
# Creates:  stata/df_forlca.dta
# Feeds:    stata/LCA neat.do and stata/lca_entropy.do, whose output is read
#           back in by R/08_lca/01_read_lca_output.R
#
# The do-files subset with `keep if site == 3` and build the composite
# indicators with `replace que_dep_szp = 1 if que_szp == 1`, so site must be
# written as its numeric code and the conditions as 0/1 rather than as factors.
#
# This file holds participant-level data and is listed in .gitignore.

# Numeric site codes as used by the do-files
site_code = c(`Zimbabwe urban`     = 1,
              `Gambia urban`       = 2,
              `Gambia rural`       = 3,
              `South Africa urban` = 4,
              `South Africa rural` = 5)

yesno_vars = c(defn_mm, "multm", "hiv_pos_known", "ce_hbp_known", "ce_diab_known")

df_forlca <- df |>
  select(f03_hid, site, con_sex, con_age, all_of(defn_mm),
         multm, multim_n, hiv_pos_known, ce_hbp_known, ce_diab_known) |>
  mutate(site = unname(site_code[as.character(site)]),
         across(all_of(yesno_vars), ~ as.integer(.x == "Yes")))

stopifnot("every site must map to a numeric code" = !any(is.na(df_forlca$site)))

write_dta(df_forlca, path = paste0(projdir, "fe3-multimorbidity/stata/df_forlca.dta"))

cat("Wrote stata/df_forlca.dta,", nrow(df_forlca), "rows.",
    "Run the two do-files in stata/ before continuing.\n")
