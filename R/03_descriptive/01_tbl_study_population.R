# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# Study population and morbidities, by site --------
# Requires: df, 00_settings/02_parameters.R, 00_settings/05_tables.R,
#           create_table_1()
# Creates:  tbl_desc_morbidities_site, fn_desctable, and the individual
#           footnote strings fn_alc, fn_smok, fn_cig, fn_bmi, fn_kcal,
#           fn_food, fn_rhe, fn_abb
# Feeds:    table 1

## Parameters --------
# Number of participants reporting rheumatoid arthritis specifically, as
# opposed to arthritis of any kind, counted at the data preparation stage.
n_rheumatoid = NA_integer_

## Derived variables --------
# Smoking status and lifetime abstinence are analysis-stage derivations from
# que_esm/que_cur and que_alc_der respectively, so they are built here rather
# than in the cleaning stage. They are built on a local copy so that the
# contents of df do not depend on source order.
#
# Variable labels are stripped so that tbl_summary() uses the display names
# given in select() below rather than the labels carried on the dataset.

df_desc <- df |>
  remove_var_label() |>
  mutate(
    que_esmcur = case_when(
      que_esm == "No"                     ~ "Never smoker",
      que_esm == "Yes" & que_cur == "No"  ~ "Previous smoker",
      que_esm == "Yes" & que_cur == "Yes" ~ "Current smoker",
      .default = NA_character_),
    que_esmcur = factor(que_esmcur, levels = c("Never smoker", "Previous smoker",
                                               "Current smoker")),
    que_alc_never = case_when(
      que_alc_der == "Yes" ~ "No",
      que_alc_der == "No"  ~ "Yes",
      .default = NA_character_),
    que_alc_never = factor(que_alc_never, levels = c("No", "Yes")))

## Footnotes --------
# Missingness is reported for the variable the table actually tabulates, not for
# a derivative of it.

fn_alc <- paste0("Alcohol use missing for ", sum(is.na(df_desc$que_alc_der)),
                 " participants. Excess alcohol use defined as XX. ",
                 "Alcohol use was more common in younger participants, across sites.")

# The table tabulates `Ever smoker` = que_esm, so missingness is counted in
# que_esm rather than in the derived que_esmcur
fn_smok <- paste0("Smoking status missing for ", sum(is.na(df_desc$que_esm)),
                  " participants.")

# que_esmcur is the denominator here, because the row is restricted to current
# smokers
fn_cig <- paste0("Cigarettes per day among current smokers only; missing for ",
                 df_desc |>
                   filter(que_esmcur == "Current smoker", is.na(que_cig)) |>
                   nrow(),
                 " current smokers.")

fn_bmi <- paste0("BMI missing for ", sum(is.na(df_desc$ce_bmi)), " participants.")

# The table tabulates `Low physical activity` = phy_low, so missingness is
# counted in phy_low rather than in que_kcal_low
fn_kcal <- paste0("Physical activity missing for ", sum(is.na(df_desc$phy_low)),
                  " participants.")

fn_food <- paste0("Food security missing for ",
                  sum(is.na(df_desc$que_food_insec_sc)), " participants.")

# Built from the n_rheumatoid parameter above, with the noun agreeing with the
# count
fn_rhe <- paste0("Arthritis was defined as any form of arthritis, including ",
                 "joint pains; only ", n_rheumatoid,
                 if (identical(n_rheumatoid, 1L)) " person " else " people ",
                 "reported rheumatoid arthritis.")

fn_abb = "Abbreviations: BMI, body mass index; GORD, gastro-oesophageal reflux disease."

fn_desctable <- paste(fn_alc, fn_smok, fn_cig, fn_bmi, fn_kcal, fn_food,
                      fn_rhe, fn_abb)

## Table of participant characteristics and morbidities, by site --------

tbl_desc_morbidities_site <- df_desc |>
  select(site,
         Sex = con_sex,
         `Age (years)` = con_age,
         `Education` = que_edm,
         `Wealth index tertile` = que_wealthindex_3cat,
         `Food security` = que_food_insec_sc,
         `Non-drinker` = que_alc_never,
         `Alcohol units/week among drinkers` = que_alc_wk,
         `Excess alcohol intake among all` = que_alc_exc,
         `Ever smoker` = que_esm,
         `Cigarettes/day among smokers` = que_cig,
         `BMI (kg/m2)` = ce_bmi,
         `BMI category` = ce_bmi_cat,
         `Low physical activity` = phy_low,
         Multimorbidity = multm,
         `Complex multimorbidity` = multm_comp,
         HIV = hiv_pos_comb,
         `Ever had tuberculosis` = que_tb,
         Hypertension = ce_hbp_dx,
         `Heart disease` = que_heart,
         Hypercholesterolaemia = que_cho,
         Diabetes = ce_diab_dx,
         GORD = que_grd,
         `Bowel disease` = que_bow,
         Anaemia = que_anm,
         Stroke = que_stk,
         Epilepsy = que_epi,
         Dementia = que_dem,
         `Parkinsons disease` = que_park,
         Neuropathy = que_nrp,
         `Mood disorders` = que_dep,
         `Severe mental illness` = que_szp,
         Arthritis = que_rhe,
         `Fragility fracture` = que_ost,
         Gout = que_gout,
         `Chronic lung disease` = que_asth,
         `Kidney disease` = que_kid,
         Cancer = que_can,
         `Thyroid disease` = que_thy,
         Allergy = que_all) |>
  tbl_summary(by = site, missing = "no") |>
  add_overall() |>
  create_table_1()
