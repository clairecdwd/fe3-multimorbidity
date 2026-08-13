# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# Analysis parameters --------
# Every fixed input the analysis depends on, in one place, so that a reader can
# see every choice at once and no script can be sourced before the parameters it
# needs exist.

## Sites --------
site_levels = c("Gambia rural", "Gambia urban", "Zimbabwe urban",
                "South Africa rural", "South Africa urban")

country_levels = c("Gambia", "Zimbabwe", "South Africa")

# Column order used in the supplementary tables
site_order = c("Overall", site_levels)

## Population weighting --------
# Proportion of each national population living in rural areas, used to combine
# the rural and urban sub-sites into a country estimate. The urban weight is the
# complement, 1 - ruralperc. Zimbabwe contributed an urban site only, which is
# applied directly to the national population.
# Source: national statistics, 2024. Update if the reference year changes.
ruralperc_gam = 0.35
ruralperc_sa  = 0.31
ruralperc_zim = 0

## Age strata for direct standardisation --------
# The three strata the study sampled within. Population counts are collapsed to
# these bands before weights are computed.
age_strata = c("40–54", "55–69", "≥70")

## Chronic conditions --------
# The 24 conditions that define multimorbidity, ordered by body system.
defn_mm = c(
  # Infections
  "hiv_pos_comb", "que_tb",
  # Cardiometabolic disease
  "ce_hbp_dx", "que_heart", "que_cho", "ce_diab_dx",
  # Gastrointestinal disease
  "que_bow", "que_grd", "que_anm",
  # Nervous system
  "que_stk", "que_epi", "que_dem", "que_park", "que_nrp",
  # Mental health disorders
  "que_dep", "que_szp",
  # Musculoskeletal disease
  "que_rhe", "que_ost", "que_gout",
  # Respiratory disease
  "que_asth",
  # Renal disease
  "que_kid",
  # Endocrine
  "que_thy",
  # Cancer
  "que_can",
  # Allergies
  "que_all")

defn_mm_labels = c(Epilepsy = "que_epi",
                   `Kidney disease` = "que_kid",
                   Tuberculosis = "que_tb",
                   HIV = "hiv_pos_comb",
                   `Chronic lung disease` = "que_asth",
                   `Heart disease` = "que_heart",
                   Hypercholesterolaemia = "que_cho",
                   `Thyroid disease` = "que_thy",
                   Cancer = "que_can",
                   Stroke = "que_stk",
                   Dementia = "que_dem",
                   Arthritis = "que_rhe",
                   `Fragility fracture` = "que_ost",
                   `Bowel disease` = "que_bow",
                   `Mood disorders` = "que_dep",
                   `Severe mental illness` = "que_szp",
                   GORD = "que_grd",
                   Anaemia = "que_anm",
                   Neuropathy = "que_nrp",
                   `Parkinsons disease` = "que_park",
                   Gout = "que_gout",
                   Diabetes = "ce_diab_dx",
                   Hypertension = "ce_hbp_dx",
                   Allergy = "que_all")

# Display order for conditions in tables and figures, by body system
defn_mm_display = c(
  "HIV", "Tuberculosis", "Hypertension", "Heart disease", "Hypercholesterolaemia",
  "Diabetes", "GORD", "Bowel disease", "Anaemia", "Stroke", "Epilepsy", "Dementia",
  "Parkinsons disease", "Neuropathy", "Mood disorders", "Severe mental illness",
  "Arthritis", "Fragility fracture", "Gout", "Chronic lung disease",
  "Kidney disease", "Cancer", "Thyroid disease", "Allergy")

defn_mm_labelsdf <- defn_mm_labels |>
  as.data.frame() |>
  rownames_to_column(var = "Condition2") |>
  mutate(Condition2 = factor(Condition2, levels = defn_mm_display))

## Conditions drawn in the intersection plots --------
# Conditions that did not contribute to any of the twelve largest intersections
# at any site are not displayed. Set order is top to bottom on the matrix panel.
set_order = c("HIV", "Tuberculosis", "Hypertension", "Heart disease",
              "Hypercholesterolaemia", "Diabetes", "GORD", "Stroke", "Epilepsy",
              "Mood disorders", "Severe mental illness", "Arthritis",
              "Fragility fracture", "Chronic lung disease")

n_intersections = 12

## Risk factor models --------
exposures = c("con_sex",
              "con_age_great40_10yr",
              "que_edm",
              "que_wealthindex_3cat",
              "que_alc_exc",
              "que_esm",
              "ce_bmi_cat",
              "que_food_insec_sc",
              "phy_low")

outcomes = c("multm", "multm_exchiv")

adjust_agesex    = "+ con_age_great40 + con_sex"
adjust_agesexsep = "+ con_sex + con_age_great40 + que_wealthindex_3cat + que_edm + que_food_insec_sc"

# BMI reference category is set explicitly inside the model loops; every other
# categorical exposure uses its first factor level as the reference, which is
# R's default treatment contrast. Levels are fixed in the analysis dataset and
# documented in assets/data_dictionary.md.
bmi_ref = "Healthy-overweight"

## Disease combinations --------
n_top_comb = 5     # combinations taken from each site before the union
sep_comb = "–"     # en dash, matching the combination names used in the paper
dp_comb = 1        # decimal places for standardised prevalence

## EQ-5D --------
eq5d_upper = 0.9        # upper censoring point for the Tobit models
eq5d_floor = 0.001      # values below this are floored before modelling
