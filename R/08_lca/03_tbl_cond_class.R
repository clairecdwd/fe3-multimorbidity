# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# Conditions by latent class --------
# Requires: 08_lca/01_read_lca_output.R (df_lca)
# Creates:  tbl_cond_class, fn_cond_class
# Feeds:    supplementary table 13
#
# The Gambia sites are excluded: the two-class solution was not interpretable
# there. Class 0, the people with no conditions, is dropped because the
# comparison of interest is between the two multimorbidity classes.

tbl_cond_class <- df_lca |>
  filter(!str_detect(as.character(site), "Gambia")) |>
  mutate(class = case_when(
    class == 0 ~ "No conditions",
    class == 1 ~ "Cardiometabolic multimorbidity",
    class == 2 ~ "HIV-related multimorbidity"),
    class = factor(class, levels = c("No conditions",
                                     "Cardiometabolic multimorbidity",
                                     "HIV-related multimorbidity"))) |>
  filter(class != "No conditions") |>
  droplevels() |>
  select(site, class,
         `N conditions` = multim_n,
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
         Depression = que_dep,
         `Severe mental illness` = que_szp,
         Arthritis = que_rhe,
         `Fragility fracture` = que_ost,
         Gout = que_gout,
         `Chronic lung disease` = que_asth,
         `Kidney disease` = que_kid,
         Cancer = que_can,
         `Thyroid disease` = que_thy,
         Allergy = que_all) |>
  tbl_summary(by = class,
              missing = "no",
              type = list(`N conditions` = "continuous")) |>
  add_p(pvalue_fun = ~ style_sigfig(., digits = 4)) |>
  as_flex_table()

# Footnote --------
# add_p() is left on its gtsummary defaults, so the test used depends on the
# variable type. Stating which test produced which p value is a reporting
# requirement, and the absence of any correction for multiplicity matters here
# because every row of the table is tested.
fn_cond_class = paste(
  "Footnotes: number of conditions compared between classes with the Wilcoxon",
  "rank-sum test; site and all condition rows compared with Pearson's",
  "chi-squared test of independence, or Fisher's exact test where any expected",
  "cell count was below five. P values are unadjusted: no correction has been",
  "made for the number of conditions tested, so individual p values should be",
  "read as descriptive rather than as formal hypothesis tests.",
  "Abbreviations: GORD = gastro-oesophageal reflux disease;",
  "HIV = human immunodeficiency virus.")
