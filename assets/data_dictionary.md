# Input data dictionary

The public pipeline starts from a single cleaned analysis dataset. Data cleaning
is **not** part of this repository: deriving the chronic condition variables
requires participant free-text responses, which cannot be published. This file
specifies exactly what the analysis dataset must contain so that the analysis
can be reproduced against an equivalently prepared dataset.

Format: Stata `.dta`, read with `haven::read_dta()`. Labelled variables are
converted to factors with `haven::as_factor()`, so **the Stata value-label order
becomes the factor level order, and the first level is the model reference
category** under R's default treatment contrasts. `R/02_prepare/01_load_data.R`
checks the level order of every variable where it matters and stops if it is
wrong.

One row per participant. All participants are aged 40 years and over.

## Identifiers

| Variable | Type | Notes |
|---|---|---|
| `f03_hid` | character or numeric | Participant identifier. Must match the identifier exported to Stata and returned in `LCA_assignment.xlsx`. |
| `con_hid` | character or numeric | Household identifier. Used only to check that each site has at least two households before a model is fitted. |

## Demographics and site

| Variable | Type | Levels, in order | Notes |
|---|---|---|---|
| `con_sex` | factor | `Male`, `Female` | Reference is `Male`. |
| `con_age` | numeric | | Age in completed years. |
| `site` | factor | `Gambia rural`, `Gambia urban`, `Zimbabwe urban`, `South Africa rural`, `South Africa urban` | Order is checked. |
| `site2` | factor | `Gambia`, `Zimbabwe`, `South Africa` | Country. |
| `con_age_band` | character | `40–44` … `80–84`, `85+` | Five-year bands, en dash separator, top-coded at 85. Must match the `Age group` labels in the population workbook. |
| `con_age_great40` | numeric | | `con_age - 40`. |
| `con_age_great40_10yr` | numeric | | `con_age_great40 / 10`. |

## Chronic conditions

All 24 are factors with levels `No`, `Yes` in that order. Missing status is
treated as the condition being absent wherever the analysis needs a complete
membership matrix; this is stated in the relevant footnotes.

`hiv_pos_comb`, `que_tb`, `ce_hbp_dx`, `que_heart`, `que_cho`, `ce_diab_dx`,
`que_bow`, `que_grd`, `que_anm`, `que_stk`, `que_epi`, `que_dem`, `que_park`,
`que_nrp`, `que_dep`, `que_szp`, `que_rhe`, `que_ost`, `que_gout`, `que_asth`,
`que_kid`, `que_thy`, `que_can`, `que_all`

Display labels are set in `R/00_settings/02_parameters.R` (`defn_mm_labels`).

## Derived multimorbidity variables

| Variable | Type | Levels | Definition |
|---|---|---|---|
| `multim_n` | numeric | | Count of chronic conditions. |
| `multim_n_exchiv` | numeric | | Count excluding HIV. |
| `multm` | factor | `No`, `Yes` | `multim_n > 1`. |
| `multm_exchiv` | factor | `No`, `Yes` | `multim_n_exchiv > 1`. |
| `multm_comp` | factor | `No`, `Yes` | Complex multimorbidity, `multim_n > 2`. |

Note that the condition count in the source pipeline is taken over a 26-item
list that is not identical to the 24 conditions above: it adds migraine,
glaucoma and previous fracture and omits fragility fracture. Deliver
`multim_n` and `multim_n_exchiv` as computed in the cleaning stage rather than
recomputing them from the 24 condition columns.

## Previously diagnosed

| Variable | Type | Levels | Notes |
|---|---|---|---|
| `hiv_pos_known` | factor | `No`, `Yes` | Missing at both Gambian sites, where HIV testing was not undertaken. |
| `ce_hbp_known` | factor | `No`, `Yes` | |
| `ce_diab_known` | factor | `No`, `Yes` | |

## Socioeconomic and behavioural

| Variable | Type | Levels, in order | Notes |
|---|---|---|---|
| `que_edm` | factor | `At least secondary`, `No/primary` | Alphabetical order, checked on load. Reference is `At least secondary`. |
| `que_wealthindex_3cat` | factor | `Low`, then the two higher tertiles | **The order of the two higher levels cannot be determined from the analysis code.** Model terms are `...High` and `...Middle`, so the reference is `Low`. Confirm the full order against the source data before publication. |
| `que_food_insec_sc` | factor | `Food secure`, `Food insecure` | Reference is `Food secure`; the estimate is therefore for insecure versus secure. |
| `que_alc_der` | factor | `No`, `Yes` | Any alcohol use. |
| `que_alc_exc` | factor | `No`, `Yes` | Excess alcohol use. The threshold is not defined anywhere in the analysis code and needs confirming. |
| `que_alc_wk` | numeric | | Units per week; missing for non-drinkers by design. |
| `que_esm` | factor | `No`, `Yes` | Ever smoked. |
| `que_cur` | factor | `No`, `Yes` | Current smoker. |
| `que_cig` | numeric | | Cigarettes per day. |
| `phy_low` | factor | `Not low`, `Low` | Low physical activity, below 3000 MET-minutes per week. |
| `que_kcal_low` | factor | | Low energy intake indicator. |

## Anthropometry

| Variable | Type | Levels, in order |
|---|---|---|
| `ce_bmi` | numeric | |
| `ce_bmi_cat` | factor | `Underweight`, `Healthy-overweight`, `Obese` |

`ce_bmi_cat` is releveled to `Healthy-overweight` inside the model loops; the
delivered order is checked on load.

## EQ-5D

| Variable | Type | Notes |
|---|---|---|
| `eq5dindex` | numeric | Value index, Zimbabwe value set, maximum 0.9. Delivered **unfloored**; the analysis floors it at 0.001 in `R/07_hrqol/01_model_eq5d.R`. |
| `eq5d5l_vas2_uk_eng` | numeric | Visual analogue scale, 0 to 100. |

## External inputs

### Population workbook

Path set as `pop_path` in `run_analysis.R`. Three sheets named `Zimbabwe`,
`Gambia`, `South Africa`, each with columns `Age group`, `Male`, `Female`,
`Total`. `Age group` labels must use the same five-year band format as
`con_age_band`. The Gambian sheet also reports `85–89` and `90+`, which are
dropped to avoid double counting against the study's top band.

### Stata latent class output

See `stata/README.md`.

## Variables deliberately excluded

The following are used only by the cleaning stage or by outputs that are not
published, and must **not** be included in the analysis dataset:

- `que_mot_othr` and all free-text condition and medication fields. These
  contain participants' own words and are identifying by construction.
- `que_med_nme*`, `que_med_ind_cln*`. One footnote counted rheumatoid arthritis
  from these; that count is now supplied as the parameter `n_rheumatoid` in
  `R/03_descriptive/01_tbl_study_population.R`.
- `que_mig`, `que_gla`, `hear_loss*`, `vis_loss*`, `prev_frac`,
  `multm_comp_exchiv`, `con_age_great40_5yr`, `n_per_household`, `que_mob`.
- Employment, disability and physical performance variables used only in a
  descriptive table that is not published.
