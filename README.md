**Multimorbidity among older people in three African countries: a population-based cross-sectional study in The Gambia, South Africa, and Zimbabwe
**

Analysis code for the Fractures E3 multimorbidity study: a cross-sectional
analysis of chronic conditions, their combinations, their risk factors, and
their association with health-related quality of life among adults aged 40 years
and over in The Gambia, Zimbabwe and South Africa.

**Citation.**  TBC

## What this code produces

Sourcing `run_analysis.R` writes two Word documents:

- `output/FE3_MM_main.docx` – tables 1 and 2, figures 1 to 3
- `output/FE3_MM_supplement.docx` – supplementary tables 5 to 21 and
  supplementary figures 2 to 8

Supplementary tables 1 to 4 (country indices, condition definitions, latent
class model specifications, STROBE checklist), supplementary figure 1 (the
recruitment flow diagram) and figure 4 (the visual abstract) were prepared by
hand and are not generated here. Numbering follows the published supplement, so
those positions are left as gaps.

## Data availability

No participant data are included in this repository. The analysis dataset holds
individual-level information on HIV status, mental health diagnoses and other
sensitive conditions from five community cohorts, and cannot be shared openly
under the consent obtained.

Data preparation is also outside this repository. Deriving the chronic condition
variables uses participants' free-text descriptions of their own conditions and
medications, which are identifying. The code here begins from a prepared
analysis dataset, specified variable by variable in
[`assets/data_dictionary.md`](assets/data_dictionary.md), so that the analysis
can be run against an equivalently prepared dataset.

Requests for access to the underlying data should be directed to the Fractures
E3 study group.

## Requirements

- R, with the packages loaded in `R/00_settings/01_packages.R`
- Stata, for the latent class models

`output/sessionInfo.txt` records the R and package versions of the run that
produced the output.

## Running it

1. Edit the paths at the top of `run_analysis.R`: the analysis dataset and the
   population workbook. `projdir` defaults to the working directory, so open the
   repository as the working directory before sourcing.
2. `source("run_analysis.R")`. It stops after writing `stata/df_forlca.dta`,
   because the latent class output does not exist yet.
3. Run the two do-files in `stata/`. [`stata/README.md`](stata/README.md)
   documents the handoff in both directions, including sheet and column names.
4. `source("run_analysis.R")` again, and it runs to completion.

Output is written to `output/`, which is created if absent. Nothing outside it
is written or removed.

## Layout

```
run_analysis.R              entry point
R/00_settings/              packages, analysis parameters, colours, plot and table formatting
R/01_functions/             shared helpers, including the site-stratified model loop
R/02_prepare/               load and check data, export the Stata input, build standardisation weights
R/03_descriptive/           table 1, supplementary tables 5 to 7
R/04_combinations/          figure 1, supplementary figures 2 and 3, supplementary tables 18 and 19
R/05_pyramids/              figure 2, supplementary figures 4 to 6
R/06_riskfactors/           supplementary figures 7 and 8, supplementary tables 20 and 21
R/07_hrqol/                 supplementary tables 8 to 10
R/08_lca/                   table 2, figure 3, supplementary tables 11 to 17
R/09_compile/               footnotes and the two output documents
stata/                      latent class do-files and the handoff documentation
assets/                     Word template, input data dictionary
dev/                        synthetic data generator, for testing that the pipeline runs
```

## Where each output comes from

| Output | Object | Script |
|---|---|---|
| Table 1 | `tbl_desc_morbidities_site` | `03_descriptive/01_tbl_study_population.R` |
| Table 2 | `table_eq5d_tobit_combo` | `08_lca/06_tbl_eq5d_combo.R` |
| Figure 1 | `plot_upset_std` | `04_combinations/02_plt_upset_standardised.R` |
| Figure 2 | `poppyramid`, `obspyramid`, `plot_conditionsoverage_country` | `05_pyramids/03`, `05_pyramids/02`, `05_pyramids/05` |
| Figure 3 | `plot_lca_oe_nogam` | `08_lca/02_tbl_lca_fit.R` |
| S table 5 | `tbl_std_morbidities` | `03_descriptive/02_tbl_standardised.R` |
| S table 6 | `tbl_std_morbidities_who` | `03_descriptive/03_tbl_standardised_who.R` |
| S table 7 | `table_propdiagnosed` | `03_descriptive/04_tbl_propdiagnosed.R` |
| S table 8 | `table_eq5dindex` | `07_hrqol/01_model_eq5d.R` |
| S table 9 | `table_eq5d_all_all` | `07_hrqol/01_model_eq5d.R` |
| S table 10 | `table_eq5dvas` | `07_hrqol/01_model_eq5d.R` |
| S table 11 | `table_lca_fit` | `08_lca/02_tbl_lca_fit.R` |
| S table 12 | `table_lca_postprob` | `08_lca/02_tbl_lca_fit.R` |
| S table 13 | `tbl_cond_class` | `08_lca/03_tbl_cond_class.R` |
| S tables 14 to 16 | `table_rf_class_zim`, `_sar`, `_sau` | `08_lca/04_model_rf_class.R` |
| S table 17 | `table_eq5d_class_tobit` | `08_lca/06_tbl_eq5d_combo.R` |
| S table 18 | `table_commondyads` | `04_combinations/03_tbl_dyads_triads.R` |
| S table 19 | `table_commontriads` | `04_combinations/03_tbl_dyads_triads.R` |
| S table 20 | `table_rf_univ_agesex` | `06_riskfactors/02_tbl_rf.R` |
| S table 21 | `table_rf_agesexsep` | `06_riskfactors/02_tbl_rf.R` |
| S figure 2 | `plot_upset` | `04_combinations/01_plt_upset_crude.R` |
| S figure 3 | `plot_upset_site_std` | `04_combinations/02_plt_upset_standardised.R` |
| S figure 4 | `obspyramid_sites`, `obspyramid_sites_exchiv` | `05_pyramids/01_obspyramid_sites.R` |
| S figure 5 | `poppyramid_exchiv` | `05_pyramids/04_poppyramid_exchiv.R` |
| S figure 6 | `plot_conditionsoverage_sites` | `05_pyramids/05_plt_condition_coverage.R` |
| S figure 7 | `plot_rf_univagesex` | `06_riskfactors/03_plt_rf.R` |
| S figure 8 | `plot_rf_agesexsep` | `06_riskfactors/03_plt_rf.R` |

Supplementary tables 18 and 19 give the disease combination prevalences quoted
in the results text. Supplementary tables 20 and 21 give the odds ratios plotted
in supplementary figures 7 and 8.

## Methodological notes

Points a reader may want to know before interpreting the output.

1. **Latent class indicator sets.** The conditions entered into the latent class
   model at each site were selected on the basis of the number of participants
   with each condition at that site, and are listed as globals in
   `stata/LCA neat.do` and in supplementary table 3. Class 1 is fixed as the
   cardiometabolic class by a constraint on the hypertension coefficient, which
   is what makes the class labels comparable across sites.
2. **Models that fail to converge are not reported.** A site by exposure by
   outcome combination whose fit raises a warning is excluded rather than
   reported. Excluded combinations are listed in the console during a run.
3. **Risk factor models use a complete-case sample.** The analysis sample is
   complete cases across all exposures other than physical activity, so both the
   univariable and the adjusted estimates are conditioned on covariate
   completeness. The N is given in the figure and table footnotes.
4. **Dyads and triads are not exclusive.** A participant with more conditions
   than the number listed contributes to every combination they have, so
   supplementary tables 18 and 19 are not directly comparable with the
   intersection figures, which show exclusive combinations.
5. **Standardised prevalence confidence intervals are Wald intervals** and are
   unreliable for the least common conditions.
6. **Entropy** is taken from `stata/lca_entropy.do`, which reports to the Stata
   results window, and is held in `R/09_compile/00_footnotes.R`. It needs
   updating by hand if the latent class models are refitted.
