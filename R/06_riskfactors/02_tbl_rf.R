# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# Risk factor tables --------
# Requires: rf_univ_output, rf_agesex_output, rf_agesexsep_output,
#           add_rf_labels(), or_flextable(), fn_rf_base
# Creates:  table_rf_univ_agesex, table_rf_agesexsep,
#           fn_rf_univ_agesex, fn_rf_agesexsep
# Feeds:    supplementary tables 20 and 21
#
# The estimates plotted in supplementary figures 7 and 8, given numerically.
# Each table mirrors its figure: rows are the multimorbidity definition and the
# exposure, columns are the five sites, each with n/N, the odds ratio and its
# p value. Reference categories are not shown, as in the figures, so n/N is the
# number at the exposed level.

build_rf_table <- function(x, with_model = FALSE) {

  out <- x |>
    add_rf_labels() |>
    filter(!is.na(term_label)) |>
    # term_label is ordered for the figures, which read bottom to top, so
    # reverse it to read top to bottom in a table
    arrange(outcome_label, desc(term_label)) |>
    rename(Multimorbidity = outcome_label, Exposure = term_label)

  if (with_model) {
    or_flextable(rename(out, Model = model),
                 id_cols = c("Multimorbidity", "Model", "Exposure"))
  } else {
    or_flextable(out, id_cols = c("Multimorbidity", "Exposure"))
  }
}

## Supplementary table 20: univariable and adjusted for age and sex --------
table_rf_univ_agesex <- bind_rows(rf_univ_output, rf_agesex_output) |>
  mutate(model = factor(model,
                        levels = c("Univariable", "Adjusted for age and sex"))) |>
  build_rf_table(with_model = TRUE)

## Supplementary table 21: adjusted for age, sex and socioeconomic position ----
table_rf_agesexsep <- rf_agesexsep_output |>
  build_rf_table()

fn_rf_univ_agesex = paste0(
  fn_rf_base, " The univariable model included the exposure alone; the adjusted ",
  "model additionally included age and sex. n/N is the number of participants at ",
  "the exposed level of the exposure over the total at that site; reference ",
  "categories are not shown. Abbreviations: OR = odds ratio; 95% CI = 95% ",
  "confidence interval; BMI = body mass index.")

fn_rf_agesexsep = paste0(
  fn_rf_base, " Models additionally included age, sex, wealth index tertile, ",
  "maximum educational attainment and food insecurity as indicators of ",
  "socioeconomic position. n/N is the number of participants at the exposed ",
  "level of the exposure over the total at that site; reference categories are ",
  "not shown. Abbreviations: OR = odds ratio; 95% CI = 95% confidence interval; ",
  "BMI = body mass index.")
