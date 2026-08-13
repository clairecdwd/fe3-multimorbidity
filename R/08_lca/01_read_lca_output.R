# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# Read the Stata latent class output --------
# Requires: lca_path and lca_assignment_path (set in run_analysis.R),
#           02_prepare/01_load_data.R (df), 00_settings/02_parameters.R
#           (site_levels, defn_mm_labelsdf)
# Creates:  lca_fit_raw, lca_cond, condition_lookup, lca_assignment, df_lca
# Feeds:    supplementary tables 11 to 17, table 2 and figure 3, all of which
#           are built in the scripts numbered 02 to 06 in this directory
#
# The latent class models are fitted in Stata (stata/LCA neat.do), which writes
# two workbooks. Their layout is fixed by the putexcel calls in that do-file and
# is documented here so that a reader does not have to open the files.
#
# LCA.xlsx
#   "Model fit statistics"  one block per site, in site_levels order. Each block
#                           is a header row carrying the site name in column A
#                           and the `estimates stats` column names in columns C
#                           to H (N, ll(null), ll(model), df, AIC, BIC) plus
#                           "SSBIC" in column I; then one row per fitted model
#                           with the model id in column B ("m1" = one class,
#                           "m2" = two classes). Blocks are separated by a blank
#                           row. Five sites x two models = ten model rows.
#   "<site> LCMM"           one sheet per site, `estat lcmean` output. Column A
#                           carries the site name on the header row only, column
#                           B the class number, column C the condition variable
#                           name, then b, se, z, pvalue, ll, ul.
#   "LCMP"                  latent class marginal probabilities. Nothing in this
#                           analysis uses them, so the sheet is not read.
#
# LCA_assignment.xlsx
#   "<site>"                one sheet per site, exported with firstrow(variables)
#                           so the column names are real. Must contain f03_hid,
#                           class4allpost1, class4allpost2 and class4allmax.

# Model fit statistics --------
sheet_lca_fit = "Model fit statistics"

lca_fit_cols = c("site_label", "model_id", "N", "ll_null", "ll_model",
                 "df", "AIC", "BIC", "SSBIC")

lca_fit_sheet <- read_excel(lca_path, sheet = sheet_lca_fit, col_names = FALSE)

if (ncol(lca_fit_sheet) != length(lca_fit_cols)) {
  stop("Sheet '", sheet_lca_fit, "' of ", lca_path, " has ",
       ncol(lca_fit_sheet), " columns; ", length(lca_fit_cols),
       " were expected (", paste(lca_fit_cols, collapse = ", "), ").\n",
       "The layout is written by putexcel in stata/LCA neat.do. If the Stata ",
       "version in use returns a different set of `estimates stats` columns, ",
       "update lca_fit_cols to match.", call. = FALSE)
}

names(lca_fit_sheet) <- lca_fit_cols

## Site labels --------
# Site labels are keyed to the sheet rather than assigned by row position, so a
# gsem model that fails to converge cannot shift the labels: the do-file writes
# the site name into column A of each block header row, and it is carried down
# onto that block's model rows. If column A is empty, positional labels are used
# instead, but only after asserting that there are exactly ten model rows.
has_site_label <- any(!is.na(lca_fit_sheet$site_label))

if (has_site_label) {
  lca_fit_sheet <- lca_fit_sheet |>
    fill(site_label, .direction = "down")
}

lca_fit_raw <- lca_fit_sheet |>
  filter(model_id %in% c("m1", "m2")) |>
  mutate(Model = case_when(model_id == "m1" ~ "1 class",
                           model_id == "m2" ~ "2 classes")) |>
  mutate(across(c(N, ll_null, ll_model, df, AIC, BIC, SSBIC), as.numeric)) |>
  mutate(across(c(AIC, BIC, SSBIC), ~ round(.x, 2)))

stopifnot(
  "Sheet 'Model fit statistics' must contain exactly two fitted models (m1, m2) for each of the five sites, ten rows in total. Fewer rows means at least one gsem model did not converge and was not written out; the site labels can no longer be trusted. Check the Stata log before going further." =
    nrow(lca_fit_raw) == 2 * length(site_levels))

if (!has_site_label) {
  lca_fit_raw$site_label <- rep(site_levels, each = 2)
  warning("Sheet '", sheet_lca_fit, "' carries no site names in column A. ",
          "Falling back to positional labelling in site_levels order. ",
          "This is only safe because the row count was checked first.",
          call. = FALSE)
}

stopifnot(
  "Site labels read from the model fit sheet do not match site_levels." =
    setequal(lca_fit_raw$site_label, site_levels),
  "Each site must contribute exactly two rows to the model fit sheet." =
    all(table(lca_fit_raw$site_label) == 2))

lca_fit_raw <- lca_fit_raw |>
  mutate(site_label = factor(site_label, levels = site_levels)) |>
  arrange(site_label, Model)

# Class-conditional marginal means --------
# Display labels for the condition variables. defn_mm_labelsdf holds the display
# label in column 1 and the variable name in column 2; the second column takes
# its name from the object it was built from, so it is renamed by position
# rather than by a name that could change.
condition_lookup <- defn_mm_labelsdf |>
  rename(condition = 2)

read_lcmm_sheet <- function(site_name) {
  read_excel(lca_path, sheet = paste(site_name, "LCMM")) |>
    rename(class_id = 2, condition = 3) |>
    filter(!is.na(condition)) |>
    mutate(site = site_name,
           class = paste("Class", as.character(class_id))) |>
    select(site, class, condition, b, ll, ul)
}

lca_cond <- map(site_levels, read_lcmm_sheet) |>
  bind_rows() |>
  left_join(condition_lookup, by = "condition") |>
  # Three of the indicators entering the latent class models are composites of
  # two conditions and so have no entry in defn_mm_labels. Condition2 is coerced
  # to character first: it arrives as a factor and case_when will not mix a
  # factor with the character labels below.
  mutate(Condition2 = case_when(
    condition == "que_dep_szp" ~ "Mental health condition",
    condition == "que_bow_grd" ~ "GORD/bowel disease",
    condition == "que_heart_cho" ~ "Cardiovascular disease",
    !is.na(Condition2) ~ as.character(Condition2)))

# Class assignment --------
# Stata fitted the models on people with at least one condition, so anyone with
# no conditions has no posterior probability and no sheet row. They are given
# class 0 below.
read_assignment_sheet <- function(site_name) {
  read_excel(lca_assignment_path, sheet = site_name) |>
    mutate(class = case_when(class4allmax == class4allpost1 ~ 1,
                             class4allmax == class4allpost2 ~ 2)) |>
    select(f03_hid, class)
}

lca_assignment <- map(site_levels, read_assignment_sheet) |>
  bind_rows()

if (anyDuplicated(lca_assignment$f03_hid) > 0) {
  stop("f03_hid is duplicated across the sheets of ", lca_assignment_path,
       ". Each participant must appear in exactly one site sheet.",
       call. = FALSE)
}

# The join key is stated explicitly and the multiplicity asserted rather than
# assumed, so an unexpected duplicate cannot quietly inflate the frame.
df_lca <- df |>
  left_join(lca_assignment, by = "f03_hid", relationship = "one-to-one") |>
  mutate(class = if_else(is.na(class), 0, class),
         class = factor(class, levels = c(0, 1, 2)))

## Check the unassigned --------
# Class 0 should be exactly the people with no conditions. More people than that
# means Stata dropped rows for some other reason, most likely listwise deletion
# on an indicator, and the class 0 group is then not what the tables say it is.
n_class0 <- sum(df_lca$class == 0)
n_nocond <- sum(df_lca$multim_n == 0, na.rm = TRUE)

if (n_class0 < n_nocond) {
  stop("Fewer participants in class 0 (", n_class0, ") than have multim_n == 0 (",
       n_nocond, "). Someone with no conditions has been given a latent class, ",
       "which the Stata models cannot do. Check the merge key.", call. = FALSE)
}

if (n_class0 > n_nocond) {
  warning("Class 0 holds ", n_class0, " participants but only ", n_nocond,
          " have multim_n == 0. The extra ", n_class0 - n_nocond,
          " were dropped by the Stata models and are being treated as having ",
          "no conditions. Check the gsem output before reporting.",
          call. = FALSE)
}

df_lca |> tabyl(class)
