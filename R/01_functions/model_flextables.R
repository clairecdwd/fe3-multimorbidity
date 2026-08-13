# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# Model output to flextable --------
# Requires: 00_settings, tidy_table.R
# Creates:  or_flextable(), multin_flextable()
# Feeds:    supplementary tables 14 to 16, 20 and 21

## Site-stratified models, sites as columns --------
# Used for the odds ratio tables that give the numbers behind supplementary
# figures 7 and 8.
# id_cols are the row labels; everything else is pivoted so that each site gets
# a block of three columns. Row labels must identify a row uniquely, otherwise
# pivot_wider silently returns list columns.
or_flextable <- function(x, id_cols) {

  value_labels = c(`n/N` = "an_N", `OR (95% CI)` = "est_ci", `p value` = "p_value_fmt")

  # Site blocks in site order, each with the three values in a fixed order
  col_order = unlist(lapply(site_levels,
                            function(s) paste0(s, "_", names(value_labels))))

  dupes <- x |>
    summarise(n = n(), .by = all_of(c(id_cols, "site"))) |>
    filter(n > 1)

  if (nrow(dupes) > 0) {
    stop("Row labels do not identify a row uniquely:\n",
         paste(capture.output(print(dupes)), collapse = "\n"), call. = FALSE)
  }

  x |>
    select(all_of(id_cols), site, all_of(unname(value_labels))) |>
    rename(all_of(value_labels)) |>
    pivot_wider(names_from = site, values_from = all_of(names(value_labels)),
                names_glue = "{site}_{.value}") |>
    select(all_of(id_cols), any_of(col_order)) |>
    flextable() |>
    separate_header(split = "_") |>
    merge_v(j = seq_along(id_cols)) |>
    tidy_table(label_cols = seq_along(id_cols))
}

## Multinomial models, latent class as column groups --------
# The reference-class columns are dropped by name rather than by position, so
# the table cannot break silently if the upstream column order changes.
multin_flextable <- function(x) {

  dupes <- x |>
    summarise(n = n(), .by = c(site, variable_label, level, y.level)) |>
    filter(n > 1)

  if (nrow(dupes) > 0) {
    stop("Row labels do not identify a row uniquely:\n",
         paste(capture.output(print(dupes)), collapse = "\n"), call. = FALSE)
  }

  x |>
    mutate(level_N = paste0(level, " (", N, ")"),
           y.level = paste("Class", y.level)) |>
    select(site, variable_label, level_N, y.level, n,
           `OR (95% CI)` = est_ci, `p value` = p_value_fmt) |>
    pivot_wider(names_from = y.level, values_from = c(n, `OR (95% CI)`, `p value`),
                names_glue = "{y.level}.{.value}") |>
    select(-any_of(c("Class 0.OR (95% CI)", "Class 0.p value"))) |>
    arrange(site, variable_label, desc(is.na(`Class 1.OR (95% CI)`))) |>
    select(Variable = variable_label, `Level (N)` = level_N,
           starts_with("Class 0"), starts_with("Class 1"), starts_with("Class 2")) |>
    flextable() |>
    separate_header(split = "[\\.]") |>
    merge_v(j = 1) |>
    tidy_table()
}
