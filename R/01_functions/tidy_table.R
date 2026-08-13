# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# House flextable formatting --------
# Requires: 00_settings/05_tables.R (fontsize, internalmargins, header_border)
# Creates:  tidy_table()
# Feeds:    every table in both output documents

tidy_table <- function(flextable_obj, label_cols = 1:2) {
  flextable_obj |>
    bold(j = 1, bold = TRUE) |>
    bold(part = "header", bold = TRUE) |>
    hline_top(part = "header", border = header_border) |>
    hline(part = "header", border = header_border) |>
    hline_bottom(part = "body", border = header_border) |>
    valign(valign = "top", part = "body") |>
    align(align = "center", part = "all") |>
    align(align = "left", j = label_cols, part = "all") |>
    fontsize(size = fontsize, part = "all") |>
    padding(padding = internalmargins) |>
    set_table_properties(layout = "autofit", width = 1)
}
