# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# gtsummary to house flextable --------
# Requires: 00_settings/05_tables.R, tidy_table.R
# Creates:  create_table_1()
# Feeds:    table 1, supplementary table 7
#
# Header labels are built for whichever stat columns are present, so five sites
# plus an overall column are labelled as readily as two.

create_table_1 <- function(tbl_object) {

  stat_cols = names(tbl_object$table_body)[startsWith(names(tbl_object$table_body), "stat_")]

  # {level} is undefined for the add_overall() column, so it is labelled separately
  temp_tbl <- tbl_object |>
    modify_header(all_stat_cols(stat_0 = FALSE) ~ "{level}\n(N={n})")

  if ("stat_0" %in% stat_cols) {
    temp_tbl <- temp_tbl |> modify_header(stat_0 ~ "Overall\n(N={N})")
  }

  labels <- temp_tbl[["table_styling"]][["header"]][["label"]]
  names(labels) <- names(temp_tbl$table_body)

  # Rows are complete where the first by-group column is populated; where there
  # is no by-group, use the overall column
  key_col = if ("stat_1" %in% stat_cols) "stat_1" else "stat_0"

  body <- temp_tbl$table_body |>
    select(var_label, label, all_of(stat_cols)) |>
    mutate(label = ifelse(var_label == label, NA, label)) |>
    filter(!is.na(.data[[key_col]]))

  # A horizontal rule wherever the variable changes
  border_rows = which(body$var_label !=
                        dplyr::lag(body$var_label, default = body$var_label[1])) - 1

  header_values = c(list(var_label = "Variable", label = "Level"),
                    as.list(labels[stat_cols]))

  body |>
    flextable() |>
    set_header_labels(values = header_values) |>
    merge_v(j = 1) |>
    hline(i = border_rows, part = "body", border = inner_border) |>
    tidy_table()
}
