# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# Measured prevalence and proportion already diagnosed --------
# Requires: df, 00_settings/05_tables.R, create_table_1()
# Creates:  table_propdiagnosed
# Feeds:    supplementary table 7
#
# For each of the three conditions ascertained by measurement or test, the
# table shows the prevalence in the whole sample and, among those found to have
# the condition, the proportion who already knew. The "known" variables are set
# to missing for participants without the condition so that the second row of
# each pair is conditional on the first.

## Table --------
tbl_propdiagnosed_gts <- df |>
  mutate(hiv_pos_known = if_else(hiv_pos_comb == "No", NA_character_,
                                 as.character(hiv_pos_known)),
         ce_hbp_known  = if_else(ce_hbp_dx == "No", NA_character_,
                                 as.character(ce_hbp_known)),
         ce_diab_known = if_else(ce_diab_dx == "No", NA_character_,
                                 as.character(ce_diab_known))) |>
  select(`Diabetes prevalence`     = ce_diab_dx,
         `Known diabetes`          = ce_diab_known,
         `HIV prevalence`          = hiv_pos_comb,
         `Known HIV`               = hiv_pos_known,
         `Hypertension prevalence` = ce_hbp_dx,
         `Known hypertension`      = ce_hbp_known,
         site) |>
  tbl_summary(by = site, missing = "no") |>
  add_overall()

## Formatting --------
# Every variable in this table is dichotomous, so tbl_summary returns one body
# row per variable and the "Level" column is "Yes" all the way down. It is
# dropped by name rather than by position, so the call does not depend on where
# create_table_1() happens to place it.
#
# The three conditions each contribute a prevalence row and a known row, so a
# solid rule after every second row separates them. The rule rows are derived
# from the number of body rows, so adding a fourth condition does not mislabel
# the table, and from the same rows create_table_1() keeps, that is those with a
# populated first by-group column.
n_body_rows = tbl_propdiagnosed_gts$table_body |>
  filter(!is.na(stat_1)) |>
  nrow()

block_rows = seq(2, n_body_rows - 1, by = 2)

stopifnot("table_propdiagnosed expects a prevalence row and a known row per condition" =
            n_body_rows %% 2 == 0)

table_propdiagnosed <- tbl_propdiagnosed_gts |>
  create_table_1() |>
  delete_columns(j = "label") |>
  hline(i = block_rows, part = "body", border = header_border)
