# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# Standardised prevalence of disease dyads and triads --------
# Requires: temp_site, site_setting_weights, set_order, n_top_comb, sep_comb,
#           dp_comb, site_order
# Creates:  df_dyads_std, df_triads_std, table_commondyads, table_commontriads,
#           fn_commondyads, fn_commontriads
# Feeds:    Supplementary tables 18 and 19
#
# The numbers behind the disease combinations quoted in the results text,
# presented precisely rather than only in the intersection figures.
#
# Combinations here are NOT exclusive. A participant with k conditions
# contributes all C(k,2) dyads and C(k,3) triads, so a cell is the standardised
# prevalence of having at least those conditions, whatever else the participant
# has. That is a different quantity from the intersection figures, which show
# exclusive combinations, and the two are not directly comparable. The footnote
# says so.

# Denominators for the column headers --------
# The analysed N, not the recruited N: standardisation restricts to age 40 and
# over with recorded sex.
n_site = temp_site |>
  count(site) |>
  mutate(site = as.character(site)) |>
  tibble::deframe()

n_site = c(Overall = sum(n_site), n_site)

n_countries = n_distinct(site_setting_weights$country)

# Combination prevalence --------
## Every combination of size k, by site --------
comb_std <- function(data, weight_col, sets, k) {
  data |>
    mutate(row_id = row_number(), w = .data[[weight_col]]) |>
    select(row_id, site, w, all_of(sets)) |>
    pivot_longer(all_of(sets), names_to = "condition", values_to = "present") |>
    mutate(present = present %in% TRUE) |>
    filter(present) |>
    summarise(n_cond = n(),
              # combn() errors when n_cond < k, so return an empty list instead
              comb = list(if (n_cond >= k) combn(sort(condition), k, simplify = FALSE)
                          else list()),
              .by = c(row_id, site, w)) |>
    unnest_longer(comb) |>
    mutate(combination = map_chr(comb, ~ paste(.x, collapse = sep_comb))) |>
    summarise(n_obs = n(),
              prev_pct = sum(w, na.rm = TRUE) * 100,
              .by = c(site, combination)) |>
    mutate(site = as.character(site))
}

## Overall row --------
# Same convention as the standardised prevalence table: combine sub-sites within
# a country by national rural/urban proportion, then take an unweighted mean of
# the three countries. Combinations absent in a country contribute zero, so the
# divisor is the fixed number of countries, not the number of rows present.
add_overall_std <- function(df_in) {
  df_overall <- df_in |>
    left_join(site_setting_weights, by = "site") |>
    summarise(p_country = sum(setting_weight * prev_pct, na.rm = TRUE),
              .by = c(country, combination)) |>
    summarise(prev_pct = sum(p_country) / n_countries, .by = combination) |>
    mutate(site = "Overall")

  df_in |>
    summarise(n_obs = sum(n_obs), .by = combination) |>
    right_join(df_overall, by = "combination") |>
    bind_rows(df_in) |>
    mutate(site = factor(site, levels = site_order))
}

df_dyads_std  <- comb_std(temp_site, "w_site", set_order, k = 2) |> add_overall_std()
df_triads_std <- comb_std(temp_site, "w_site", set_order, k = 3) |> add_overall_std()

## Checks --------
# A dyad cell must equal the standardised prevalence of the two conditions
# co-occurring, computed directly. If these disagree the combn() step is wrong.
check_hiv_htn <- temp_site |>
  summarise(direct = sum(w_site[HIV %in% TRUE & Hypertension %in% TRUE],
                         na.rm = TRUE) * 100, .by = site)

print(check_hiv_htn)
print(df_dyads_std |> filter(combination == paste("HIV", "Hypertension", sep = sep_comb)))

# Dyad prevalences are not a decomposition and do not sum to 100: the total is
# 100 times the mean number of dyads per person. Printed so the number is not
# mistaken for a failed check.
print(df_dyads_std |> summarise(sum_prev = sum(prev_pct), .by = site))

# Render --------
# Row selection is the union of each site's top n_top_comb and the overall top
# n_top_comb, ordered by the overall rank. A strict overall top n would drop
# combinations that lead in a single site, which are exactly the ones the
# results text quotes.
tbl_comb_std <- function(df_in, label) {

  df_ranked <- df_in |>
    mutate(rank = min_rank(desc(prev_pct)), .by = site)

  keep = df_ranked |> filter(rank <= n_top_comb) |> distinct(combination) |> pull(combination)

  ord = df_ranked |>
    filter(site == "Overall", combination %in% keep) |>
    arrange(rank, combination) |>
    pull(combination)

  df_ranked |>
    filter(combination %in% keep) |>
    # Cells are rank followed by the standardised percentage in brackets, not a
    # raw count
    mutate(cell = if_else(
      is.na(prev_pct), NA_character_,
      paste0(rank, " (", trimws(format(round(prev_pct, dp_comb), nsmall = dp_comb)), "%)"))) |>
    select(site, combination, cell) |>
    pivot_wider(names_from = site, values_from = cell) |>
    mutate(combination = factor(combination, levels = ord)) |>
    arrange(combination) |>
    mutate(combination = as.character(combination)) |>
    select(combination, any_of(site_order)) |>
    rename(!!label := combination) |>
    rename_with(~ paste0(.x, " (N=", n_site[.x], ")"), .cols = any_of(site_order)) |>
    flextable() |>
    tidy_table()
}

table_commondyads  <- tbl_comb_std(df_dyads_std,  label = "Dyad")
table_commontriads <- tbl_comb_std(df_triads_std, label = "Triad")

# Footnotes --------
fn_comb_std_base = paste0(
  "The value in brackets is the age- and sex-standardised prevalence (%), that is the ",
  "proportion of the national population aged 40 years and over expected to have at ",
  "least the conditions listed, by direct standardisation to national population ",
  "structure (sex by ", paste(age_strata, collapse = ", "), " years). Combinations are ",
  "not exclusive: a participant with more conditions than the number listed contributes ",
  "to every combination they have, so a participant may be counted in several rows and ",
  "prevalences do not sum to 100%. Conditions considered are the ", length(set_order),
  " shown in figure 1; note that figure 1 shows exclusive combinations and is therefore ",
  "not directly comparable with this table. Missing condition status is treated as the ",
  "condition being absent. Combinations shown are those ranking in the top ", n_top_comb,
  " overall or within at least one site, ordered by overall rank. Header numbers show ",
  "the number of participants included in the standardised analysis, that is those aged ",
  "40 years and over with recorded sex. Where a cell is empty, no participants at that ",
  "site had that combination.")

fn_commondyads  = paste0("A dyad is two conditions co-existing in an individual. ",
                         fn_comb_std_base)
fn_commontriads = paste0("A triad is three conditions co-existing in an individual. ",
                         fn_comb_std_base)

# Numbers quoted in the results text --------
walk(list(dyads = df_dyads_std, triads = df_triads_std), function(x) {
  x |>
    filter(site != "Overall") |>
    slice_max(prev_pct, n = 2, by = site) |>
    arrange(site, desc(prev_pct)) |>
    mutate(prev_fmt = paste0(format(round(prev_pct, dp_comb), nsmall = dp_comb), "%")) |>
    select(site, combination, n_obs, prev_fmt) |>
    print(n = Inf)
})
