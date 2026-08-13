# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# Estimate and p value formatting --------
# Requires: nothing
# Creates: format_est_ci(), format_p(), split_term()
# Feeds:   every table of model output

## Estimate with confidence interval --------
# Two decimal places, trailing zeros kept, bounds joined by an en dash.
format_est_ci <- function(estimate, conf.low, conf.high, dp = 2, sep = "–") {
  paste0(trimws(format(round(estimate, dp), nsmall = dp)), " (",
         trimws(format(round(conf.low, dp), nsmall = dp)), sep,
         trimws(format(round(conf.high, dp), nsmall = dp)), ")")
}

## p value --------
# Exact values to two significant figures, one below 0.01, floored at <0.001.
format_p <- function(p) {
  case_when(
    p >= 0.01               ~ paste0(signif(p, 2)),
    p <  0.01 & p >  0.001  ~ paste0(signif(p, 1)),
    p <= 0.001              ~ "<0.001"
  )
}

## Split a model term into variable and level --------
# Model terms for a categorical exposure are paste0(variable, level). Matching
# against the known exposure names splits them exactly, without depending on how
# the factor levels happen to be capitalised.
split_term <- function(term, variables) {
  hit <- map_chr(term, function(x) {
    matches <- variables[startsWith(x, variables)]
    if (length(matches) == 0) return(NA_character_)
    # Longest match wins, so que_edm does not capture que_edm_something
    matches[which.max(nchar(matches))]
  })

  tibble(term = term,
         variable = if_else(is.na(hit), term, hit),
         level = if_else(is.na(hit), NA_character_, substring(term, nchar(hit) + 1)))
}
