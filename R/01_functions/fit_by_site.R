# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# Site-stratified model fitting --------
# Requires: format_estimates.R
# Creates:  fit_by_site(), attach_denominators()
# Feeds:    supplementary figures 7 and 8, supplementary tables 8 to 10,
#           14 to 17, 20 and 21, and table 2
#
# One site loop, called by every model script with a different fitting function.
#
# A model that emits ANY warning is discarded. That is a strong rule, so rather
# than dropping the combination quietly the discarded models are recorded and
# returned as an attribute, and printed at the end of the run.

fit_by_site <- function(data, sites, outcome, exposures, adjust = "",
                        fit_fun, tidy_fun, prepare_site = identity) {

  dropped <- tibble(site = character(), exposure = character(),
                    outcome = character(), reason = character(),
                    message = character())

  results <- lapply(sites, function(s) {

    df_site <- data |>
      filter(site == s) |>
      prepare_site()

    lapply(outcome, function(o) {

      lapply(exposures, function(i) {

        cat("\n--- Site:", s, "| Exposure:", i, "| Outcome:", o, "---\n")

        # Pre-checks. An exposure with a single observed level, or a site with
        # too few households, cannot support a model.
        exposure_data <- df_site[[i]]
        exposure_data <- exposure_data[!is.na(exposure_data)]

        if (length(unique(exposure_data)) <= 1) {
          dropped <<- add_row(dropped, site = s, exposure = i, outcome = o,
                              reason = "constant exposure",
                              message = "single unique value excluding NAs")
          return(NULL)
        }

        if (is.factor(exposure_data) && nlevels(droplevels(exposure_data)) < 2) {
          dropped <<- add_row(dropped, site = s, exposure = i, outcome = o,
                              reason = "constant exposure",
                              message = "fewer than two levels after droplevels")
          return(NULL)
        }

        if (length(unique(df_site$con_hid[!is.na(df_site$con_hid)])) < 2) {
          dropped <<- add_row(dropped, site = s, exposure = i, outcome = o,
                              reason = "insufficient clusters",
                              message = "fewer than two households")
          return(NULL)
        }

        mod_formula <- as.formula(paste(o, "~", i, adjust))

        # withCallingHandlers rather than tryCatch for the warning arm: a
        # tryCatch warning handler runs after the stack has unwound, by which
        # point the muffleWarning restart no longer exists and invoking it
        # raises a second, uncaught error.
        # Only the fit is watched for warnings. A model that does not converge,
        # or that separates, is dropped. Warnings raised while tidying are not a
        # property of the model and do not disqualify it.
        warn_msg <- NULL

        model_fit <- tryCatch(
          withCallingHandlers(
            fit_fun(mod_formula, df_site),
            warning = function(w) {
              warn_msg <<- conditionMessage(w)
              invokeRestart("muffleWarning")
            }
          ),
          error = function(e) {
            dropped <<- add_row(dropped, site = s, exposure = i, outcome = o,
                                reason = "error", message = conditionMessage(e))
            NULL
          }
        )

        if (!is.null(warn_msg)) {
          dropped <<- add_row(dropped, site = s, exposure = i, outcome = o,
                              reason = "warning", message = warn_msg)
          return(NULL)
        }

        if (is.null(model_fit)) return(NULL)

        out <- tryCatch(
          tidy_fun(model_fit),
          error = function(e) {
            dropped <<- add_row(dropped, site = s, exposure = i, outcome = o,
                                reason = "tidy failed", message = conditionMessage(e))
            NULL
          }
        )

        if (is.null(out)) return(NULL)

        out |> mutate(site = s, outcome = o, .before = 1)
      }) |>
        bind_rows()
    }) |>
      bind_rows()
  }) |>
    bind_rows()

  # An empty result means nothing downstream has a term column to work with, so
  # stop here where the reason is still visible rather than several joins later
  if (nrow(results) == 0) {
    stop("No models were retained for outcome(s) ", paste(outcome, collapse = ", "),
         ".\n", paste(capture.output(print(dropped, n = Inf)), collapse = "\n"),
         call. = FALSE)
  }

  attr(results, "dropped") <- dropped
  results
}

## Attach denominators --------
# n / N for each level of each categorical exposure, by site, joined onto the
# model output. Reference levels have no model term, so they arrive with NA
# estimates and are dropped at the plotting stage.
attach_denominators <- function(results, data, exposures) {
  denom <- data |>
    select(site, all_of(exposures)) |>
    select(where(is.factor)) |>
    mutate(across(everything(), as.character)) |>
    pivot_longer(any_of(exposures), names_to = "variable", values_to = "level") |>
    filter(!is.na(level)) |>
    count(site, variable, level, name = "n") |>
    group_by(site, variable) |>
    mutate(N = sum(n),
           an_N = paste0(n, "/", N)) |>
    ungroup() |>
    mutate(site = as.character(site))

  full_join(results, denom, by = c("site", "variable", "level"))
}
