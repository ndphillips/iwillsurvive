#' Create an iwillsurvive object
#'
#' @param data dataframe. A one-row-per-patient dataframe
#' @param followup_time character. The column representing followup time.
#' @param event_status character. The column representing event status -- must be logical.
#' @param patient_id character. ... uniquep patientid.
#' @param terms character. A vector of model terms.
#' @param type character. See ?survival::survfit.formula
#' @param fit survfit. An optional survfit object created from survival:survfit(). If specified, no additional model(s) will be fit.
#' @param event_title character. An optional name of
#' @param index_title character.
#' @param title character. An optional title of the analysis that is passed to print() and plot() methods.
#' @param followup_time_units character. Units used in followup time
#' @param verbose logical. If TRUE, return messages
#'
#' @importFrom magrittr '%>%'
#' @import scales
#' @import cli
#'
#' @return survfit. See ?survival::survfit.object to learn more
#' @export
#'
#' @examples
#'
#' cohort_survival
#'
#' # No Terms
#'
#' cohort_fit <- iwillsurvive(cohort_survival,
#'   followup_time = "followup_days"
#' )
#' cohort_fit
#'
#' # Now including a term
#'
#' cohort_fit <- iwillsurvive(cohort_survival,
#'   followup_time = "followup_days",
#'   terms = "condition"
#' )
#' cohort_fit
iwillsurvive <- function(data,
                         followup_time = "followup_months",
                         event_status = "event_status",
                         patient_id = "patientid",
                         terms = NULL,
                         type = "right",
                         fit = NULL,
                         event_title = "<event>",
                         index_title = "<index>",
                         title = NULL,
                         followup_time_units = NULL,
                         verbose = TRUE) {
  testthat::expect_true(followup_time %in% names(data))
  testthat::expect_true(event_status %in% names(data))
  testthat::expect_is(data[[event_status]], "logical")

  patient_n <- nrow(data)
  event_n <- sum(data[[event_status]])

  patientid_col <- names(data)[tolower(names(data)) == "patientid"]

  if (verbose) {
    cli::cli_rule(left = "iwillsurvive")

    my_message <- paste0(
      "- ", scales::comma(event_n), " of ", scales::comma(patient_n), " (",
      scales::percent(event_n / patient_n), ") patient(s) experienced the event."
    )

    message(my_message)
  }

  if (is.null(fit)) {
    if (is.null(terms)) {
      my_expr <- paste0(
        "survival::survfit(survival::Surv(",
        followup_time, ", ",
        event_status, ", type = '", type, "') ~ 1, data = data)"
      )
    } else {
      my_expr <- paste0(
        "survival::survfit(survival::Surv(",
        followup_time, ", ",
        event_status, ", type = '", type, "') ~ ",
        paste(terms, collapse = " + "),
        ", data = data)"
      )
    }

    fit <- eval(parse(text = my_expr))

    if (verbose) {
      message(paste0("- ", my_expr))

      if (is.null(terms)) {
        my_message <- paste0(
          "- Median Survival = ",
          round(summary(fit)$table[which(names(summary(fit)$table) == "median")], 2)
        )

        message(my_message)
      }
    }
  }

  # If followup_time_units isn't specified, try to guess it

  if (is.null(followup_time_units)) {
    followup_time_units <- c("days", "months", "years")[stringr::str_detect(
      followup_time,
      pattern = c("day", "month", "year")
    )]
  }

  # Extract fit_summary

  fit_summary_table <- summary(fit)$table

  if (is.null(dim(fit_summary_table))) {
    fit_summary_table <- purrr::map_df(fit_summary_table, .f = function(x) {
      x
    }) %>%
      dplyr::mutate(strata = "all") %>%
      dplyr::select(strata, dplyr::everything())
  } else {
    fit_summary_table <- tibble::as_tibble(fit_summary_table,
      rownames = "strata"
    )
  }

  out <- list(
    data = data,
    fit = fit,
    fit_summary = fit_summary_table,
    terms = terms,
    event_title = event_title,
    index_title = index_title,
    followup_time_col = followup_time,
    followup_time_units = followup_time_units,
    timeatrisk_col = followup_time,
    event_status_col = event_status,
    patientid_col = patientid_col,
    title = title
  )

  class(out) <- "iwillsurvive"

  out
}
