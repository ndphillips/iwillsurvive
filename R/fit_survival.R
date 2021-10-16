#' Create an iwillsurvive object
#'
#' @param cohort dataframe. A one-row-per-patient dataframe
#' @param followup_time character.
#' @param event_status character.
#' @param patient_id character.
#' @param terms character.
#' @param type character. See ?survival::survfit.formula
#' @param fit survfit. An optional survfit object created from
#' survival:survfit(). If specified, no additional model(s) will be fit.
#' @param event_title character.
#' @param index_title character.
#' @param title character.
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
#' cohort <- data.frame(
#'   patientid = 1:20,
#'   followup_months = c(
#'     6.1, 15.4, 22, 24.6, 25.6, 26.1, 28.7, 46.9, 54.5, 55, 62.2,
#'     65.5, 88.1, 108.5, 116, 119.1, 119.6, 169.1, 317.8, 381.7
#'   ),
#'   event_status = c(
#'     FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE,
#'     TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE
#'   ),
#'   group = c(
#'     "a", "b", "a", "b", "a", "b", "a", "b", "a", "b",
#'     "a", "b", "a", "b", "a", "b", "a", "b", "a", "b"
#'   )
#' )
#'
#' # Ignoring group term
#' cohort_fit <- iwillsurvive(cohort)
#'
#' # Now including some covariates
#' cohort_fit <- iwillsurvive(cohort,
#'   terms = "group"
#' )
iwillsurvive <- function(cohort,
                         followup_time = "followup_months",
                         event_status = "event_status",
                         patient_id = "patientid",
                         terms = NULL,
                         type = "right",
                         fit = NULL,
                         event_title = NULL,
                         index_title = NULL,
                         title = NULL,
                         followup_time_units = NULL,
                         verbose = TRUE) {
  testthat::expect_true(followup_time %in% names(cohort))
  testthat::expect_true(event_status %in% names(cohort))
  testthat::expect_is(cohort[[event_status]], "logical")

  patient_n <- nrow(cohort)
  event_n <- sum(cohort[[event_status]])

  patientid_col <- names(cohort)[tolower(names(cohort)) == "patientid"]

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
        event_status, ", type = '", type, "') ~ 1, data = cohort)"
      )
    } else {
      my_expr <- paste0(
        "survival::survfit(survival::Surv(",
        followup_time, ", ",
        event_status, ", type = '", type, "') ~ ",
        paste(terms, collapse = " + "),
        ", data = cohort)"
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

  out <- list(
    cohort = cohort,
    fit = fit,
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
