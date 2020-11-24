#' Fit a survival model with a easy to use interface
#'
#' This is a wrapper around survival::survfit() with a cleaner interface.
#'
#' @param cohort dataframe. A one-row-per-patient dataframe
#' @param followup_time character.
#' @param event_status character.
#' @param patient_id character.
#' @param terms character.
#' @param type character. See ?survival::survfit.formula
#' @param event_title character.
#' @param index_title character.
#' @param title character.
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
#'   followup_time = c(
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
#'
#' cohort_fit <- fit_survival(cohort)
#' cohort_fit
#'
#' # Now including some covariates
#'
#' cohort_fit <- fit_survival(cohort, terms = "group")
#' cohort_fit
fit_survival <- function(cohort,
                         followup_time = "followup_time",
                         event_status = "event_status",
                         patient_id = "patientid",
                         terms = NULL,
                         type = "right",
                         event_title = NULL,
                         index_title = NULL,
                         title = NULL,
                         verbose = TRUE) {
  testthat::expect_true(followup_time %in% names(cohort))
  testthat::expect_true(event_status %in% names(cohort))
  testthat::expect_is(cohort[[event_status]], "logical")

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

  cohort_surv <- eval(parse(text = my_expr))

  if (verbose) {
    patient_n <- nrow(cohort)
    event_n <- sum(cohort[[event_status]])

    cli::cli_rule(left = "fit_survival")

    message(paste0("- ", my_expr))

    my_message <- paste0(
      "- ", scales::comma(event_n), " of ", scales::comma(patient_n), " (",
      scales::percent(event_n / patient_n), ") patient(s) experienced the event."
    )

    message(my_message)

    if (is.null(terms)) {
      my_message <- paste0(
        "- Median Survival = ",
        round(summary(cohort_surv)$table[which(names(summary(cohort_surv)$table) == "median")], 2)
      )

      message(my_message)
    }
  }

  patientid_col <- names(cohort)[tolower(names(cohort)) == "patientid"]

  out <- list(
    cohort = cohort,
    fit = cohort_surv,
    terms = terms,
    event_title = event_title,
    index_title = index_title,
    followup_time_col = followup_time,
    timeatrisk_col = followup_time,
    event_status_col = event_status,
    patientid_col = patientid_col,
    title = title
  )

  class(out) <- "iwillsurvive"

  out
}
