#' Print method for an iwillsurvive object
#'
#' @param x iwillsurvive. An iwillsurvive object created from \code{iwillsurvive}
#' @param ... currently not used.
#'
#' @export
#'
print.iwillsurvive <- function(x, ...) {
  testthat::expect_is(x, "iwillsurvive")

  cohort_n <- nrow(x$cohort)
  event_n <- sum(x$cohort[[x$event_status_col]])
  censor_n <- sum(x$cohort[[x$event_status_col]] == FALSE)

  strata_n <- nrow(x$fit_summary)


  cat(paste0(
    "An iwillsurvive object estimating survival from ",
    x$index_title, " to ", x$event_title
  ), "\n\n")

  # Cohort size ------------
  cat(paste0(
    scales::comma(cohort_n), " Patients:\n",
    x$event_title, " = ", scales::comma(event_n),
    " (", scales::percent(event_n / cohort_n, accuracy = 1),
    "), ",
    "Censored = ",
    scales::comma(censor_n),
    " (", scales::percent(censor_n / cohort_n, accuracy = 1),
    ")\n"
  ), sep = "")


  # add median survival

  cat("\nMedian Survival (", x$followup_time_units, "):\n", sep = "")

  for (row_i in 1:strata_n) {
    cat(
      stringr::str_remove_all(x$fit_summary$strata[row_i],
        pattern = "condition="
      ), " = ",
      round(x$fit_summary$median[row_i], 1),
      sep = ""
    )

    if (row_i < nrow(x$fit_summary)) {
      cat(", ")
    }
  }

  if (strata_n > 1) {
    cat("\ndiff =", crayon::bold(crayon::green(round(diff(range(x$fit_summary$median)), 0))))
  }

  cat("\n\n")

  # ascii survival curve --------------------------------------------

  print_ascii_survival(x$fit)

}
