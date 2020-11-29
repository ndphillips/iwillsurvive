#' Calculate event_status, a logical column indicating whether or not an event occured.
#'
#' @param cohort dataframe. A one-row-per-patient dataframe.
#' @param event_date character. The name of a column in cohort indicating event dates.
#' @param event_status_rename character. (Optional) Name of the resulting event_status column
#'
#' @return dataframe.
#' @export
#'
derive_event_status <- function(cohort = NULL,
                                event_date = NULL,
                                event_status_rename = "event_status") {

  testthat::expect_true(!is.null(cohort))
  testthat::expect_true(!is.null(event_date))
  testthat::expect_true(event_date %in% names(cohort))

  event_date_sym <- rlang::sym(event_date)
  event_status_sym <- rlang::sym(event_status_rename)


  cohort <- cohort %>%
    dplyr::mutate(!!event_status_sym := !is.na(!!event_date_sym))

  cohort
}
