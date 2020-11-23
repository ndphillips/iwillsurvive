#' Calculate follow up date from event_date and censor_date columns
#'
#' @param cohort dataframe. A one-row-per-patient dataframe.
#' @param event_date character. The name of a column in cohort indicating event dates.
#' @param censor_date character. The name of a date column in cohort indicating censor dates.
#'
#' @importFrom rlang .data
#'
#' @return dataframe.
#' @export
#'
#'
derive_followup_date <- function(cohort = NULL,
                                 event_date = NULL,
                                 censor_date = "censor_date") {
  testthat::expect_true(!is.null(cohort))
  testthat::expect_true(!is.null(event_date))
  testthat::expect_true(censor_date %in% names(cohort))

  event_date_sym <- rlang::sym(event_date)
  censor_date_sym <- rlang::sym(censor_date)

  cohort <- cohort %>%
    dplyr::mutate(followup_date = dplyr::coalesce(
      !!event_date_sym,
      !!censor_date_sym
    ))

  cohort
}

#' Calculate followup_time, the number of days from index to follow up in days
#'
#' @param cohort dataframe. A one-row-per-patient dataframe.
#' @param index_date character. The name of a column in cohort indicating index dates.
#' @param followup_date character. The name of a column in cohort indicating follow up dates.
#' @param followup_time_rename character. (Optional) Name of the resulting follow up days column.
#'
#' @return dataframe.
#' @export
#'
derive_followup_time <- function(cohort,
                                 index_date = "index_date",
                                 followup_date = "followup_date",
                                 followup_time_rename = "followup_days") {
  testthat::expect_true(!is.null(cohort))
  testthat::expect_true(index_date %in% names(cohort))
  testthat::expect_true(followup_date %in% names(cohort))

  index_date_sym <- rlang::sym(index_date)
  followup_date_sym <- rlang::sym(followup_date)

  followup_time_rename_sym <- rlang::sym(followup_time_rename)

  cohort <- cohort %>%
    dplyr::mutate(!!followup_time_rename := as.numeric(!!followup_date_sym - !!index_date_sym))

  cohort
}


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
