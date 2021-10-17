#' Calculate follow up date from event_date and censor_date columns
#'
#' @param data dataframe. A one-row-per-patient dataframe.
#' @param event_date character. The name of a column in data indicating event dates.
#' @param censor_date character. The name of a date column in data indicating censor dates.
#'
#' @importFrom rlang .data
#'
#' @return dataframe.
#' @export
#'
#'
derive_followup_date <- function(data = NULL,
                                 event_date = NULL,
                                 censor_date = "censor_date") {
  testthat::expect_true(!is.null(data))
  testthat::expect_true(!is.null(event_date))
  testthat::expect_true(censor_date %in% names(data))

  event_date_sym <- rlang::sym(event_date)
  censor_date_sym <- rlang::sym(censor_date)

  data <- data %>%
    dplyr::mutate(followup_date = dplyr::coalesce(
      !!event_date_sym,
      !!censor_date_sym
    ))

  data
}

#' Calculate followup_time, the number of days from index to follow up in days
#'
#' @param data dataframe. A one-row-per-patient dataframe.
#' @param index_date character. The name of a column in data indicating index dates.
#' @param followup_date character. The name of a column in data indicating follow up dates.
#' @param followup_time_rename character. (Optional) Name of the resulting follow up days column.
#'
#' @return dataframe.
#' @export
#'
derive_followup_time <- function(data,
                                 index_date = "index_date",
                                 followup_date = "followup_date",
                                 followup_time_rename = "followup_days") {
  testthat::expect_true(!is.null(data))
  testthat::expect_true(index_date %in% names(data))
  testthat::expect_true(followup_date %in% names(data))

  index_date_sym <- rlang::sym(index_date)
  followup_date_sym <- rlang::sym(followup_date)

  followup_time_rename_sym <- rlang::sym(followup_time_rename)

  data <- data %>%
    dplyr::mutate(!!followup_time_rename := as.numeric(!!followup_date_sym - !!index_date_sym))

  data
}


#' Calculate event_status, a logical column indicating whether or not an event occured.
#'
#' @param data dataframe. A one-row-per-patient dataframe.
#' @param event_date character. The name of a column in data indicating event dates.
#' @param event_status_rename character. (Optional) Name of the resulting event_status column
#'
#' @return dataframe.
#' @export
#'
derive_event_status <- function(data = NULL,
                                event_date = NULL,
                                event_status_rename = "event_status") {
  testthat::expect_true(!is.null(data))
  testthat::expect_true(!is.null(event_date))
  testthat::expect_true(event_date %in% names(data))

  event_date_sym <- rlang::sym(event_date)
  event_status_sym <- rlang::sym(event_status_rename)

  data <- data %>%
    dplyr::mutate(!!event_status_sym := !is.na(!!event_date_sym))

  data
}
