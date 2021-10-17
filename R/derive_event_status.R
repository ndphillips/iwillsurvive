#' Calculate event_status, a logical column indicating whether or not an event occured.
#'
#' @param data dataframe. A one-row-per-patient dataframe.
#' @param event_date character. The name of a column in cohort indicating event dates.
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
