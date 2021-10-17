#' Calculate follow up date from event_date and censor_date columns
#'
#' @param data dataframe. A one-row-per-patient dataframe.
#' @param event_date character. The name of a column in cohort indicating event dates.
#' @param censor_date character. The name of a date column in cohort indicating censor dates.
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
