#' Calculate followup_time, the number of days from index to follow up in days
#'
#' @param data dataframe. A one-row-per-patient dataframe.
#' @param index_date character. The name of a column in cohort indicating index dates.
#' @param followup_date character. The name of a column in cohort indicating follow up dates.
#'
#' @return The cohort dataframe with new columns followup_days, followup_months,
#' followup_years.
#'
#' @export
#'
#' @examples
#'
#' data <- cohort_raw %>%
#'   derive_followup_date(
#'     event_date = "dateofdeath",
#'     censor_date = "lastvisitdate"
#'   )
#'
#' # Derive follow up time from index to follow up in days, months and years.
#'
#' data %>%
#'   derive_followup_time(
#'     index_date = "lotstartdate",
#'     followup_date = "followup_date"
#'   )
#' @references
#'
#' https://pumas.nasa.gov/examples/index.php?id=46
#'
derive_followup_time <- function(data,
                                 index_date = "index_date",
                                 followup_date = "followup_date") {
  testthat::expect_true(!is.null(data))
  testthat::expect_true(index_date %in% names(data))
  testthat::expect_true(followup_date %in% names(data))

  index_date_sym <- rlang::sym(index_date)
  followup_date_sym <- rlang::sym(followup_date)

  data <- data %>%
    dplyr::mutate(
      followup_days := as.numeric(difftime(!!followup_date_sym, !!index_date_sym,
        units = "days"
      )),
      followup_weeks = as.numeric(difftime(!!followup_date_sym, !!index_date_sym,
        units = "weeks"
      ))
    )

  data
}
