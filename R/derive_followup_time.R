#' Calculate followup_time, the number of days from index to follow up in days
#'
#' @param cohort dataframe. A one-row-per-patient dataframe.
#' @param index_date character. The name of a column in cohort indicating index dates.
#' @param followup_date character. The name of a column in cohort indicating follow up dates.
#' @param days_in_month numeric. Average number of days in a month. Defaults to
#' 30.44.
#' @param days_in_year numeric. Average number of days in a year. Defaults to
#' 365.24
#'
#' @return The cohort dataframe with new columns followup_days, followup_months,
#' followup_years.
#'
#' @export
#'
#' @examples
#'
#' cohort <- ez_cohort %>%
#'                derive_followup_date(event_date = "dateofdeath",
#'                                     censor_date = "lastvisitdate")
#'
#' # Derive follow up time from index to follow upin days, months and years.
#'
#' cohort %>%
#'   derive_followup_time(index_date = "lotstartdate",
#'                        followup_date = "followup_date")
#'
#'
#' @references
#'
#' https://pumas.nasa.gov/examples/index.php?id=46
#'
derive_followup_time <- function(cohort,
                                 index_date = "index_date",
                                 followup_date = "followup_date",
                                 days_in_month = 30.44,
                                 days_in_year = 365.24) {

  testthat::expect_true(!is.null(cohort))
  testthat::expect_true(index_date %in% names(cohort))
  testthat::expect_true(followup_date %in% names(cohort))

  index_date_sym <- rlang::sym(index_date)
  followup_date_sym <- rlang::sym(followup_date)

  cohort <- cohort %>%
    dplyr::mutate(followup_days := as.numeric(!!followup_date_sym - !!index_date_sym),
                  followup_months = followup_days / days_in_month,
                  followup_years = followup_days / days_in_year)

  cohort
}
