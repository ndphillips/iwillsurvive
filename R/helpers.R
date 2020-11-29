


#' Get the index name from an iwillsurvive object
#'
#' @param object iwillsurvive.
#'
#' @return character.
#'
get_index_title <- function(object) {
  testthat::expect_is(object, "iwillsurvive")

  object$index_title
}

#' Get the event name from an iwillsurvive object
#'
#' @param object iwillsurvive.
#'
#' @return character.
#'
get_event_title <- function(object) {
  testthat::expect_is(object, "iwillsurvive")

  object$event_title
}

#' Get the event name from an iwillsurvive object
#'
#' @param object iwillsurvive.
#'
#' @return character.
#'
get_event_status_col <- function(object) {
  testthat::expect_is(object, "iwillsurvive")

  object$event_status_col
}

#' Get the event name from an iwillsurvive object
#'
#' @param object iwillsurvive.
#'
#' @return character.
#'
get_patientid_col <- function(object) {
  testthat::expect_is(object, "iwillsurvive")

  object$patientid_col
}


#' Get the event name from an iwillsurvive object
#'
#' @param object iwillsurvive.
#'
#' @return character.
#'
get_followup_time_col <- function(object) {
  testthat::expect_is(object, "iwillsurvive")

  object$followup_time_col
}
