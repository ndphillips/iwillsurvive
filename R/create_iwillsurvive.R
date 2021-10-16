#
#
# cohort <- ez_cohort %>%
#   derive_followup_date(
#     event_date = "dateofdeath",
#     censor_date = "lastvisitdate"
#   ) %>%
#   derive_followup_time(index_date = "lotstartdate") %>%
#   derive_event_status(event_date = "dateofdeath")
#
# fit <- fit_survival(cohort,
#                     followup_time = "followup_days",
#                     event_status = "event_status"
# )
#
#
#
#
# create_iwillsurvive(cohort, fit, event+name)


create_iwillsurvive <- function(fit = NULL,
                                cohort = NULL,
                                event_name = NULL,
                                index_name = NULL,
                                title = NULL) {
  testthat::expect_true(!is.null(cohort))
  testthat::expect_is(cohort, "data.frame")

  out <- list(
    cohort = cohort,
    fit = fit,
    event_name = event_name,
    index_name = index_name,
    title = title
  )

  class(out) <- "iwillsurvive"

  return(out)
}
