test_that("fit survival works with different types of inputs", {
  cohort <- ez_cohort %>%
    derive_followup_date(
      event_date = "dateofdeath",
      censor_date = "lastvisitdate"
    ) %>%
    derive_followup_time(index_date = "lotstartdate") %>%
    derive_event_status(event_date = "dateofdeath")

  object <- iwillsurvive(cohort,
    followup_time = "followup_days",
    event_status = "event_status"
  )

  testthat::expect_is(object, "iwillsurvive")
})
