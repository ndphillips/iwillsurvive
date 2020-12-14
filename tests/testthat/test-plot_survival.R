# Create a basic cohort

cohort <- cohort_raw %>%
  derive_followup_date(
    event_date = "dateofdeath",
    censor_date = "lastvisitdate"
  ) %>%
  derive_followup_time(index_date = "lotstartdate") %>%
  derive_event_status(event_date = "dateofdeath")


test_that("multiplication works", {
  object <- iwillsurvive(cohort,
    followup_time = "followup_days",
    event_status = "event_status"
  )


  p <- plot(object)

  testthat::expect_true("grob" %in% class(p))
})
