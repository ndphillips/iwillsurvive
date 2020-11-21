test_that("fit survival works with different types of inputs", {


  cohort <- ez_cohort %>%

    derive_follow_up_date(event_date = "DateOfDeath",
                          censor_date = "CensorDate") %>%

    derive_follow_up_time(index_date = "LOT1StartDate") %>%

    derive_event_status(event_date = "DateOfDeath")

  fit <- fit_survival(cohort,
    follow_up_time = "follow_up_days",
    event_status = "event_status"
  )

  testthat::expect_is(fit, "survfit")



})
