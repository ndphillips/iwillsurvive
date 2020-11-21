test_that("fit survival works with different types of inputs", {


  cohort <- ez_cohort %>%

    derive_follow_up_date(event_date = "dateofdeath",
                          censor_date = "censordate") %>%

    derive_follow_up_time(index_date = "lotstartdate") %>%

    derive_event_status(event_date = "dateofdeath")

  fit <- fit_survival(cohort,
    follow_up_time = "follow_up_days",
    event_status = "event_status"
  )

  testthat::expect_is(fit, "survfit")



})
