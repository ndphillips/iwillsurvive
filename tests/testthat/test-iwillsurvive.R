# Create a basic cohort

data <- cohort_raw %>%
  derive_followup_date(
    event_date = "dateofdeath",
    censor_date = "lastvisitdate"
  ) %>%
  derive_followup_time(index_date = "lotstartdate") %>%
  derive_event_status(event_date = "dateofdeath")

test_that("iwillsurvive works with standard inputs", {
  object <- iwillsurvive(data,
    followup_time = "followup_days",
    event_status = "event_status"
  )

  testthat::expect_is(
    object,
    "iwillsurvive"
  )
})

test_that("iwillsurvive works with existing survfit object", {
  fit <- survival::survfit(
    survival::Surv(followup_days, event_status,
      type = "right"
    ) ~ 1,
    data = data
  )

  object <- iwillsurvive(data,
    fit = fit,
    followup_time = "followup_days"
  )

  testthat::expect_is(
    object,
    "iwillsurvive"
  )
})

test_that("iwillsurvive gives the same results as survfit", {
  # No terms -----------------------------------------------

  fit <- survival::survfit(
    survival::Surv(followup_days, event_status,
      type = "right"
    ) ~ 1,
    data = data
  )

  object_iws <- iwillsurvive(data,
    followup_time = "followup_days",
    event_status = "event_status"
  )

  testthat::expect_identical(fit, object_iws$fit)

  # 1 Term -----------------------------------------------

  fit <- survival::survfit(
    survival::Surv(followup_days, event_status,
      type = "right"
    ) ~ condition,
    data = data
  )

  object_iws <- iwillsurvive(data,
    terms = "condition",
    followup_time = "followup_days",
    event_status = "event_status"
  )

  testthat::expect_identical(fit, object_iws$fit)
})
