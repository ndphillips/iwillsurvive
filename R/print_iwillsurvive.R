#' Print method for an iwillsurvive object
#'
#' @param x iwillsurvive. An iwillsurvive object created from \code{iwillsurvive}
#' @param ... currently not used.
#'
#' @export
#'
print.iwillsurvive <- function(x, ...) {
  testthat::expect_is(x, "iwillsurvive")

  cohort_n <- nrow(x$cohort)
  event_n <- sum(x$cohort[[x$event_status_col]])
  censor_n <- sum(x$cohort[[x$event_status_col]] == FALSE)

  strata_n <- nrow(x$fit_summary)


  cat(paste0(
    "An iwillsurvive object estimating survival from ",
    x$index_title, " to ", x$event_title
  ), "\n\n")

  # Cohort size ------------
  cat(paste0(
    scales::comma(cohort_n), " Patients:\n",
    x$event_title, " = ", scales::comma(event_n),
    " (", scales::percent(event_n / cohort_n, accuracy = 1),
    "), ",
    "Censored = ",
    scales::comma(censor_n),
    " (", scales::percent(censor_n / cohort_n, accuracy = 1),
    ")\n"
  ), sep = "")


  # add median survival

  cat("\nMedian Survival (", x$followup_time_units, "):\n", sep = "")

  for (row_i in 1:strata_n) {
    cat(stringr::str_remove_all(x$fit_summary$strata[row_i],
      pattern = "condition="
    ), " = ",
    round(x$fit_summary$median[row_i], 1),
    sep = ""
    )

    if (row_i < nrow(x$fit_summary)) {
      cat(", ")
    }
  }

  if (strata_n > 1) {
    cat("\ndiff =", crayon::bold(crayon::green(round(diff(range(x$fit_summary$median)), 0))))
  }

  cat("\n\n")

  # ascii survival curve --------------------------------------------

  ascii_df <- x$fit %>%
    broom::tidy()

  if ("strata" %in% names(ascii_df) == FALSE) {
    ascii_df <- ascii_df %>%
      dplyr::mutate(strata = "all")
  }

  time_max <- max(ascii_df$time)

  time_cuts_n <- 45
  height_max <- 16


  ascii_df <- ascii_df %>%
    dplyr::mutate(strata = stringr::str_remove_all(strata,
      pattern = "condition="
    )) %>%
    dplyr::filter(time > 0) %>%
    dplyr::select(time, estimate, strata) %>%
    dplyr::mutate(
      time_cut = cut(time, breaks = time_cuts_n),
      estimate_cut = cut(estimate, breaks = seq(-.001, 1, length.out = height_max))
    ) %>%
    dplyr::group_by(strata, estimate_cut) %>%
    dplyr::summarise(time_cut = first(time_cut), .groups = "drop") %>%
    dplyr::mutate(
      time_cut = as.numeric(time_cut),
      estimate_cut = as.numeric(estimate_cut)
    ) %>%
    dplyr::group_by(time_cut, estimate_cut) %>%
    dplyr::summarise(strata = paste(strata, collapse = "XXX"), .groups = "drop") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(strata = dplyr::case_when(
      stringr::str_detect(strata, "XXX") ~ "*",
      TRUE ~ substr(strata, 1, 1)
    ))


  for (estimate_cut_i in sort(unique(ascii_df$estimate_cut), decreasing = TRUE)) {
    at_median <- estimate_cut_i == median(unique(ascii_df$estimate_cut))

    # What are the time_cut_values?

    temp <- ascii_df %>%
      dplyr::filter(estimate_cut == estimate_cut_i) %>%
      dplyr::arrange(time_cut)

    time_cut_loc <- temp$time_cut
    char <- temp$strata

    spaces <- rep(NA, length(time_cut_loc))

    spaces[1] <- time_cut_loc[1] - 1

    if (length(spaces) > 1) {
      new <- time_cut_loc - lag(time_cut_loc, 1) - 1

      spaces[2:length(spaces)] <- new[2:length(new)]
    }

    cat("|")

    # Starting spaces

    cat(crayon::silver(rep(".", length = spaces[1])), sep = "")
    cat(char[1])

    # Middle

    if (length(spaces) > 1) {
      for (j in 2:length(spaces)) {
        if (at_median) {
          cat(crayon::bold(crayon::green(rep("=", length = spaces[j]),
            sep = ""
          ),
          sep = ""
          ), sep = "")
          cat(char[j])
        } else {
          cat(crayon::silver(rep(".", length = spaces[j])), sep = "")
          cat(char[j])
        }
      }
    }

    # Ending spaces

    if (at_median) {
      median_range <- round(max(x$fit_summary$median) - min(x$fit_summary$median), 0)

      cat(crayon::silver(rep(".", time_cuts_n - sum(spaces) - length(spaces)),
        sep = ""
      ), sep = "")
      #
      # cat(crayon::silver("....."))
      # cat(median_range)
      # cat(" ", x$followup_time_units, "", sep = "")
      # cat(crayon::silver(rep(".", time_cuts_n - sum(spaces) - length(spaces) - stringr::str_length(x$followup_time_units) - 5 - floor(log(median_range, base = 10)) - 1),
      #     sep = ""), sep = "")
    } else {
      cat(crayon::silver(rep(".", length = time_cuts_n - sum(spaces) - length(spaces))), sep = "")
    }

    cat("\n")
  }
}
