#' Print method for an iwillsurvive object
#'
#' @param object
#'
#' @export
#'
print.iwillsurvive <- function(object) {
  testthat::expect_is(object, "iwillsurvive")

  cohort_n <- nrow(object$cohort)
  event_n <- sum(object$cohort[[object$event_status_col]])
  censor_n <- sum(object$cohort[[object$event_status_col]] == FALSE)

  # Cohort size ------------
  cat(paste0(
    scales::comma(cohort_n), " Patients: ",
    scales::comma(event_n),
    " (", scales::percent(event_n / cohort_n, accuracy = .1),
    ") ", object$event_title,
    ", ",
    scales::comma(censor_n),
    " (", scales::percent(censor_n / cohort_n, accuracy = .1),
    ") Censored\n\n"
  ))

  # ascii survival curve --------------------------------------------

  ascii_df <- object$fit %>%
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
          cat(crayon::bgYellow(crayon::green(rep("-", length = spaces[j]),
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
      median_range <- round(max(object$fit_summary$median) - min(object$fit_summary$median), 0)

      cat(crayon::silver(rep(".", time_cuts_n - sum(spaces) - length(spaces)),
        sep = ""
      ), sep = "")
      #
      # cat(crayon::silver("....."))
      # cat(median_range)
      # cat(" ", object$followup_time_units, "", sep = "")
      # cat(crayon::silver(rep(".", time_cuts_n - sum(spaces) - length(spaces) - stringr::str_length(object$followup_time_units) - 5 - floor(log(median_range, base = 10)) - 1),
      #     sep = ""), sep = "")
    } else {
      cat(crayon::silver(rep(".", length = time_cuts_n - sum(spaces) - length(spaces))), sep = "")
    }

    cat("\n")
  }
}
