#' Plot a patient timeline containing
#'
#' @param cohort dataframe. A ORPP dataframe containing columns representing time at
#' risk and event status
#' @param follow_up_time character. Name of a column in cohort
#' @param event_status character. Name of a column in cohort
#' @param follow_up_time_units character. The units that time at risk are calculated in
#' @param event_name character. Name of the event such as "death", "next treatment"
#' @param index_name character. Name of the index such as "LOT1 Start" or "Metastatic Diagnosis"
#' @param add_median logical. If TRUE, add a line showing median.
#' @param point_size numeric.
#' @param line_size numeric.
#' @param theme A ggplot2 theme
#'
#' @return ggplot2
#' @export
#'
#' @examples
#'
#' cohort <- data.frame(
#'   patientid = 1:20,
#'   follow_up_time = c(
#'     6.1, 15.4, 22, 24.6, 25.6, 26.1, 28.7, 46.9, 54.5, 55, 62.2,
#'     65.5, 88.1, 108.5, 116, 119.1, 119.6, 169.1, 317.8, 381.7
#'   ),
#'   event_status = c(
#'     FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE,
#'     TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE
#'   )
#' )
#'
#' plot_follow_up_time(cohort,
#'   follow_up_time_units = "days",
#'   event_name = "Death",
#'   index_name = "LOT1 Start"
#' )
plot_follow_up_time <- function(cohort = NULL,
                          follow_up_time = "follow_up_time",
                          event_status = "event_status",
                          follow_up_time_units = NULL,
                          event_name = NULL,
                          index_name = "index",
                          add_median = TRUE,
                          point_size = 1,
                          line_size = .5,
                          theme = ggplot2::theme_minimal()) {
  follow_up_time_col <- follow_up_time
  event_status_col <- event_status
  patientid_col <- names(cohort)[tolower(names(cohort)) == "patientid"]

  if (length(patientid_col) == 0) {
    stop("I couldn't find a column like 'patientid'")
  }

  cohort <- cohort %>%
    dplyr::mutate(patientid = !!rlang::sym(patientid_col))

  testthat::expect_true("data.frame" %in% class(cohort))
  testthat::expect_true("patientid" %in% tolower(names(cohort)))

  patient_n <- nrow(cohort)

  cohort <- cohort %>%
    dplyr::mutate(
      follow_up_time = !!rlang::sym(follow_up_time_col),
      event_status = !!rlang::sym(event_status_col)
    ) %>%
    dplyr::arrange(follow_up_time) %>%
    dplyr::mutate(patientid = as.numeric(factor(patientid,
      ordered = TRUE,
      levels = patientid
    )))


  if (!is.null(follow_up_time_units)) {
    x_lab <- paste0("Time at Risk (", stringr::str_to_title(follow_up_time_units), ")")
  } else {
    x_lab <- "Time at Risk"
  }

  # Create title


  my_title <- paste0("Time at risk: From ", index_name)

  if (!is.null(event_name)) {
    my_title <- paste0("Time at risk: From ", index_name, " to ", event_name)
  }

  p <- ggplot2::ggplot(cohort, ggplot2::aes(
    x = follow_up_time,
    y = patientid,
    shape = event_status,
    col = event_status
  )) +
    ggplot2::geom_segment(ggplot2::aes(
      x = 0,
      y = patientid,
      xend = follow_up_time,
      yend = patientid
    ),
    size = line_size
    ) +
    ggplot2::geom_point(size = point_size) +
    ggplot2::scale_shape_manual(values = c(3, 16)) +
    ggplot2::scale_color_manual(values = c("#1ECBB7", "#4941D1")) +
    ggplot2::labs(
      y = "Patients",
      title = my_title,
      subtitle = paste0("Cohort N = ", scales::comma(patient_n)),
      x = x_lab
    ) +
    ggplot2::theme(
      legend.position = "top",
      panel.grid.minor.y = ggplot2::element_blank()
    )


  if (!is.null(event_name)) {
    p <- p +
      ggplot2::labs(color = event_name, shape = event_name)
  }


  if (add_median) {
    median_follow_up_time <- median(cohort$follow_up_time)

    p <- p +
      ggplot2::geom_vline(xintercept = median_follow_up_time, lty = 3) +
      ggplot2::annotate("label",
        x = median_follow_up_time,
        y = .1 * patient_n,
        label = round(median_follow_up_time, 1), size = 3
      ) +
      ggplot2::annotate("text",
        x = median_follow_up_time + .05 * max(cohort$follow_up_time),
        y = .1 * patient_n, adj = 0,
        label = "Median Time at Risk", size = 3
      )
  }


  p <- p + theme + ggplot2::theme(legend.position = "top")

  p
}
