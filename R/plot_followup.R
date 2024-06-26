#' Plot a patient timeline containing
#'
#' @param object iwillsurvive. An iwillsurvive object
#' @param add_median logical. If TRUE, add a line showing median.
#' @param point_size numeric. Width of points
#' @param line_size numeric. Width of lines
#' @param theme A ggplot2 theme
#'
#' @import ggplot2
#' @import scales
#' @import stringr
#' @importFrom stats median
#'
#' @return ggplot2
#' @export
#'
#' @examples
#'
#' cohort_iws <- iwillsurvive(cohort_survival,
#'   followup_time = "followup_days",
#'   index_title = "LOT1 Start Date",
#'   event_title = "Death",
#'   terms = "condition"
#' )
#'
#' plot_followup(cohort_iws)
#'
plot_followup <- function(object = NULL,
                          add_median = TRUE,
                          point_size = 1,
                          line_size = .5,
                          theme = ggplot2::theme_minimal()) {
  followup_time_sym <- rlang::sym(get_followup_time_col(object))
  event_status_sym <- rlang::sym(get_event_status_col(object))
  patientid_sym <- rlang::sym(get_patientid_col(object))

  patient_n <- nrow(object$data)

  data <- object$data %>%
    dplyr::mutate(
      t_followup_time = !!rlang::sym(followup_time_sym),
      t_event_status = !!rlang::sym(event_status_sym)
    ) %>%
    dplyr::arrange(t_followup_time) %>%
    dplyr::mutate(t_patientid = as.numeric(factor(!!patientid_sym,
      ordered = TRUE,
      levels = !!patientid_sym
    )))

  if (!is.null(object$followup_time_units)) {
    x_lab <- paste0("Time at Risk (", stringr::str_to_title(object$followup_time_units), ")")
  } else {
    x_lab <- "Time at Risk"
  }

  # Create title

  index_title <- get_index_title(object)
  event_title <- get_event_title(object)

  my_title <- paste0("Time at risk: From ", index_title)

  if (!is.null(event_title)) {
    my_title <- paste0("Time at risk: From ", index_title, " to ", event_title)
  }

  p <- ggplot2::ggplot(data, ggplot2::aes(
    x = t_followup_time,
    y = t_patientid,
    shape = t_event_status,
    col = t_event_status
  )) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = 0,
        y = t_patientid,
        xend = t_followup_time,
        yend = t_patientid
      ),
      size = line_size
    ) +
    ggplot2::geom_point(size = point_size) +
    ggplot2::scale_shape_manual(values = c(3, 16)) +
    ggplot2::scale_color_manual(values = c(
      grDevices::gray(.7, alpha = .8),
      grDevices::gray(.2, alpha = .8)
    )) +
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

  if (!is.null(event_title)) {
    p <- p +
      ggplot2::labs(color = event_title, shape = event_title)
  }

  if (add_median) {
    median_followup_time <- stats::median(data$t_followup_time)

    p <- p +
      ggplot2::geom_vline(xintercept = median_followup_time, lty = 3) +
      ggplot2::annotate("label",
        x = median_followup_time,
        y = .1 * patient_n,
        label = round(median_followup_time, 1), size = 3
      ) +
      ggplot2::annotate("text",
        x = median_followup_time + .05 * max(data$t_followup_time),
        y = .1 * patient_n, adj = 0,
        label = "Median Time at Risk", size = 3
      )
  }

  p <- p + theme + ggplot2::theme(legend.position = "top")

  p
}
