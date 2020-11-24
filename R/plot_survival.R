#' Plot the results of a survival analysis
#'
#' @param object iwillsurvive. An iwillsurvivel object created from fit_survival
#' @param cohort dataframe. A one-row-per-patient cohort used in generating fit.
#' @param followup_time_units character. The units that time at risk are calculated in
#' @param ggtheme theme. A ggplot2 theme
#' @param palette character. Colors for the color palette
#' @param conf.int logical. See ?survminer::ggsurvplot
#' @param risk.table logical. See ?survminer::ggsurvplot
#' @param method character. Experimental...
#'
#' @import ggplot2
#' @import scales
#'
#' @return ggplot2
#' @export
#'
plot_survival <- function(object = NULL,
                          cohort = NULL,
                          followup_time_units = NULL,
                          ggtheme = ggplot2::theme_bw(),
                          palette = c("#4941D1", "#00B6DA"),
                          conf.int = TRUE,
                          risk.table = TRUE,
                          method = "survminer"
                          ) {
#
#
#   cohort <- ez_cohort %>%
#
#     derive_followup_date(event_date = "dateofdeath",
#                          censor_date = "lastvisitdate") %>%
#
#     derive_followup_time(index_date = "lotstartdate") %>%
#
#     derive_event_status(event_date = "dateofdeath")
#
#   cohort_iws <- fit_survival(cohort,
#                              followup_time = "followup_days",
#                              event_title = "Death",
#                              index_title = "LOT1 Start")
#
#
#
#   object <- cohort_iws

  patient_n <- sum(object$fit$n)

  index_title <- get_index_title(object)
  event_title <- get_event_title(object)


  if (method == "survminer") {

    p <- suppressWarnings({

      survminer::ggsurvplot(
        fit = object$fit,
        data = cohort,
        surv.median.line = "hv",
        conf.int = conf.int,
        risk.table = risk.table,
        tables.height = 0.2,
        tables.theme = survminer::theme_cleantable(),
        palette = palette,
        ggtheme = ggtheme
      )

  })}


  if (method == "iwillsurvive") {


  plot_df <- tibble::tibble(lb = object$fit$lower,
                            ub = object$fit$upper,
                            fit = object$fit$surv,
                            time = object$fit$time,
                            status = object$fit$n.event
                            ) %>%
    arrange(time)

  if (!is.null(object$terms)) {

    if (length(object$terms) > 1) {

      stop("Can't plot a survival object with more than 1 term :(")

    } else {

      plot_df <- plot_df %>%
        dplyr::mutate(strata = object$cohort[[object$terms]])

      terms_n <- length(unique(object$cohort[[object$terms]]))

    }

  } else {

    plot_df <- plot_df %>%
      dplyr::mutate(strata = "all")

      terms_n <- 1
  }


  plot_df <- plot_df %>%
    dplyr::arrange(strata, dplyr::desc(lb))


  p <- ggplot2::ggplot(plot_df,
                       ggplot2::aes(x = time, group = strata, col = strata)) +
    scale_y_continuous(labels = scales::label_percent()) +
    scale_x_continuous(labels = scales::label_comma())


  # Loop over conditions

  for (strata_i in unique(plot_df$strata)) {

    data <- plot_df %>%
      filter(strata == strata_i)

    # Add LB

    p <- p +
      ggplot2::geom_path(data = data,
                         ggplot2::aes(y = lb))

    # Add UB

    p <- p +
      ggplot2::geom_line(data = data,
                         ggplot2::aes(y = ub),
                         col = scales::alpha(palette[1], alpha = .2))

    # Add band

    band_df <- data.frame(x = c(data$time, rev(data$time)),
                          y = c(data$lb, rev(data$ub)),
                          strata = strata_i)

    p <- p +
      ggplot2::geom_polygon(data = band_df,
                            ggplot2::aes(x = x, y = y),
                            fill = scales::alpha(palette[1], alpha = .1))


    # Add km estimate ------------------------------------------

    p <- p +
      ggplot2::geom_line(data = data,
                         ggplot2::aes(y = fit),
                         col = scales::alpha(palette[1], alpha = 1))

    # Add points -----------------------------------------------

    # Add censoring

    p <- p +
      ggplot2::geom_point(data = data %>% filter(status == 0),
                          ggplot2::aes(y = fit),
                          col = scales::alpha(palette[1], alpha = 1),
                          pch = 21,
                          fill = scales::alpha("white", .8))

    # Add events

    p <- p +
      ggplot2::geom_point(data = data %>% filter(status == 1),
                          ggplot2::aes(y = fit),
                          col = scales::alpha(palette[1], alpha = 1),
                          pch = 3,
                          fill = scales::alpha("white", .8))




  }



  # Add event tics

  # p <- p +
  #   ggplot2::geom_point(data = plot_df %>% filter(status == 1),
  #                       ggplot2::aes(y = -.02),
  #                       col = scales::alpha(palette[1], alpha = 1),
  #                       pch = 3,
  #                       fill = scales::alpha("white", .8))
  #


  }

  my_title <- paste0("Survival: From ", index_title)

  if (!is.null(event_title)) {
    my_title <- paste0("Survival: From ", index_title, " to ", event_title)
  }

  p <- p +
    ggplot2::labs(
      title = my_title,
      subtitle = paste0("Cohort N = ", scales::comma(patient_n))
    )

  if (!is.null(followup_time_units)) {
    p <- p + ggplot2::labs(x = paste0("Time (in ", followup_time_units, ")"))
  }


  p

}
