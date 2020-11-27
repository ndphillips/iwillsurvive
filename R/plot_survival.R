#' Plot the results of a survival analysis
#'
#' @param object iwillsurvive. An iwillsurvive object created from fit_survival
#' @param cohort dataframe. A one-row-per-patient cohort used in generating fit.
#' @param followup_time_units character. The units that time at risk are calculated in
#' @param ggtheme theme. A ggplot2 theme
#' @param palette character. Colors for the color palette
#' @param conf_int logical. See ?survminer::ggsurvplot
#' @param add_median logical. If TRUE, show median survival
#' @param add_median_delta logical.
#' @param risk.table logical. See ?survminer::ggsurvplot
#' @param index_title character.
#' @param event_title character.
#' @param event_nudge_y numeric.
#' @param method character. Experimental...
#'
#' @import ggplot2
#' @import scales
#'
#' @return ggplot2
#' @export
#'
plot_survival <- function(object = NULL,
                          followup_time_units = NULL,
                          ggtheme = ggplot2::theme_bw(),
                          palette = c("#4941D1", "#00B6DA"),
                          conf_int = TRUE,
                          add_median = TRUE,
                          add_median_delta = TRUE,
                          risk.table = TRUE,
                          index_title = NULL,
                          event_title = NULL,
                          method = "iwillsurvive",
                          event_nudge_y = .15) {

  plot_df <- broom::tidy(object$fit)
  cohort <- object$cohort

  patient_n <- nrow(cohort)

  if (is.null(event_title)) {
    event_title <- object$event_title
  }


  if (is.null(index_title)) {
    index_title <- object$index_title
  }


  fit_summary <- summary(object$fit)$table

  # Create km plot {p_km} ------------------------------------------------------
  {
    if (method == "survminer") {
      p_km <- suppressWarnings({
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
      })
    }

    if (method == "iwillsurvive") {
      plot_df <- plot_df %>%
        dplyr::mutate(strata = stringr::str_remove_all(strata, pattern = "condition="))

      stata_values <- unique(plot_df$strata)
      strata_n <- length(stata_values)

      if ("strata" %in% names(plot_df) == FALSE) {
        plot_df <- plot_df %>%
          dplyr::mutate(strata = "all")
      }

      plot_df <- plot_df %>%
        dplyr::arrange(strata, time)

      p_km <- ggplot2::ggplot(
        plot_df,
        ggplot2::aes(
          x = time,
          group = strata,
          y = estimate,
          col = strata
        )
      ) +
        ggplot2::scale_y_continuous(labels = scales::label_percent()) +
        ggplot2::scale_x_continuous(labels = scales::label_comma())


      p_km_bld <- ggplot2::ggplot_build(p_km)

      time_minor_breaks <- p_km_bld$layout$panel_params[[1]]$x.sec$minor_breaks
      time_lims <- p_km_bld$layout$panel_params[[1]]$x.range


      # Loop over conditions

      for (strata_i in unique(plot_df$strata)) {
        data <- plot_df %>%
          filter(strata == strata_i) %>%
          filter(is.finite(estimate), is.finite(conf.low), is.finite(conf.high))

        # Add conf.low

        p_km <- p_km +
          ggplot2::geom_path(
            data = data,
            ggplot2::aes(y = conf.low),
            alpha = .2
          )

        # Add conf.high

        p_km <- p_km +
          ggplot2::geom_line(
            data = data,
            ggplot2::aes(y = conf.high),
            alpha = .2
          )


        p_km <- p_km +
          ggplot2::geom_ribbon(
            data = data,
            ggplot2::aes(
              x = time,
              ymin = conf.low,
              ymax = conf.high,
              fill = strata
            ),
            alpha = .2, lwd = 0
          )

        p_km <- p_km +
          ggplot2::geom_line(
            data = data,
            ggplot2::aes(y = estimate)
          )

        p_km <- p_km +
          ggplot2::geom_point(
            data = data %>% filter(n.censor > 0),
            ggplot2::aes(y = estimate),
            alpha = 1,
            pch = "|", size = 3,
            fill = scales::alpha("white", .8)
          )
      }

      if (add_median) {

        surv_median <- fit_summary[, stringr::str_detect(colnames(fit_summary), "median")] %>%
          tibble::as_tibble(rownames = "strata") %>%
          dplyr::mutate(strata = stringr::str_remove_all(strata, pattern = "condition=")) %>%
          dplyr::mutate(y = .5) %>%
          dplyr::mutate(value = round(value, 0))

        p_km <- p_km +
          ggrepel::geom_label_repel(
            data = surv_median, mapping = ggplot2::aes(
              x = value,
              y = y,
              label = value
            ),
            direction = "y",
            min.segment.length = 0,
            nudge_y = .1, segment.colour = "black"
          )

        p_km <- p_km +
          ggplot2::annotate("segment",
            x = -Inf,
            xend = max(surv_median$value),
            y = .5,
            yend = .5,
            col = gray(.2),
            lty = 3
          )

        p_km <- p_km +
          ggplot2::annotate("text",
            x = min(time_lims),
            y = .55,
            col = "black",
            adj = 0,
            label = "Median"
          )

        if (add_median_delta) {

          median_delta <- surv_median %>%
            dplyr::mutate(
              value_max = max(value),
              value_min = min(value)
            ) %>%
            dplyr::filter(value == value_max | value == value_min)

          p_km <- p_km +
            ggplot2::annotate("segment",
              x = min(surv_median$value),
              xend = max(surv_median$value),
              y = .05,
              yend = .05
            )

          # Add dotted connectors

          p_km <- p_km +
            ggplot2::geom_segment(
              data = median_delta,
              ggplot2::aes(
                x = value,
                y = -Inf,
                xend = value,
                yend = .5
              ), lty = 3
            )


          # Add ends
          median_delta_ends <- median_delta %>%
            dplyr::mutate(y = .02, yend = .08)

          p_km <- p_km +
            ggplot2::geom_segment(
              data = median_delta_ends,
              ggplot2::aes(
                x = value,
                y = y,
                xend = value,
                yend = yend
              )
            )

          median_diff <- max(median_delta$value) - min(median_delta$value)

          delta_text <- paste0(round(median_diff, 1), followup_time_units)

          p_km <- p_km +
            ggplot2::annotate("text",
              x = min(median_delta$value) + median_diff / 2,
              y = .125,
              label = delta_text
            )
        }
      }
    }

    my_title <- paste0("Survival: From ", index_title)

    if (!is.null(event_title)) {
      my_title <- paste0("Survival: From ", index_title, " to ", event_title)
    }

    p_km <- p_km +
      ggplot2::labs(
        title = my_title,
        subtitle = paste0("Cohort N = ", scales::comma(patient_n)),
        y = "Survival Probability",
        x = "Time"
      )

    if (!is.null(followup_time_units)) {
      p_km <- p_km + ggplot2::labs(x = paste0("Time (in ", followup_time_units, ")"))
    }

    # Add theeme

    p_km <- p_km + ggtheme +
      ggplot2::theme(legend.position = "top")
  }

  # Add risk table ---------------------------------------------------

  risk_table <- tidyr::expand_grid(
    strata = stata_values,
    time = time_minor_breaks
  )


  risk_table <- purrr::map_dfr(1:nrow(risk_table),
    .f = function(row_i) {
      strata_i <- risk_table$strata[row_i]
      time_i <- risk_table$time[row_i]

      at_risk_i <- suppressWarnings({plot_df %>%
        dplyr::filter(time < time_i, strata == strata_i) %>%
        dplyr::filter(time == max(time)) %>%
        dplyr::pull(n.risk)})

      censored_i <- plot_df %>%
        dplyr::filter(time < time_i, strata == strata_i) %>%
        dplyr::summarise(N = sum(n.censor)) %>%
        dplyr::pull(N)

      tibble::tibble(
        strata = strata_i,
        time = time_i,
        risk_n = at_risk_i,
        censored_n = censored_i
      )
    }
  )

  risk_table <- risk_table %>%
    dplyr::bind_rows(
      plot_df %>%
        dplyr::group_by(strata) %>%
        dplyr::summarise(
          risk_n = max(n.risk),
          censored_n = 0
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(time = 0)
    ) %>%
    dplyr::arrange(strata, time) %>%
    dplyr::mutate(lab = paste0(risk_n, " (", censored_n, ")")) %>%
    dplyr::mutate(strata_y = as.numeric(factor(strata)) * -.1 - .2)

  p_km <- p_km +
    ggplot2::geom_text(
      data = risk_table,
      ggplot2::aes(
        x = time,
        y = strata_y,
        adj = 0,
        label = lab
      ),
      size = 3,
      col = "black"
    ) +
    ggplot2::coord_cartesian(
      # xlim = c(0, max(time_minor_breaks)),
      ylim = c(0, 1),
      clip = "off"
    ) + # This keeps the labels from disappearing
    ggplot2::theme(plot.margin = ggplot2::unit(c(1, 1, 4, 1), "lines"))

  # Add stata labels

  strata_labs <- risk_table %>%
    dplyr::group_by(strata) %>%
    dplyr::summarise(y = first(strata_y)) %>%
    dplyr::mutate(x = min(time_lims))

  p_km <- p_km +
    ggplot2::geom_label(
      data = strata_labs,
      mapping = ggplot2::aes(
        x = x, y = y,
        label = strata
      ),
      adj = 1, size = 3
    )

  # Create time at risk plot {p_tar} -----------------------------------------
#
#   event_df <- plot_df %>%
#     dplyr::select(time, strata, event = n.event, censor = n.censor) %>%
#     tidyr::pivot_longer(
#       cols = event:censor,
#       names_to = "outcome",
#       values_to = "count"
#     ) %>%
#     dplyr::filter(count > 0)
#
#   # Duplicate any with counts > 1 so there is one row per event
#
#   while (max(event_df$count) > 1) {
#     count_max <- max(event_df$count)
#
#     event_df_max <- event_df %>%
#       filter(count == count_max)
#
#     new_df <- purrr::map_df(1:count_max, .f = function(x) {
#       event_df_max %>%
#         dplyr::mutate(count = 1)
#     }) %>%
#       dplyr::arrange(time)
#
#     event_df <- event_df %>%
#       dplyr::filter(count != count_max) %>%
#       dplyr::bind_rows(new_df)
#   }
#
#   event_df <- event_df %>%
#     dplyr::mutate(nudge_y = dplyr::case_when(
#       outcome == "event" ~ event_nudge_y,
#       outcome == "censor" ~ -event_nudge_y
#     )) %>%
#     dplyr::mutate(strata_num = as.numeric(factor(strata)))
#
#
#   ylim <- c(.5, strata_n + .5)
#
#   p_tar <- ggplot2::ggplot(
#     event_df,
#     ggplot2::aes(
#       x = time,
#       y = strata_num,
#       shape = outcome,
#       col = strata
#     )
#   ) +
#     ggplot2::scale_shape_manual(values = c(3, 21))
#
#   # Add boxplots
#
#   p_tar <- p_tar +
#     ggplot2::geom_boxplot(
#       data = event_df,
#       mapping = ggplot2::aes(
#         x = time,
#         y = strata_num,
#         shape = NULL, group = strata
#       ),
#       outlier.shape = NA,
#       fill = "white", col = gray(.1, .5)
#     )
#
#   p_tar <- p_tar +
#     ggplot2::geom_point(
#       position = ggplot2::position_nudge(y = event_df$nudge_y),
#       fill = "white"
#     )
#
#   p_tar <- p_tar +
#     ggplot2::labs(title = "Time At Risk", y = "") +
#     ggplot2::scale_y_continuous(
#       labels = stata_values,
#       limits = ylim,
#       breaks = 1:strata_n
#     ) +
#     ggplot2::scale_x_continuous(
#       labels = scales::label_comma(),
#       limits = time_lims
#     )
#
#   p_tar <- p_tar + ggtheme
#
#
#   p_tar <- p_tar +
#     ggplot2::theme(
#       # axis.title.y = ggplot2::element_blank(),
#       # axis.text.y = ggplot2::element_text(color="white")
#       # axis.ticks = ggplot2::element_blank()
#     ) +
#     ggplot2::guides(shape = FALSE, col = FALSE) +
#     ggplot2::theme(
#       plot.margin = ggplot2::unit(c(1, 1, 1, 1), "lines"),
#       panel.grid.minor.y = ggplot2::element_blank()
#     )
#
#   # Create final plot -----------------------
#
#   ggpubr::ggarrange(p_km, p_tar,
#     ncol = 1,
#     nrow = 2,
#     heights = c(3, 1.25)
#   )

  p_km
}
