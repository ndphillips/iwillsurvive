#' Plot an iwillsurvive x.
#'
#' @param x iwillsurvive. An iwillsurvive object created from \code{iwillsurvive()}
#' @param ggtheme theme. A ggplot2 theme
#' @param palette character. The name of a paleete. See ?ggplot2::scale_colour_brewer for examples
#' @param simple logical. If TRUE, only plot the Kaplan-Meier estimate
#' @param title character. Plot title
#' @param subtitle character. Plot subtitle
#' @param censor_pch character. The point shape for censor ticks. See \code{?points}.
#' @param censor_size numeric. Size of censor ticks.
#' @param add_gridlines logical. If TRUE, include gridlines
#' @param add_confidence logical. If TRUE, include a confidence interval
#' @param add_censor_ticks logical. If TRUE, show censoring tick marks
#' @param add_labels logical. If TRUE, show verbal labels
#' @param add_median logical. If TRUE, show median survival
#' @param add_median_delta logical.
#' @param anchor_arrow logical. If TRUE, use an arrow in pointing to the anchor
#' @param legend_position character. Where should the strata labels be located?
#'   Either 'inside' for inside the plot, or 'top', or 'right'
#' @param legend_anchor_y numeric. Y locations of anchors for legends.
#'   Only used if legend_position = "inside"
#' @param legend_nudge_y numeric.
#' @param legend_position_x numeric. X position of the legend(s)
#' @param xlim numeric.
#' @param x_breaks numeric. Major breaks for the x-axis
#' @param label_size numeric. Size of the labels.
#' @param label_color character. Color of labels.
#' @param median_flag_nudge_y numeric. Amount to nudge median label.
#' @param median_flag_thickness numeric. Thickness of the flag border
#' @param risk_table logical. If TRUE, include the risk table
#' @param risk_table_title character. Title for the risk table
#' @param risk_size numeric. Size of font in risk table.
#' @param risk_label_size numeric. Size of labels in risk table
#' @param index_title character.
#' @param event_title character.
#' @param median_flag_size numeric.
#' @param event_nudge_y numeric.
#' @param panel_heights numeric. Heights of the KM and Risk panels
#' @param ... currently ignored
#'
#' @import ggplot2
#' @import scales
#'
#' @return ggplot2
#' @export
#'
#' @examples
#' # Set things up by creating an iwillsurvive x
#'
#' cohort <- cohort_raw %>%
#'   derive_followup_date(
#'     event_date = "dateofdeath",
#'     censor_date = "lastvisitdate"
#'   ) %>%
#'   derive_followup_time(index_date = "lotstartdate") %>%
#'   derive_event_status(event_date = "dateofdeath")
#'
#' cohort_iws <- iwillsurvive(cohort,
#'   followup_time = "followup_days",
#'   terms = "condition",
#'   event_title = "Death",
#'   index_title = "LOT1 Start"
#' )
#'
#' # Default plot
#' plot(cohort_iws)
#'
#' # Simpler version
#' plot(cohort_iws,
#'   simple = TRUE,
#'   risk_table = FALSE,
#'   add_confidence = FALSE
#' )
#'
#' # Customized version
#' plot(cohort_iws,
#'   add_confidence = FALSE,
#'   add_median_delta = FALSE,
#'   censor_pch = 3,
#'   censor_size = 5,
#'   legend_position_x = c(600, 400),
#'   legend_nudge_y = c(.25, .3),
#'   median_flag_nudge_y = .15,
#'   anchor_arrow = TRUE,
#'   palette = "Dark2",
#'   title = "My Title",
#'   subtitle = "My Subttitle",
#'   risk_table_title = "My Risk Table Title"
#' )
plot.iwillsurvive <- function(x = NULL,
                              ggtheme = ggplot2::theme_bw(),
                              palette = "Set1",
                              simple = FALSE,
                              title = NULL,
                              subtitle = NULL,
                              censor_pch = "|",
                              censor_size = 3,
                              add_gridlines = TRUE,
                              add_confidence = TRUE,
                              add_censor_ticks = TRUE,
                              add_labels = TRUE,
                              add_median = TRUE,
                              add_median_delta = TRUE,
                              anchor_arrow = FALSE,
                              legend_position = "inside",
                              legend_anchor_y = .5,
                              legend_nudge_y = NULL,
                              legend_position_x = NULL,
                              xlim = NULL,
                              x_breaks = NULL,
                              label_size = 3,
                              label_color = gray(0),
                              median_flag_nudge_y = .1,
                              median_flag_thickness = .7,
                              risk_table = TRUE,
                              risk_table_title = NULL,
                              risk_size = 3.5,
                              risk_label_size = 1.25,
                              index_title = NULL,
                              event_title = NULL,
                              median_flag_size = 4,
                              event_nudge_y = .15,
                              panel_heights = c(3, 1),
                              ...) {
  testthat::expect_is(x, "iwillsurvive")

  plot_df <- broom::tidy(x$fit)
  cohort <- x$cohort

  patient_n <- nrow(cohort)

  if (is.null(event_title)) {
    event_title <- x$event_title
  }

  if (is.null(index_title)) {
    index_title <- x$index_title
  }

  if (simple) {
    add_labels <- FALSE
    add_median <- FALSE
    add_median_delta <- FALSE
    legend_position <- "top"
  }


  if ("strata" %in% names(plot_df) == FALSE) {
    plot_df$strata <- "all"
  }

  # Create km plot {p_km} ------------------------------------------------------
  {
    plot_df <- plot_df %>%
      dplyr::mutate(strata = stringr::str_remove_all(strata,
        pattern = "condition="
      ))

    strata_values <- unique(plot_df$strata)
    strata_n <- length(strata_values)

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
      ggplot2::scale_y_continuous(labels = scales::label_percent())

    p_km_bld <- ggplot2::ggplot_build(p_km)

    time_major_breaks <- p_km_bld$layout$panel_params[[1]]$x.sec$breaks
    time_minor_breaks <- p_km_bld$layout$panel_params[[1]]$x.sec$minor_breaks
    time_lims <- p_km_bld$layout$panel_params[[1]]$x.range

    # Loop over conditions

    for (strata_i in unique(plot_df$strata)) {
      data <- plot_df %>%
        filter(strata == strata_i) %>%
        filter(is.finite(estimate), is.finite(conf.low), is.finite(conf.high))


      if (add_confidence) {

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
      }

      p_km <- p_km +
        ggplot2::geom_line(
          data = data,
          ggplot2::aes(y = estimate)
        )

      if (add_censor_ticks) {
        p_km <- p_km +
          ggplot2::geom_point(
            data = data %>% filter(n.censor > 0),
            ggplot2::aes(y = estimate),
            alpha = 1,
            pch = censor_pch,
            size = censor_size,
            fill = scales::alpha("white", .8)
          )
      }
    }

    if (add_median) {
      surv_median <- x$fit_summary %>%
        dplyr::select(strata, median) %>%
        dplyr::mutate(strata = stringr::str_remove_all(strata, pattern = "condition=")) %>%
        dplyr::mutate(y = .5) %>%
        dplyr::mutate(value = round(median, 0))

      # Add the median flag

      p_km <- p_km +
        ggrepel::geom_label_repel(
          data = surv_median,
          mapping = ggplot2::aes(
            x = value,
            y = y,
            label = value
          ),
          direction = "y",
          label.size = median_flag_thickness,
          min.segment.length = 0,
          nudge_y = median_flag_nudge_y,
          size = median_flag_size,
          segment.colour = "black"
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

      if (add_labels) {
        p_km <- p_km +
          ggplot2::annotate("text",
            x = 0, # min(time_lims),
            y = .5,
            col = label_color,
            adj = 0,
            label = "Median\nSurvival",
            size = label_size
          )
      }

      p_km <- p_km +
        ggplot2::geom_point(
          data = surv_median,
          mapping = ggplot2::aes(
            x = value,
            y = .5
          ),
          pch = 21,
          fill = "white",
          size = 4, stroke = 1
        )

      if (add_median_delta & strata_n > 1) {
        horizontal_bar_y <- .08

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
            y = horizontal_bar_y,
            yend = horizontal_bar_y,
            col = gray(.8)
          )

        # Add dotted connectors

        p_km <- p_km +
          ggplot2::geom_segment(
            data = median_delta,
            ggplot2::aes(
              x = value,
              y = -Inf,
              xend = value,
              yend = .5,
            ), lty = 3
          )

        # Add ends
        median_delta_ends <- median_delta %>%
          dplyr::mutate(
            y = horizontal_bar_y - .03,
            yend = horizontal_bar_y + .03
          )

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

        median_diff <- diff(range(x$fit_summary$median))

        delta_text <- paste0(round(median_diff, 0))
        # delta_text <- paste0(round(median_diff, 1), x$followup_time_units)

        p_km <- p_km +
          ggplot2::annotate("text",
            x = min(median_delta$value) + median_diff / 2,
            y = .125,
            label = delta_text
          )

        if (add_labels) {
          suppressWarnings({
            lab <- expression(paste(Delta, " Median"))

            p_km <- p_km + ggplot2::annotate("text",
              x = min(median_delta$value) + median_diff / 2,
              y = .04,
              label = lab,
              size = label_size,
              col = label_color
            )
          })
        }
      }
    }


    # Set title

    if (!is.null(title)) {
      my_title <- title
    } else {
      my_title <- paste0("Survival: From ", index_title)

      if (!is.null(event_title)) {
        my_title <- paste0("Survival: From ", index_title, " to ", event_title)
      }
    }

    if (!is.null(subtitle)) {
      my_subtitle <- subtitle
    } else {
      my_subtitle <- paste0("N = ", scales::comma(patient_n))
    }

    if (!is.null(x$followup_time_units)) {
      x_lab <- paste0("Time (", stringr::str_to_title(x$followup_time_units), ")")
    } else {
      x_lab <- "Time"
    }

    p_km <- p_km +
      ggplot2::labs(
        title = my_title,
        subtitle = my_subtitle,
        y = "Survival Probability",
        x = x_lab
      )

    # Add legend

    if (legend_position %in% c("top", "right")) {
      p_km <- p_km + ggtheme +
        ggplot2::theme(legend.position = "top")
    }

    if (legend_position == "inside") {
      if (is.null(legend_position_x)) {

        # Put first
        legend_position_x <- rev(c(
          max(plot_df$time) * .05,
          rep(max(plot_df$time) * .4, strata_n - 1)
        ))
      }

      p_km <- p_km +
        ggtheme +
        ggplot2::theme(
          legend.position = "none", # This keeps the labels from disappearing
          plot.margin = ggplot2::unit(c(1, 1, 1, 3), "lines")
        )

      # Get the x positions corresponding to  legend_anchor_y

      if (is.null(legend_nudge_y)) {
        legend_nudge_y <- rev(c(
          -.15,
          rep(.1, strata_n - 1)
        ))
      }

      temp <- tibble::tibble(
        strata = strata_values,
        legend_anchor_y = rep(legend_anchor_y,
          length.out = length(strata_values)
        )
      )

      legend_positions <- plot_df %>%
        dplyr::left_join(temp, by = "strata") %>%
        dplyr::group_by(strata) %>%
        dplyr::mutate(dev = abs(estimate - legend_anchor_y)) %>%
        dplyr::filter(dev == min(dev)) %>%
        dplyr::slice(1) %>%
        dplyr::mutate(
          x = time,
          y = legend_anchor_y
        ) %>%
        dplyr::select(strata, x, y) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(nudge_x = 0)

      if (!is.null(legend_position_x)) {
        temp <- tibble::tibble(
          strata = strata_values,
          legend_position_x = rep(legend_position_x,
            length.out = length(strata_values)
          )
        )

        legend_positions <- legend_positions %>%
          dplyr::left_join(temp, by = "strata") %>%
          dplyr::mutate(
            nudge_x = legend_position_x - x
          )
      }


      my_arrow <- if (anchor_arrow) {
        arrow(length = unit(0.02, "npc"))
      } else {
        NULL
      }

      if (strata_n > 1) {
        p_km <- p_km +
          ggrepel::geom_label_repel(
            data = legend_positions,
            mapping = ggplot2::aes(
              x = x, y = y,
              group = strata,
              label = strata
            ),
            nudge_x = legend_positions$nudge_x,
            nudge_y = legend_nudge_y,
            direction = "y",
            segment.size = .5,
            arrow = my_arrow,
            segment.color = "black",
            label.size = 0
          )
      }
    }
  }

  # Add risk table ---------------------------------------------------

  if (risk_table) {
    risk_df <- tidyr::expand_grid(
      strata = strata_values,
      time = time_minor_breaks
    )

    risk_df <- purrr::map_dfr(1:nrow(risk_df),
      .f = function(row_i) {
        strata_i <- risk_df$strata[row_i]
        time_i <- risk_df$time[row_i]

        at_risk_i <- suppressWarnings({
          plot_df %>%
            dplyr::filter(time < time_i, strata == strata_i) %>%
            dplyr::filter(time == max(time)) %>%
            dplyr::pull(n.risk)
        })

        censored_i <- plot_df %>%
          dplyr::filter(time < time_i, strata == strata_i) %>%
          dplyr::summarise(N = sum(n.censor), .groups = "drop") %>%
          dplyr::pull(N)

        tibble::tibble(
          strata = strata_i,
          time = time_i,
          risk_n = at_risk_i,
          censored_n = censored_i
        )
      }
    )

    risk_df <- risk_df %>%
      dplyr::bind_rows(
        plot_df %>%
          dplyr::group_by(strata) %>%
          dplyr::summarise(
            risk_n = max(n.risk),
            censored_n = 0, .groups = "drop"
          ) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(time = 0)
      ) %>%
      dplyr::arrange(strata, time) %>%
      dplyr::mutate(lab = paste0(risk_n, " (", censored_n, ")")) %>%
      dplyr::mutate(strata_y = as.numeric(factor(strata)) * -.1 - .2)


    p_risk <- ggplot2::ggplot(
      data = risk_df,
      mapping = ggplot2::aes(
        x = time,
        y = strata,
        label = lab
      )
    ) +
      ggplot2::geom_text(
        data = risk_df,
        ggplot2::aes(
          x = time,
          adj = 0,
          label = lab
        ),
        size = risk_size,
        col = "black"
      ) +
      # ggplot2::coord_cartesian(
      #   # xlim = c(0, max(time_minor_breaks)),
      #   clip = "off"
      # ) + # This keeps the labels from disappearing
      ggplot2::labs(y = "", x = "") +
      ggtheme +
      ggplot2::theme(
        plot.margin = ggplot2::unit(c(0, 1, 1, 2.5), "lines"),
        legend.position = "none"
      )

    # Add stata labels

    strata_labs <- risk_df %>%
      dplyr::group_by(strata) %>%
      dplyr::summarise(y = first(strata_y), .groups = "drop") %>%
      dplyr::mutate(x = min(time_lims))

    # p_risk <- p_risk +
    #   ggplot2::geom_label(
    #     data = strata_labs,
    #     mapping = ggplot2::aes(
    #       x = x, y = y,
    #       label = strata
    #     ),
    #     adj = 1, size = risk_size
    #  )
  }

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
  #       labels = strata_values,
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


  if (is.null(xlim)) {
    my_limits <- time_lims
  } else {
    my_limits <- xlim
  }

  if (is.null(x_breaks)) {
    my_breaks <- time_major_breaks
  } else {
    my_breaks <- x_breaks
  }


  p_km <- p_km +
    ggplot2::theme(plot.margin = ggplot2::unit(c(1, 1, .5, 2), "lines"))


  if (add_gridlines == FALSE) {
    p_km <- p_km +
      ggplot2::theme(panel.grid = ggplot2::element_blank())
  }

  p_km <- p_km + ggplot2::scale_x_continuous(
    breaks = my_breaks,
    limits = my_limits,
    expand = c(0, 0),
    labels = scales::comma
  ) +
    ggplot2::scale_colour_brewer(palette = palette)

  g_km <- ggplot2::ggplotGrob(p_km)



  if (risk_table) {
    if (is.null(risk_table_title)) {
      risk_table_title <- "At Risk (Censored)"
    }

    p_risk <- p_risk +
      ggplot2::theme(plot.margin = ggplot2::unit(c(0, 1, 0, 2), "lines"))

    p_risk <- p_risk +
      ggplot2::theme(panel.grid = ggplot2::element_blank())

    p_risk <- p_risk + ggplot2::scale_x_continuous(
      breaks = my_breaks,
      limits = my_limits,
      expand = c(0, 0),
      labels = scales::comma
    ) +
      suppressWarnings({
        ggplot2::theme(
          axis.text.y = ggplot2::element_text(
            color = RColorBrewer::brewer.pal(max(c(strata_n, 3)), palette),
            size = ggplot2::rel(risk_label_size)
          ),
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor.x = ggplot2::element_blank(),
        )
      }) +
      ggplot2::labs(subtitle = risk_table_title)

    g_risk <- ggplot2::ggplotGrob(p_risk)
  }


  if (risk_table) {
    maxWidth <- grid::unit.pmax(
      g_km$widths[2:5],
      g_risk$widths[2:5]
    )

    g_risk$widths[2:5] <- as.list(maxWidth)
  } else {
    maxWidth <- grid::unit.pmax(g_km$widths[2:5])
  }

  g_km$widths[2:5] <- as.list(maxWidth)


  if (risk_table) {
    gridExtra::grid.arrange(g_km, g_risk,
      ncol = 1,
      heights = panel_heights
    )
  } else {
    gridExtra::grid.arrange(g_km,
      ncol = 1
    )
  }
}
