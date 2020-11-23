#' Plot the results of a survival analysis
#'
#' @param object iwillsurvive. An iwillsurvivel object created from fit_survival
#' @param cohort dataframe. A one-row-per-patient cohort used in generating fit.
#' @param followup_time_units character. The units that time at risk are calculated in
#' @param ggtheme theme. A ggplot2 theme
#' @param palette character. Colors for the color palette
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
                          palette = c("#4941D1", "#00B6DA")) {
  patient_n <- sum(object$fit$n)

  index_title <- get_index_title(object)
  event_title <- get_event_title(object)

  p <- survminer::ggsurvplot(
    fit = object$fit,
    data = cohort,
    surv.median.line = "hv",
    conf.int = TRUE,
    risk.table = TRUE,
    tables.height = 0.2,
    tables.theme = survminer::theme_cleantable(),
    palette = palette,
    ggtheme = ggtheme
  )


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
