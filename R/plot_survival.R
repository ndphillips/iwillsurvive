#' Plot the results of a survival analysis
#'
#' @param fit survfit. A survfit object created from survival::survfit()
#' @param cohort dataframe. A one-row-per-patient cohort used in generating fit.
#' @param event_name character. Name of the event such as "death", "next treatment"
#' @param index_name character. Name of the index such as "LOT1 Start" or "Metastatic Diagnosis"
#' @param followup_time_units character. The units that time at risk are calculated in
#' @param ggtheme theme. A ggplot2 theme
#' @param palette character. Colors for the color palette
#'
#'
#' @import ggplot2
#' @import scales
#'
#' @return ggplot2
#' @export
#'
#' @examples
plot_survival <- function(object = NULL,
                          cohort = NULL,
                          followup_time_units = NULL,
                          ggtheme = ggplot2::theme_bw(),
                          palette = c("#4941D1", "#00B6DA")) {

   patient_n <- sum(object$fit$n)

   index_name <- get_index_name(object)
   event_name <- get_event_name(object)

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


  my_title <- paste0("Survival: From ", index_name)

  if (!is.null(event_name)) {
    my_title <- paste0("Survival: From ", index_name, " to ", event_name)
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
