#' Plot the results of a survival analysis
#'
#' @param fit survfit. A survfit object created from survival::survfit()
#' @param cohort dataframe. A one-row-per-patient cohort used in generating fit.
#' @param event_name character. Name of the event such as "death", "next treatment"
#' @param index_name character. Name of the index such as "LOT1 Start" or "Metastatic Diagnosis"
#' @param follow_up_time_units character. The units that time at risk are calculated in
#' @param ggtheme theme. A ggplot2 theme
#' @param palette character. Colors for the color palette
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
#' cohort_fit <- fit_survival(cohort)
#'
#' plot_survival(
#'   fit = cohort_fit,
#'   cohort = cohort,
#'   event_name = "Death",
#'   index_name = "Metastatic Diagnosis"
#' )
plot_survival <- function(fit = NULL,
                          cohort = NULL,
                          event_name = NULL,
                          index_name = "index",
                          follow_up_time_units = NULL,
                          ggtheme = ggplot2::theme_bw(),
                          palette = c("#4941D1", "#00B6DA")) {
  patient_n <- sum(fit$n)

  p <- survminer::ggsurvplot(
    fit = fit,
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

  if (!is.null(follow_up_time_units)) {
    p <- p + ggplot2::labs(x = paste0("Time (in ", follow_up_time_units, ")"))
  }

  p
}
