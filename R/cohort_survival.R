#'
#' A simulated dataset containing the survival data of 250 patients
#'
#' @format A data frame with 1626 rows and 2 variables:
#' \describe{
#'   \item{patientid}{character. Unique patientid}
#'   \item{sex}{character. Patient sex}
#'   \item{age}{numeric. Patient's age at index}
#'   \item{condition}{character. The condition a patient was in, either placebo or drug}
#'   \item{followup_days}{numeric. Days from index to followup}
#'   \item{event_status}{logical. TRUE = Death, FALSE = Censored}
#' }
"cohort_survival"
