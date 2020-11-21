#'
#' A simulated dataset containing the survival data of 100 patients
#'
#' @format A data frame with 1626 rows and 2 variables:
#' \describe{
#'   \item{patientid}{character. Unique patientid}
#'   \item{group}{character. A grouping variable}
#'   \item{lotstartdate}{date. Date at which first line of therapy starts. used
#'   as an indexdate}
#'   \item{censordate}{date. Date when patients are censored if the event
#'   (death) has not occured}
#'   \item{dateofdeath}{date. Date of death if known.}
#' }
"ez_cohort"
