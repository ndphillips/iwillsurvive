#'
#' A simulated dataset containing the survival data of 100 patients
#'
#' @format A data frame with 1626 rows and 2 variables:
#' \describe{
#'   \item{patientid}{character. Unique patientid}
#'   \item{sex}{character. Patient sex}
#'   \item{age}{numeric. Patient's age at index}
#'   \item{condition}{character. The condition a patient was in.}
#'   \item{lotstartdate}{date. Date at which first line of therapy starts. used
#'   as an indexdate}
#'   \item{lastvisitdate}{date. Last known patient contactdate}
#'   \item{dateofdeath}{date. Date of death if known.}
#' }
"cohort_raw"
