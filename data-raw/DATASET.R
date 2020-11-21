## code to prepare `DATASET` dataset goes here
set.seed(100)

patient_n <- 100

index_date <- sample(seq(as.Date("2020/01/01"), as.Date("2020/10/31"), by = "day"),
  size = patient_n,
  replace = TRUE
)

censor_date <- index_date + rexp(n = patient_n, rate = .01)
event_date <- index_date + rexp(n = patient_n, rate = .01)

event_date[event_date > censor_date] <- NA
censor_date[!is.na(event_date)] <- NA
last_contact_date <- dplyr::coalesce(event_date, censor_date)

ez_cohort <- tibble::tibble(
  patientid = sprintf("F%05d", 1:patient_n),
  group = sample(c("placebo", "drug"), size = patient_n, replace = TRUE),
  lotstartdate = index_date,
  censordate = censor_date,
  dateofdeath = event_date
)

usethis::use_data(ez_cohort, overwrite = TRUE)
