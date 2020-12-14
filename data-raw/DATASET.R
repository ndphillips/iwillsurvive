## code to prepare `DATASET` dataset goes here
set.seed(100)

patient_n <- 250

index_date <- sample(seq(as.Date("2015/01/01"),
  as.Date("2020/10/31"),
  by = "day"
),
size = patient_n,
replace = TRUE
)

# Patient condition
condition <- sample(c("placebo", "drug"), size = patient_n, replace = TRUE)

placebo_n <- sum(condition == "placebo")
drug_n <- sum(condition == "drug")

tte_placebo <- 1
tte_drug <- 2

# Date that event occurs (theoretically)
eventdate <- index_date
eventdate[condition == "placebo"] <- index_date[condition == "placebo"] + rexp(
  n = placebo_n,
  rate = 1 / tte_placebo
) * 365

eventdate[condition == "drug"] <- index_date[condition == "drug"] + rexp(
  n = drug_n,
  rate = 1 / tte_drug
) * 365

# Date that observation stops
data_cutoff_date <- as.Date("2021/01/01")

# Observed event dates
event_date_obs <- eventdate
event_date_obs[event_date_obs > data_cutoff_date] <- NA

event_n <- sum(!is.na(event_date_obs))
censor_n <- sum(is.na(event_date_obs))


lastcontactdate <- eventdate

lastcontactdate[eventdate > data_cutoff_date] <- data_cutoff_date + rnorm(sum(eventdate > data_cutoff_date),
  mean = -30,
  sd = 5
)

lastcontactdate[eventdate <= data_cutoff_date] <- eventdate[eventdate <= data_cutoff_date] + rnorm(sum(eventdate <= data_cutoff_date),
  mean = -30,
  sd = 5
)

cohort_raw <- tibble::tibble(
  patientid = sprintf("F%05d", 1:patient_n),
  sex = sample(c("m", "f"), size = patient_n, replace = TRUE),
  age = rnorm(patient_n, mean = 45, sd = 10),
  condition = condition,
  lotstartdate = index_date,
  lastvisitdate = lastcontactdate,
  dateofdeath = event_date_obs
)

usethis::use_data(cohort_raw, overwrite = TRUE)

cohort_survival <- cohort_raw %>%

  derive_followup_date(event_date = "dateofdeath",
                       censor_date = "lastvisitdate") %>%

  derive_followup_time(index_date = "lotstartdate") %>%

  derive_event_status(event_date = "dateofdeath") %>%

select(patientid, sex, age, condition, followup_days, event_status)


usethis::use_data(cohort_survival, overwrite = TRUE)

