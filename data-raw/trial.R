# Taken from:
# https://github.com/ddsjoberg/gtsummary/blob/main/data-raw/trial.R

# Made some modifications

# Results from a cohort study of Drug A vs B

set.seed(8976)
n <- 200
trial <-
  tibble::tibble(
    trt = sample(c("Drug A", "Drug B"), n, replace = TRUE),
    age = rnorm(n, mean = 50, sd = 15) |> as.integer(),
    marker = rgamma(n, 1, 1) |> round(digits = 3),
    stage = sample(c("T1", "T2", "T3", "T4"), size = n, replace = TRUE) |> factor(),
    grade = sample(c("I", "II", "III"), size = n, replace = TRUE) |> factor(),
    response_prob =
      1 / (1 + exp(-((trt == "Drug") - 0.2 * as.numeric(stage) - 0.1 * as.numeric(grade) + 0.1 * marker))),
    response = runif(n) < response_prob,
    ttdeath_true =
      exp(1 + 0.2 * response +
        -0.1 * as.numeric(stage) +
        -0.1 * as.numeric(grade) +
        rnorm(n, sd = 0.5)) * 12,
    death = ifelse(ttdeath_true <= 24, 1L, 0L),
    ttdeath = pmin(ttdeath_true, 24) |> round(digits = 2),

    # New columns added below:
    visit_date = sample(seq.Date(as.Date("2020-01-01"), as.Date("2022-01-01"), by = "days"), n, replace = TRUE), # Date
    follow_up_date = sample(seq.Date(as.Date("2022-01-02"), as.Date("2024-01-01"), by = "days"), n, replace = TRUE), # Date

    has_side_effects = sample(c(TRUE, FALSE), n, replace = TRUE), # Logical
    enrolled_in_study = sample(c(TRUE, FALSE), n, replace = TRUE), # Logical

    bmi = round(rnorm(n, mean = 25, sd = 4), 1), # Numeric
    systolic_bp = round(rnorm(n, mean = 120, sd = 15)), # Numeric

    patient_id = paste0("ID-", sprintf("%03d", seq(1, n))), # Character
    hospital = sample(c("Hospital A", "Hospital B", "Hospital C"), n, replace = TRUE), # Character

    insurance = sample(c("Private", "Medicaid", "Medicare"), n, replace = TRUE) |> factor(), # Factor
    smoking_status = sample(c("Non-smoker", "Former smoker", "Current smoker"), n, replace = TRUE) |> factor() # Factor
  ) |>
  dplyr::mutate(
    age = ifelse(runif(n) < 0.95, age, NA_real_),
    marker = ifelse(runif(n) < 0.95, marker, NA_real_),
    response = ifelse(runif(n) < 0.95, response, NA_integer_)
  ) |>
  dplyr::select(-dplyr::one_of("response_prob", "ttdeath_true"))

summary(trial)

# Adding labels to new columns
attr(trial$visit_date, "label") <- "Date of Visit"
attr(trial$follow_up_date, "label") <- "Date of Follow-Up"
attr(trial$has_side_effects, "label") <- "Has Side Effects"
attr(trial$enrolled_in_study, "label") <- "Enrolled in Study"
attr(trial$bmi, "label") <- "Body Mass Index (BMI)"
attr(trial$systolic_bp, "label") <- "Systolic Blood Pressure"
attr(trial$patient_id, "label") <- "Patient ID"
attr(trial$hospital, "label") <- "Hospital"
attr(trial$insurance, "label") <- "Insurance Type"
attr(trial$smoking_status, "label") <- "Smoking Status"

trial |> readr::write_csv(file = here::here("data/df_trial.csv"))
