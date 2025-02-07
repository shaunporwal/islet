# Modified version of trial data generation with ~5% difference

set.seed(8977) # Changed seed slightly
n <- 200
trial <-
  tibble::tibble(
    trt = sample(c("Drug A", "Drug B"), n, replace = TRUE),
    age = rnorm(n, mean = 52.5, sd = 15.75) |> as.integer(), # Increased mean and sd by ~5%
    marker = rgamma(n, 1.05, 0.95) |> round(digits = 3), # Slightly adjusted gamma parameters
    stage = sample(c("T1", "T2", "T3", "T4"), size = n, prob = c(0.26, 0.24, 0.25, 0.25), replace = TRUE) |> factor(), # Added slight prob skew
    grade = sample(c("I", "II", "III"), size = n, prob = c(0.34, 0.33, 0.33), replace = TRUE) |> factor(), # Added slight prob skew
    response_prob =
      1 / (1 + exp(-((trt == "Drug") - 0.21 * as.numeric(stage) - 0.105 * as.numeric(grade) + 0.105 * marker))), # Slightly adjusted coefficients
    response = runif(n) < response_prob,
    ttdeath_true =
      exp(1.05 + 0.21 * response + # Slightly adjusted coefficients
        -0.105 * as.numeric(stage) +
        -0.105 * as.numeric(grade) +
        rnorm(n, sd = 0.525)) * 12,
    death = ifelse(ttdeath_true <= 24, 1L, 0L),
    ttdeath = pmin(ttdeath_true, 24) |> round(digits = 2),

    # Modified date ranges slightly
    visit_date = sample(seq.Date(as.Date("2020-01-15"), as.Date("2022-01-15"), by = "days"), n, replace = TRUE),
    follow_up_date = sample(seq.Date(as.Date("2022-01-15"), as.Date("2024-01-15"), by = "days"), n, replace = TRUE),

    # Adjusted probabilities slightly for logical variables
    has_side_effects = sample(c(TRUE, FALSE), n, prob = c(0.48, 0.52), replace = TRUE),
    enrolled_in_study = sample(c(TRUE, FALSE), n, prob = c(0.51, 0.49), replace = TRUE),

    # Adjusted means and SDs slightly
    bmi = round(rnorm(n, mean = 26.25, sd = 4.2), 1), # Increased mean and sd by ~5%
    systolic_bp = round(rnorm(n, mean = 126, sd = 15.75)), # Increased mean and sd by ~5%

    patient_id = paste0("ID-", sprintf("%03d", seq(1, n))), # Same format, different seed
    hospital = sample(c("Hospital A", "Hospital B", "Hospital C"), n, prob = c(0.34, 0.33, 0.33), replace = TRUE),

    # Adjusted probabilities slightly for factor variables
    insurance = sample(c("Private", "Medicaid", "Medicare"), n, prob = c(0.34, 0.33, 0.33), replace = TRUE) |> factor(),
    smoking_status = sample(c("Non-smoker", "Former smoker", "Current smoker"), n, prob = c(0.34, 0.33, 0.33), replace = TRUE) |> factor()
  ) |>
  dplyr::mutate(
    age = ifelse(runif(n) < 0.945, age, NA_real_), # Slightly adjusted NA probability
    marker = ifelse(runif(n) < 0.945, marker, NA_real_),
    response = ifelse(runif(n) < 0.945, response, NA_integer_)
  ) |>
  dplyr::select(-dplyr::one_of("response_prob", "ttdeath_true"))

# Adding labels to new columns (same as original)
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

trial |> readr::write_csv(file = here::here("data/df_trial2_5p_difference.csv"))
