set.seed(8977) # Changed seed slightly
n <- 200

# Function to generate highly unstructured doctor's notes about patient comorbidities
generate_doctor_notes <- function() {
  comorbidities <- c(
    "diabetes mellitus", "hypertension", "hyperlipidemia", "COPD", "asthma",
    "coronary artery disease", "chronic kidney disease", "heart failure",
    "stroke", "obesity", "anemia", "depression", "arthritis",
    "gastroesophageal reflux disease", "chronic liver disease",
    "peripheral vascular disease", "atrial fibrillation"
  )

  no_comorbidity_phrases <- c(
    "No comorbidities.",
    "Patient denies any chronic conditions.",
    "No significant comorbidities present.",
    "Patient exhibits no chronic illnesses.",
    "Medical history is unremarkable regarding comorbidities."
  )

  templates <- c(
    "Patient has a history of %s.",
    "History includes %s.",
    "Chronic conditions: %s.",
    "Notes indicate %s.",
    "Patient exhibits signs of %s."
  )

  additional_details <- c(
    "Patient complains of intermittent chest pain.",
    "Noted irregular heartbeat during last visit.",
    "Lab results pending; consider repeating tests.",
    "Patient appears fatigued with no clear cause.",
    "Family history significant for cardiovascular disease.",
    "Patient reports stress at work affecting sleep.",
    "Follow-up in 2 weeks to re-assess condition.",
    "Inconsistencies in patient self-reports noted.",
    "Minor concerns with medication adherence.",
    "Patient exhibits signs of dehydration and malaise.",
    "Observed slight tremors and episodes of confusion.",
    "Patient has been non-compliant with medication dosage.",
    "Occasional nausea and dizziness reported.",
    "Blood pressure has been fluctuating without clear pattern."
  )

  free_text_notes <- c(
    "Patient exhibits erratic symptoms, including sporadic dizziness and fatigue. Consider comprehensive metabolic panel.",
    "Complaints of blurred vision and intermittent joint pain; patient is non-compliant with therapy.",
    "Notes: occasional coughing fits and unexpected weight loss. Monitor closely.",
    "Patient's history is inconsistent. Lab values show anomalies in blood sugar and cholesterol levels.",
    "Observed slight tremors and episodes of confusion. No definitive diagnosis yet.",
    "Patient has unstructured complaints ranging from mild headaches to severe abdominal pain. Further evaluation required."
  )

  medical_jargon <- c(
    "BP: 130/85", "EKG abnormal", "ABG: pH 7.35", "ECG reveals non-specific ST changes",
    "CRP elevated", "WBC count high", "Hgb dropped by 1.2 g/dL", "LDL levels borderline",
    "Patient exhibits tachycardia", "O2 saturation at 92%", "Temp: 38.2°C"
  )

  # With probability, return a completely free-text note
  if (runif(1) < 0.3) {
    note <- sample(free_text_notes, 1)
  } else {
    # Decide how many comorbidities to mention (0 means no conditions)
    k <- sample(0:3, 1, prob = c(0.3, 0.4, 0.2, 0.1))
    if (k == 0) {
      note <- sample(no_comorbidity_phrases, 1)
    } else {
      conditions <- sample(comorbidities, k, replace = FALSE)
      conditions_str <- paste(conditions, collapse = ", ")
      note_template <- sample(templates, 1)
      note <- sprintf(note_template, conditions_str)
    }
    # Append additional unstructured details with probability
    if (runif(1) < 0.5) {
      extra_notes <- paste(sample(additional_details, sample(1:2, 1)), collapse = " ")
      note <- paste(note, extra_notes, sep = " ")
    }
  }

  # Append random medical jargon with probability
  if (runif(1) < 0.5) {
    jargon_sample <- sample(medical_jargon, sample(1:2, 1))
    note <- paste(note, paste(jargon_sample, collapse = "; "), sep = " | ")
  }

  # Append random date in various formats with probability
  if (runif(1) < 0.4) {
    random_date <- sample(seq.Date(as.Date("2018-01-01"), as.Date("2024-12-31"), by = "day"), 1)
    formatted_date <- format(random_date, sample(c("%m/%d/%Y", "%Y-%m-%d", "%d-%b-%Y"), 1))
    note <- paste(note, sprintf("Note entered on %s?", formatted_date), sep = " ")
  }

  # Randomly append extra question marks
  if (runif(1) < 0.3) {
    note <- paste(note, sample(c("?", "??", "?!", "!?"), 1))
  }

  # Randomly insert typos or shorthand notations with low probability
  if (runif(1) < 0.2) {
    note <- gsub("patient", "pt", note, ignore.case = TRUE)
    note <- gsub("history", "hx", note, ignore.case = TRUE)
  }

  note
}

trial <-
  tibble::tibble(
    trt = sample(c("Drug A", "Drug B"), n, replace = TRUE),
    age = as.integer(rnorm(n, mean = 52.5, sd = 15.75)),
    marker = round(rgamma(n, 1.05, 0.95), digits = 3),
    stage = factor(sample(
      c("T1", "T2", "T3", "T4"),
      size = n,
      prob = c(0.26, 0.24, 0.25, 0.25),
      replace = TRUE
    )),
    grade = factor(sample(
      c("I", "II", "III"),
      size = n,
      prob = c(0.34, 0.33, 0.33),
      replace = TRUE
    )),
    response_prob = 1 / (1 + exp(-((trt == "Drug") - 0.21 * as.numeric(stage) - 0.105 * as.numeric(grade) + 0.105 * marker))),
    response = runif(n) < response_prob,
    ttdeath_true = exp(
      1.05 + 0.21 * response +
        -0.105 * as.numeric(stage) +
        -0.105 * as.numeric(grade) +
        rnorm(n, sd = 0.525)
    ) * 12,
    death = ifelse(ttdeath_true <= 24, 1L, 0L),
    ttdeath = round(pmin(ttdeath_true, 24), digits = 2),
    visit_date = sample(
      seq.Date(as.Date("2020-01-15"), as.Date("2022-01-15"), by = "days"),
      n,
      replace = TRUE
    ),
    follow_up_date = sample(
      seq.Date(as.Date("2022-01-15"), as.Date("2024-01-15"), by = "days"),
      n,
      replace = TRUE
    ),
    has_side_effects = sample(c(TRUE, FALSE), n, prob = c(0.48, 0.52), replace = TRUE),
    enrolled_in_study = sample(c(TRUE, FALSE), n, prob = c(0.51, 0.49), replace = TRUE),
    bmi = round(rnorm(n, mean = 26.25, sd = 4.2), 1),
    systolic_bp = round(rnorm(n, mean = 126, sd = 15.75)),
    patient_id = paste0("ID-", sprintf("%03d", seq(1, n))),
    hospital = sample(
      c("Hospital A", "Hospital B", "Hospital C"),
      n,
      prob = c(0.34, 0.33, 0.33),
      replace = TRUE
    ),
    insurance = factor(sample(
      c("Private", "Medicaid", "Medicare"),
      n,
      prob = c(0.34, 0.33, 0.33),
      replace = TRUE
    )),
    smoking_status = factor(sample(
      c("Non-smoker", "Former smoker", "Current smoker"),
      n,
      prob = c(0.34, 0.33, 0.33),
      replace = TRUE
    )),
    doctor_notes = replicate(n, generate_doctor_notes())
  ) |>
  dplyr::mutate(
    age = ifelse(runif(n) < 0.945, age, NA_real_),
    marker = ifelse(runif(n) < 0.945, marker, NA_real_),
    response = ifelse(runif(n) < 0.945, response, NA_integer_)
  ) |>
  dplyr::select(-dplyr::one_of("response_prob", "ttdeath_true"))

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

trial |> readr::write_csv(file = here::here("inst/extdata/df_trial3_unstructured_col.csv"))
