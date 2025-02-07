library(testthat)
library(openai)

test_that("llm_structure adds a structured column to the dataframe", {
  if (Sys.getenv("openai_secret_key") == "") {
    skip("openai_secret_key not set, skipping test")
  } else {
    print("openai_secret_key found")
  }

  df_unstruct_col <- islet::read_raw_data(file = "https://raw.githubusercontent.com/shaunporwal/islet/refs/heads/main/data/df_trial3_unstructured_col.csv")

  result_df <- islet::llm_structure(df_unstruct_col, "DOCTOR_NOTES", "Structure these notes: ")

  expect_true("DOCTOR_NOTES_llm" %in% names(result_df))
  expect_equal(nrow(result_df), nrow(df_test))
  expect_type(result_df$DOCTOR_NOTES_llm, "character")
  expect_true(all(nchar(result_df$DOCTOR_NOTES_llm) > 0))
})
