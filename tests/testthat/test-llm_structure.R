library(testthat)

test_that("llm_structure adds a structured column to the dataframe", {
  if (Sys.getenv("openai_secret_key") == "") {
    skip("openai_secret_key not set, skipping test")
  } else {
    print("openai_secret_key found")
  }

  df_test <- readr::read_csv(file = "https://raw.githubusercontent.com/shaunporwal/islet/refs/heads/main/inst/extdata/df_trial3_unstructured_col.csv")

  df_unstruct_col <- islet::read_raw_data(
    file = "https://raw.githubusercontent.com/shaunporwal/islet/refs/heads/main/inst/extdata/df_trial3_unstructured_col.csv"
  ) |> head(1)

  result_df <- islet::llm_structure(
    df = df_unstruct_col,
    col_name = "DOCTOR_NOTES",
    prompt = "output 1 if there are heart issues, 0 if not. just 1s or 0s. make sure to have an output, you can not output nothing"
  )

  expect_true("DOCTOR_NOTES_llm" %in% names(result_df))
  expect_type(result_df$DOCTOR_NOTES_llm, "character")
  expect_true(all(nchar(result_df$DOCTOR_NOTES_llm) > 0))
  expect_true(all(result_df$DOCTOR_NOTES_llm %in% c("1", "0")))
})
