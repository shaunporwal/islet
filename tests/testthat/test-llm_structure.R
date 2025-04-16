library(testthat)

test_that("llm_structure adds a structured column to the dataframe", {
  # Skip if openai package isn't installed
  skip_if_not_installed("openai")
  # Skip if API key isn't set
  skip_if_not(Sys.getenv("openai_secret_key") != "", "openai_secret_key not set")
  # Skip if offline, as this test requires an internet connection
  skip_if_offline()

  print("openai_secret_key found and online, running test")

  df_test <- readr::read_csv(file = "https://raw.githubusercontent.com/shaunporwal/islet/refs/heads/main/inst/extdata/df_trial3_unstructured_col.csv", show_col_types = FALSE)

  df_unstruct_col <- islet::read_raw_data(
    file = "https://raw.githubusercontent.com/shaunporwal/islet/refs/heads/main/inst/extdata/df_trial3_unstructured_col.csv"
  ) |> head(1)

  # Wrap the actual API call in a tryCatch or check response validity
  result_df <- tryCatch(
    {
      islet::llm_structure(
        df = df_unstruct_col,
        col_name = "DOCTOR_NOTES",
        prompt = "output 1 if there are heart issues, 0 if not. just 1s or 0s. make sure to have an output, you can not output nothing"
      )
    },
    error = function(e) {
      skip(paste("OpenAI API call failed:", e$message))
    }
  )

  expect_true("DOCTOR_NOTES_llm" %in% names(result_df))
  expect_type(result_df$DOCTOR_NOTES_llm, "character")
  # Check that the result is not empty or NULL, allowing for potential API errors returning non-string
  expect_true(length(result_df$DOCTOR_NOTES_llm) > 0 && !is.null(result_df$DOCTOR_NOTES_llm) && nzchar(result_df$DOCTOR_NOTES_llm))
  expect_true(all(result_df$DOCTOR_NOTES_llm %in% c("1", "0")))
})
