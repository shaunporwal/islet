#' Add Structured Column to a Dataframe Using a Language Model
#'
#' Processes a column in a data frame using a language model.
#'
#' @param df A data frame containing the input data.
#' @param col_name A character string specifying the column name to process.
#' @param prompt A character string containing the prompt for the language model.
#' @param model A character string specifying the language model to use (e.g., "gpt-4o-mini").
#'
#' @return A data frame with an additional column named \code{<col_name>_llm} containing the language model's output.
#'
#' @export
llm_structure <- function(df, col_name, prompt, model = "gpt-4o-mini") {
  key <- Sys.getenv("openai_secret_key")
  if (key == "") {
    if (interactive()) {
      message("Environment variable 'openai_secret_key' not detected. Please add it to your .Renviron file in your home directory (e.g., add the line 'openai_secret_key=YOUR_KEY').")
    } else {
      stop("Environment variable 'openai_secret_key' not detected. Please add it to your .Renviron file in your home directory (e.g., add the line 'openai_secret_key=YOUR_KEY').")
    }
  }

  new_col_name <- paste0(col_name, "_llm")
  df[[new_col_name]] <- sapply(df[[col_name]], function(cell_value) {
    result <- openai::create_chat_completion(
      model = model,
      messages = list(
        list(role = "system", content = prompt),
        list(role = "user", content = cell_value)
      ),
      max_tokens = 100
    )
    result$choices[[1]]$message$content
  })
  df
}
