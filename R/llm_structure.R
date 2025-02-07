#' Structure Data using LLM
#'
#' Applies an LLM to structure data from a specified column.
#'
#' @param df A dataframe.
#' @param col_name Name of the column in df to be processed.
#' @param prompt Prompt to be combined with each cell's value.
#'
#' @return Dataframe with a new column suffixed with "_llm".
#' @export
llm_structure <- function(df, col_name, prompt) {
  new_col_name <- paste0(col_name, "_llm")
  df[[new_col_name]] <- sapply(df[[col_name]], function(cell_value) {
    full_prompt <- paste(prompt, cell_value)
    result <- openai::create_completion(
      model = "text-davinci-003",
      prompt = full_prompt,
      max_tokens = 100
    )
    result$choices[[1]]$text
  })
  df
}
