#' Structure Data using LLM
#'
#' Applies an LLM to structure data from a specified column.
#'
#' @param df A dataframe.
#' @param col_name Name of the column in df to be processed.
#' @param prompt Prompt to be used as the system message.
#'
#' @return Dataframe with a new column suffixed with "_llm".
#' @export
llm_structure <- function(df, col_name, prompt, model = "gpt-3.5-turbo") {
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
