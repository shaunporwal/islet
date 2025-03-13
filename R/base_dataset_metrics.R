#' Compute Metrics for Base Dataset Selection
#'
#' This function computes a metrics table for a set of dataframes provided as a named list.
#' It compares the unique values of a specified identifier column across the dataframes.
#' The identifier column is coerced to a specified type before comparison.
#' The resulting table includes the count of unique identifiers, the total common identifiers
#' shared with other dataframes, and a logical flag indicating the main dataset (the one with
#' the highest total common identifier count).
#'
#' @param named_list_dfs A named list of dataframes.
#' @param identifier A character string specifying the column name used as the identifier.
#' @param identifier_type A character string specifying the type to which the identifier column should be coerced.
#'        Valid options include "numeric", "character", "integer", and "factor".
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{DataFrame}{Name of the dataframe.}
#'   \item{Unique_Count}{Number of unique identifier values in the dataframe.}
#'   \item{Total_Common}{Sum of identifier overlaps with all other dataframes.}
#'   \item{Is_Main}{Logical, TRUE if the dataframe is considered the main dataset based on the maximum total common count.}
#' }
#'
#' @details The main dataset is determined by comparing the number of common identifier values with all other dataframes.
#'
#' @examples
#' \dontrun{
#' # Assume df1, df2, df3 are dataframes with a column 'MRN'
#' named_list <- list(df1 = df1, df2 = df2, df3 = df3)
#' metrics <- base_dataset_metrics(named_list, "MRN", "numeric")
#' print(metrics)
#' }
#'
#' @export
base_dataset_metrics <- function(named_list_dfs, identifier, identifier_type) {
  list_ids <- lapply(named_list_dfs, function(df) {
    df[[identifier]] <- switch(identifier_type,
      "numeric" = as.numeric(df[[identifier]]),
      "character" = as.character(df[[identifier]]),
      "integer" = as.integer(df[[identifier]]),
      "factor" = as.factor(df[[identifier]]),
      stop("Unsupported identifier_type provided.")
    )
    unique(df[[identifier]])
  })

  df_names <- names(named_list_dfs)
  n <- length(list_ids)

  pairwise_mat <- matrix(0, nrow = n, ncol = n, dimnames = list(df_names, df_names))
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        pairwise_mat[i, j] <- length(intersect(list_ids[[i]], list_ids[[j]]))
      } else {
        pairwise_mat[i, j] <- NA
      }
    }
  }

  total_common <- rowSums(pairwise_mat, na.rm = TRUE)
  unique_count <- sapply(list_ids, length)
  is_main <- ifelse(total_common == max(total_common), TRUE, FALSE)

  metrics_table <- data.frame(
    DataFrame = df_names,
    Unique_Count = unique_count,
    Total_Common = total_common,
    Is_Main = is_main,
    stringsAsFactors = FALSE
  )

  return(metrics_table)
}
