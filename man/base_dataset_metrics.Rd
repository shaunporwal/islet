% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/base_dataset_metrics.R
\name{base_dataset_metrics}
\alias{base_dataset_metrics}
\title{Compute Metrics for Base Dataset Selection}
\usage{
base_dataset_metrics(named_list_dfs, identifier, identifier_type)
}
\arguments{
\item{named_list_dfs}{A named list of dataframes.}

\item{identifier}{A character string specifying the column name used as the identifier.}

\item{identifier_type}{A character string specifying the type to which the identifier column should be coerced.
Valid options include "numeric", "character", "integer", and "factor".}
}
\value{
A data frame with the following columns:
\describe{
\item{DataFrame}{Name of the dataframe.}
\item{Unique_Count}{Number of unique identifier values in the dataframe.}
\item{Total_Common}{Sum of identifier overlaps with all other dataframes.}
\item{Is_Main}{Logical, TRUE if the dataframe is considered the main dataset based on the maximum total common count.}
}
}
\description{
This function computes a metrics table for a set of dataframes provided as a named list.
It compares the unique values of a specified identifier column across the dataframes.
The identifier column is coerced to a specified type before comparison.
The resulting table includes the count of unique identifiers, the total common identifiers
shared with other dataframes, and a logical flag indicating the main dataset (the one with
the highest total common identifier count).
}
\details{
The main dataset is determined by comparing the number of common identifier values with all other dataframes.
}
\examples{
\dontrun{
# Assume df1, df2, df3 are dataframes with a column 'MRN'
named_list <- list(df1 = df1, df2 = df2, df3 = df3)
metrics <- base_dataset_metrics(named_list, "MRN", "numeric")
print(metrics)
}

}
