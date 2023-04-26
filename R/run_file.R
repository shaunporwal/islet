#' Function to run the "function"/"scripts" that populate main.R
#'
#' Wrapper function for a do.call() command applied to script function.  Benefit is de-clutter the calls.  Drawback is debugging more convoluted.
#'
#' @param file A function/script call
#' @param args A list of parameters that should be specified prior to running the function/script specified in "file"
#' @param ignore_warnings For now, a default logic to muffle a few routine "warnings" that are likely best to prevent from clogging up the output in the main.R script (NAs values created).  Can set to "" to override this functionality.
#'
#'@return return_list
#'
#' @export
#'
run_file <- function(file,
                     args = NULL,
                     ignore_warnings = "NAs introduced by coercion|failed to parse|no non-missing arguments to max; returning -Inf|no non-missing arguments to min; returning Inf") {



  # Calling handler defintion ---------------------------------------------

  # TODO: update "file" to function

  # TODO: later could update the use of case_when(all(!is.na()) ~ ...) type logic to muffle the irrelevant -Inf type warnings itself, so these do not need to be escaped above.

  # TODO: all this warning  handler stuff is to avoid logging out these kinds of warnings in the console when running...
  #       You could alternatively, log out all ignored warnings. And also have a function that reads those out.
  #       Not even sure how robust this implementation is.
  warning_condition <- function(w) {FALSE}

  if (!is.null(ignore_warnings)) {
    warning_condition <- function(w) {
      any(grepl(ignore_warnings, w))
    }
  }

  h <- function(w) if (warning_condition(w)) {
    invokeRestart( "muffleWarning" )
  }



  # Run file ----------------------------------------------------------------

  if (is.null(args)) {

    file() %>%
      withCallingHandlers(warning = h)


  } else {


    file %>%
      do.call(args) %>%
      withCallingHandlers(warning = h)
  }

}
