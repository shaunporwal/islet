
#' Compare 2 dataframes
#'
#' @param oldData Old data
#' @param newData New data
#' @param suffixTerm Suffix term
#' @param indOutcomes indent outcomes (not sure if this is accurate)
#' @param surgCol surgeon column name
#'
#' @return final df list
#' @export
#'
#' @examples


# library(islet)
compare_dataframes <- function(oldData = NULL, newData = NULL, suffixTerm = "", indOutcomes = c(""), surgCol = "surgeon") {

  # source("O:/Outcomes/Andrew/Amplio/Functions/ryanw/parseFunction.R")

  # Analyze dataframe
  if (is.null(newData))
    oldData <- parse_function(oldData, "old", indOutcomes, surgCol, addYears = TRUE)
  else
    oldData <- parse_function(oldData, "old", indOutcomes, surgCol)

  if (is.null(newData)) { #only looking at results for one dataframe

    # # Remove tidyr if loaded in workspace because function "complete" overloads function used in mice package
    # if ("package:tidyr" %in% search())
    #  detach("package:tidyr", unload=TRUE)

    oldData <- lapply(oldData, function(x){

      if (!is.null(x$field))
        x <- x %>% filter(!(field %in% c("dummyDate", "dummyPOSI", "dummyChar", "dummyNum", "dummyFactor"))) %>% ungroup()

      #x <- data.frame(lapply(x, function(y){y <- ifelse(is.nan(y), NA, y)}))

      return(x)
    })

    return(oldData)

  }
  else{ # comparing to dataframes

    # Analyze dataframe
    newData <- parse_function(newData, "new", indOutcomes, surgCol)

    # parse_function will return: numericDf, factorDf, charDf, binaryDf, dateDf

    # merge these manually for now
    # Note: suppressWarnings() used here since field is created as a factor by gather() and
    #   joining by uneven levels will result in the columns being converted to characters: joining factors with different levels, coercing to character vector
    numericJoin <- suppressWarnings(full_join(oldData[[1]], newData[[1]], by="field"))
    numericJoin <- numericJoin[ , c(1,2,9,3,10,4,11,5,12,6,13,7,14,8,15)]

    factorJoin <- suppressWarnings(full_join(oldData[[2]], newData[[2]], by="field"))
    factorJoin <- factorJoin[ , c(1,2,5,3,6,4,7)]

    charJoin <- suppressWarnings(full_join(oldData[[3]], newData[[3]], by="field"))
    charJoin <- charJoin[ , c(1,2,5,3,6,4,7)]

    binJoin <- suppressWarnings(full_join(oldData[[4]], newData[[4]], by="field"))
    binJoin <- binJoin[ , c(1,2,4,3,5)]

    dateJoin <- suppressWarnings(full_join(oldData[[5]], newData[[5]], by="field"))
    dateJoin <- dateJoin[ , c(1,2,5,3,6,4,7)]

    surgJoin <- suppressWarnings(full_join(oldData[[6]], newData[[6]], by="surgeon"))
    surgJoin <- surgJoin[ , order(names(surgJoin))]
    surgJoin <- surgJoin[c("surgeon", setdiff(names(surgJoin), "surgeon"))]

    # Final comparison
    finalDfList <- list(numericJoin, factorJoin, charJoin, binJoin, dateJoin, surgJoin)

    # # Remove tidyr if loaded in workspace because function "complete" overloads function used in mice package
    # if ("package:tidyr" %in% search())
    #  detach("package:tidyr", unload=TRUE)


    # Clean up dataframes: 1) remove dummy rows 2) turn NaN's to NA's
    finalDfList <- lapply(finalDfList, function(x){
      if (!is.atomic(x)){ # This is check for when surgJoin is just "NODATA"

        if (!is.null(x$field))
          x <- x %>% filter(!(field %in% c("dummyDate", "dummyPOSI", "dummyChar", "dummyNum", "dummyFactor"))) %>% ungroup()
      }
      #x <- data.frame(lapply(x, function(y){ <- ifelse(is.nan(y), NA, y)}))

      return(x)
    })

    return(finalDfList)
  }
}

# Later, filter these out:

# filter these rows:
#   parseDf$dummyDate <- as.Date("1000-01-01")
# parseDf$dummyPOSI <- ymd("2016-12-01")
# parseDf$dummyChar <- ""
# parseDf$dummyNum <- -1
# parseDf$dummyFactor <- as.factor(c(0))
