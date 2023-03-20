#' Parsing function
#'
#' @param parse_df Dataframe to parse
#' @param suffixTerm Suffix term
#' @param indOutcomes Indent outcomes?
#' @param surgCol Surgeon Column
#' @param addYears Add years
#'
#' @return Result Dataframe
#' @export
#'
#' @examples

parse_function <- function(parse_df, suffixTerm = "", indOutcomes = c(""), surgCol = "surgeon", addYears = FALSE) {

  parse_df <- parse_df %>% ungroup()

  # functions won't work correctly below unless there's at least one row in each dataframe
  parse_df$dummyDate <- as.Date("1000-01-01")
  parse_df$dummyChar <- ""
  parse_df$dummyNum <- -1
  parse_df$dummyFactor <- as.factor(c(0))

  # Dates
  minDate <- parse_df %>%
    summarise_if(function(y) (is.Date(y)), funs(min(., na.rm = TRUE))) %>%
    mutate(id_tempCol = 1) %>%
    gather("field", "minDate", 1:(ncol(.)-1)) %>% select(-id_tempCol)

  maxDate <- parse_df %>%
    summarise_if(function(y) (is.Date(y)), funs(max(., na.rm = TRUE))) %>%
    mutate(id_tempCol = 1) %>%
    gather("field", "maxDate", 1:(ncol(.)-1)) %>% select(-id_tempCol)

  percNaDate <- parse_df %>%
    summarise_if(function(y) (is.Date(y)), funs(round(sum(is.na(.))/n(), digits = 3))) %>%
    mutate(id_tempCol = 1) %>%
    gather("field", "percNaDate", 1:(ncol(.)-1)) %>% select(-id_tempCol)

  datesDf <- full_join(minDate, maxDate, by="field") %>% full_join(percNaDate, by="field")

  posiDf <- NULL
  if (length(parse_df %>% select_if(is.POSIXct)) > 0)
  {
    minPOSIXct <- parse_df %>%
      summarise_if(function(y) (is.POSIXct(y)), funs(min(., na.rm = TRUE))) %>%
      mutate(id_tempCol = 1) %>%
      gather("field", "minDate", 1:(ncol(.)-1)) %>% select(-id_tempCol)

    maxPOSIXct <- parse_df %>%
      summarise_if(function(y) (is.POSIXct(y)), funs(max(., na.rm = TRUE))) %>%
      mutate(id_tempCol = 1) %>%
      gather("field", "maxDate", 1:(ncol(.)-1)) %>% select(-id_tempCol)

    percNaPOSIXct <- parse_df %>%
      summarise_if(function(y) (is.POSIXct(y)), funs(round(sum(is.na(.))/n(), digits = 3))) %>%
      mutate(id_tempCol = 1) %>%
      gather("field", "percNaDate", 1:(ncol(.)-1)) %>% select(-id_tempCol)

    posiDf <- full_join(minPOSIXct, maxPOSIXct, by="field") %>% full_join(percNaPOSIXct, by="field")
  }

  dateDf <- rbind(datesDf, posiDf)

  # Binary
  ratioBinary <- parse_df %>%
    summarise_if(function(y) (is.logical(y) | (is.numeric(y) & length(unique(y[!is.na(y)])) <= 2)),
                 funs(round(sum(., na.rm=TRUE)/sum(!is.na(.)), digits = 3)*100)) %>%
    mutate(id_tempCol = 1) %>%
    gather("field", "ratioBinary", 1:(ncol(.)-1)) %>% select(-id_tempCol)

  percNaBinary <- parse_df %>%
    summarise_if(function(y) (is.logical(y) | (is.numeric(y) & length(unique(y[!is.na(y)])) <= 2)),
                 funs(round(sum(is.na(.))/n(), digits = 3))) %>%
    mutate(id_tempCol = 1) %>%
    gather("field", "percNaBinary", 1:(ncol(.)-1)) %>% select(-id_tempCol)

  binaryDf <- full_join(ratioBinary, percNaBinary, by="field")

  # Add year values
  if (addYears)
  {
    # Mean
    binYearsMean <- NULL
    binYearsMean <- byYearValues(binaryDf$field, "mean", parse_df, TRUE)

    # perNa
    binYearsNA <- NULL
    binYearsNA <- byYearValues(binaryDf$field, "percNA", parse_df, TRUE)

    # count
    #     binYearsCount <- NULL
    #     binYearsCount <- byYearValues(binaryDf$field, "count", parse_df)
  }


  # Characters
  valuesChar <- parse_df %>%
    summarise_if(function(y) (is.character(y)),
                 funs(paste(unique(.), collapse=", "))) %>%
    mutate(id_tempCol = 1) %>%
    gather("field", "valuesChar", 1:(ncol(.)-1)) %>% select(-id_tempCol)


  distinctChar <- parse_df %>%
    summarise_if(function(y) (is.character(y)),
                 funs(length(unique(.[!is.na(.)])))) %>%
    mutate(id_tempCol = 1) %>%
    gather("field", "distinctChar", 1:(ncol(.)-1)) %>% select(-id_tempCol)

  perBlankNAChar <- parse_df %>%
    summarise_if(function(y) (is.character(y)),
                 funs(round(sum(. == "" | is.na(.), na.rm = TRUE)/n(), digits = 3))) %>%
    mutate(id_tempCol = 1) %>%
    gather("field", "perBlankNAChar", 1:(ncol(.)-1)) %>% select(-id_tempCol)

  charDf <- full_join(distinctChar, valuesChar, by="field") %>% full_join(perBlankNAChar, by="field")

  # Add year values
  if (addYears)
  {
    # Mean
    charYearsMean <- NULL
    charYearsMean <- byYearValues(charDf$field, "mean", parse_df)

    # perNa
    charYearsNA <- NULL
    charYearsNA <- byYearValues(charDf$field, "percNA", parse_df)

    # count
    #     charYearsCount <- NULL
    #     charYearsCount <- byYearValues(binaryDf$field, "count", parse_df)
  }

  # factors
  distinctFactor <- parse_df %>%
    summarise_if(function(y) (is.factor(y)),
                 funs(length(levels(.)))) %>%
    mutate(id_tempCol = 1) %>%
    gather("field", "distinctFactor", 1:(ncol(.)-1)) %>% select(-id_tempCol)

  valuesFactor <- parse_df %>%
    summarise_if(function(y) (is.factor(y)),
                 funs(paste(levels(.), collapse=", "))) %>%
    mutate(id_tempCol = 1) %>%
    gather("field", "valuesFactor", 1:(ncol(.)-1)) %>% select(-id_tempCol)

  perNaFactor <- parse_df %>%
    summarise_if(function(y) (is.factor(y)),
                 funs(round(sum(is.na(.))/n(), digits = 3))) %>%
    mutate(id_tempCol = 1) %>%
    gather("field", "perNaFactor", 1:(ncol(.)-1)) %>% select(-id_tempCol)

  factorDf <- full_join(distinctFactor, valuesFactor, by="field") %>% full_join(perNaFactor, by="field")


  # Add year values
  if (addYears)
  {
    # Mean
    factorYearsMean <- NULL
    factorYearsMean <- byYearValues(factorDf$field, "mean", parse_df)

    # perNa
    factorYearsNA <- NULL
    factorYearsNA <- byYearValues(factorDf$field, "percNA", parse_df)

    # count
    factorYearsCount <- NULL
    factorYearsCount <- byYearValues(binaryDf$field, "count", parse_df)
  }

  # Numeric
  meanNumeric <- parse_df %>%
    summarise_if(function(y) (is.numeric(y) & length(unique(y[!is.na(y)])) > 2),
                 funs(round(mean(., na.rm = TRUE), digits = 3))) %>%
    mutate(id_tempCol = 1) %>%
    gather("field", "meanNumeric", 1:(ncol(.)-1)) %>% select(-id_tempCol)

  minNumeric <- parse_df %>%
    summarise_if(function(y) (is.numeric(y) & length(unique(y[!is.na(y)])) > 2),
                 funs(round(min(., na.rm = TRUE), digits = 3))) %>%
    mutate(id_tempCol = 1) %>%
    gather("field", "minNumeric", 1:(ncol(.)-1)) %>% select(-id_tempCol)

  maxNumeric <- parse_df %>%
    summarise_if(function(y) (is.numeric(y) & length(unique(y[!is.na(y)])) > 2),
                 funs(round(max(., na.rm = TRUE), digits = 3))) %>%
    mutate(id_tempCol = 1) %>%
    gather("field", "maxNumeric", 1:(ncol(.)-1)) %>% select(-id_tempCol)

  medianNumeric <- parse_df %>%
    summarise_if(function(y) (is.numeric(y) & length(unique(y[!is.na(y)])) > 2),
                 funs(round(median(., na.rm = TRUE), digits = 3))) %>%
    mutate(id_tempCol = 1) %>%
    gather("field", "medianNumeric", 1:(ncol(.)-1)) %>% select(-id_tempCol)

  q25Numeric <- parse_df %>%
    summarise_if(function(y) (is.numeric(y) & length(unique(y[!is.na(y)])) > 2),
                 funs(round(quantile(., c(.25), na.rm = TRUE), digits = 3))) %>%
    mutate(id_tempCol = 1) %>%
    gather("field", "q25Numeric", 1:(ncol(.)-1)) %>% select(-id_tempCol)

  q75Numeric <- parse_df %>%
    summarise_if(function(y) (is.numeric(y) & length(unique(y[!is.na(y)])) > 2),
                 funs(round(quantile(., c(.75), na.rm = TRUE), digits = 3))) %>%
    mutate(id_tempCol = 1) %>%
    gather("field", "q75Numeric", 1:(ncol(.)-1)) %>% select(-id_tempCol)

  perNaNumeric <- parse_df %>%
    summarise_if(function(y) (is.numeric(y) & length(unique(y[!is.na(y)])) > 2),
                 funs(round(sum(is.na(.))/n(), digits = 3))) %>%
    mutate(id_tempCol = 1) %>%
    gather("field", "perNaNumeric", 1:(ncol(.)-1)) %>% select(-id_tempCol)


  numericDf <- full_join(meanNumeric, minNumeric, by="field") %>%
    full_join(maxNumeric, by="field") %>%
    full_join(medianNumeric, by="field") %>%
    full_join(q25Numeric, by="field") %>%
    full_join(q75Numeric, by="field") %>%
    full_join(perNaNumeric, by="field")

  # Add year values
  if (addYears)
  {
    # Mean
    numYearsMean <- NULL
    numYearsMean <- byYearValues(numericDf$field, "mean", parse_df)

    # perNa
    numYearsNA <- NULL
    numYearsNA <- byYearValues(numericDf$field, "percNA", parse_df)

    # count
    #     numYearsCount <- NULL
    #     numYearsCount <- byYearValues(binaryDf$field, "count", parse_df)
  }

  # Compare specific outcomes per Surgeon

  if (indOutcomes[[1]] != "") {

    # check here to make sure column names are all valid
    discard <- lapply(indOutcomes, function(x) {
      if (!(any(x %in% colnames(parse_df)))) {
        stop("Check that the individual outcomes exists in the dataframe provided.")
      }
    })


    # Check to make sure surgeon field exists in dataframe
    if (!(any(surgCol %in% colnames(parse_df)))) {
      stop("Check that the designated surgeon column exists in the dataframe provided.")
    }

    surgDf <- parse_df %>% select(one_of(surgCol, indOutcomes)) %>%
      group_by_(surgCol) %>%
      summarize_all(funs(round(mean(., na.rm = TRUE), digits = 3)))

    names(surgDf[,surgCol]) <- "surgeon"

    #colnames(surgDf) <- indOutcomes

  }else {
    surgDf <- data.frame(surgeon = c("NODATA"))
  }

  # merge by-year dataframes
  if (addYears)
  {
    # confirm all columns are ordered in the same way
    # RW this is a new check added 6/11/18 due to a change in
    # "everything()" reliably ordering the columns
    if(any(colnames(binYearsMean) != colnames(charYearsMean)) |
       any(colnames(binYearsMean) != colnames(factorYearsMean)) |
       any(colnames(binYearsMean) != colnames(numYearsMean))) {
      stop("By year mean dataframes do not all have equivalent columns.")
    }

    if(any(colnames(binYearsNA) != colnames(charYearsNA)) |
       any(colnames(binYearsNA) != colnames(factorYearsNA)) |
       any(colnames(binYearsNA) != colnames(numYearsNA))) {
      stop("By year NA dataframes do not all have equivalent columns.")
    }

    yearsMean <- rbind(binYearsMean, charYearsMean, factorYearsMean, numYearsMean)
    yearsNA <- rbind(binYearsNA, charYearsNA, factorYearsNA, numYearsNA)
  }

  if (addYears)
  {
    # Save dataframes as a list
    listDf <-list(numericDf, factorDf, charDf, binaryDf, dateDf, surgDf, yearsMean, yearsNA)
  }
  else
    listDf <-list(numericDf, factorDf, charDf, binaryDf, dateDf, surgDf)

  return(listDf)

}


byYearValues <- function(columns, summaryType, readFromDf, addCount = FALSE)
{
  resultDf <- NULL


  if (addCount == TRUE)
  {
    #temp <- readFromDf[,unique(c("yos", as.character(columns[1])))] # TODO: not tested
    temp <- readFromDf %>% select(one_of(unique(c("yos", as.character(columns[1])))))
    temp <- temp %>% group_by(yos) %>% summarize_all(function(x) sum(is.na(x)) + sum(!is.na(x)))

    # transpose
    temp <- data.frame(t(temp))

    colnames(temp) <- paste0(temp["yos",], " ", summaryType)
    temp <- temp[2,]
    resultDf <- temp
    row.names(resultDf) <- NULL
    resultDf$field <- "TOTAL COUNT"
  }

  for (i in columns)
  {
    #temp <- readFromDf[,unique(c(i, "yos"))]
    temp <- readFromDf %>% select(one_of(unique(c(i, "yos"))))


    if (summaryType == "mean")
    {
      temp <- temp %>% group_by(yos) %>% summarize_all(function(x) round(mean(x, na.rm=TRUE), digits=2))
    }
    else if (summaryType == "percNA")
    {
      temp <- temp %>% group_by(yos) %>% summarize_all(function(x) round(mean(is.na(x)), digits=2))
    }
    else if (summaryType == "count")
    {
      temp <- temp %>% group_by(yos) %>% summarize_all(function(x) sum(is.na(x)) + sum(!is.na(x)))
    }
    else
      stop("Unknown summary type!")

    # transpose
    temp <- data.frame(t(temp))

    colnames(temp) <- paste0(temp["yos",], " ", summaryType)
    temp <- temp[2,]
    temp$field <- rownames(temp)

    resultDf <- rbind(resultDf, temp)

  }


  resultDf <- resultDf %>% select(field, everything())

  resultDf$field <- as.factor(resultDf$field)

  row.names(resultDf) <- NULL

  return(resultDf)

}
