read.robust <- function(fname, header = TRUE, sep = ",", ...) {
  data <- read.data(fname, header = header, sep = sep, ...)
  data <- convert.POSIX(data)

}

TruncateCategories <- function(data, response.name = NULL, max.levels = 10) {
  if (!is.null(response.name)) {
    y <- data[, response.name]
  } else {
    y <- NULL
  }

  for (i in 1:ncol(data)) {
    if (is.factor(data[, i])) {
      data[, i]  <- TruncateCategoriesSingleColumn(
        data[, i], y, max.levels = max.levels)
    }
  }
  return(data)
}

TruncateCategoriesSingleColumn <- function(x, y, max.levels) {
  if (is.character(x)) {
    z <- as.factor(x)
  } else {
    z <- x
  }
  if is.factor(z) {
    if (is.null(y)) {
      z <- FrequencyTruncate(z, max.levels)
    } else if(is.numeric(y)) {
      z <- NumericTruncate(z, y, max.levels)
    } else if(is.factor(y)) {
      z <- CategoricalTruncate(z, y, max.levels)
    } else if(is.character(y)) {
      z <- CategoricalTruncate(z, as.factor(y), max.levels)
    }
    return(z)
  } else {
    return(x)
  }
}

FrequencyTruncate <- function(x, max.levels) {
  ## Return the most frequently occurring levels of the factor x.
  x <- as.factor(x)
  nlev <- length(levels(x))
  if (nlev <= max.levels) {
    return(x)
  }
  if (max.levels < 1) {
    max.levels <- 1
  }
  top.levels <- names(rev(sort(table(x)))[1:max.levels])

}


ImbuePOSIX <- function(data) {
  ## Convert any columns in a data frame that are convertible to POSIXct.
  ##
  ## Args:
  ##   data:  A data frame.
  ##
  ## Returns:
  ##   data, after converting any columns that match a standard date or
  ##   date/time format to POSIXct.
  for (i in 1:ncol(data)) {
    if (is.factor(data[, i]) || is.character(data[, i])) {
      data[, i] <- ImbuePOSIXColumn(data[, i])
    }
  }
  return(data)
}

ImbuePOSIXColumn <- function(y) {
  ## If y can be converted to POSIXct, return the conversion.  Otherwise return
  ## y.
  if (is.factor(y)) {
    z <- as.character(y)
  } else {
    z <- y
  }
  if (!is.character(z)) {
    return(y)
  }

  ## The date/time formats to try.  Prefer POSIX style dates.  Then the format
  ## we use here in 'Murica!  Finally try one of those pinko commie European
  ## conventions.
  tryFormats <- c("%Y-%m-%d %H:%M:%OS",
                  "%Y/%m/%d %H:%M:%OS",
                  "%Y-%m-%d %H:%M",
                  "%Y/%m/%d %H:%M",
                  "%Y-%m-%d",
                  "%Y/%m/%d",
                  "%m/%d/%Y",
                  "%m/%d/%Y %H:%M:%0S",
                  "%d/%m/%Y",
                  "%d/%m/%Y %H:%M:%0S")
  tryCatch(
  {
    ## If the conversion works, then we've got a valid date or datetime.  Return
    ## it.
    dt <- as.POSIXct(z, tryFormats=tryFormats)
    return(dt)
  },
  error = function(cond) {
    ## If an error was encountered, then return the original input, unaltered.
    return(y)
  },
  warning = function(cond) {
    ## If a warning was encountered, ignore it and press on.
    return(dt)
  },
  finally = {
    ## Cleanup would go here, if we needed any.
  }
  )
}
