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
  if (!is.factor(x)) {
    return(x)
  }

}
