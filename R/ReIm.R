#' Splits complex numbers into real and imaginary vectors
#'
#' @param complex vector of complex values
#' @param data a data.table
#' @param column unquoted name of the column to split into real and imaginary part
#' @param format Whether to return a long or wide dataset.
#'
#' @export
ReIm <- function(complex) {
  list(Real = Re(complex), Imaginary = Im(complex))
}

#' @export
#' @rdname ReIm
sep_ReIm <- function(data, column, format = c("longer", "wider")) {
  R <- part <- I <- NULL
  names <- c("Real", "Imaginary")


  if (missing(column)) {
    complex <- vapply(data, function(x) inherits(x, "complex"), TRUE)
    if (sum(complex) > 1) {
      stop("`column` missing and more than one complex column found")
    }
    if (sum(complex) == 0) {
      warning("`column` missing and no complex column found. Returning unchanged data")
      return(data)
    }

    col <- colnames(data)[complex]
  } else {
    col <- deparse(substitute(column))
  }


  data <- data.table::copy(data)[, (names) := ReIm(get(col))]


  if (format[1] == "longer") {
    data[, c(col) := NULL]
    data <- data.table::setDT(tidyr::pivot_longer(data, Real:Imaginary, names_to = "part", values_to = col))
    data[, part := factor(part, levels = names, ordered = TRUE)]
  }

  return(data[])
}

#' @param part Character vector with elements "Real" and "Imaginary"
#' @export
#' @rdname ReIm
factor_ReIm <- function(part) {
  factor(part, levels = c("Real", "Imaginary"), ordered = TRUE)
}
