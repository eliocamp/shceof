#' Applies a function to a vector only once for each value
#'
#' In cases when you have a large vector with a lot of repeated values
#' (for example, the dimension columns of a spatial field in a  data.table)
#' applying a function to each unique value of a vector and then repeating
#' the result is faster and more memory efficient than applying the function
#' to each element.
#'
#' @param x, vector
#' @param f, function to apply. It needs to be a function that takes 1 value and
#' returns 1 value.
#' @param ..., additional arguments passed to `f`
#'
#' @returns
#' a vector the same length as `x`
#'
#' @export
on_unique <- function(x, f, ...) {
  x_unique <- unique(x)
  result <- f(x_unique, ...)
  result[match(x, x_unique)]
}
