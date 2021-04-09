#' Computes weights for seasonal weighted means
#'
#' [weighted.mean()] is not GForce-optimised in data.table and
#' therefore, using it to compute weighted means is much slower
#' than computing the weights first and the computing `mean(x*w)`
#'
#'
#' @param time, a vector of times
#' @param groups vector of the same length as `time` that
#' defines the grouping.
#'
#' @returns
#' a vector of the same length as `time` that when used as
#' weights in `mean(x*w)` returns the same as
#' `weighted.mean(x, w)` computed for each group in `groups`.
#'
#'
#' @export
season_weights <- function(time, groups = seasonally(time)) {
  w <- as.numeric(on_unique(time, lubridate::days_in_month))

  setDT(list(w = w, groups = groups))[, w := w/mean(w), by = groups]$w
}


