#' @export
is.sa <- function(data, lons = c(265, 325), lats = c(-80, -10)) {
  data[, lon %between% lons & lat %between% lats]
}

#' @export
filter_sa <- function(data, lons = c(265, 325), lats = c(-60, -10)) {
  data[is.sa(data, lons = lons, lats = lats)]
}
