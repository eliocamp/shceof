#' Quick maps
#'
#'
#' @param subset a function that will be applied to the map data.
#' @param color,size,fill parameters passed to [ggplot2::geom_polygon()]
#' @param wrap vector of length 2 interpreted as the longitude range of a global map (see [maps::map()])
#' @param keep proportions of point to retain (passed to [rmapshaper::ms_simplify()])
#' @param ... other arguments passed to [ggplot2::geom_polygon()]
#'
#' @export
geom_qmap <- function(subset = identity,
                      crop = NULL,
                      color = "gray50", size = 0.2,
                      fill = NA, wrap = c(0, 360), weighting = 0.7,
                      keep = 0.015, ...) {
  lon <- lat <- group <- NULL
  data <- map_simple(wrap = wrap, keep  = keep, weighting = weighting)

  if (!is.null(crop)) {
    bbox <- sf::st_bbox(data)

    for (n in names(crop)) {
      bbox[[n]] <- crop[[n]]
    }

    data <- sf::st_crop(data, bbox)
  }


  subset <- purrr::as_mapper(subset)
  data <- subset(data)

  ggplot2::geom_sf(data = data,
                   inherit.aes = FALSE,
                   color = color,
                   size = size,
                   fill = fill,
                   ...)

}

map_simple <- function(wrap = c(0, 360), keep = 0.015, weighting = 0.7) {
  map <- maps::map("world", fill = TRUE,
                   col = "transparent", plot = FALSE, wrap = wrap)
  map <- sf::st_as_sf(map)
  if (keep != 1) {
    map <- rmapshaper::ms_simplify(map, keep = keep, weighting = weighting)
  }


  map
}

fortify <- ggplot2::fortify

# zzz.R
.onLoad <- function(lib, pkg) {
  map_simple <<- memoise::memoise(map_simple)
  fortify <<- memoise::memoise(fortify)
}



`%||%` <- function (x, y) {
  if (is_null(x))
    y
  else x
}
