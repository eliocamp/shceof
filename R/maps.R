#' Quick maps
#'
#'
#' @param subset a function that will be applied to the map data.
#' @param crop optional bounding box to crop the map.
#' @param color,size,fill parameters passed to [ggplot2::geom_polygon()]
#' @param ... other arguments passed to [ggplot2::geom_polygon()]
#'
#' @export
geom_qmap <- function(subset = identity,
                      crop = NULL,
                      color = "gray50", size = 0.3,
                      fill = NA, ...) {
  lon <- lat <- group <- NULL
  data <- simplemap

  if (!is.null(crop)) {
    bbox <- sf::st_bbox(data)

    for (n in names(crop)) {
      bbox[[n]] <- crop[[n]]
    }

    data <- sf::st_crop(data, bbox)
  }

  data <- subset(data)

  ggplot2::geom_sf(data = data,
                   inherit.aes = FALSE,
                   color = color,
                   size = size,
                   fill = fill,
                   ...)

}



`%||%` <- function (x, y) {
  if (is.null(x))
    y
  else x
}
