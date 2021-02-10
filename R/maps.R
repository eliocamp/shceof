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
geom_qmap <- function(subset = identity, color = "gray50", size = 0.2,
                      fill = NA, wrap = c(0, 360), weighting = 0.7,
                      keep = 0.015, ...) {
  lon <- lat <- group <- NULL
  data <- map_simple(wrap = wrap, keep  = keep, weighting = weighting)

  data <- data %>%
    fortify() %>%
    data.table::as.data.table() %>%
    .[, c("long", "lat", "group")] %>%
    data.table::setnames("long", "lon")
  subset <- purrr::as_mapper(subset)
  data <- subset(data)

  ggplot2::geom_polygon(data = data,
                        ggplot2::aes(lon, lat, group = group),
                        color = color,
                        size = size,
                        fill = fill,
                        ...)
}

map_simple <- function(wrap = c(0, 360), keep = 0.015, weighting = 0.7) {
  map <- maps::map("world", fill = TRUE,
                   col = "transparent", plot = FALSE, wrap = wrap)
  IDs <- vapply(strsplit(map$names, ":"), function(x) x[1],
                "")
  proj <- sp::CRS("+proj=longlat +datum=WGS84")
  map <- maptools::map2SpatialPolygons(map, IDs = IDs,
                                       proj4string = proj)

  if (keep != 1) {
    map <- rmapshaper::ms_simplify(map, keep = keep, weighting = weighting)
  }
  map
}

fortify <- ggplot2::fortify

# zzz.R
.onLoad <- function(pkgname, libname) {
  map_simple <<- memoise::memoise(map_simple,
                                  cache = memoise::cache_filesystem(here::here("analysis/cache/memoise/")))
  fortify <<- memoise::memoise(fortify,
                               cache = memoise::cache_filesystem(here::here("analysis/cache/memoise/")))
}


