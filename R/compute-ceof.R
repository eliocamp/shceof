#' Computes ceof
#'
#' @param hgt vector of geopotential height observations
#' @param lon,lat.lev vectors of locations of geopotential height observations
#' @param temporal Logical whether to compute temporal anomalies.
#' @param lats.eof Range of latitudes to use.
#' @param n Numeric vector of EOFs to compute.
#'
#' @export
compute_ceof <- function(hgt, lon, lat, lev, temporal = FALSE, lats.eof = c(-90, -20), n = 1:2) {
  if (is.data.frame(hgt)) {
    dt <- hgt
  } else {
    dt <- data.table::data.table(hgt, lon, lat, lev)
  }

  dt <- dt[lev %in% c(50, 200) & lat %between% lats.eof] %>%
    .[, hgt := metR::Anomaly(hgt),
      by  = .(lat, time, lev)]

  if (temporal) {
    dt <- dt[, hgt := metR::Anomaly(hgt), by = .(lon, lat, lev)]
  }

  dt %>%
    .[, hgt := hgt/stats::sd(hgt), by = .(lev)] %>%
    .[, hgt := hgt*sqrt(cos(lat*pi/180))] %>%
    .[, hgt.cpx := spectral::analyticFunction(hgt),
      by = .(lat, time, lev)] %>%
    .[, hgt := hgt.cpx] %>%
    metR::EOF(hgt ~ time | lon + lat + lev, n = n, suffix = "cEOF", data = .)
}

