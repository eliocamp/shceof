#' Computes ceof
#'
#' @param dt data.table.
#' @param temporal Logical whether to compute temporal anomalies.
#' @param lats.eof Range of latitudes.
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
    .[, hgt := Anomaly(hgt),
      by  = .(lat, time, lev)]

  if (temporal) {
    dt <- dt[, hgt := Anomaly(hgt), by = .(lon, lat, lev)]
  }

  dt %>%
    .[, hgt := hgt/sd(hgt), by = .(lev)] %>%
    .[, hgt := hgt*sqrt(cos(lat*pi/180))] %>%
    .[, hgt.cpx := spectral::analyticFunction(hgt),
      by = .(lat, time, lev)] %>%
    .[, hgt := hgt.cpx] %>%
    EOF(hgt ~ time | lon + lat + lev, n = n, suffix = "cEOF", data = .)
}

# compute_ceof <- function(dt, temporal = FALSE, lats = c(-80, -20), n = 1:2) {
#   # browser()
#   dt <- dt[lev %in% c(50, 200) & lat %between% lats] %>%
#     .[, hgt := hgt - mean(hgt),
#       by  = .(lat, year, lev)]
#
#   if (temporal) {
#     dt <- dt[, hgt := Anomaly(hgt), by = .(lon, lat, lev)]
#   }
#
#   dt %>%
#     .[, hgt := hgt/sd(hgt), by = .(lev)] %>%
#     .[, hgt := hgt*sqrt(cos(lat*pi/180))] %>%
#     .[, hgt.cpx := spectral::analyticFunction(hgt),
#       by = .(lat, year, lev)] %>%
#     .[, hgt := hgt.cpx] %>%
#     EOF(hgt ~ year | lon + lat + lev, n = n, suffix = "PC", data = .)
# }
