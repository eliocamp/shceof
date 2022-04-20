#' Computes wave flux
#'
#' @param psi data.table with columns lon, lat and psi.z
#' @param p pressure level in hPa
#' @param a radius of the Earth in meters
#'
#' @export
WaveFlux <- function(psi, p = 250, a = 6371000) {
  k <- p*100/(a^2*2000)
  psi <- data.table::copy(psi)
  psi[, c("psi.dlon", "psi.dlat") := metR::Derivate(psi.z ~ lon + lat,
                                                    cyclical = c(TRUE, FALSE))] %>%
    .[, psi.ddlon := metR::Derivate(psi.z ~ lon, cyclical = TRUE, order = 2),
      by = lat] %>%
    .[, psi.dlondlat := metR::Derivate(psi.dlon ~ lat),
      by = lon] %>%
    .[, `:=`(f.lon = k/cos(lat*pi/180)*(psi.dlon^2 - psi.z*psi.ddlon),
             f.lat = k*(psi.dlon*psi.dlat - psi.z*psi.dlondlat))]
  list(f.lon = psi$f.lon, f.lat = psi$f.lat)
}
