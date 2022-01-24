library(metR)
library(data.table)
library(magrittr)
library(shceof)


enso <- rsoi::download_oni(TRUE, data_path("raw", "oni.csv")) %>%
  as.data.table() %>%
  .[, .(time = lubridate::as_datetime(Date), oni = ONI)] %>%
  na.omit() %>%
  .[season(time) == "SON"] %>%
  .[, oni := oni*season_weights(time)] %>%
  .[, .(oni = mean(oni)), by = .(time = seasonally(time))]


sst <- ERSST() %>%
  readRDS() %>%
  .[season(time) == "SON"] %>%
  normalise_coords() %>%
  .[, w := season_weights(time)] %>%
  .[, .(t = mean(t*w)), by = .(lon, lat, time = seasonally(time))]

x <- "irlba"
has_irlba <- requireNamespace(x, quietly = TRUE)
if (has_irlba) {
  stop("If installed, metR::EOF uses irlba, but that package has a bug with complex matrices\n(see: https://github.com/bwlewis/irlba/issues/55). Uninstall irlba with renv::remove(\"irlba\") and try again.")
}


cmap <- ReadNetCDF(CMAP(), vars = c(pp = "precip"),
                   subset = list(lat = -60:10)) %>%
  normalise_coords() %>%
  .[, w := season_weights(time)] %>%
  .[, .(pp = mean(pp*w)), by = .(lon, lat, time = seasonally(time))]

t2m <- ERA5_T2M() %>%
  ReadNetCDF() %>%
  normalise_coords() %>%
  .[, w := season_weights(time)] %>%
  .[, .(t2m = mean(t2m*w)), by = .(lon, lat, time = seasonally(time))]

era5 <- ERA5_geopotential() %>%
  ReadNetCDF(vars = c(hgt = "z"),
             subset = list(level = list(200, 50),
                           latitude = -90:10)) %>%
  normalise_coords() %>%
  .[season(time) == "SON"] %>%
  .[, w := season_weights(time)] %>%
  .[, .(hgt = mean(hgt*w)), by = .(lev, lon, lat, time = seasonally(time))]

# Scaling magical numbers. (come from psi-test.Rmd)
# Not really important, though.
scaling <- c(add = -2.364646e+06, mult = 3.954828e+13)
psi <- ERA5_vorticity() %>%
  ReadNetCDF(vars = "vo") %>%
  normalise_coords() %>%
  .[, lev := 200] %>%
  .[season(time) == "SON"] %>%
  .[, w := season_weights(time)] %>%
  .[, .(vo = mean(vo*w)), by = .(lon, lat, lev, time = seasonally(time))] %>%
  .[, solve_poisson(vo, lon, lat), by = .(lev, time)] %>%
  setnames("value", "psi") %>%
  .[, psi := psi*scaling[2] + scaling[1]] %>%
  .[, psi.z := Anomaly(psi), by = .(time, lev, lat)]


psa <- ERA5_geopotential() %>%
  ReadNetCDF(vars = c(hgt = "z"),
             subset = list(level = list(500),
                           latitude = -90:0)) %>%
  normalise_coords() %>%
  .[, w := season_weights(time)] %>%
  .[, .(hgt = mean(hgt*w)), by = .(lon, lat, time = seasonally(time))] %>%
  .[, value := Anomaly(hgt)*sqrt(cos(lat*pi/180)),
    by = .(lon, lat, season(time))] %>%
  EOF(value ~ time | lon + lat, n = 1:3, data = .)

psa <- lapply(psa, function(x) {
  copy(x)[, PC := fcase(PC == "PC1", "SAM",
                        PC == "PC2", "PSA1",
                        PC == "PC3", "PSA2")]
})


psa_time <- psa$left[season(time) == "SON"]




# Scaling magical numbers. (come from psi-test.Rmd)
# Not really important, though.
scaling <- c(add = -2.364646e+06, mult = 3.954828e+13)
psi <- ERA5_vorticity() %>%
  ReadNetCDF(vars = "vo") %>%
  normalise_coords() %>%
  .[, lev := 200] %>%
  .[season(time) == "SON"] %>%
  .[, w := season_weights(time)] %>%
  .[, .(vo = mean(vo*w)), by = .(lon, lat, lev, time = seasonally(time))] %>%
  .[, solve_poisson(vo, lon, lat), by = .(lev, time)] %>%
  setnames("value", "psi") %>%
  .[, psi := psi*scaling[2] + scaling[1]] %>%
  .[, psi.z := Anomaly(psi), by = .(time, lev, lat)]


psa <- ERA5_geopotential() %>%
  ReadNetCDF(vars = c(hgt = "z"),
             subset = list(level = list(500),
                           latitude = -90:0)) %>%
  normalise_coords() %>%
  .[, w := season_weights(time)] %>%
  .[, .(hgt = mean(hgt*w)), by = .(lon, lat, time = seasonally(time))] %>%
  .[, value := Anomaly(hgt)*sqrt(cos(lat*pi/180)),
    by = .(lon, lat, season(time))] %>%
  EOF(value ~ time | lon + lat, n = 1:3, data = .)

psa <- lapply(psa, function(x) {
  copy(x)[, PC := fcase(PC == "PC1", "SAM",
                        PC == "PC2", "PSA1",
                        PC == "PC3", "PSA2")]
})


psa_time <- psa$left[season(time) == "SON"]

ceof <- ERA5_geopotential() %>%
  ReadNetCDF(vars = c(hgt = "z"),
             subset = list(latitude = -90:10,
                           level = list(200, 50))) %>%
  normalise_coords() %>%
  .[season(time) == "SON"] %>%
  .[, w := season_weights(time)] %>%
  .[, .(hgt = mean(hgt*w)), by = .(lev, lon, lat, time = seasonally(time))] %>%
  .[, season := season(time)] %>%
  .[, .(eof = list(compute_ceof(.SD))), by = .(season)]

rotate <- function(z, angle = 0) {
  complex(real = cos(angle), imaginary = sin(angle)) * z
}

angles <- seq(-pi, pi, by = .5*pi/180)

# rotations_cEOF1 <- lapply(angles, function(a) {
#   ceof$eof[[1]]$left[cEOF == "cEOF1"] %>%
#     copy() %>%
#     .[, hgt := rotate(hgt, a)] %>%
#     sep_ReIm() %>%
#     .[, .(mean = mean(hgt)), by = part] %>%
#     .[, rotation := a]
# }) %>%
#   rbindlist()
#
#
# best_rotation_cEOF1 <- rotations_cEOF1[part == "Real"][which.min(mean)]$rotation



with_enso <-  cut(ceof$eof[[1]], 2)$left %>%
  copy() %>%
  .[enso, on = "time"] %>%
  na.omit()

rotations_cEOF2 <- lapply(angles, function(a) {
  with_enso %>%
    .[, hgt2 := rotate(hgt, a)] %>%
    .[, .(R = cor(Re(hgt2), oni),
          I = cor(Im(hgt2), oni))] %>%
    .[, rotation := a]
}) %>%
  rbindlist()

best_rotation_cEOF2 <- rotations_cEOF2[I > 0][which.min(abs(R))]$rotation


eof_rotated_left <- ceof$eof[[1]]$left %>%
  copy() %>%
  .[cEOF == "cEOF2", hgt := rotate(hgt, best_rotation_cEOF2)]

eof_rotated_right <-  ceof$eof[[1]]$right %>%
  copy() %>%
  .[cEOF == "cEOF2", hgt := rotate(hgt, best_rotation_cEOF2)]

ceof_unrotated <- copy(ceof)

ceof$eof[[1]]$left <- eof_rotated_left
ceof$eof[[1]]$right <- eof_rotated_right

ceof <- rbindlist(list(rotated = ceof,
                       unrotated = ceof_unrotated),
                  idcol = "rotation")

rotated <- function(x) x[rotation == "rotated"]
unrotated <- function(x) x[rotation == "unrotated"]

ceof <- rotated(ceof)



regr <- ceof[, eof[[1]]$left, by = .(season)] %>%
  .[season == "SON"] %>%
  sep_ReIm(format = "wide") %>%
  .[, hgt := NULL] %>%
  .[, `:=`(Real = as.numeric(scale(Real)), Imaginary = as.numeric(scale(Imaginary))),
    by = .(season, cEOF)] %>%
  .[melt(era5, id.vars = c("time", "lon", "lat", "lev")), on = "time", allow.cartesian = TRUE] %>%
  .[, FitLm(value, Imaginary, Real, se = TRUE),
    by = .(lev, lon, lat, variable, cEOF)] %>%
  rm_intercept() %>%
  .[, term := factor_ReIm(term)]



rotate_reim <- function(real, imaginary, angle = 0) {
  z <- complex(real = real, imaginary = imaginary)
  z <- rotate(z, angle)
  list(real = Re(z),
       imaginary = Im(z))
}

angles_regr <- -c(0, 45, 90, 45+90)*pi/180
angles_labs <- setNames(c("0º (~PSA2)", "45º", "90º (~PSA1)", "135º"), angles_regr*180/pi)

sst_for_regression <- ceof[, eof[[1]]$left, by = .(season)] %>%
  # .[cEOF == "cEOF2"] %>%
  copy() %>%
  sep_ReIm(hgt, "wide") %>%
  .[, hgt := NULL] %>%
  .[sst, on = "time", allow.cartesian = TRUE] %>%
  .[season(time) == "SON"] %>%
  na.omit()


sst_regr <- lapply(angles_regr, function(a) {
  sst_for_regression %>%
    copy() %>%
    .[, c("Real", "Imaginary") := rotate_reim(Real, Imaginary, a)] %>%
    .[, `:=`(Real = Real/sd(Real), Imaginary = Imaginary/sd(Imaginary)),
      by = .(season, cEOF)] %>%
    .[, FitLm(t, Imaginary, Real, se = TRUE), by = .(lon, lat, cEOF, season)] %>%
    rm_intercept() %>%
    .[term == "Real"] %>%
    .[, term := factor_ReIm(term)] %>%
    .[, angle := a*180/pi]
}) %>%
  rbindlist()

psi_for_regression <-  ceof[, eof[[1]]$left, by = .(season)] %>%
  # .[cEOF == "cEOF2"] %>%
  copy() %>%
  sep_ReIm(hgt, "wide") %>%
  .[, hgt := NULL] %>%
  .[psi, on = "time", allow.cartesian = TRUE] %>%
  .[season(time) == "SON"] %>%
  .[lev == 200]


psi_regr <- lapply(angles_regr, function(a) {
  psi_for_regression %>%
    copy() %>%
    .[, c("Real", "Imaginary") := rotate_reim(Real, Imaginary, a)] %>%
    .[, `:=`(Real = Real/sd(Real), Imaginary = Imaginary/sd(Imaginary)),
      by = .(season, cEOF)] %>%
    .[, FitLm(psi.z, Real, Imaginary, se = TRUE),by = .(lon, lat, lev, cEOF, season)] %>%
    rm_intercept() %>%
    .[term == "Real"] %>%
    .[, psi.z := Anomaly(estimate), by = .(cEOF, season, lev, lat)] %>%
    .[, c("fx", "fy") := shceof::WaveFlux(.SD, p = 200),
      by = .(lev, cEOF, season, term)] %>%
    .[, term := factor_ReIm(term)] %>%
    .[, angle := a*180/pi]
}) %>%
  rbindlist()



pp_for_regression <- ceof[, eof[[1]]$left, by = season] %>%
  # .[cEOF == "cEOF2"] %>%
  copy() %>%
  sep_ReIm(hgt, "wide") %>%
  .[, hgt := NULL] %>%
  .[cmap, on = "time", allow.cartesian = TRUE] %>%
  .[season(time) == "SON"] %>%
  na.omit() %>%
  .[, pp := pp/sd(pp), by = .(lon, lat)]

pp_regr <- lapply(angles_regr, function(a) {
  pp_for_regression %>%
    copy() %>%
    .[, c("Real", "Imaginary") := rotate_reim(Real, Imaginary, a)] %>%
    .[, `:=`(Real = Real/sd(Real), Imaginary = Imaginary/sd(Imaginary)),
      by = .(season, cEOF)] %>%
    .[, FitLm(pp, Imaginary, Real, se = TRUE),
      by = .(lon, lat, cEOF, season)] %>%
    .[term == "Real"] %>%
    rm_intercept() %>%
    .[, term := factor_ReIm(term)] %>%
    .[, angle := a*180/pi]

}) %>%
  rbindlist()


t2m_for_regression <-  ceof[, eof[[1]]$left, by = season] %>%
  # .[cEOF == "cEOF2"] %>%
  copy() %>%
  sep_ReIm(hgt, "wide") %>%
  .[, hgt := NULL] %>%
  .[t2m, on = "time", allow.cartesian = TRUE] %>%
  .[season(time) == "SON"] %>%
  na.omit()

t2m_regr <- lapply(angles_regr, function(a) {
  t2m_for_regression %>%
    copy() %>%
    .[, c("Real", "Imaginary") := rotate_reim(Real, Imaginary, a)] %>%
    .[, `:=`(Real = Real/sd(Real), Imaginary = Imaginary/sd(Imaginary)),
      by = .(season, cEOF)] %>%
    .[, FitLm(t2m, Imaginary, Real, se = TRUE),
      by = .(lon, lat, cEOF, season)] %>%
    .[term == "Real"] %>%
    rm_intercept() %>%
    .[, term := factor_ReIm(term)] %>%
    .[, angle := a*180/pi]

}) %>%
  rbindlist()



hgt850 <- ERA5_geopotential() %>%
  ReadNetCDF(vars = c(hgt = "z"),
             subset = list(level = 850)) %>%
  normalise_coords() %>%
  .[season(time) == "SON"] %>%
  .[, w := season_weights(time)] %>%
  .[, .(hgt = mean(hgt*w)), by = .(lon, lat, time = seasonally(time))]

hgt850 <- ceof[, eof[[1]]$left, by = .(season)] %>%
  .[season == "SON"] %>%
  sep_ReIm(format = "wide") %>%
  .[, hgt := NULL] %>%
  .[, `:=`(Real = as.numeric(scale(Real)), Imaginary = as.numeric(scale(Imaginary))),
    by = .(season, cEOF)] %>%
  .[hgt850, on = "time", allow.cartesian = TRUE]


hgt850_regr <- lapply(angles_regr, function(a) {
  hgt850 %>%
    copy() %>%
    .[, c("Real", "Imaginary") := rotate_reim(Real, Imaginary, a)] %>%
    .[, `:=`(Real = Real/sd(Real), Imaginary = Imaginary/sd(Imaginary)),
      by = .(season, cEOF)] %>%
    .[, FitLm(hgt, Imaginary, Real, se = TRUE), by = .(lon, lat, cEOF, season)] %>%
    rm_intercept() %>%
    .[term == "Real"] %>%
    .[, term := factor_ReIm(term)] %>%
    .[, angle := a*180/pi]
}) %>%
  rbindlist()
