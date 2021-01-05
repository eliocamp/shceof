if (FALSE) {
lapl <- function(y, div)   {

  ddx <- t(apply(y, 1, function(x) metR:::.derv(x, matrix$coldims$lat,
                                                order = 2, cyclical = FALSE, fill = TRUE)))
  ddy <- apply(y, 2, function(x) metR:::.derv(x, matrix$rowdims$lon, order = 2, cyclical = TRUE))
  lap <- ddx + ddy + div


  return(dY)
}
library(metR)
data <- "~/DATOS/NCEP Reanalysis/psi.mon.mean.nc" %>%
  ReadNetCDF(subset = list(level = 0.1682,
                           time = c("2020-01-01"))) %>%
  .[, psi := Anomaly(psi), by = lat]




laplacian <- function(x, lon, lat) {
  xbk <- x
  x <- x$matrix
  prev <- function(N) c(N, seq_len(N)[-N])
  nex <- function(N) c(seq_len(N)[-1], 1)

  lon <- lon*pi/180
  lat <- (lat+90)*pi/180

  dlon <- diff(lon)[1]
  dlat <- diff(lat)[1]

  Nrow <- nrow(x)
  Ncol <- ncol(x)

  sinlat <- sin(lat)
  sinplus <- sin(lat + dlat/2)
  sinminus <- sin(lat - dlat/2)

  lap <- 1/dlat^2*(sinplus*(x[nex(Nrow), ] - x) - sinminus*(x - x[prev(Nrow), ])) +
    1/(dlon^2*sinlat^2)*(-2*x + x[, nex(Ncol)] + x[, prev(Ncol)])


  # lap[c(1, Nrow), ] <- lap[c(2, Nrow-1), ]
  xbk$matrix <- lap
  xbk
}

jacob_laplacian <- function(x, lon, lat, forcing) {
  xbk <- x
  x <- x$matrix
  prev <- function(N) c(N, seq_len(N)[-N])
  nex <- function(N) c(seq_len(N)[-1], 1)

  lon <- lon*pi/180
  lat <- (lat+90)*pi/180

  dlon <- diff(lon)[1]
  dlat <- diff(lat)[1]

  Nrow <- nrow(x)
  Ncol <- ncol(x)

  sinlat <- sin(lat)
  sinplus <- sin(lat + dlat/2)
  sinminus <- sin(lat - dlat/2)

  h <- (sinplus + sinminus)/(dlat^2*sinlat) + 2/(dlon^2*sinlat^2)


  approx <- 1/dlat^2*(sinplus*(x[nex(Nrow), ]) - sinminus*(0 - x[prev(Nrow), ])) +
    1/(dlon^2*sinlat^2)*(x[, nex(Ncol)] + x[, prev(Ncol)]) - forcing

  approx <- approx/h
  # browser()
  # approx[c(1, Nrow), ] <- lap[c(2, Nrow-1), ]
  xbk$matrix <- approx
  xbk
}

true <- metR:::.tidy2matrix(data, lat ~ lon, value.var = "psi")
vort <- laplacian(true, lon = true$coldims$lon, lat = true$rowdims$lat)
forcing <- vort


f <- forcing$matrix

colat <- abs(forcing$rowdims$lat - 90)*pi/180
lon <- forcing$coldims$lon*pi/180

M <- length(colat) - 1L
N <- length(lon) - 1L

TS <- as.single(min(colat))
TF <- as.single(max(colat))

# especify derivative of the solution ( = 0)
MBDCND <- 3L
BDTS <- as.single(rep(0., N+1))
BDTF <- as.single(rep(0., N+1))

PS <- as.single(min(lon))
PF <- as.single(max(lon))

# Periodic boundary condition
NBDCND <- 0L

BDPS <- 1L  # dummy value
BDPF <- 1L  # dummy value

# lalbda = 0 to get poisson equation
ELMBDA <- as.single(0.)

F1 <- as.single(f)


IDIMF <- M+1L

# Working memory
W <- rep(0., 4*(N+1) + (16 + (ceiling(log2(N+1))))*(M+1))

PERTRB <- 1L
IERROR <- 0L


res <- .Fortran("hwsssp_",
                TS,
                TF, M,MBDCND,BDTS,BDTF,
                PS,PF,N,NBDCND,BDPS,BDPF,ELMBDA,
                F = F1,IDIMF,PERTRB,IERROR= IERROR,W)

result <- f
result[, ] <- res$F

image(true$matrix)


data <- metR::ReadNetCDF(ERA5(), vars = "vo",
                         subset = list(level = 200))

data_trueish <- metR::ReadNetCDF("~/DATOS/NCEP Reanalysis/psi.mon.mean.nc" ,
                   subset = list(level = 0.1682,
                           time = range(data$time))) %>%
  .[, psi_z := metR::Anomaly(psi), by = .(time, lat)]


ble <- data[, solve_poisson(vo, longitude, latitude), by = time]
cor <- data[, solve_poisson(vo + metR::coriolis(latitude), longitude, latitude), by = time]

library(ggplot2)
data.table::setDT(ble)
ble[, value_z := value - mean(value), by = .(lat, time)]
cor[, value_z := value - mean(value), by = .(lat, time)]

ggplot(ble[time == lubridate::as_datetime("2000-01-01")], aes(lon, lat)) +
  geom_contour_filled(aes(z = value_z)) +
  geom_contour(data = data_trueish[time == lubridate::as_datetime("2000-01-01")],
               aes(z = psi, linetype = stat(factor(sign(..level..))))) +
  facet_grid(~factor(time))



x <- unique(data$longitude)
y <- unique(data$latitude)

data_trueish_interpolated <- data_trueish[, metR::Interpolate(psi ~ lon + lat, x.out = x, y.out = y), by = time]

all <- data_trueish_interpolated[cor, on = c("time", "lon", "lat")]

all %>% na.omit() %>% .[, cor(value, psi)]

all %>%
  .[is.finite(psi)] %>%
  .[, psi/value] %>%
  .[is.finite(.)] %>%
  mean

plot(as.vector(true$matrix), as.vector(result))

# cosvar <- cos(data[, get(ind.names[2])]*pi/180)

#
# a <- 6371000
# for (var in seq_along(dernames)) {
#   data[, dernames[[var]][1] := get(dernames[[var]][1])*(180/pi/(a*cosvar))^order[1]]
#   data[, dernames[[var]][2] := get(dernames[[var]][2])*(180/pi/a)^order[1]]

old <- true
old$matrix[, ] <- rnorm(ncol(old$matrix)*nrow(old$matrix))
# old <- true
times <- 1000
tol <- 0.01
change <- vector(length = times)
for (i in seq_len(times)) {
  new <- jacob_laplacian(old, lon = old$coldims$lon, lat = old$rowdims$lat, forcing$matrix)

  change[i] <- sqrt(sum((old$matrix - new$matrix)^2, na.rm = TRUE))

  old <- new

}

change[times]
image(forcing$matrix)
image(new$matrix)
image(true$matrix)
image(laplacian(new, lat = true$rowdims$lat, lon = true$coldims$lon)$matrix)

plot(as.vector(true$matrix), as.vector(new$matrix))

plot(as.vector(laplacian(new, lat = true$rowdims$lat, lon = true$coldims$lon)$matrix),
     as.vector(vort$matrix))

plot(change)


image(true$matrix)


converged
ggplot(data, aes(lon, lat)) +
  geom_raster(aes(fill = new)) +
  geom_contour2(aes(z = psi)) +
  scale_fill_divergent()


ggplot(data, aes(vort, new)) +
  geom_point()


# stodes is used, so we should specify the size of the work array (lrw)
# We take a rather large value
# By specifying pos = TRUE, only positive values are allowed.

# system.time(
  ST2 <- rootSolve::steady.2D(y_start, func = diffusion2D, parms = params, pos = TRUE,
                   dimens = c(ncol(y), nrow(y)), lrw = 1000000,
                   atol = 1e-10, rtol = 1e-10, ctol = 1e-10)
# )

}
