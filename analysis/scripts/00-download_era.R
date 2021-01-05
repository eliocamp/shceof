library(lubridate)
library(ecmwfr)

# Todo esto no sirve! Baja ERA interim y hay que bajar ERA5

levs <- c(10, 20, 30, 50, 70, 100, 150, 200, 250, 300, 400, 500, 600, 700, 850,
          925, 1000)

dates <- seq.Date(as.Date("1979-01-01"), as.Date("2018-12-31"), "1 month")
dates <- paste0(year(dates),
                formatC(month(dates), width = 2, flag = "0"),
                formatC(day(dates), width = 2, flag = "0"),
                collapse = "/")

hgt_request <- list(
  class    = "ei",
  dataset  = "interim",
  date     = dates,
  expver   = "1",
  grid     = "2.5/2.5",
  levelist = levs,
  levtype  = "pl",
  param    = "129.128",
  stream   = "moda",
  type     = "an",
  format   = "netcdf",
  target   = "hgt.mon.mean.nc"
)

u_request <- list(
  class    = "ei",
  dataset  = "interim",
  date     = dates,
  expver   = "1",
  grid     = "2.5/2.5",
  levelist = levs,
  levtype  = "pl",
  param    = "131.128",
  stream   = "moda",
  type     = "an",
  format   = "netcdf",
  target   = "uwnd.mon.mean.nc"
)

path <- here::here("datos", "ERA-Interim")
wf_request(hgt_request, job_name = "hgt_era", path = path)
wf_request(u_request, job_name = "u_era", path = path)
