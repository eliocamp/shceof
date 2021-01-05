library(metR)
library(data.table)
library(magrittr)
library(here)


setnames <- function(x, ...) {
  names <- c(...)
  # print(names)
  data.table::setnames(x, names(names), unname(names))
}


subset_ncep <- list(lat = -90:0,
                    time = c("1979-01-01", "2018-12-31"))
subset_era <- list(latitude = -90:0,
                   time = c("1979-01-01", "2018-12-31"))

ncep <- ReadNetCDF(here("datos", "NCEP Reanalysis", "hgt.mon.mean.nc"),
                   subset = subset_ncep) %>% 
  .[, u := ReadNetCDF(here("datos", "NCEP Reanalysis", "uwnd.mon.mean.nc"),
                      out = "vector", 
                      subset = subset_ncep)] %>% 
  .[, dataset := "ncep"] %>% 
  setnames(., level = "lev")

era <- ReadNetCDF(here("datos", "ERA-Interim", "hgt.mon.mean.nc"),
                  c(hgt = "z"),
                  subset = subset_era) %>%
  .[, hgt := hgt/9.8] %>%
  .[, u := ReadNetCDF(here("datos", "ERA-Interim", "uwnd.mon.mean.nc"),
                      c(u = "u"),
                      out = "vector",
                      subset = subset_era)] %>%
  .[, dataset := "era"] %>%
  setnames(longitude = "lon", latitude = "lat", level = "lev")

datos <- rbind(era, ncep)
# datos <- ncep
# remove(ncep)
remove(era, ncep)

datos <- datos[, .(u = mean(u), hgt = mean(hgt)), 
               by = .(lev, lon, lat, year(time), season(time), dataset)]


saveRDS(datos, "datos/datos.Rda")
