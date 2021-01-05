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
  .[, dataset := "ncep"] %>% 
  setnames(., level = "lev") %>% 
  .[lev %in% c(200, 50)]

era <- ReadNetCDF(here("datos", "ERA-Interim", "hgt.mon.mean.nc"),
                  c(hgt = "z"),
                  subset = subset_era) %>%
  .[, hgt := hgt/9.8] %>%
  .[, dataset := "era"] %>%
  setnames(longitude = "lon", latitude = "lat", level = "lev") %>% 
  .[lev %in% c(200, 50)]

era20 <- rbind(ReadNetCDF(here("datos", "ERA-20C", "hgt.mon.mean.nc"),
                          c(hgt = "z"),
                          subset = list(latitude = -90:0, 
                                        level = 50)),
               ReadNetCDF(here("datos", "ERA-20C", "hgt.mon.mean.nc"),
                          c(hgt = "z"),
                          subset = list(latitude = -90:0, 
                                        level = 200))) %>% 
  .[, hgt := hgt/9.8] %>%
  .[, dataset := "era20"] %>%
  setnames(longitude = "lon", latitude = "lat", level = "lev")

datos <- rbind(era20, era, ncep)
# datos <- ncep
# remove(ncep)
remove(era, ncep, era20)

datos <- datos[, .(hgt = mean(hgt)), 
               by = .(lev, lon, lat, year(time), season(time), dataset)]


saveRDS(datos, here("datos", "datos.Rds"))
